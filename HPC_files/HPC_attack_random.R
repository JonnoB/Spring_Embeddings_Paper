
###########################
###########################
# # 
# # MYRIAD RUN SCRIPT
# # 
# # This is an R script that runs a single attack to failure accroding to a set of input parameters. It is designed to be run on
# # an HPC cluster, allowing each attack to run independtly when scheduled
# # 
# # A question is how much time is spent loading the necessary packages and data as this may mean that several attacks should be combined
# # to reduce the overhead.
# # 
# # 
###########################
###########################

######
######
##
##This script allow large numbers of random attacks on a graph to be distributed across many cpus. 
## The file out puts 4 files the failure mode df for nodes and edges the attack summary file and a file containing 4 co-failure matrices
##
######
######


packages <- c("rlang", "dplyr", "tidyr", "purrr", "tibble", "forcats", "igraph", "devtools", "minpack.lm")

new.packages <- packages[!(packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

sapply(packages, library, character.only = TRUE)



#install_github("JonnoB/PowerGridNetworking")
library(PowerGridNetworking)

#Set up file system to read the correct folders this switches between aws and windows mode

#an example file to load
#load_file <- "UK_high_voltage"
#load_file <- "IEEE_118_igraph"

#creates the correct root depending on whether this is on the cloud or not
if(dir.exists("/home/jonno")){
  #This folder is for use on my machine
  project_folder <- "/home/jonno/Dropbox/IEEE_Networks"
  basewd <- "/home/jonno"
  load_data_files_path <- file.path(project_folder) #load the files
  save_data_files_path <- file.path(project_folder, "ten_k_attacks", load_file) #save the files
}else{
  #This is for the folder that is on the cloud
  project_folder <- getwd()
  basewd <- "/home/ucabbou"
  load_data_files_path <- file.path(basewd) #load the files
  save_data_files_path <- file.path(project_folder) #save the files
  
  #If it is not on my computer then the variables need to be loaded from the system environment
  #Get the task ID
  task_id <- Sys.getenv("SGE_TASK_ID")
  load_file <- Sys.getenv("GRAPH_NAME") #file name of the graph to load
}

#Load some other useful functions
list.files(file.path(basewd, "Useful_PhD__R_Functions"), pattern = ".R", full.names = T) %>%
  walk(~source(.x))

list.files(file.path(basewd, "Flow_Spring_System"), pattern = ".R", full.names = T) %>%
  walk(~source(.x))


graph_path <- file.path(load_data_files_path, "power_grid_graphs", paste0(load_file , ".rds"))

#read the target graph
g <- readRDS(file = graph_path) 

tot_nodes <- vcount(g)
tot_edges <- ecount(g)

#Make the co failure matrices as integer matrices of 0 for which the simulations are added on top
node_co_failure <- matrix(0L, ncol = tot_nodes, nrow = tot_nodes)
node_co_failure_no_target <-  matrix(0L, ncol = tot_nodes, nrow = tot_nodes)
edge_co_failure <-  matrix(0L, ncol = tot_edges, nrow = tot_edges)
edge_co_failure_no_target <- matrix(0L, ncol = tot_edges, nrow = tot_edges)

AttackSeriesSummary_list <- list()
edge_failure_mode <- list()
node_failure_mode <- list()

start_time <- Sys.time()
#only 10 to act as a test
#for(i in 1:10){
for(i in (1:10000)[rep(1:10, 1000)==task_id]){
  #print(i)
  #Set the seed for the random order and generate the edge deletion order
  set.seed(i)
  #when deleteing edges, any node that has both generation and demand will always survive to the end
  DeletionOrder <- RandomAttack(g, Target = "Edges", Number = ecount(g), Name = "edge_name")
  #Construction the deletion order function.
  
  if(!("edge_capacity" %in% edge_attr_names(g))){
    #if the IEEE networks are loaded they are automatically given PL value of 2.
    #The IEEE networks don't have an edge capacity value so that is why they can be identified
    g <- Proportional_Load(g, alpha = 2, PowerFlow = "power_flow", Link.Limit = "edge_capacity")
    
  }
  
  FixedNodes <- quo(FixedStrategyAttack(g, DeletionOrder, "Edge", Name =  "edge_name"))
  # print("attack grid")
  AttackSeries <- attack_the_grid(g = g,
                                  AttackStrategy = FixedNodes,
                                  g0 = NULL,
                                  TotalAttackRounds = 1000,
                                  CascadeMode = TRUE,
                                  Demand = "demand",
                                  Generation = "generation",
                                  EdgeName = "edge_name",
                                  VertexName = "name",
                                  Net_generation = "net_generation",
                                  power_flow = "power_flow",
                                  edge_capacity = "edge_capacity",
                                  target = "edges")
  
  #print("Extract summaries")
  #This is because doing all the extraction at the end would take a very long time, so I break it up over all the jobs.
  #This way I only need to combine the saved files but not operate on them.
  AttackSeriesSummary <- AttackSeries %>% 
    extract_network_stats(.) %>%
    #I add in the attack parameters for conveniance so I don't need to do it when I load the data.
    #This is just as joining a very large table can be slow
    mutate(
      simulation_id = i,
      graph = load_file
    ) 
  
  AttackSeriesSummary_list[[i]] <- AttackSeriesSummary
  
  #rowSums(edge_failure_round) %>% table
  co_failure_list <- network_co_failure(AttackSeries)
  
  
  #Stack the co-failure matrices
  node_co_failure <- node_co_failure + co_failure_list$node_co_failure
  node_co_failure_no_target <- node_co_failure_no_target + co_failure_list$node_co_failure_no_target
  edge_co_failure <- edge_co_failure + co_failure_list$edge_co_failure
  edge_co_failure_no_target <- edge_co_failure_no_target + co_failure_list$edge_co_failure_no_target
  
  #extend the list of the failure mode dataframes
  edge_failure_mode[[i]] <- co_failure_list$edge_failure_mode  %>%
    mutate(
      simulation_id = i,
      graph = load_file
    ) 
  
  node_failure_mode[[i]] <- co_failure_list$node_failure_mode  %>%
    mutate(
      simulation_id = i,
      graph = load_file
    ) 
  
  
}

#The combined files are saved, this greatly reduces the amount of files saved an the subseuent processing time

#The node co failure matrices recombined into lists
co_failure_list_out <-list(node_co_failure = node_co_failure,
     node_co_failure_no_target = node_co_failure_no_target,
     edge_co_failure = edge_co_failure,
     edge_co_failure_no_target = edge_co_failure_no_target)

#collapse the lists to a single data frame
AttackSeriesSummary_list <- bind_rows(AttackSeriesSummary_list)
edge_failure_mode <- bind_rows(edge_failure_mode)
node_failure_mode <- bind_rows(node_failure_mode)


summary_save_path <- file.path(save_data_files_path, "collapse_summaries", paste0("task_id_", task_id, ".rds"))
if(!file.exists(save_data_files_path)){ dir.create(save_data_files_path, recursive = T)}

saveRDS(AttackSeriesSummary_list, 
        file = file.path(save_data_files_path, paste0("collapse_summaries_task_id_", task_id, ".rds")))
saveRDS(edge_failure_mode, 
        file = file.path(save_data_files_path, paste0("edge_failure_mode_task_id_", task_id, ".rds")))
saveRDS(node_failure_mode, 
        file = file.path(save_data_files_path, paste0("node_failure_mode_task_id_", task_id, ".rds")))

saveRDS(co_failure_list_out, 
        file = file.path(save_data_files_path, paste0("co_failure_task_id_", task_id, ".rds")))

stop_time <- Sys.time()

print(paste0("time taken for attacks ", round(difftime(stop_time, start_time, units = "mins"), 2), " minutes"))

#Once the attack has completed the script terminates

#######################
#######################
##
##
##END
##
##
########################
########################