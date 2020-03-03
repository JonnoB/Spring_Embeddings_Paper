
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
start_time <- Sys.time()

packages <- c("rlang", "dplyr", "tidyr", "purrr", "tibble", "forcats", "igraph", "devtools", "minpack.lm", "devtools")

new.packages <- packages[!(packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

sapply(packages, library, character.only = TRUE)



#install_github("JonnoB/PowerGridNetworking")


#Set up file system to read the correct folders this switches between aws and windows mode

#creates the correct root depending on whether this is on the cloud or not
if(dir.exists("/home/jonno")){
  #This folder is for use on my machine
  project_folder <- "/home/jonno/Dropbox/IEEE_Networks"
  basewd <- "/home/jonno"
  load_data_files_path <- file.path(project_folder) #load the files
  save_data_files_path <- file.path(project_folder, "attacks", "PL",load_file) #save the files
}else{
  #If it is not on my computer then the variables need to be loaded from the system environment
  #Get the task ID
  task_id <- Sys.getenv("SGE_TASK_ID")
  load_file <- Sys.getenv("GRAPH_NAME") #file name of the graph to load
  
  #This is for the folder that is on the cloud
  project_folder <- getwd()
  basewd <- "/home/ucabbou"
  load_data_files_path <- file.path(basewd) #load the files
  save_data_files_path <- file.path(project_folder, "collapse_summaries", load_file)#save the files
  
  #  install(file.path("~/PowerGridNetworking"))
}

library(PowerGridNetworking)

#the list all the attacks are stored in before being combined into a single dataframe
AttackSeriesSummary_list <- list()

#Load some other useful functions
list.files(file.path(basewd, "Useful_PhD__R_Functions"), pattern = ".R", full.names = T) %>%
  walk(~source(.x))

list.files(file.path(basewd, "Flow_Spring_System"), pattern = ".R", full.names = T) %>%
  walk(~source(.x))

#The compute group used for this calculation filters the parameter df down to groups which have equal
#amounts of networks from each load level. This allows for stable blocks of time.
compute_group_value <- task_id
print(paste0("Target graph ", load_file, " Compute group ", compute_group_value))
print("Load the parameter sheet")
#The parameter file is generated here. It only takes 2 seconds so is not a problem for in code generation
parameter_df_temp <-  generate_pl_parameters(load_file) 

print(paste0("run ", nrow(parameter_df_temp), " sims"))

for(i in (1:nrow(parameter_df_temp))){
  #for(i in 1:10){ #test purposes
  start_iter_time <- Sys.time()
  ##
  ##
  ##This block below gets the variables necessary to perform the calculation
  ##
  ##
  Iter <- parameter_df_temp %>%
    slice(i)
  # print("check paths")
  #The paths that will be used in this analysis
  graph_path <- file.path(load_data_files_path, Iter$graph_path)
  Iter_collapse_path <- file.path(save_data_files_path, "collapse_sets", Iter$collapse_base)
  Iter_collapse_summary_path <- file.path(save_data_files_path, "collapse_summaries", Iter$collapse_base)
  
  #read the target graph
  g <- readRDS(file = graph_path) 
  
      
      #Proportionally load the network and redistribute that load according to the parameter settings
      g <- g %>% Proportional_Load(., Iter$carrying_capacity, PowerFlow = "power_flow", "Link.Limit" = "edge_capacity")
      
      #Set the seed for the random order and generate the edge deletion order
      set.seed(Iter$deletion_seed)
      DeletionOrder <- RandomAttack(g, Target = "Edges", Number = ecount(g), Name = "edge_name")
      #Construction the deletion order function.
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
      
      #I really only need a very small amount of information from the attack. That is the round the gc was lost
      AttackSeriesSummary <- AttackSeries %>% 
        extract_network_stats(.) %>%
        filter(attack_round == AttackSeries$gc_loss_round) %>%
        #I add in the attack parameters for conveniance so I don't need to do it when I load the data.
        #This is just as joining a very large table can be slow
        mutate(
          simulation_id = Iter$simulation_id,
          carrying_capacity = Iter$carrying_capacity,
          graph = Iter$graph
        ) 
      
      AttackSeriesSummary_list[[i]] <- AttackSeriesSummary
      
      stop_iter_time <- Sys.time()
      #iteration time
      print( stop_iter_time-start_iter_time )
} 


AttackSeriesSummary_list <- bind_rows(AttackSeriesSummary_list)

#The bash script determines that the files will be saved inside a folder named after the graph
#so the files here only need to be named by the task ID.
#It is then also easier to find files that are missing and possibly just re-run them a normal machine

if(!file.exists(save_data_files_path)) dir.create(save_data_files_path, recursive = T)
saveRDS(AttackSeriesSummary_list, file = file.path(save_data_files_path, paste0("task_id_", task_id, ".rds")))
#After the simulation and its summary are saved to the drive the next in the compute group is calculated

stop_time <- Sys.time()

print(stop_time-start_time)

#Once all the simulations in the compute group have been saved the script is complete

#                                  !!!!!!!!!!!!!!!!!!!!!N.B.!!!!!!!!!!!!!!!!!
#when this script is run on the HPC the shell script will tar up all the files and export them from the temp directory to the Scratch directory
#Becuase of this is it importand that if a folder is being generated, the entire contents of the folder should be generated
#during a single array task. otherwise folders with the same name will be exported and could overwrite previous work.

#######################
#######################
##
##
##END
##
##
########################
########################