
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
##This script allow large numbers of random attacks on a graph to be distributed across many cpus. The networks need to have
## had the edge limits predefined on loading. 
##
######
######

start_time <- Sys.time()

packages <- c("rlang", "dplyr", "tidyr", "purrr", "tibble", "forcats", "igraph", "devtools", "minpack.lm")

new.packages <- packages[!(packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

sapply(packages, library, character.only = TRUE)



#install_github("JonnoB/PowerGridNetworking")
library(PowerGridNetworking)

#Set up file system to read the correct folders this switches between aws and windows mode

#creates the correct root depending on whether this is on the cloud or not
if(dir.exists("/home/jonno")){
  #This folder is for use on my machine
  project_folder <- "/home/jonno/Dropbox/IEEE_Networks"
  basewd <- "/home/jonno"
  load_data_files_path <- file.path(project_folder) #load the files
  save_data_files_path <- file.path(project_folder) #save the files
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
      
      for(i in (1:10000)[rep(1:100, 100)==task_id]){
      
      #Set the seed for the random order and generate the edge deletion order
      set.seed(i)
      DeletionOrder <- RandomAttack(g, Target = "Edges", Number = ecount(g), Name = "edge_name")
      #Construction the deletion order function.
      FixedNodes <- quo(FixedStrategyAttack(g, DeletionOrder, "Edge", Name =  "edge_name"))
     # print("attack grid")
      AttackSeries <- suppressMessages(AttackTheGrid(NetworkList = list(list(g)),
                                                     AttackStrategy = FixedNodes,
                                                     referenceGrid = NULL,
                                                     MinMaxComp = 0.0,
                                                     TotalAttackRounds = 1000,
                                                     CascadeMode = TRUE,
                                                     CumulativeAttacks = NULL,
                                                     Demand = "demand",
                                                     Generation = "generation",
                                                     EdgeName = "edge_name",
                                                     VertexName = "name",
                                                     Net_generation = "net_generation",
                                                     power_flow = "power_flow",
                                                     edge_limit = "edge_capacity"))
      
      #print("Extract summaries")
      #This is because doing all the extraction at the end would take a very long time, so I break it up over all the jobs.
      #This way I only need to combine the saved files but not operate on them.
      AttackSeriesSummary <- AttackSeries %>%
        ExtractNetworkStats(Generation = "generation", 
                            EdgeName = "edge_name", 
                            PowerFlow = "power_flow", 
                            Link.Limit = "edge_capacity") %>%
        #I add in the attack parameters for conveniance so I don't need to do it when I load the data.
        #This is just as joining a very large table can be slow
        mutate(
          simulation_id = i,
          graph = load_file
        ) 
      
      
      #Both the attack series and the summary are saved to store as much information as possible in one go.
      
      #The structure is generated as needed and so any new paths can just be created at this point.
      #There is very little overhead in doing it this way
      #print("save results")
      
      summary_save_path <- file.path(save_data_files_path, "collapse_summaries", paste0("simulation_id_", i, ".rds"))
        if(!file.exists(dirname(summary_save_path))){ dir.create(dirname(summary_save_path), recursive = T)}
      print(summary_save_path)
      #This is not saved as it takes up so much space
      #saveRDS(AttackSeries, file = Iter_collapse_path)
      
      saveRDS(AttackSeriesSummary, file = summary_save_path)
      #After the simulation and its summary are saved to the drive the next in the compute group is calculated
    }

stop_time <- Sys.time()

print(stop_time-start_time)
  
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