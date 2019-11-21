
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

#The compute group used for this calculation filters the parameter df down to groups which have equal
#amounts of networks from each load level. This allows for stable blocks of time.
compute_group_value <- task_id
print("Load the parameter sheet")
#The parameter file is generated here. It only takes 2 seconds so is not a problem for in code generation
parameter_df_temp <-  generate_concentrator_parameters(load_file) %>% #temporary to get timings
  filter(
    #simulation_id ==1,
    compute_group == compute_group_value) #this variable is inserted into the file
#To test this file add in a filter so that only simulation 1 is calculated.

print("run sims")
1:nrow(parameter_df_temp) %>%
  walk(~{
    
    ##
    ##
    ##This block below gets the variables necessary to perform the calculation
    ##
    ##
    Iter <- parameter_df_temp %>%
      slice(.x)
   # print("check paths")
    #The paths that will be used in this analysis
    graph_path <- file.path(load_data_files_path, Iter$graph_path)
    Iter_collapse_path <- file.path(save_data_files_path, "collapse_sets", Iter$collapse_base)
    Iter_collapse_summary_path <- file.path(save_data_files_path, "collapse_summaries", Iter$collapse_base)
   

    ##
    ##
    ## Check to see if the file already exists if it does skip this iteration. This allows easier restarts
    ##
    ##
    
    if(!file.exists(Iter_collapse_path)){
      

      #read the target graph
      g <- readRDS(file = graph_path) 
      
      #Proportionally load the network and redistribute that load according to the parameter settings
      g <- g %>% Proportional_Load(., Iter$carrying_capacity, PowerFlow = "power_flow", "Link.Limit" = "edge_capacity") %>%
        redistribute_excess(., 
                            largest = Iter$largest, 
                            smallest = Iter$smallest, 
                            fraction = Iter$fraction, 
                            flow = power_flow, 
                            edge_capacity = edge_capacity,
                            robin_hood_mode = Iter$robin_hood_mode,
                            output_graph = TRUE)
      
      #Set the seed for the random order and generate the edge deletion order
      set.seed(Iter$deletion_seed)
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
          simulation_id = Iter$simulation_id,
          carrying_capacity = Iter$carrying_capacity,
          smallest = Iter$smallest,
          largest = Iter$largest,
          fract = Iter$fraction,
          robin_hood_mode = Iter$robin_hood_mode,
          graph = Iter$graph
        ) 
      
      
      #Both the attack series and the summary are saved to store as much information as possible in one go.
      
      #The structure is generated as needed and so any new paths can just be created at this point.
      #There is very little overhead in doing it this way
      #print("save results")
      c(dirname(Iter_collapse_path), dirname(Iter_collapse_summary_path) ) %>% walk(~{
        if(!file.exists(.x)) dir.create(.x, recursive = T)
      })
      
      saveRDS(AttackSeries, file = Iter_collapse_path)
      
      saveRDS(AttackSeriesSummary, file = Iter_collapse_summary_path)
      #After the simulation and its summary are saved to the drive the next in the compute group is calculated
    } 
    
    return(NULL) #dump everything when the loop finishes. This is an attempt to retain memory and speed up parallel processing... 
    #I don't know if it works
  })

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