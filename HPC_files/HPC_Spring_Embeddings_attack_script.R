
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

packages <- c("rlang", "dplyr", "tidyr", "purrr", "tibble", "forcats", "igraph")#, "devtools", "minpack.lm" )#this may be can be removeed

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
  analysis_parameter_file_path <- file.path(project_folder, "analysis_parameter_files")
  HPC_script_path <- file.path(project_folde/tmpdir/job/2059520.10/r, "HPC_parameter_files")
  load_data_files_path <- file.path(project_folder) #load the files
  save_data_files_path <- file.path(project_folder) #save the files
}else{
  #This is for the folder that is on the cloud
  project_folder <- getwd()
  basewd <- "/home/ucabbou"
  analysis_parameter_file_path <- file.path(basewd, "analysis_parameter_files") #In myriad the parameter files are in a folder on the home dir
  #not in the project folder like when it is done on my own comp
  HPC_script_path <- file.path(basewd, "HPC_parameter_files")
  load_data_files_path <- file.path(basewd) #load the files
  save_data_files_path <- file.path(project_folder) #save the files

  
  #If it is not on my computer then the variables need to be loaded from the system environment
  #Get the task ID
  task_id <- Sys.getenv("SGE_TASK_ID")
  HPC_start_up_file <- Sys.getenv("HPC_start_up_file")
  }


#Load some other useful functions
list.files(file.path(basewd, "Useful_PhD__R_Functions"), pattern = ".R", full.names = T) %>%
  walk(~source(.x))

list.files(file.path(basewd, "Flow_Spring_System"), pattern = ".R", full.names = T) %>%
  walk(~source(.x))

temp <- read.delim(file.path(HPC_script_path, HPC_start_up_file), sep =" ", header = TRUE) %>%
  filter(compute_group == task_id)

compute_group_value <- temp$compute_group
#compute_group <- temp$compute_group #as the df variable is called compute group, this cannot be otherise the variable just has to equal itself and the grouping is not used

load_file <- temp$load_file

parameter_df_temp <- readRDS(file.path(analysis_parameter_file_path, load_file)) %>% #arrange(compute_group) %>% #temporary to get timings
  filter(compute_group == compute_group_value) %>% #this variable is inserted into the file
  ungroup %>%
  group_by(ec) %>%
  mutate(outer_loop = 1:n()) %>%
  ungroup %>%
  arrange(outer_loop)

1:nrow(parameter_df_temp) %>%
  walk(~{
    
    ##
    ##
    ##This block below gets the variables necessary to perform the calculation
    ##
    ##
    Iter <- parameter_df_temp %>%
      slice(.x)
    
    scramble_network <- Iter$scramble_network #Needs to be added into the target orders file
    graph_path <- file.path(load_data_files_path, Iter$graph_path)
    Iter_collapse_path <- file.path(save_data_files_path, Iter$collapse_path)
    Iter_collapse_summary_path <- file.path(save_data_files_path, Iter$collapse_summary_path)
    scramble_seed <- Iter$seed
    ec <- Iter$ec
    v <- Iter$v
    fract <- Iter$fract
    permutation <- Iter$permutation
    deletion_seed <- Iter$deletion_seed
    simulation_id <- Iter$simulation_id


    ##
    ##
    ## Check to see if the file already exists if it does skip this iteration. This allows easier restarts
    ##
    ##
    
    if(!file.exists(Iter_collapse_path)){
      
      ##
      ##
      ##Once the variables have been taken from the parameter file the calculation can begin
      ##
      ##
      
      g <- readRDS(file = graph_path) #read the target graph
      
      
      #Proportionally load the network
      g <- Proportional_Load(g, alpha = ec, PowerFlow = "power_flow",
                             Link.Limit = "edge_limit")
      
      #For most of the simulations we will scramble the proportionally loaded networks,
      #However when we are calculating the limits of 1 and Inf as well as the proportional line for the test cases
      #scrambling is not required. As a result these lines of code make the overall script more flexible.
      if(scramble_network){
        #scramble the excess capaacity
        edge_order_df <- create_scrambled_edges(g, scramble_seed, fract = fract)
        
        g <- g %>%
          set.edge.attribute(., "edge_limit", value = edge_order_df$edge_limit)
        
      }
      
      #Set the seed for the random order and generate the edge deletion order
      set.seed(deletion_seed)
      DeletionOrder <- RandomAttack(g, Target = "Edges", Number = ecount(g), Name = "edge_name")
      #Construction the deletion order function.
      FixedNodes <- quo(FixedStrategyAttack(g, DeletionOrder, "Edge", Name =  "edge_name"))
      
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
                                                     edge_limit = "edge_limit"))
      
      
      #This is because doing all the extraction at the end would take a very long time, so I break it up over all the jobs.
      #This way I only need to combine the saved files but not operate on them.
      AttackSeriesSummary <- AttackSeries %>%
        ExtractNetworkStats(Generation = "generation", EdgeName = "edge_name", PowerFlow = "power_flow", Link.Limit = "edge_limit") %>%
        #I add in the attack parameters for conveniance so I don't need to do it when I load the data.
        #This is just as joining a very large table can be slow
        mutate(simulation_id = simulation_id,
               ec = ec,
               v = v,
               fract = fract,
               permutation = permutation) 
      
      
      #Both the attack series and the summary are saved to store as much information as possible in one go.
      
      #The structure is generated as needed and so any new paths can just be created at this point.
      #There is very little overhead in doing it this way
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