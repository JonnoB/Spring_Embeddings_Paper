
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

packages <- c("rlang", "dplyr", "tidyr", "purrr", "tibble", "forcats", "igraph", "devtools", "minpack.lm")#, "devtools", "minpack.lm" )#this may be can be removeed

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
  HPC_script_path <- file.path(project_folder, "HPC_parameter_files")
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
  filter(compute_group_strain == task_id)

compute_group_value <- temp$compute_group_strain
#compute_group <- temp$compute_group #as the df variable is called compute group, this cannot be otherise the variable just has to equal itself and the grouping is not used
param_v <- temp$v
param_ec <- temp$ec
param_fract <- temp$fract
load_file <- temp$load_file

parameter_df_temp <- readRDS(file.path(analysis_parameter_file_path, load_file)) %>% #arrange(compute_group) %>%#temporary to get timings
  filter(
    compute_group_strain == compute_group_value, #this variable is inserted into the file from the program that runs it 
    v == param_v,
    ec == param_ec,
    fract == param_fract,
    simulation_id ==1, #This value is set to 1 as there are 100 simulations all using an identical graph, we only need 1 of them.
  )

print(paste("pararmeters loaded. Task number", task_id))

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
    Iter_embedding_path <- file.path(save_data_files_path, Iter$embeddings_path)
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
    
    if(!file.exists(Iter_embedding_path)){
      
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
      common_time <- 0.01
      common_Iter <- 20000
      common_tol <- 1e-10
      common_mass <- 1
      
      #Sets up the graph so that all the embedding stuff can be calculated without problem
      current_graph  <- g %>%
        set.edge.attribute(. , "distance", value = 1) %>%
        set.edge.attribute(., "Area", value = 1) %>%
        calc_spring_youngs_modulus(., "power_flow", "edge_limit", minimum_value = 100, stretch_range = 1000) %>%
        calc_spring_constant(., E ="E", A = "Area", distance = "distance") %>%
        normalise__dc_load(.,  
                           generation = "generation", 
                           demand  = "demand",
                           net_generation = "net_generation", 
                           capacity = "edge_limit",
                           edge_name = "edge_name", 
                           node_name = "name",
                           power_flow = "power_flow")
      
      print("Full graph complete")
      
      List_of_BiConComps <- create_balanced_blocks(current_graph, 
                                                   force = "net_generation", 
                                                   flow = "power_flow")
      
      #find the largest component and use that as the origin block
      giant_componant <-List_of_BiConComps %>% map_dbl(~vcount(.x)) %>% which.max()
      
      print("Giant component found")
      
      #use the largest block to set the simulation parameters k and m.
      #k needs to be sufficiently stretched to allow enough topology variation. otherwise all that happens is a 
      #surface angled in the direct of net power flow. Which is interesting but not that interesting
      OriginBlock_complete <- Find_network_balance(g = List_of_BiConComps[[giant_componant]],
                                                   force ="net_generation",
                                                   flow = "power_flow",
                                                   distance = "distance",
                                                   capacity = "edge_limit",
                                                   edge_name = "edge_name",
                                                   tstep = common_time,
                                                   tol = common_tol,
                                                   maxIter = common_Iter,
                                                   mass = common_mass,
                                                   verbose = FALSE)
      
      print("Origin block complete")
      
      #Calculate the height embeddings using the Orgin block as a base
      height_embeddings_df <- Create_stabilised_blocks(g = current_graph,
                                                       OriginBlock = OriginBlock_complete,
                                                       OriginBlock_number = giant_componant,
                                                       force ="net_generation",
                                                       flow = "power_flow",
                                                       distance = "distance",
                                                       capacity = "edge_limit",
                                                       edge_name = "edge_name",
                                                       tstep = common_time,
                                                       tol = common_tol,
                                                       maxIter = common_Iter,
                                                       mass = common_mass,
                                                       verbose = FALSE)
      
      print("Height embeddings complete")
      
      #Extract edge tension and strain from the network
      tension_strain_embeddings <- calc_tension_strain(g = current_graph,
                                                       height_embeddings_df,
                                                       distance = "distance", 
                                                       capacity = "edge_limit", 
                                                       flow = "power_flow", 
                                                       edge_name = "edge_name", 
                                                       k = "k")
      
      print("Strain and Tension embeddings complete")
      embeddings_data <- list(node_embeddings = height_embeddings_df, edge_embeddings = tension_strain_embeddings)
      
      #The structure is generated as needed and so any new paths can just be created at this point.
      #There is very little overhead in doing it this way
      if(!file.exists(dirname(Iter_embedding_path))){ dir.create(dirname(Iter_embedding_path), recursive = T) }
      print("Saving file")
      saveRDS(embeddings_data, file = Iter_embedding_path)
      #After the simulation and its summary are saved to the drive the next in the compute group is calculated
    } 
    
      return(NULL) #dump everything when the loop finishes. This is an attempt to retain memory and speed up parallel processing... 
    #I don't know if it works
  })

stop_time <- Sys.time()

print(stop_time-start_time)

#Once all the simulations in the compute group have been saved the script is complete

#######################
#######################
##
##
##END
##
##
########################
########################