
#This script tests how to vary r and c in the strain calculation

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
  library(NetworkSpringEmbedding) #to load the package on my comp. on the HPC the non-package version is used
  
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
  load_file <- Sys.getenv("GRAPH_NAME") #file name of the graph to load
}


#Load some other useful functions
list.files(file.path(basewd, "Useful_PhD__R_Functions"), pattern = ".R", full.names = T) %>%
  walk(~source(.x))

list.files(file.path(basewd, "Flow_Spring_System"), pattern = ".R", full.names = T) %>%
  walk(~source(.x))

print("Load the parameter sheet")
parameter_df_temp <- generate_concentrator_parameters(load_file) %>% #arrange(compute_group) %>% #temporary to get timings
  filter(simulation_id ==1) %>% #only 1 sim from each set needs the strain calculated as they are all the same.
  select(-compute_group_strain)
#this dput structure is the parameters of the simulation that allow capacity values of 1 and Inf to converge, 
#this should reduce the probability of a network diverging for a given r c value by setting an appropriate coef_drag and tstep
parameter_df_temp <- expand.grid(r = 10^(1:5), c = 10^(1:5)) %>% 
  as_tibble() %>%
  mutate(
    compute_group_strain = 1:n(),
    simulation_id = 1) %>% 
  left_join(parameter_df_temp) %>%
  mutate(embeddings_path = gsub(".rds","", embeddings_path) %>%
           paste0(., "_r_", r, "_c_", c, ".rds")
  ) %>%
  filter(compute_group_strain == task_id) 


# temp <- parameter_df_temp %>%
#   filter(fraction == 1, carrying_capacity ==7, largest == 0.3, smallest == 0.5, robin_hood_mode == T)
print(paste("pararmeters loaded. Task number", task_id))

1:nrow(parameter_df_temp) %>%
  walk(~{
    #.x <- 1620
    ##
    ##
    ##This block below gets the variables necessary to perform the calculation
    ##
    ##
    Iter <- parameter_df_temp %>%
      slice(.x)
    #The paths that will be used in this analysis
    graph_path <- file.path(load_data_files_path, Iter$graph_path)
    Iter_collapse_path <- file.path(save_data_files_path, "collapse_sets", Iter$collapse_base)
    Iter_collapse_summary_path <- file.path(save_data_files_path, "collapse_summaries", Iter$collapse_base)
    Iter_embedding_path <- file.path(save_data_files_path, Iter$embeddings_path)
    
    
    ##
    ##
    ## Check to see if the file already exists if it does skip this iteration. This allows easier restarts
    ##
    ##
    
    # if(!file.exists(Iter_embedding_path)){
    
    ##
    ##
    ##Once the variables have been taken from the parameter file the calculation can begin
    ##
    ##
    
    g <- readRDS(file = graph_path) #read the target graph
    
    
    #Proportionally load the network and redistribute that load according to the parameter settings
    g <- g %>% Proportional_Load(., Iter$carrying_capacity , PowerFlow = "power_flow", "Link.Limit" = "edge_capacity") %>%
      redistribute_excess(.,
                          largest = Iter$largest,
                          smallest = Iter$smallest,
                          fraction = Iter$fraction,
                          flow = power_flow,
                          edge_capacity = edge_capacity,
                          robin_hood_mode = Iter$robin_hood_mode,
                          output_graph = TRUE)
    
    #testing the output
    #test <- as_data_frame(g)
    #test <- as_data_frame(current_graph)
    #ggplot(test, aes(x = ((power_flow)), y = k)) + geom_point()
    
    common_time <- 0.01
    common_Iter <- 200000
    common_tol <- 2e-3 #the residual staticforce that the system will terminate, it is effectively converged
    common_mass <- 1
    common_r <- Iter$r
    common_c <- Iter$c
    
    
    #Sets up the graph so that all the embedding stuff can be calculated without problem
    current_graph  <- g %>%
      set.edge.attribute(. , "distance", value = 1) %>%
      calc_spring_area(., "power_flow", minimum_value = sqrt(common_c), range = sqrt(common_r)) %>% #calculate a flexible Area
      calc_spring_youngs_modulus(., "power_flow", "edge_capacity", minimum_value = sqrt(common_c), stretch_range = sqrt(common_r)) %>%
      calc_spring_constant(., E ="E", A = "Area", distance = "distance") %>%
      #the other alternative where hypotenuse of the two values is taken not the area
      # calc_spring_area(., "power_flow", minimum_value = common_c, range = common_r) %>% #calculate a flexible Area
      # calc_spring_youngs_modulus(., "power_flow", "edge_capacity", minimum_value = common_c, stretch_range = common_r) %>%
      # calc_spring_constant(., E ="E", A = "Area", distance = "distance") %>%
      # set_edge_attr(., name = "k", value = sqrt(edge_attr(., name = "k"))) %>%
      normalise_dc_load(.,  
                        generation = "generation", 
                        demand  = "demand",
                        net_generation = "net_generation", 
                        capacity = "edge_capacity",
                        edge_name = "edge_name", 
                        node_name = "name",
                        power_flow = "power_flow")
    
    
    embeddings_data <- auto_SETSe(current_graph, 
                                  force ="net_generation", 
                                  flow = "power_flow", 
                                  distance = "distance", 
                                  capacity = "edge_capacity",
                                  edge_name = "edge_name",
                                  tstep = common_time, 
                                  mass = common_mass, 
                                  max_iter = common_Iter, 
                                  tol = common_tol,
                                  sparse = FALSE,
                                  hyper_iters = 200,
                                  hyper_tol = 0.01,
                                  hyper_max = 30000,
                                  sample = 100,
                                  verbose = F)
    
    #The structure is generated as needed and so any new paths can just be created at this point.
    #There is very little overhead in doing it this way
    if(!file.exists(dirname(Iter_embedding_path))){ dir.create(dirname(Iter_embedding_path), recursive = T) }
    print("Saving file")
    saveRDS(embeddings_data, file = Iter_embedding_path)
    #After the simulation and its summary are saved to the drive the next in the compute group is calculated
    #} 
    
    return(NULL) #dump everything when the loop finishes. This is an attempt to retain memory and speed up parallel processing... 
    #I don't know if it works
  }
  )

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