
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
  
  #load_file <- "IEEE_14_igraph"
  
  project_folder <- "/home/jonno/Dropbox/IEEE_Networks"
  basewd <- "/home/jonno"
  load_data_files_path <- file.path(project_folder) #load the files
  save_data_files_path <- file.path(project_folder, "embeddings", "PL",load_file) #save the files
  
  library(PowerGridNetworking)
}else{
  #If it is not on my computer then the variables need to be loaded from the system environment
  #Get the task ID
  task_id <- Sys.getenv("SGE_TASK_ID")
  load_file <- Sys.getenv("GRAPH_NAME") #file name of the graph to load
  
  #This is for the folder that is on the cloud
  project_folder <- getwd()
  basewd <- "/home/ucabbou"
  load_data_files_path <- file.path(basewd) #load the files
  save_data_files_path <- file.path(project_folder, load_file)#save the files
  
  #  install(file.path("~/PowerGridNetworking"))
  
  #Load some other useful functions
  list.files(file.path(basewd, "Useful_PhD__R_Functions"), pattern = ".R", full.names = T) %>%
    walk(~source(.x))
  
  list.files(file.path(basewd, "Flow_Spring_System"), pattern = ".R", full.names = T) %>%
    walk(~source(.x))
}

#the list all the attacks are stored in before being combined into a single dataframe
AttackSeriesSummary_list <- list()



#The compute group used for this calculation filters the parameter df down to groups which have equal
#amounts of networks from each load level. This allows for stable blocks of time.
compute_group_value <- task_id
print(paste0("Target graph ", load_file, " Compute group ", compute_group_value))
print("Load the parameter sheet")
#The parameter file is generated here. It only takes 2 seconds so is not a problem for in code generation
parameter_df_temp <-  generate_pl_parameters(load_file) %>%
  filter(simulation_id ==1)

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
  Iter_embedding_path <- file.path(save_data_files_path, basename(Iter$embeddings_path))
  
  #read the target graph
  g <- readRDS(file = graph_path) 
  
  
  #Proportionally load the network and redistribute that load according to the parameter settings
  g <- g %>% Proportional_Load(., Iter$carrying_capacity, PowerFlow = "power_flow", "Link.Limit" = "edge_capacity")
  
  common_time <- 0.01
  common_Iter <- 200000
  common_tol <- 2e-4
  common_mass <- 1
  
  #Sets up the graph so that all the embedding stuff can be calculated without problem
  current_graph  <- g %>%
    set.edge.attribute(. , "distance", value = 1) %>%
    set.edge.attribute(., "Area", value = 1) %>%
    calc_spring_youngs_modulus(., "power_flow", "edge_capacity", minimum_value = 100, stretch_range = 1000) %>%
    calc_spring_constant(., E ="E", A = "Area", distance = "distance") %>%
    normalise_dc_load(.,  
                      generation = "generation", 
                      demand  = "demand",
                      net_generation = "net_generation", 
                      capacity = "edge_capacity",
                      edge_name = "edge_name", 
                      node_name = "name",
                      power_flow = "power_flow")
  
  
  # print("Full graph complete")
  
  #autosets finds the correct drag coefficient to 
  embeddings_data <- SETSe_bicomp(current_graph, 
                                  force ="net_generation", 
                                  distance = "distance", 
                                  edge_name = "edge_name",
                                  tstep = common_time, 
                                  mass = sum(abs(vertex_attr(current_graph, "net_generation")))/vcount(current_graph), #mass is a function of systemic force
                                  max_iter = common_Iter, 
                                  tol = common_tol,
                                  static_limit = sum(abs(vertex_attr(current_graph, "net_generation"))),
                                  sparse = FALSE,
                                  hyper_iters = 50,
                                  hyper_tol = 0.0,
                                  hyper_max = 30000,
                                  sample = 100,
                                  verbose = T)
  
  line_load_df  <-   as_data_frame(current_graph) %>%
    mutate(line_load =abs(power_flow)/edge_capacity)
  
  embeddings_data$edge_embeddings <- embeddings_data$edge_embeddings %>%
    left_join(line_load_df %>% select(edge_name, line_load))
  
  #The structure is generated as needed and so any new paths can just be created at this point.
  #There is very little overhead in doing it this way
  if(!file.exists(dirname(Iter_embedding_path))){ dir.create(dirname(Iter_embedding_path), recursive = T) }
  #   print("Saving file")
  
  saveRDS(embeddings_data, file = Iter_embedding_path)
  #After the simulation and its summary are saved to the drive the next in the compute group is calculated
  
  stop_iter_time <- Sys.time()
  
  print(paste("Iteration", i, "complete. Time taken", signif(difftime(stop_iter_time, start_iter_time , units = "secs"), 4), "seconds"))
} 


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