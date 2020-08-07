
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
  library(NetworkSpringEmbedding)
}else{
  #This is for the folder that is on the cloud
  project_folder <- getwd()
  basewd <- "/home/ucabbou"
  #on the home dir not in the project folder like when it is done on my own comp
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
parameter_df_temp <-  generate_concentrator_parameters(load_file) #%>%
# filter(carrying_capacity == 20,
#        smallest == 0.5,
#        largest == 0.1,
#        fraction == 1,
#        robin_hood_mode == T)

#I messed up the times so need to insert this if statment to selectively delete the files that have been calculated already
#then re-run the files that have not been re-calculated yet
# if(file.exists(file.path(load_data_files_path, "missing_files", "strain_concentrator.rds"))){
#     
#   parameter_df_temp <- parameter_df_temp %>%  
#   left_join(readRDS(file.path(load_data_files_path, "missing_files", "strain_concentrator.rds"))) %>%
#     filter(is.na(compute_group_strain_2),
#            simulation_id ==1) %>% #remove all files that have been made already
#   mutate(compute_group_strain = 1:n())
#   
# }

#filter to current job number
parameter_df_temp <- parameter_df_temp %>%
  filter(#compute_group_strain == compute_group_value, #this variable is inserted into the file
         simulation_id ==1) #only 1 sim from each set needs the strain calculated as they are all the same.

print(paste("pararmeters loaded. Task number", task_id))
print("run sims")
1:nrow(parameter_df_temp) %>%
  walk(~{
    loop_start <- Sys.time()
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
    
    if(!file.exists(Iter_embedding_path)){
      
      ##
      ##
      ##Once the variables have been taken from the parameter file the calculation can begin
      ##
      ##
      
      g <- readRDS(file = graph_path) #read the target graph
      
      
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
      
      common_time <- 0.01
      common_Iter <- 60000 #This works for networks of 49k nodes so should be fine for these small networks
      common_tol <- 2e-4 #The force is normalised across the networks so the tolerance can also be normalised across networks
      common_mass <- 1
      
      #Sets up the graph so that all the embedding stuff can be calculated without problem
      current_graph  <- g %>%
        set.edge.attribute(. , "distance", value = 1) %>%
        set.edge.attribute(., "Area", value = 1) %>%
        calc_spring_youngs_modulus(., "power_flow", "edge_capacity", minimum_value = 100, stretch_range = 1000) %>%
        calc_spring_constant(., E ="E", A = "Area", distance = "distance") %>%
        #This function makes the load across all the networks the same
        #The resultant total absolute force is 2
        normalise_dc_load(.,  
                          generation = "generation", 
                          demand  = "demand",
                          net_generation = "net_generation", 
                          capacity = "edge_capacity",
                          edge_name = "edge_name", 
                          node_name = "name",
                          power_flow = "power_flow")
      
      #This allows the line load to be extracted and added in to the edge embeddings.
      #This used to be part of the embeddings process but was removed 
      #as it is specific to only capacity limited flow networks.
      #This addon here means the rest of the code in the project doesn't need to be changed
      line_load_df  <-   as_data_frame(current_graph) %>%
        mutate(line_load =abs(power_flow)/edge_capacity)

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
                                      hyper_tol = 0.01,
                                      hyper_max = 10000, #This really doesn't need to be that big for these networks
                                      sample = 100,
                                      verbose = T)
      
      embeddings_data$edge_embeddings <- embeddings_data$edge_embeddings %>%
        left_join(line_load_df %>% select(edge_name, line_load))
      
      
      #The structure is generated as needed and so any new paths can just be created at this point.
      #There is very little overhead in doing it this way
      if(!file.exists(dirname(Iter_embedding_path))){ dir.create(dirname(Iter_embedding_path), recursive = T) }
   #   print("Saving file")
      saveRDS(embeddings_data, file = Iter_embedding_path)
      #After the simulation and its summary are saved to the drive the next in the compute group is calculated
      
      loop_stop <- Sys.time()
      
      print(paste("Iteration", .x, "complete. Time taken", signif(difftime(loop_stop, loop_start, units = "secs"), 4), "seconds"))
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