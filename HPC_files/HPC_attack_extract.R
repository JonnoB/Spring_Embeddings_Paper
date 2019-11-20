
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
#start_time <- Sys.time()

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
  load_data_files_path <- file.path("/home/jonno/HPC_jobs") #load the files
  save_data_files_path <- file.path(project_folder) #save the files
  
}else{
  #This is for the folder that is on the cloud
  project_folder <- getwd()
  basewd <- "/home/ucabbou"
  analysis_parameter_file_path <- file.path(basewd, "analysis_parameter_files") #In myriad the parameter files are in a folder on the home dir
  #not in the project folder like when it is done on my own comp
  HPC_script_path <- file.path(basewd, "HPC_parameter_files")
  load_data_files_path <- "~/completed_files" #load the files
  save_data_files_path <- file.path(project_folder) #save the files
  
  
  #If it is not on my computer then the variables need to be loaded from the system environment
  #Get the task ID
  #task_id <- Sys.getenv("SGE_TASK_ID")
 # HPC_start_up_file <- Sys.getenv("HPC_start_up_file")
}


#Load some other useful functions
list.files(file.path(basewd, "Useful_PhD__R_Functions"), pattern = ".R", full.names = T) %>%
  walk(~source(.x))

list.files(file.path(basewd, "Flow_Spring_System"), pattern = ".R", full.names = T) %>%
  walk(~source(.x))

print("extract files")
  list.files(load_data_files_path) %>% walk(~{
    
    untar_myriad_collapse_summaries(load_data_files_path, tar_file = .x, extraction_directory = project_folder)
    
  })

  #stop_time <- Sys.time()
  
 # print(stop_time-start_time)