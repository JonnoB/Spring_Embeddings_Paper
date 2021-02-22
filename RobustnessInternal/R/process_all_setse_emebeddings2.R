#' All concentrator Setse embeddings
#' 
#'  Loads the setse data and saves it is not already saved
#'  
#'  The function loads and processes the file output from the HPC script. It then saves the processed data
#'  If the processed data is already saved this is loaded to save time.
#'  
#' @param processed_path A character string. The location of the processed files
#' @param folder_paths A character vector. The paths of each folder to load and process, each folder represents a graph
#' @param graph_agg A data frame of the summarised attack data that matches the strain data
#' @param PL_SETSe_emebeddings A dataframe the output of the pl equivalent of this function
#'  
#'  
#' @export
#'  
process_all_setse_emebeddings2 <- function(processed_path, folder_paths, graph_agg, PL_SETSe_emebeddings){
  if(file.exists(processed_path)){
    
    all_SETSe_emebeddings2 <- readRDS(processed_path)
    
    
  } else {
    
    
    
    all_SETSe_emebeddings <- list.files(folder_paths, full.names = T, pattern = ".rds", recursive = T) %>% #
      map_df(~{read_rds(.x) %>%
          mutate(graph = .x %>% dirname() %>% basename())}) %>%
      mutate_at(c("fract" ,"carrying_capacity", "largest", "smallest", "robin_hood_mode"),
                as.character) %>%
      mutate(graph = str_remove(graph, ".rds"))

    
    PL_SETSe_emebeddings_extrema <- PL_SETSe_emebeddings %>%
      filter(carrying_capacity==1 | carrying_capacity==Inf) %>%
      select(graph, average_type, metric, min_val, max_val)
    
    all_SETSe_emebeddings2 <- left_join(graph_agg %>% 
                                          #The loading and alpha values in graph_agg are at the point of collapse not at initialiation. That is why they are removed and replaced with the values from the strain calc
                                          select(-mean_alpha, -mean_loading,-median_loading) %>% #
                                          mutate(
                                            carrying_capacity = as.character(carrying_capacity),
                                            largest = as.character(largest),
                                            smallest = as.character(smallest),
                                            fract = as.character(fract),
                                            robin_hood_mode = as.character(robin_hood_mode),
                                            
                                          ),
                                        all_SETSe_emebeddings,
                                        by = c("carrying_capacity", "smallest", "largest", "fract", "robin_hood_mode", "graph")) %>%
      mutate(mean_alpha = 1/mean_alpha) %>%
      mutate_at(1:4, list(as.numeric)) %>%
      group_by(graph) %>%
      #mutate_at(vars(mean_loading:median_elev), kappa) %>%
      pivot_longer(.,cols = mean_loading:median_elev, names_to = "metric") %>%
      # mutate_at(vars(mean_loading:median_tension), kappa) %>%
      # pivot_longer(.,cols = mean_loading:median_tension, names_to = "metric") %>%
      separate(., col ="metric", into = c("average_type", "metric"), sep ="_") %>%
      ungroup %>%
      left_join(PL_SETSe_emebeddings_extrema) %>%
       mutate(value_raw = value,
              value =  (value-min_val)/(max_val-min_val))
    
    saveRDS(all_SETSe_emebeddings2, processed_path)
    
  }
  
  return(all_SETSe_emebeddings2)
  
}
