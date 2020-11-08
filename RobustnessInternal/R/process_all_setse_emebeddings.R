#' All concentrator Setse embeddings
#' 
#'  Loads the setse data and saves it is not already saved
#'  
#'  The function loads and processes the file output from the HPC script. It then saves the processed data
#'  If the processed data is already saved this is loaded to save time.
#'  
#'  @param processed_path A character string. The location of the processed files
#'  @param folder_paths A character vector. The paths of each folder to load and process, each folder represents a graph
#'  @param graph_agg A data frame of the summarised attack data that matches the strain data
#'  @param PL_SETSe_emebeddings A dataframe the output of the pl equivalent of this function
#'  
#'  
#'  @export
#'  
process_all_setse_emebeddings <- function(processed_path, folder_paths, graph_agg, PL_SETSe_emebeddings){
  if(file.exists(processed_path)){
    
    all_SETSe_emebeddings2 <- readRDS(processed_path)
    
    
  } else {
    
    
    
    all_SETSe_emebeddings <- folder_paths %>% #
      #list.files(embeddings_path, full.names = T)[!grepl("PL", list.files(embeddings_path))] %>%#
      map_df(~{
        print(basename(.x))
        
        Out <- list.files(.x, full.names = T) %>%
          map_df(~{
            file_name = .x
            temp <- readRDS(file_name)
             temp_edge <- temp$edge_embeddings
            
             temp_node <- temp$node_embeddings %>%
               summarise(
                 mean_elev = mean(abs(elevation)),
                 median_elev = median(abs(elevation))
                                      )
               
             
            parts <- str_split(basename(file_name), pattern = "_", simplify = T)
            
            temp_edge %>%
              summarise(
                mean_ratiostrain = mean(strain, na.rm = T)/median(strain, na.rm = T),
                mean_ratiotension =  mean(tension, na.rm = T)/median(tension, na.rm = T),
                mean_loading =mean(line_load, na.rm = T),
                median_loading = median(line_load, na.rm = T),
                mean_alpha = mean(1/line_load, na.rm = T),
                median_alpha = median(1/line_load, na.rm = T),
                #mean_logstrain = mean(log10(strain), na.rm = T),
                # mean_logtension = mean(log10(tension), na.rm = T),
                 mean_strain = mean(strain, na.rm = T),
                 median_strain = median(strain, na.rm = T),
                mean_tension = mean(tension, na.rm = T),
                median_tension = median(tension, na.rm = T)
              ) %>%
              bind_cols(temp_node) %>%
              mutate(
                static_force = sum(abs(temp$node_embeddings$static_force)),
                fract = parts[2],
                carrying_capacity = parts[4],
                largest = parts[6],
                smallest = parts[8],
                robin_hood_mode = str_remove(parts[11], pattern = ".rds"))
            
          }) %>% mutate(graph = basename(.x))
        
        return(Out)
        
      })
    # .x <- "/home/jonno/Dropbox/IEEE_Networks/embeddings/IEEE_118_igraph/fract_1_ec_20_largest_0.5_smallest_0.5_robin_hood_TRUE.rds" 
    #This just checks to see if everything has converged and it more or less has
    # test <- all_SETSe_emebeddings %>%
    #   mutate(converged = static_force<=2e-3) %>%
    #   group_by(graph) %>%
    #   summarise(converged = sum(converged),
    #             counts = n())
    
    
    
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
                                        all_SETSe_emebeddings) %>%
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
