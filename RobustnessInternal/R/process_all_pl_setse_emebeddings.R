#' Process PL data
#' 
#' This function takes the path of the PL attacks and the path of the PL embeddings and constructs the PL data frame
#' 
# @param processed_path A character string. The path the tp processed data
#' @param strain_folders A character string. The path to the strain folders
#' @param attack_folders A character string. The path to the attack folders
#' 
#' @export

process_all_pl_setse_emebeddings <- function( strain_folders, attack_folders){
  
  
  
  PL_graph_agg <-list.files(attack_folders, full.names = T, recursive = T) %>%
    map_df(~{
      
      file_path <-.x
     temp  <-read_rds(file_path) 
     return(temp)
    }) %>% 
    group_by(carrying_capacity, graph) %>%
    summarise_all(mean) %>%
    ungroup %>%
    mutate(mean_alpha = 1/mean_alpha,
           median_alpha = 1/median_alpha) %>%
    select(-median_alpha) %>%
    mutate(carrying_capacity = signif(carrying_capacity))
  
  PL_SETSe_emebeddings_raw <- list.files(strain_folders, full.names = T) %>%
    map_df(~{
      print(basename(.x))
      test <- list.files(.x, full.names = T) %>%
        map_df(~{
          file_name = .x
          temp <- readRDS(file_name)
          temp_edge<- temp$edge_embeddings
          
          parts <- str_split(basename(file_name), pattern = "_", simplify = T)
          
          temp_node <- temp$node_embeddings %>%
            summarise(
              mean_elev = mean(abs(elevation)),
              median_elev = median(abs(elevation))
            )
          
          temp_edge %>%
            summarise(mean_loading =mean(line_load, na.rm = T),
                      median_loading = median(line_load, na.rm = T),
                      mean_alpha = mean(1/line_load, na.rm = T),
                      median_alpha = median(1/line_load, na.rm = T),
                      mean_strain = mean(strain, na.rm = T),
                      median_strain = median(strain, na.rm = T),
                      mean_tension = mean(tension, na.rm = T),
                      median_tension = median(tension, na.rm = T)
            ) %>%
            bind_cols(temp_node) %>%
            mutate(
              static_force = sum(abs(temp$node_embeddings$static_force)),
              carrying_capacity =str_remove(parts[2], pattern = ".rds"))
          
        }) %>% mutate(graph = basename(.x))
      
    }) %>%
    left_join(PL_graph_agg %>% 
                select(-mean_alpha, -mean_loading,-median_loading) %>%
                mutate(
                  carrying_capacity = as.character(carrying_capacity)
                ),
              .)  %>%
    mutate(mean_alpha = 1/mean_alpha,
           carrying_capacity = as.numeric(carrying_capacity))
  
  #This block allows the extrema to be found as these act as the normalising limits for all metrics
  #These values are also used to normalise the redistributed results
  PL_SETSe_emebeddings_extrema <- PL_SETSe_emebeddings_raw %>%
    filter(carrying_capacity %in% c(1, Inf)) %>%
    pivot_longer(.,cols = mean_loading:median_elev, names_to = "metric") %>%
    separate(., col ="metric", into = c("average_type", "metric"), sep ="_") %>%
    select(carrying_capacity, graph, average_type:value) %>%
    group_by(graph,average_type, metric) %>%
    mutate(carrying_capacity = ifelse(value == max(value), "max_val", "min_val")) %>% #This ensures that tension is the right way round
    pivot_wider(., names_from = carrying_capacity, values_from = value) %>%
    ungroup
  
  PL_SETSe_emebeddings <-PL_SETSe_emebeddings_raw %>%
    pivot_longer(.,cols = mean_loading:median_elev, names_to = "metric") %>%
    separate(., col ="metric", into = c("average_type", "metric"), sep ="_") %>%
    left_join(PL_SETSe_emebeddings_extrema) %>%
    mutate(value_raw = value,
           value =  (value-min_val)/(max_val-min_val))
  # 
  # PL_SETSe_emebeddings <- PL_SETSe_emebeddings_raw %>%
  #   group_by(graph) %>%
  #   mutate_at(vars(mean_loading:median_tension),kappa) %>% #The kappa value makes everything relative to the column max and min
  #   #This should be changed as then the PL values can be used to create the limits for the non-pl graphs
  #   pivot_longer(.,cols = mean_loading:median_tension, names_to = "metric") %>%
  #   separate(., col ="metric", into = c("average_type", "metric"), sep ="_")
  # 
  
  return(PL_SETSe_emebeddings)
}