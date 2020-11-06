#' generate data for powergrid stats
#' 
#' Produces a data frame that can be converted to a table for the paper
#' 
#' @param file_paths string vector, the full file paths of the networks to analyse
#' 
#' @export



create_powergrid_network_statistics <- function(file_paths){
  
  network_summary_stats <- file_paths %>%
    map_df(~{
      readRDS(.x) %>%
        NetworkStats() %>%
        filter(Metric != "Betweenness") %>%
        mutate(graph = basename(.x),
               Betweenness = mean(betweenness(readRDS(.x), normalized = T))
        )
      
      
    }) %>%
    filter(Metric != "Components") %>%
    pivot_wider(names_from = "Metric", values_from = "value") %>%
    arrange(Nodes) %>%
    mutate(
      Nodes = as.integer(Nodes),
      Edges =  as.integer(Edges),
      Betweenness = round(Betweenness, 2),
      Degree = round(Degree, 2),
      Assortativity = round(Assortativity, 2),
      Clustering = round(Clustering, 2),
      Distance = round(Distance, 2),
      graph = str_remove(graph, "_igraph") %>% 
        str_remove(., ".rds") %>%  
        str_replace_all(., "_", " ") %>%
        str_replace(., "UK high voltage", "UK HV")) %>%
    rename(
      Betw = Betweenness, 
      Deg = Degree,
      Assort = Assortativity,
      Clust = Clustering,
      Dist = Distance) %>%
    select(Graph = graph, Nodes, Edges, everything())
  
}