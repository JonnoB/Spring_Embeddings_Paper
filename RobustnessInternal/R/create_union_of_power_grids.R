#' put all the power grids into a single network
#' 
#' This is sugar to clean up the code and make a single powergrid with shared attributes from all the power grids
#' 
#' @export


create_union_of_power_grids <- function(){
  
  plot_list <- list.files(file.path(embeddings_path, "PL"))[c(2,3,5,1,4,6)] %>%
    map(~{
      
      target_graph <- .x # "IEEE_118_igraph"
      
      plot_title <- .x %>% str_remove(., "_igraph") %>% str_replace_all(., "_", " ")
      
      
      embeddings_data <- read_rds(file.path(embeddings_path, "PL",target_graph, "ec_5.rds"))
      
      g <- read_rds(file.path(power_grid_graphs_path, paste0(target_graph, ".rds")))
      
      nodes_df <- as_data_frame(g, what = "vertices") %>%
        mutate(type = case_when(
          net_generation>0 ~ "generator",
          net_generation<0 ~"demand",
          TRUE ~ "transfer"
        )  ) %>%
        left_join(embeddings_data$node_embeddings %>% select(name = node, elevation), by = "name")
      
      print(cor(nodes_df$net_generation, nodes_df$elevation))
      
      edges_df <- as_data_frame(g) %>%
        mutate(edge_name = paste(from, to, sep = "-")) %>%
        left_join(embeddings_data$edge_embeddings %>% select(edge_name, tension, strain), by = "edge_name")
      
      g <- graph_from_data_frame(edges_df, directed = FALSE, vertices = nodes_df) 
      
      g <- g %>%
        set.vertex.attribute(., "name", value = (paste0(vcount(g), "_",get.vertex.attribute(g, "name")))) %>%
        set.vertex.attribute(., "type", value = plot_title) %>%
        set.vertex.attribute(., "Name", value = "A") #Name was causing problems and is irrelevant for this plot
      
      igraph::as_data_frame(g, what = "both")
    }
    ) %>%
    #the list is now two elements long and made up of a edge and vertex part
    transpose() %>%
    #join the elements from each part of the list into dataframes
    map(~bind_rows(.x)) %>%
    {graph_from_data_frame(d = .$edges, directed = FALSE, vertices = .$vertices)}
  
  
  kappa_modifier <- as_data_frame(plot_list, what = "vertices") %>%
    group_by(type) %>%
    mutate(elevation = kappa(elevation)) %>%
    ungroup
  
 g <- graph_from_data_frame(as_data_frame(plot_list), directed = FALSE, vertices = kappa_modifier ) 
 
 return(g)
  
}