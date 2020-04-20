
#This script permutes the generation and demand of the network and finds the sets embedding of it.

{
  g <- readRDS(file.path(power_grid_graphs_path,  paste0("IEEE_", 118, "_igraph.rds"))) 
  
  g <- g %>%
    as_data_frame(., what = "vertices") %>%
    mutate(root_attr = demand != 0 | generation != 0) %>% pull(root_attr) %>%
    set_vertex_attr(g,name =  "root_attr", value = .)
  
}


0:100 %>% walk(~{
  graph_id <- .x
  
  target_file <-  file.path(project_folder, "permuted_pl", "SETSe", paste0("network_", .x, ".rds"))
  
  #it the file does not exist make it otherwise skip
  if(!file.exists(target_file)){
    
    if(.x == 0){
      g2 <-g
    } else{
      g2 <- reorder_vertex_attributes(g, root_attr, change_attributes = c("demand", "generation", "net_generation"), seed = .x)
    }
    
    strain_range <- c(100, 550, 1100) %>% map(~{
      g2 <- g2 %>%
        Proportional_Load(., 5, PowerFlow = "power_flow", Link.Limit = "edge_capacity") %>% #as_data_frame() %>%
        set.edge.attribute(. , "distance", value = 1) %>%
        set.edge.attribute(., "Area", value = 1) %>%
        set.edge.attribute(., "k", value = .x) %>%
        #calc_spring_youngs_modulus(., "power_flow", "edge_capacity", minimum_value = 100, stretch_range = 1000) %>%
       # calc_spring_constant(., E ="E", A = "Area", distance = "distance") %>%
        normalise_dc_load(.,  
                          generation = "generation", 
                          demand  = "demand",
                          net_generation = "net_generation", 
                          capacity = "edge_capacity",
                          edge_name = "edge_name", 
                          node_name = "name",
                          power_flow = "power_flow")
      
      
      auto_SETSe(g2,
                 capacity = "edge_capacity") 
      })
    
    #This could be put inside the previous map but whatever
   k_type <- c("min", "mid", "max")
    
  embeddings_combines <- 1:3 %>% map(~{
     
    elevation <-strain_range[[.x]]$node_embeddings %>%
      select(node, elevation) %>%
      mutate( type = (k_type[.x]))
    
   tension<-  strain_range[[.x]]$edge_embeddings %>%
      select(edge_name, tension) %>%
      mutate( type = (k_type[.x]))
    
   return(list(elevation = elevation, tension = tension))
   }) %>%
    transpose() %>%
    map(., ~bind_rows(.x) %>% mutate(graph =   graph_id))
  

  assortativity_df <- tibble(assortativity = assortativity(g2, vertex_attr(g2,"net_generation")), graph =  graph_id)



load_profile_df <- as_data_frame(g2, what = "vertices" ) %>%
  select(name, net_generation) %>%
  mutate(type  = !!paste("graph_", graph_id))

test <- list(elevation = embeddings_combines$elevation, 
             tension = embeddings_combines$tension, 
             assortativity_df = assortativity_df,
             load_profile_df = load_profile_df)  

  saveRDS(test, file = target_file)
  
  } #end of if file exsits
  
})


permuted_pl_df <-list.files(file.path(project_folder, "permuted_pl", "SETSe"), full.names = T) %>%
  map(~{
    
    temp <-  readRDS(.x)
    
  }) %>%
  transpose() %>%
  map(., ~bind_rows(.x))


permuted_pl_df$load_profile_df

permuted_pl_df$assortativity_df

left_join(permuted_pl_df$elevation %>%
  group_by(graph, type) %>%
  summarise(elevation = mean(abs(elevation))),

permuted_pl_df$tension %>%
  group_by(graph, type) %>%
  summarise(tension = mean(abs(tension)))) %>%
  left_join(., permuted_pl_df$assortativity_df) %>%
  ggplot(aes(x = tension, y = elevation, colour = type,
             shape = graph == 0)) +
  geom_line(aes(group = graph), alpha = 0.1, colour = "black") + geom_point()
  


#100 repititions of random order attacks til failure on 1000 different graphs
1:100 %>%
  walk(~{
    
    target_file <-  file.path(project_folder, "permuted_pl", "collapse_folder", paste0("attack_order_", .x, ".rds"))
    
    #it the file does not exist make it otherwise skip
    if(!file.exists(target_file)){
      order_number <-.x
      set.seed(.x)
      DeletionOrder <- RandomAttack(g, Target = "Edges", Number = ecount(g), Name = "edge_name")
      
      test <- 0:100 %>% map_df(~{
        print(paste("Order number", order_number, "graph number", .x))
        
        if(.x == 0){
          g2 <-g
        } else{
        g2 <- reorder_vertex_attributes(g, root_attr, change_attributes = c("demand", "generation", "net_generation"), seed = .x)
        }
        
        g2 <- g2%>%
          Proportional_Load(., 5, PowerFlow = "power_flow", Link.Limit = "edge_capacity") %>%
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
        
        #Construction the deletion order function.
        FixedNodes <- quo(FixedStrategyAttack(g, DeletionOrder, "Edge", Name =  "edge_name"))
        
        # print("attack grid")
        AttackSeries <- attack_the_grid(g = g2,
                                        AttackStrategy = FixedNodes,
                                        g0 = NULL,
                                        TotalAttackRounds = 1000,
                                        CascadeMode = TRUE,
                                        Demand = "demand",
                                        Generation = "generation",
                                        EdgeName = "edge_name",
                                        VertexName = "name",
                                        Net_generation = "net_generation",
                                        power_flow = "power_flow",
                                        edge_capacity = "edge_capacity",
                                        target = "edges")
        
        
     #   test <- network_co_failure(AttackSeries)
        
        #I really only need a very small amount of information from the attack. That is the round the gc was lost
        AttackSeriesSummary <- AttackSeries %>% 
          extract_network_stats(.) %>%
          filter(attack_round == AttackSeries$gc_loss_round) %>%
          mutate(graph = .x)
        
        return(AttackSeriesSummary)
        
      })
      
      saveRDS(test , target_file)
      
    }
    
  })



permuted_pl_df <-list.files(file.path(project_folder, "permuted_pl", "SETSe"), full.names = T) %>%
  map_df(~{
    
    temp <-  readRDS(.x)
    
   target_file <- str_split(basename(.x), pattern = "_")
    
    tibble(
      strain = mean(temp$edge_embeddings$strain),
      elevation = mean(abs(temp$node_embeddings$elevation)),
      tension  = mean(temp$edge_embeddings$tension),
      graph = str_remove(target_file[[1]][2], ".rds") %>% as.integer()) 
    
  })



permuted_pl_df %>%
  ggplot(aes( x = tension, y = elevation, colour = graph!=0)) + 
  geom_point() 

attack_results <- list.files(file.path(project_folder, "permuted_pl", "collapse_folder"), full.names = T) %>%
  map_df(~{

    target_file <- str_split(basename(.x), pattern = "_") 
    temp <-  readRDS(.x) %>%
      mutate(attack_order =str_remove(target_file[[1]][3], ".rds") %>% as.integer() )    
    
    return(temp)
  }) %>%
  select(attack_round:edges, graph, attack_order) 
  


attack_results  %>%
  group_by(graph) %>%
  summarise_all(., .funs = list(mean = mean, sd = sd, max =  max,  min = min, median = median)) %>%
  left_join(permuted_pl_df)%>%
  ggplot(aes(colour = attack_round_median, x = tension, y = elevation)) + geom_point() +
  scale_color_viridis() +
  labs(title = "Tension vs elevation for randomnly permuted node attributes in IEEE 118 with all edges 
       alpha = 5")


test2 <-1:100 %>%
  map_df(~{
    
    test2 <- attack_results %>%
      filter(attack_order ==.x) %>%
      left_join(permuted_pl_df, by = "graph")
    
   tibble(cor1 =  cor(test2$blackout_size, test2$tension),
          cor2 = cor(test2$attack_round, test2$tension),
          attack_order = .x) 
    
  })
#the correlation appears to be significant but weak
t.test(test2$cor1, alternative = "greater")