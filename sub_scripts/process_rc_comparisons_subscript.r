#N.B.
#The mean and median strain is the value at the loss of gc point. The correct value comes in when the strain data is added


#Gets all the metrics into long format.
#Allows the metrics to be plotted against the number of attacks needed before network collapse
#using the combined data frame aggregate the results by parameter group
IEEE_118_agg_res <-all_graph_agg %>% filter(graph == "IEEE_118_igraph") 


if(file.exists("/home/jonno/test_strain/IEEE_118_strainrc/rc_metrics_df.rds")){
  
  test_rc_metrics <-read_rds("/home/jonno/test_strain/IEEE_118_strainrc/rc_metrics_df.rds")
  
} else {
  
  test_rc_metrics <- list.files("/home/jonno/test_strain/IEEE_118_strainrc/IEEE_118_igraph", 
                                full.names = T, 
                                pattern = ".rds") %>% map_df(~{
                                  print(.x)
                                  temp <- readRDS(.x) #aggreagte_strain_files(file_name)
                                  
                                  parts <- str_split(basename(.x), pattern = "_", simplify = T)
                                  
                                  #convert to range values
                                  strain_norm_df <-temp$edge_embeddings %>% 
                                    summarise(mean_strain = mean(strain),
                                              median_strain = median(strain),
                                              mean_tension = mean(tension),
                                              median_tension = median(tension),
                                              energy = sum(0.5*k*strain^2)) %>%
                                    mutate(
                                      static_force = sum(abs(temp$node_embeddings$static_force)),
                                      fract = parts[2],
                                      carrying_capacity = parts[4],
                                      largest = parts[6],
                                      smallest = parts[8],
                                      robin_hood_mode = parts[11],
                                      r = parts[13],
                                      c = str_remove(parts[15], pattern = ".rds"))
                                })
  saveRDS(test_rc_metrics, "/home/jonno/test_strain/IEEE_118_strainrc/rc_metrics_df.rds")
}

test_rc_metrics <- IEEE_118_agg_res %>%
  #mutate(mean_alpha = 1/mean_alpha) %>%
  select(-gc_present, -simulation_id) %>%
  left_join(test_rc_metrics %>%  
              mutate_at(., .vars = vars(fract:smallest), .funs = as.numeric) %>%
              mutate(robin_hood_mode = robin_hood_mode =="TRUE"), 
            by = c("carrying_capacity", "smallest", "largest", "fract", "robin_hood_mode")) %>%
  rename(mean_energy = energy) %>%
  pivot_longer(cols = mean_loading:mean_energy, names_to = "metric") %>%
  separate(., col = metric, into =c("average_type", "metric"),  sep="_" )

metric_combos <- test_rc_metrics %>%
  filter(metric %in% c("strain", "tension")) %>%
  distinct(., average_type, metric, r,c) %>%
  filter(metric !="alpha")

metric_performance <-1:nrow(metric_combos) %>%
  map_df(~{
   # print(.x)
    temp <- test_rc_metrics %>%
      filter(
        static_force<0.002,
        !is.na(value),
        r == metric_combos$r[.x],
        c == metric_combos$c[.x],
        metric == metric_combos$metric[.x],
        average_type == metric_combos$average_type[.x]
      )
    
    loess_mod <- loess(formula =attack_round~ value, 
                       data = temp)
    
    model_comp <- temp  %>%
      mutate(preds = predict(loess_mod)
      )
    
    multi_metric(data = model_comp, truth = attack_round, estimate = preds) %>% 
      mutate(metric = metric_combos$metric[.x],
             average_type = metric_combos$average_type[.x],
             r = temp$r[1],
             c = temp$c[1])
    
  })

rm(IEEE_118_agg_res)
