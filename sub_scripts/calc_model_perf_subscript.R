#This is used to get the model performance across all metrics
#It does this for both with and without error spikes



multi_metric <- metric_set(rmse, rsq, mae, smape)
metric_combos <- expand_grid(
  metrics = unique(all_SETSe_emebeddings$metric),
  average_type =unique(all_SETSe_emebeddings$average_type),
  graph = unique(all_SETSe_emebeddings$graph)) #%>%
# filter(graph!="UK_high_voltage")

metric_performance_all <-1:nrow(metric_combos) %>%
  map_df(~{
    print(.x)
    temp <- all_SETSe_emebeddings %>%
      filter(
        # !(carrying_capacity>1.5 & as.logical(robin_hood_mode) & fract == 1),
        average_type == metric_combos$average_type[.x],
        metric == metric_combos$metrics[.x],
        graph == metric_combos$graph[.x])
    
    loess_mod <- loess(formula =attack_round~ value, 
                       data = temp)
    
    model_comp <- temp  %>%
      mutate(preds = predict(loess_mod)
      )
    
    multi_metric(data = model_comp, truth = attack_round, estimate = preds) %>% 
      mutate(metric = metric_combos$metrics[.x],
             average_type = metric_combos$average_type[.x],
             graph = metric_combos$graph[.x])
    
  }) %>%
  mutate(combo = paste(average_type, metric)) 


metric_performance_plot_df  <- metric_performance_all  %>%
  rename(eval_metric = .metric) %>%
  group_by(eval_metric, graph) %>%
  mutate(rank = ifelse(eval_metric =="rsq", rank(-.estimate), rank(.estimate)),
         graph_order  = factor(gsub("_igraph", "", graph), 
                               levels = c("IEEE_14", "IEEE_30", "IEEE_57", "IEEE_118","IEEE_300", "UK_high_voltage")))

metric_performance_all_no_spikes <-1:nrow(metric_combos) %>%
  map_df(~{
    print(.x)
    temp <- all_SETSe_emebeddings %>%
      filter(
       !(carrying_capacity>1.5 & as.logical(robin_hood_mode) & fract == 1),
        average_type == metric_combos$average_type[.x],
        metric == metric_combos$metrics[.x],
        graph == metric_combos$graph[.x])
    
    loess_mod <- loess(formula =attack_round~ value, 
                       data = temp)
    
    model_comp <- temp  %>%
      mutate(preds = predict(loess_mod)
      )
    
    multi_metric(data = model_comp, truth = attack_round, estimate = preds) %>% 
      mutate(metric = metric_combos$metrics[.x],
             average_type = metric_combos$average_type[.x],
             graph = metric_combos$graph[.x])
    
  }) %>%
  mutate(combo = paste(average_type, metric)) 


metric_performance_plot_df_nospikes  <- metric_performance_all_no_spikes  %>%
  rename(eval_metric = .metric) %>%
  group_by(eval_metric, graph) %>%
  mutate(rank = ifelse(eval_metric =="rsq", rank(-.estimate), rank(.estimate)),
         graph_order  = factor(gsub("_igraph", "", graph), 
                               levels = c("IEEE_14", "IEEE_30", "IEEE_57", "IEEE_118","IEEE_300", "UK_high_voltage")))


metric_performance_plot_df <- bind_rows(metric_performance_plot_df %>% mutate(error_type ="standard"),
                                        metric_performance_plot_df_nospikes %>% mutate(error_type ="no_spikes")
                                        )


rm(metric_performance_all); rm(metric_performance_all_no_spikes); rm(metric_performance_plot_df_nospikes)
