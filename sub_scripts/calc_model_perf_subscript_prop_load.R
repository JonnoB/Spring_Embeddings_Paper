#This script is based off the earlier version which used the generated data to calculate the model. on many cross validated models
#This version uses the proportional loading results to generated a model
#then compares the performance of that model to the distribution data.
#The point of this is the compare the 


multi_metric <- metric_set(rmse, rsq, mae, smape)
metric_combos <- expand_grid(
  metrics = unique(all_SETSe_emebeddings$metric),
  average_type =unique(all_SETSe_emebeddings$average_type),
  graph = unique(all_SETSe_emebeddings$graph)) %>%
  filter(average_type =="mean",
         metrics != "elev",
         metrics != "strain")

metric_performance_all <-1:nrow(metric_combos) %>%
  map_df(~{
    print(.x)
    temp <- all_SETSe_emebeddings %>%
      filter(
        average_type == metric_combos$average_type[.x],
        metric == metric_combos$metrics[.x],
        graph == metric_combos$graph[.x])
    
    model_dat <- formula(attack_round ~ value)
    
    
    set.seed(4622)
    rs_obj <- vfold_cv(temp, v = 10, repeats = 10)

    Out <- 1:length(rs_obj$splits) %>%
      map_df(~{
        
        loess_holdout_results(rs_obj$splits[[.x]], model_dat, metrics_out = TRUE) %>%
          mutate(Repeat = rs_obj$id[.x],
                 Fold = rs_obj$id2[.x])
        
      }) %>% 
      mutate(metric = metric_combos$metrics[.x],
             average_type = metric_combos$average_type[.x],
             graph = metric_combos$graph[.x])
    
    return(Out)
    
  }) %>%
  mutate(combo = paste(average_type, metric)) %>%
  rename(eval_metric = .metric) %>%
  ungroup


metric_performance_plot_df  <- metric_performance_all   %>%
  group_by(eval_metric, metric, average_type, graph, combo) %>%
  summarise(.estimate = mean(.estimate)) %>%
  group_by(eval_metric, graph) %>%
  mutate(rank = ifelse(eval_metric =="rsq", rank(-.estimate), rank(.estimate)),
         graph_order  = factor(gsub("_igraph", "", graph), 
                               levels = c("IEEE_14", "IEEE_30", "IEEE_57", "IEEE_118","IEEE_300", "UK_high_voltage"))) %>%
  ungroup


metric_performance_plot_df <- metric_performance_plot_df %>% mutate(error_type ="standard")

