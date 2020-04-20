#This script creates a cross validated bivariate model and two other example models
#These models improve on the performance of the single variate model considerably

model_combos <- expand_grid(
  graph = unique(all_SETSe_emebeddings$graph),
  tibble(  metric_a = c("tension","loading", "tension"),
           metric_b = c("mean_ratiostrain", "mean_ratiostrain", "loading"))
)

#create crossvalidated two variable models of round major component is lost
model_xval_res <-1:nrow(mode_combos) %>%
  map_df(~{
    
    temp <- all_SETSe_emebeddings %>%
      filter(
        graph == model_combos$graph[.x],
        average_type == "mean") %>%
      pivot_wider(., names_from = metric, values_from = value)
    
    model_dat <- reformulate(c( model_combos$metric_a[.x], model_combos$metric_b[.x]), 
                             response = "attack_round") #attack round can be changed to blackout or generation
    
    
    set.seed(4622)
    rs_obj <- vfold_cv(temp, v = 10, repeats = 10)
    
    Out <- 1:length(rs_obj$splits) %>%
      map_df(~{
        
        loess_holdout_results(rs_obj$splits[[.x]], model_dat,  metrics_out = TRUE) %>%
          mutate(Repeat = rs_obj$id[.x],
                 Fold = rs_obj$id2[.x])
        
      })%>% 
      mutate(
        graph = model_combos$graph[.x],
        metric_a = model_combos$metric_a[.x],
        metric_b =  model_combos$metric_b[.x])
    
    return(Out)
  })


mean_model_xval_res <- model_xval_res %>%
  rename(eval_metric = .metric) %>%
  group_by(eval_metric, graph, metric_a, metric_b) %>%
  summarise(.estimate = mean(.estimate)) %>%
  ungroup %>%
  group_by(eval_metric, graph) %>%
  mutate(rank = rank(ifelse(eval_metric=="rsq",-.estimate, .estimate))) %>%
  ungroup()

mean_metric_performance <- metric_performance_plot_df %>%
  filter(error_type =="standard") %>%
  filter(rank == 1) %>%
  rename(simple_estimate = .estimate)

combined_perf <-left_join(mean_model_xval_res, mean_metric_performance, by = c("eval_metric", "graph")) %>%
  mutate(improved = ifelse(eval_metric =="rsq", .estimate>simple_estimate, .estimate<simple_estimate),
         diff = .estimate-simple_estimate,
         ratio = .estimate/simple_estimate,
         perc_change = ifelse(eval_metric =="rsq",-diff/(1-simple_estimate) , diff/simple_estimate)) %>%
  ungroup


model_comparison <- 1:nrow(combined_perf) %>%
  map_df(~{
    
    temp  <-combined_perf %>%
      slice(.x)
    
    temp_2 <- model_xval_res %>%
      filter(graph==temp$graph, metric_a ==temp$metric_a, metric_b == temp$metric_b, .metric ==temp$eval_metric)
    
    temp_3 <- metric_performance_all %>%
      filter(graph ==temp$graph, combo == temp$combo, eval_metric ==temp$eval_metric)
    
    t.test(x = temp_2$.estimate, y = temp_3$.estimate, alternative = ifelse(temp$eval_metric=="rsq", "greater" ,"less")) %>% tidy %>%
      bind_cols(temp)
    
  }) %>%
  mutate(significant = p.value<5e-2)


