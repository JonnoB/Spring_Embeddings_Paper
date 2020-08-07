#This is used to get the model performance across all metrics
#It does this for both with and without error spikes



multi_metric <- metric_set(rmse, rsq, mae, smape)
metric_combos <- expand_grid(
  metrics = unique(all_SETSe_emebeddings$metric),
  average_type =unique(all_SETSe_emebeddings$average_type),
  graph = unique(all_SETSe_emebeddings$graph)) %>%
  filter(average_type =="mean",
         metrics != "elev",
        # metrics != "strain"
         )


pl_metric_combos <- expand_grid(
  metrics = unique(PL_SETSe_emebeddings$metric),
  average_type =unique(PL_SETSe_emebeddings$average_type),
  graph = unique(PL_SETSe_emebeddings$graph)) %>%
  filter(average_type =="mean",
         metrics != "elev",
         # metrics != "strain"
  )

#This is to check that the UK high voltage is included
#test <- left_join(metric_combos, pl_metric_combos %>% mutate(not = 1))

#The formula used to create the model
model_dat <- formula(attack_round ~ value)

metric_performance_all <-1:nrow(metric_combos) %>%
  map_df(~{
    print(.x)
    
    temp <- all_SETSe_emebeddings %>%
      filter(
        average_type == metric_combos$average_type[.x],
        metric == metric_combos$metrics[.x],
        graph == metric_combos$graph[.x])
    
    
    pl_temp <- PL_SETSe_emebeddings %>%
      filter(
        average_type == metric_combos$average_type[.x],
        metric == metric_combos$metrics[.x],
        graph == metric_combos$graph[.x])
    
    
    pl_loess_mod <- loess(formula = model_dat,
                       data =  pl_temp)
    
    
    set.seed(4622)
    rs_obj <- vfold_cv(temp, v = 10, repeats = 10)

    #distribution version
    Out <- 1:length(rs_obj$splits) %>%
      map_df(~{
        
        loess_holdout_results(rs_obj$splits[[.x]], model_dat, metrics_out = TRUE) %>%
          mutate(Repeat = rs_obj$id[.x],
                 Fold = rs_obj$id2[.x])
        
      }) 
    
    #proportional_loading version
    pl_Out <- 1:length(rs_obj$splits) %>%
      map_df(~{
        
        holdout <- assessment(rs_obj$splits[[.x]])
        # Fit the model to the 90%
        #model is pre-fitted to the PL data and so doesn't need to be built
        
        # `augment` will save the predictions with the holdout data set
        model_comp <- holdout %>%
          mutate(preds =  predict(pl_loess_mod, holdout))
        
        #Output the accuracy of the model
        multi_metric(data = model_comp, truth = attack_round, estimate = preds) %>%
          mutate(Repeat = rs_obj$id[.x],
                 Fold = rs_obj$id2[.x]) 
        
      }) %>%
      rename(.pl_estimate = .estimate)
    
    #combine the two dataframes and return the result
   Out <- left_join(Out, pl_Out) %>% 
      mutate(metric = metric_combos$metrics[.x],
             average_type = metric_combos$average_type[.x],
             graph = metric_combos$graph[.x])
    
    
    return(Out)
    
  }) %>%
  mutate(combo = paste(average_type, metric)) %>%
  rename(eval_metric = .metric) %>%
  ungroup



