#' generate the dataframe showing the cross validated error of each metric
#'
#' This is used so that the error can be plotted showing the performance differences between the methods
#' elevation is excluded for now but can be re-included. This will require a change to the processing functions as well
#' 
#' @param all_SETSe_emebeddings A dataframe this is loaded earlier in the project
#' @param PL_SETSe_emebeddings A dataframe this is loaded earlier in the project
#' 
#' @export

cross_validated_setse_embeddings <- function(all_SETSe_emebeddings, PL_SETSe_emebeddings){
  
  #This is used to get the model performance across all metrics
  #It does this for both with and without error spikes
  
  #The metrics that will be tested
  multi_metric <- metric_set(rmse, rsq, mae, smape)
  
  #the dataframe that defines the network metric combinations to be analysed
  metric_combos <-all_SETSe_emebeddings %>%
    select(metric, average_type, graph)%>%
    distinct() %>%
    filter(average_type =="mean"
    )
  
  #The combinations used when the PL value is taken to be base case
  pl_metric_combos <- PL_SETSe_emebeddings %>%
    select(metric, average_type, graph)%>%
    distinct() %>%
    filter(average_type =="mean"
    )
  
  #This is to check that the UK high voltage is included
  #test <- left_join(metric_combos, pl_metric_combos %>% mutate(not = 1))
  
  #The formula used to create the model
  model_dat <- formula(attack_round ~ value)
  
  metric_performance_all <-1:nrow(metric_combos) %>%
    map_df(~{
      print(paste(.x, "of",nrow(metric_combos), metric_combos$metric[.x], metric_combos$graph[.x] ))
      
      temp <- all_SETSe_emebeddings %>%
        filter(
          average_type == metric_combos$average_type[.x],
          metric == metric_combos$metric[.x],
          graph == metric_combos$graph[.x])
      
      
      pl_temp <- PL_SETSe_emebeddings %>%
        filter(
          average_type == metric_combos$average_type[.x],
          metric == metric_combos$metric[.x],
          graph == metric_combos$graph[.x])
      
      
      pl_loess_mod <- gam(formula = model_dat,
                            data =  pl_temp)
      
      
      set.seed(4622)
      rs_obj <- vfold_cv(temp, v = 10, repeats = 10)
      
      #distribution version
      Out <- 1:length(rs_obj$splits) %>%
        map_df(~{
          
          gam_holdout_results(rs_obj$splits[[.x]], model_dat, metrics_out = TRUE) %>%
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
      Out <- left_join(Out, pl_Out, by = c(".metric", ".estimator", "Repeat", "Fold")) %>% 
        mutate(metric = metric_combos$metric[.x],
               average_type = metric_combos$average_type[.x],
               graph = metric_combos$graph[.x])
      
      
      return(Out)
      
    }) %>%
    mutate(combo = paste(average_type, metric)) %>%
    rename(eval_metric = .metric) %>%
    ungroup 
  
  error_df <- metric_performance_all %>%
    mutate( graph_order  = factor(gsub("_igraph", "", graph), 
                                  levels = c("IEEE_14", "IEEE_30", "IEEE_57", "IEEE_118","IEEE_300",
                                             "UK_high_voltage")),
            perf_diff = .estimate-.pl_estimate) #%>%
    # filter(#error_type =="standard",
    #   metric != "strain"
    # ) %>%
    # filter(metric !="alpha")
    # 
  
  return(error_df)
  
  
}