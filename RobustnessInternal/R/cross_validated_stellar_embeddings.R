#' Produces performance metrics for the stellargraph embeddings
#' 
#' Takes a dataframe of stellargraph embeddings and returns a cross validated dataframe of 
#' performance metrics from GAM across all dimensions with y being the point of loss of the giant component
#' 
#' @param stellar_all a dataframe. 
#' 
#' @export

#This is used to get the model performance across all metrics

cross_validated_stellar_embeddings <-function(stellar_all){

#The metrics that will be tested
multi_metric <- metric_set(rmse, rsq, mae, smape)


#The formula used to create the model
xnam <- paste0("X", 0:31)
model_dat <- as.formula(paste("attack_round ~ ", paste(xnam, collapse= "+")))


metric_combos <- stellar_all %>%
  select(metric, average_type, graph)%>%
  distinct()

other_metric_performance_all <- 1:nrow(metric_combos) %>%
  map_df(~{
    
    print(paste(.x, "of", nrow(metric_combos), sep = " "))

    temp <- stellar_all  %>%
      filter(
        average_type == metric_combos$average_type[.x],
        metric == metric_combos$metric[.x],
        graph == metric_combos$graph[.x])

    set.seed(4622)
    rs_obj <- vfold_cv(temp, v = 10, repeats = 10)
    
    #distribution version
    Out <- 1:length(rs_obj$splits) %>%
      map_df(~{
        
        gam_holdout_results(splits = rs_obj$splits[[.x]], model_dat = model_dat, metric_out = TRUE) %>%
          mutate(Repeat = rs_obj$id[.x],
                 Fold = rs_obj$id2[.x])
        
      })  %>% 
      mutate(metric = metric_combos$metric[.x],
             average_type = metric_combos$average_type[.x],
             graph = metric_combos$graph[.x])
    
    
    return(Out)
    
  }) %>%
  rename(eval_metric = .metric) %>%
  ungroup

 return(other_metric_performance_all)
}