#' cross validate performance using loes
#' 
#' @param splits The random splits to be used when creating the model these are created by
#' vfold_cv from the rsample package
#' @param model_dat formula the formula that will be used to create the model
#' @param metrics_out logical output either the model performance metrics or the actual predictions
#' 
#' @export

loess_holdout_results <- function(splits, model_dat, metrics_out = TRUE ,...) {
  
  train_df = analysis(splits)
  
  # Save the 10%
  holdout <- assessment(splits)
  # Fit the model to the 90%
  loess_mod <- loess(formula = model_dat,
                     data = train_df)
  
  # `augment` will save the predictions with the holdout data set
  model_comp <- holdout %>%
    mutate(preds =  predict(loess_mod, holdout))
  
  #Output the accuracy of the model
  metric_summary <- multi_metric(data = model_comp, truth = attack_round, estimate = preds) 
  
  if(metrics_out){
    Out <- metric_summary
  } else {
    
    Out <- model_comp
    
  }
  
  return(Out)
  
}