#' coerce values between 0 and 1
#' 
#' This function simply coerces the values of a vector to be a fraction of the minimum and maximum values
#' It is useful for putting everything on the same relative scale
#' 
#' @param value a numeric vector. The vector of values to transform
#' 
#' @export


#Is this function here neccessary? or is it in some folder that gets it all laoded?
#function that finds what fraction of the full range the metric is
kappa <- function(value){
  
  (value-min(value))/(max(value) - min(value))
  
}
