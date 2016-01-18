#' Compute the posterior median
#' 
#' @param x an object of class \code{Bolstad}.
#' @param \dots currently ignored 
#' @author James Curran
#' @method median Bolstad
#' 
#' @export
#' 
median.Bolstad = function(x, ...){
  return(quantile(x, probs = 0.5))
}

