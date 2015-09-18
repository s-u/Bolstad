#' as.data.frame.Bolstad
#' @param x an object of class \code{Bolstad}
#' @param probs numeric vector of probabilities with values in \eqn{[0,1]}.
#' @param \dots, any extra arguments needed.
#' @export as.data.frame.Bolstad
as.data.frame.Bolstad = function(x, ...){
  result = data.frame(param.x = x$param.x, prior = x$prior, likelihood = x$likelihood, 
                      posterior = x$posterior, ...)
  names(result)[1] = x$name
  return(result)
}