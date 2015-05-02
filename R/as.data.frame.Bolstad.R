as.data.frame.Bolstad = function(x, ...){
  result = data.frame(param.x = x$param.x, prior = x$prior, likelihood = x$likelihood, 
                      posterior = x$posterior, ...)
  names(result)[1] = x$name
  return(result)
}