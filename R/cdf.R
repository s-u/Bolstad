cdf = function(x, ...){
  UseMethod("cdf")
}

cdf.Bolstad = function(x, ...){
  if(class(x) != "Bolstad")
    stop("x must be an object of class Bolstad")
  
  if(any(grepl("cdf", names(x))))
    return(x$cdf)
  
  res = sintegral(x$param.x, x$posterior)$cdf
  return(splinefun(res$x, res$y))
}
