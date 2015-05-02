mean.Bolstad = function(x, ...){
  if(any(grepl("mean", names(x))))
    return(x$mean)
  
  xVals = x$param.x
  fx = approxfun(xVals, xVals * x$posterior)
  
  return(integrate(fx, min(xVals), max(xVals))$value)
}

