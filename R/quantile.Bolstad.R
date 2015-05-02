quantile.Bolstad = function(x, probs = seq(0, 1, 0.25), ...){
  if(any(grepl("quantileFun", names(x))))
    return(x$quantileFun(probs, ...))
  
  res = sintegral(x$param.x, x$posterior)$cdf
  qFn = approxfun(res$y, res$x)
  
  return(qFn(probs))
}
