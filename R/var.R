var = function(x, ...)
  UseMethod("var")

var.default = function(x, ...){
  stats::var(x, ...)
}

var.Bolstad = function(x, ...){
  if(any(grepl("var", names(x))))
    return(x$var)
  
  xVals = x$param.x
  mx = mean(x, ...)
  fx = approxfun(xVals, (xVals - mx)^2 * x$posterior)
  
  return(integrate(fx, min(xVals), max(xVals))$value)
}
