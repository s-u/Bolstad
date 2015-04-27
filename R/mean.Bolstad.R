mean.Bolstad = function(x, ...){
  xVals = x$param.x
  fx = approxfun(xVals, xVals * x$posterior)
  
  return(integrate(fx, min(xVals), max(xVals))$value)
}

var = function(x, ...)
  UseMethod("var")

var.default = function(x, y = NULL, na.rm = FALSE, use){
  stats:::var(x, y = NULL, na.rm = FALSE, use)
}

var.Bolstad = function(x, ...){
  xVals = x$param.x
  mx = mean(x, ...)
  fx = approxfun(xVals, (xVals - mx)^2 * x$posterior)
  
  return(integrate(fx, min(xVals), max(xVals))$value)
}

sd = function(x, ...)
  UseMethod("sd")

sd.default = function(x, na.rm = FALSE){
  stats:::sd(x, na.rm)
}

sd.Bolstad = function(x){
  return(sqrt(var(x)))
}

cdf = function(x, ...){
  UseMethod("cdf")
}

cdf.Bolstad = function(x, ...){
  if(class(x) != "Bolstad")
    stop("x must be an object of class Bolstad")
  
    res = sintegral(x$param.x, x$posterior, warn = FALSE)$cdf
    return(splinefun(res$x, res$y))
}

quantile.Bolstad = function(x, probs = seq(0, 1, 0.25), ...){
  res = sintegral(x$param.x, x$posterior, warn = FALSE)$cdf
  qFn = splinefun(res$y, res$x)
  
  return(qFn(probs))
}

median.Bolstad = function(x, ...){
  return(quantile(x, probs = 0.5, ...))
}

IQR = function(x, ...)
  UseMethod("IQR")

IQR.default = function(x, na.rm = FALSE, type = 7, ...){
  stats:::IQR(x, na.rm, type)
}

IQR.Bolstad = function(x, ...){
  diff(quantile(x, probs = c(0.25, 0.75)))
}