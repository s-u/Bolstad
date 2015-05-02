IQR = function(x, ...){
  UseMethod("IQR")
}

IQR.default = function(x, ...){
  stats::IQR(x, ...)
}

IQR.Bolstad = function(x, ...){
  return(diff(quantile(x, probs = c(0.25, 0.75), ...)))
}