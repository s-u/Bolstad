sd = function(x, ...)
  UseMethod("sd")

sd.default = function(x, ...){
  stats::sd(x, ...)
}

sd.Bolstad = function(x, ...){
  return(sqrt(var(x, ...)))
}
