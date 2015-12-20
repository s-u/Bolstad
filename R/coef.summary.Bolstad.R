coef.summary.Bolstad = function(object, ...) {
  coef.mat = matrix(nrow = object$rank, ncol = 3)
  
  colnames(coef.mat) = c("Posterior Mean", "Std. Error", "t value")
  rownames(coef.mat) = c(object$terms)
  
  coef.mat[, "Posterior Mean"] = object$coef
  coef.mat[, "Std. Error"] = object$std.err
  coef.mat[, "t value"] = object$coef / object$std.err
  
  return(coef.mat)
}