#' @export

print.summary.Bolstad = function(x, digits = max(3L, getOption("digits") - 3L), ...) {
  cat("\nCall:\n")
  cat(paste0(deparse(x$call), sep = "\n"), "\n")
  
  cat("Residuals:", "\n")
  resid = x$residuals
  if (x$res.df > 5L) {
    nam <- c("Min", "1Q", "Median", "3Q", "Max")
    rq <- if (length(dim(resid)) == 2L) 
      structure(apply(t(resid), 1L, quantile), dimnames = list(nam, 
                                                               dimnames(resid)[[2L]]))
    else {
      zz <- zapsmall(quantile(resid), digits + 1L)
      structure(zz, names = nam)
    }
  }
  print(rq, digits = digits, ...)
  
  cat("\nCoefficients:\n")
  coef.mat = coef(x)
  print(format(coef.mat, digits = digits), print.gap = 2L, quote = FALSE, ...)
  
  cat("---\n")
  
  if (x$prior) {
    cat("Prior Coefficients:\n")
    names(x$prior.coef) = x$terms
    print(format(x$prior.coef, digits = digits), print.gap = 2L, quote = FALSE, ...)
    
    cat("\nPrior Covariance Matrix:\n")
    dimnames(x$prior.cov) = list(x$terms, x$terms)
    print(format(x$prior.cov, digits = digits), print.gap = 2L, quote = FALSE, ...)
    
  } else {
    cat("\nNote: No prior given (Using flat prior).\n")
  }
}