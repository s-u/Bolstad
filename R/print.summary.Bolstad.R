#' Print method for objects of class \code{sscsample}
#' 
#' This function provides a print summary method for the output of
#' \code{sscsample}. The \code{sscsample} produces a large number of samples
#' from a fixed population using either simple random, stratified, or cluster
#' sampling. This function provides the means of each sample plus the number of
#' observations from each ethnicity stratum in the sample.
#' 
#' 
#' @param x an object of class \code{sscsamp} produced by \code{sscsample}
#' @param \dots any other arguments that are to be passed to \code{cat}
#' @author James Curran
#' @seealso \code{\link{sscsample}}
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
  coef.mat = matrix(nrow = x$rank, ncol = 3)
  
  colnames(coef.mat) = c("Posterior Mean", "Std. Error", "t value")
  rownames(coef.mat) = c(x$terms)
  
  coef.mat[, "Posterior Mean"] = x$coef
  coef.mat[, "Std. Error"] = x$std.err
  coef.mat[, "t value"] = x$coef / x$std.err
  
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