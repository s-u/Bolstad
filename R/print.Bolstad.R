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

print.Bolstad = function(x, digits = max(3L, getOption("digits") - 3L), ...) {
  getTermLabels = function(x) 
    attr(x$terms, "term.labels")
  
  cat("\nCall:", paste0(deparse(x$call)), sep = "\n", collapse= "\n")
  
  if (length(coef(x))) {
    cat("Coefficients:\n")
    c = coef(x)
    names(c) = c("(Intercept)", getTermLabels(x))
    print(format(c, digits = digits), print.gap = 2L, quote = FALSE)
  }
  cat("\n")
}

