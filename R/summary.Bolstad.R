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

summary.Bolstad = function(object, ...) {
  getTermLabels = function(x) 
    attr(x$terms, "term.labels")
  
  z = object
  
  # if prior.coef = NULL; prior = FALSE
  
  ans = list(rank = z$rank,
             call = z$call,
             terms = c("(Intercept)", getTermLabels(z)),
             coef = z$coefficients,
             std.err = diag(z$post.var),
             prior = TRUE,
             prior.coef = rep(0, 7), 
             prior.cov = diag(rep(1e6,7)),
             residuals = as.vector(z$residuals),
             res.df = z$df.residual)
  class(ans) = "summary.Bolstad"
  ans
}

