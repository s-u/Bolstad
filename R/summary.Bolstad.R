#' Summarizing Bayesian Multiple Linear Regression
#' 
#' \code{summary} method for output of \code{\link{bayes.lm}}.
#' 
#' 
#' @param object an object of "\code{Bolstad}" that is the result of a call to \code{\link{bayes.lm}}
#' @param \dots any further arguments to be passed to \code{print}
#' 
#' 
#' @seealso The function to fit the model \code{\link{bayes.lm}}
#' 
#' The function \code{\link{coef}} to extract the matrix of posterior means along with standard errors and t-statistics.
#' 
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
             res.df = z$df.residual, ...)
  class(ans) = "summary.Bolstad"
  ans
}

