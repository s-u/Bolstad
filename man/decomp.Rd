\name{decomp}
\alias{decomp}
\title{
Plot the prior, likelihood, and posterior on the same plot.
}
\description{
This function takes any object of class \code{Bolstad} and plots the prior, likelihood and posterior on the same plot. The aim is to show the influence of the prior, and the likelihood on the posterior.
}
\usage{
decomp(x, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{an object of class \code{Bolstad}.}
  \item{\dots}{any other arguments to be passed to the \code{plot} function.}
}
\author{
James Curran
}
\note{
Note that \code{xlab}, \code{ylab}, \code{main}, \code{axes}, \code{xlim}, \code{ylim} and 
\code{type} are all used in the function so specifying them is unlikely to have any effect.
}
\examples{
# an example with a binomial sampling situation
results = binobp(4, 12, 3, 3, plot = FALSE)
decomp(results)

# an example with normal data
y = c(2.99,5.56,2.83,3.47)
results = normnp(y, 3, 2, 1, plot = FALSE)
decomp(results)
}
\keyword{plots}
