\name{print.sscsamp}
\alias{print.sscsamp}
\title{
Print method for objects of class \code{sscsample}
}
\description{
This function provides a print summary method for the output of \code{sscsample}. The \code{sscsample} produces a large number of samples from a fixed population using either simple random, stratified, or cluster sampling. This function provides the means of each sample plus the number of observations from each ethnicity stratum in the sample.
}
\usage{
\method{print}{sscsamp}(x, \dots)
}
\arguments{
  \item{x}{an object of class \code{sscsamp} produced by \code{sscsample}}
  \item{\dots}{any other arguments that are to be passed to \code{cat}}
}
\author{James Curran}
\seealso{
\code{\link{sscsample}}
}

