\name{slug}
\alias{slug}
\docType{data}
\title{
Slug data
}
\description{
Lengths and weights of 100 slugs from the species Limax maximus collected around Hamilton, New Zealand.
}
\usage{data(slug)}
\format{
  A data frame with 100 observations on the following 4 variables.
  \describe{
    \item{\code{length}}{length (mm) of the slug}
    \item{\code{weight}}{weight (g) of the slug}
    \item{\code{log.len}}{natural logarithm of the \code{length}}
    \item{\code{log.wt}}{natural logarithm of the \code{weight}}
  }
}
\references{
Barker, G. and McGhie, R. (1984). The Biology of Introduced Slugs (Pulmonata) in New Zealand: Introduction and Notes on Limax Maximus, NZ Entomologist 8, 106--111.
}
\examples{
data(slug)
plot(weight~length, data = slug)
plot(log.wt~log.len, data = slug)

}
\keyword{datasets}
