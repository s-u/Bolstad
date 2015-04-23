\name{sscsample.data}
\alias{sscsample.data}
\docType{data}
\title{
Data for simple random sampling, stratified sampling, and clusting sampling experiments
}
\description{
A simulated population made up of 100 individuals. The individuals come
from three ethnic groups with population proportions of 40\%, 40\%,
and 20\%, respectively. There are twenty neighborhoods, and five
individuals live in each one.  Now, the income distribution may be
different for the three ethnic groups.  Also, individuals in the
same neighborhood tend to be more similar than individuals in
different neighborhoods.
}
\usage{data("sscsample.data")}
\format{
  A data frame with 100 observations on the following 3 variables.
  \describe{
    \item{\code{income}}{Simulated income in $10,000}
    \item{\code{ethnicity}}{A numerical vector indicating the ethnic group of the observation}
    \item{\code{neighborhood}}{A numeric vector indicating the neighborhood of the observation}
  }
}
\examples{
data(sscsample.data)
plot(income~ethnicity, data = sscsample.data)
}
\keyword{datasets}
