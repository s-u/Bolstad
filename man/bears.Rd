\name{bears}
\alias{bears}
\docType{data}
\title{
bears
}
\description{
Body measurements for 143 wild bears.
}
\usage{data("bears")}
\format{
  A data frame with 143 observations on the following 12 variables.
  \describe{
    \item{\code{ID}}{Indentification number}
    \item{\code{Age}}{Bear's age, in months. Note, wild bears are always born in January, so an expert can estimate the bear's age without directly asking it how old it is.
}
    \item{\code{Month}}{Month when the measurement was made. 1 = Jan., 12 = Dec. Since bears hibernate in the winter, their body shape probably depends on the season.
}
    \item{\code{Sex}}{1 = male 2 = female
}
    \item{\code{Head.L}}{Length of the head, in inches
}
    \item{\code{Head.W}}{Width of the head, in inches
}
    \item{\code{Neck.G}}{Girth (distance around) the neck, in inches
}
    \item{\code{Length}}{Body length, in inches
}
    \item{\code{Chest.G}}{Girth (distance around) the chest, in inches
}
    \item{\code{Weight}}{Weight of the bear, in pounds
}
    \item{\code{Obs.No}}{Observation number for this bear. For example, the bear with ID = 41 (Bertha) was measured on four occasions, in the months coded 7, 8, 11, and 5. The value of Obs.No goes from 1 to 4 for these observations.
}
    \item{\code{Name}}{The names of the bears given to them by the researchers}
  }
}
\details{
Wild bears were anesthetized, and their bodies were measured and weighed. One goal of the study was to make a table (or perhaps a set of tables) for hunters, so they could estimate the weight of a bear based on other measurements. This would be used because in the forest it is easier to measure the length of a bear, for example, than it is to weigh it.
}
\source{
This data is in the example data set Bears.MTW distributed with Minitab
}
\references{
This data set was supplied by Gary Alt. Entertaining references are in Reader's Digest April, 1979, and Sports Afield September, 1981.
}
\examples{
data(bears)
boxplot(Weight~Sex, data = bears)
}
\keyword{datasets}
