\name{regressioneffects}
\alias{regressioneffects}
\title{Show regressioneffects}
\usage{
regressioneffects(x)
}
\arguments{
  \item{x}{object of class "seas"}
}
\description{
matrix of regression variables multiplied by the vector of
estimated regression coefficients
}
\examples{
# trading day and easter adjustment w/o seasonal adjustment:
x <- seas(AirPassengers)

summary(x)
re <- regressioneffects(x)
ce <- re[, 'Trading.Day'] + re[, 'Holiday']
 # be aware of log transformation
AirPassengersWoTd <- exp(log(AirPassengers) - ce)
}

