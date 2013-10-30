\name{outlier}
\alias{outlier}
\title{Outlier Time series}
\usage{
  outlier(x)
}
\arguments{
  \item{x}{an object of class "seas".}
}
\value{
  character string time series with outliers.
}
\description{
  Returns a time series that contains the names of the
  outliers.
}
\examples{
x <- seas(AirPassengers)
outlier(x)
}

