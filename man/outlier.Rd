\name{outlier}
\alias{outlier}
\title{Outlier Time series}
\usage{
  outlier(x)
}
\arguments{
  \item{x}{an object of class \code{"seas"}.}
}
\value{
  character string time series with outliers.
}
\description{
  Returns an object of class \code{"ts"} that contains the
  names of the outliers.
}
\examples{
\dontrun{
x <- seas(AirPassengers)
outlier(x)
}
}

