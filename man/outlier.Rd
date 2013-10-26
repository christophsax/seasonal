\name{outlier}
\alias{outlier}
\title{A time series with outliers}
\usage{
  outlier(x)
}
\arguments{
  \item{x}{time series template}

  \item{outlier}{character string with outliers}
}
\description{
  A time series with outliers
}
\examples{
x <- seas(AirPassengers)
outlier(x)
}

