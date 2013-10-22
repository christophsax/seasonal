\name{OutlierTimeSeries}
\alias{OutlierTimeSeries}
\title{A string time series with outliers}
\usage{
  OutlierTimeSeries(x, outlier)
}
\arguments{
  \item{x}{time series template}

  \item{outlier}{character string with outliers}
}
\description{
  A string time series with outliers
}
\examples{
OutlierTimeSeries(AirPassengers, c("ao1951.May", "ao1954.Feb", "ls1956.Feb"))
OutlierTimeSeries(austres, c("ao1981.1", "ao1984.2", "ls1986.4"))
OutlierTimeSeries(austres, character())
}

