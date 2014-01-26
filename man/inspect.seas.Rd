\name{inspect.seas}
\alias{inspect.seas}
\title{Inspection Function for Seasonal Adjustment (RStudio only)}
\usage{
\method{inspect}{seas}(x, ...)
}
\arguments{
  \item{x}{an object of class \code{"ts"}}

  \item{...}{additional spec/arguments options}
}
\description{
Inspect the seasonal adjustment of a time series.
\code{inspect} uses the \code{manipulate} package from
RStudio and can not be used without it.
}
\examples{
\dontrun{
inspect(AirPassengers)

# pass arbitrary spec/arguments to inspect:
inspect(AirPassengers, estimate.maxiter = 1000)
}
}

