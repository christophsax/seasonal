\name{arimamodel}
\alias{arimamodel}
\title{Structure of the ARIMA Model}
\usage{
arimamodel(x)
}
\arguments{
  \item{x}{object of class \code{"seas"}}
}
\value{
numerical vector of length 6
}
\description{
Numerical vector of the form (p d q)(P D Q), containing the
non-seasonal and seasonal part of the ARIMA model.
}

