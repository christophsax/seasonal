\name{fivebestmdl}
\alias{fivebestmdl}
\title{Extract the five best ARIMA models}
\usage{
fivebestmdl(x)
}
\arguments{
  \item{x}{object of class "seas"}
}
\description{
if the \code{automdl} spec is activated (as in the
default), the function \code{fivebestmdl} returns the five
best models as chosen by the BIC criterion.
}
\examples{
\dontrun{
x <- seas(AirPassengers)
fivebestmdl(x)
}
}

