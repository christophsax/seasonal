\name{residplot}
\alias{residplot}
\title{Plot the Residuals of an X13 regARIMA model}
\usage{
  residplot(x, outliers = TRUE, ...)
}
\arguments{
  \item{x}{an object of class \code{"seas"}, usually, a
  result of a call to \code{\link{seas}}.}

  \item{outliers}{logical, should the oultiers be drawn}

  \item{\dots}{further arguments passed to \code{ts.plot}.}
}
\value{
  returns a plot as its side effect.
}
\description{
  Plot the residuals of an X13 regARIMA model, as well as
  the outliers. Optionally draw the trend series.
}
\examples{
x <- seas(AirPassengers, regression.aictest = c("td", "easter"))
residplot(x)
residplot(x, outliers = FALSE)
}
\seealso{
  \code{\link{seas}} for the main function.
}

