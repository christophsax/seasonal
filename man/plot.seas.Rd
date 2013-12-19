\name{plot.seas}
\alias{plot.seas}
\title{Plot the Adjusted and Unadjusted Series}
\usage{
\method{plot}{seas}(x, outliers = TRUE, trend = FALSE, ...)
}
\arguments{
  \item{x}{an object of class \code{"seas"}, usually, a
  result of a call to \code{\link{seas}}.}

  \item{outliers}{logical, should the oultiers be drawn}

  \item{trend}{logical, should the trend be drawn}

  \item{\dots}{further arguments passed to \code{ts.plot}.}
}
\value{
returns a plot as its side effect.
}
\description{
\code{plot} method for class \code{"seas"}. Plot the
adjusted and unadjusted series, as well as the outliers.
Optionally draw the trend series.
}
\examples{
\dontrun{
x <- seas(AirPassengers, regression.aictest = c("td", "easter"))
plot(x)
plot(x, outliers = FALSE)
plot(x, trend = TRUE)
}
}
\seealso{
\code{\link{seas}} for the main function.
}

