\name{monthplot.seas}
\alias{monthplot.seas}
\title{Plot the Seasonal or Irregular Factors}
\usage{
  \method{monthplot}{seas} (x, choice = "seasonal", ...)
}
\arguments{
  \item{x}{an object of class \code{"seas"}, usually, a
  result of a call to \code{\link{seas}}.}

  \item{choice}{character string, either \code{"seasonal"}
  or \code{"irregular"}.}

  \item{\dots}{further arguments passed to
  \code{monthplot.default}.}
}
\value{
  returns a plot as its side effect.
}
\description{
  \code{monthplot} method for class \code{"seas"}. Plot the
  seasonal factors or the irregular factors.
}
\examples{
x <- seas(AirPassengers, regression.aictest = c("td", "easter"))
monthplot(x)
monthplot(x, choice = "irregular")
}
\seealso{
  \code{\link{seas}} for the main function.
}

