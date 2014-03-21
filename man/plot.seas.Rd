\name{plot.seas}
\alias{monthplot.seas}
\alias{plot.revisions}
\alias{plot.seas}
\alias{plot.slidingspans}
\alias{residplot}
\title{Seasonal Adjustment Plots}
\usage{
\method{plot}{seas}(x, outliers = TRUE, trend = FALSE,
  main = "unadjusted and seasonally adjusted series", ...)

residplot(x, outliers = TRUE, ...)

\method{monthplot}{seas}(x, choice = "seasonal", ...)

\method{plot}{slidingspans}(x, main = "sliding spans", ...)

\method{plot}{revisions}(x, series = c("saestimates", "chngestimates",
  "sarevisions", "sfestimates", "trendestimates"), ...)
}
\arguments{
  \item{x}{an object of class \code{"seas"}, usually, a
  result of a call to \code{\link{seas}}.}

  \item{outliers}{logical, should the oultiers be drawn.}

  \item{trend}{logical, should the trend be drawn.}

  \item{choice}{character string, \code{"seasonal"}
  (default) or \code{"irregular"}.}

  \item{main}{character string, title of the graph.}

  \item{series}{name of the series to plot}

  \item{\dots}{further arguments passed to the plotting
  functions.}
}
\value{
All plot functions returns a plot as their side effect.
}
\description{
Functions to graphically analyze a \code{"seas"} object.
More graphs can be plotted with standard R functions (see
examples). For diagnostical statistics, see
\code{\link{qs}}.
}
\details{
\code{plot} calls the plot method for class \code{"seas"}.
It plots the adjusted and unadjusted series, as well as the
outliers. Optionally draws the trend series.

\code{residplot} plots the residuals and the outliers.

\code{monthplot} calls the monthplot method for class
\code{"seas"}. It plot the seasonal and SI component
periodwise. Like the default method `monthplot` can be used
for all frequencies.

\code{plot.slidingspans} calls the plot method for objects
of class \code{"slidingspans"}. It draws the seasonal
component for the analyzed spans.

\code{plot.revisions} calls the plot method for objects of
class \code{"revisons"}. It draws concurrent and the latest
estimation of the seasonal adjusted series.
}
\examples{
\dontrun{

m <- seas(AirPassengers)

plot(m)
plot(m, outliers = FALSE)
plot(m, trend = TRUE)

residplot(m)
residplot(m, outliers = FALSE)

monthplot(m)

plot(slidingspans(m))
rev <- revisions(m)
plot(rev, "chngestimates")

# use R functions to analyze "seas" models
pacf(resid(m))
spectrum(diff(resid(m)))
plot(density(resid(m)))
qqnorm(resid(m))
}
}
\references{
Vignette with a more detailed description:
\url{http://cran.r-project.org/web/packages/seasonal/vignettes/seas.pdf}

Wiki page with a comprehensive list of R examples from the
X-13ARIMA-SEATS manual:
\url{https://github.com/christophsax/seasonal/wiki/Examples-of-X-13ARIMA-SEATS-in-R}

Official X-13ARIMA-SEATS manual:
\url{http://www.census.gov/ts/x13as/docX13AS.pdf}
}
\seealso{
\code{\link{seas}}, for the main function.

\code{\link{qs}}, for diagnostical statistics.
}

