\name{plot.seas}
\alias{hist.seas}
\alias{monthplot.seas}
\alias{plot.revisions}
\alias{plot.seas}
\alias{plot.slidingspans}
\alias{residplot}
\title{Plot Functions for Seasonal Adjustment Models}
\usage{
\method{plot}{seas}(x, outliers = TRUE, trend = FALSE,
  main = "unadjusted and seasonally adjusted series", ylab = "value", ...)

residplot(x, outliers = TRUE, ...)

\method{monthplot}{seas}(x, choice = "seasonal", ...)

\method{hist}{seas}(x, ...)

\method{plot}{revisions}(x)

\method{plot}{slidingspans}(x, main = "sliding spans",
  ylab = "Seasonal Component", xlab = "", ...)
}
\arguments{
  \item{x}{an object of class \code{"seas"}, usually, a
  result of a call to \code{\link{seas}}.}

  \item{outliers}{logical, should the oultiers be drawn}

  \item{trend}{logical, should the trend be drawn}

  \item{choice}{character string, \code{"seasonal"}
  (default) or \code{"irregular"}.}

  \item{\dots}{further arguments passed to other methods.}
}
\value{
All plot functions returns a plot as their side effect.
}
\description{
Functions to graphically analyze a seasonal adjustment
model.
}
\details{
\code{plot} calls the plot method for class \code{"seas"}.
It plots the adjusted and unadjusted series, as well as the
outliers. Optionally draws the trend series.

\code{monthplot} calls the monthplot method for class
\code{"seas"}. It plot the seasonal and SI component
periodwise. Like the default method `monthplot` can be used
for all frequencies. \code{residplot} plots the residuals
and the outliers.

\code{plot.slidingspans} calls the plot method for models
of class \code{"slidingspans"}. It draws the seasonal
component for the analyzed spans.

\code{plot.revisions} calls the plot method for models of
class \code{"revisons"}. It draws concurrent and the latest
estimation of the seasonal component.

\code{hist.revisions} calls the hist method for models of
class \code{"seas"}. Plots a histogram of the residuals and
compares it to a normal distribution.
}
\examples{
\dontrun{
x <- seas(AirPassengers)

plot(x)
plot(x, outliers = FALSE)
plot(x, trend = TRUE)

residplot(x)
residplot(x, outliers = FALSE)

monthplot(x)

plot(slidingspans(x))

plot(revisions(x))

hist(x)

# Using R base functions for analyzing "seas" models.
pacf(resid(x))
spectrum(diff(resid(x)))

}
}
\seealso{
\code{\link{seas}} for the main function.
}

