\name{inspect}
\alias{inspect}
\alias{inspect.seas}
\title{Interactively Inspect a Seasonal Adjustment Model (RStudio only)}
\usage{
inspect(x)

\method{inspect}{seas}(x)
}
\arguments{
  \item{x}{an object of class \code{"seas"}}
}
\description{
Interactively inspect a \code{"seas"} object.
\code{inspect} uses the \code{manipulate} package and can
only be used with RStudio. The goal of \code{inspect} is to
summarize all relevant options, plots and statistics that
should be usually considered.
}
\details{
The \code{inspect} function opens an interactive window
that allows for the manipulation of a number of arguments.
It offers several views to analyze the series graphically.
With each change, the adjustment process and the
visualizations are recalculated. Summary statics are shown
in the R console.

With the \code{Show static call} option, a replicable
static call is also shown in the console. Note that this
option will double the time for recalculation, as the
static function also tests the static call each time (this
is a beta feature of seasonal, which allows intensive
testing; it may be disabled in future versions).
}
\examples{
\dontrun{

m <- seas(AirPassengers)

inspect(m)

# pass arbitrary spec-arguments to inspect:
m2 <- seas(AirPassengers, estimate.maxiter = 1000)
inspect(m2)
}
}
\references{
R Studio IDE: \url{http://www.rstudio.com/ide/}
}
\seealso{
\code{\link{seas}} for the main function of seasonal.
}

