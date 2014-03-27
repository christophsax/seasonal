\name{out}
\alias{out}
\title{Import X-13ARIMA-SEATS Text Output}
\usage{
out(x, line = 1, n = 100, search = NULL, file = c("out", "log", "err"))
}
\arguments{
  \item{x}{an object of class \code{"seas"}.}

  \item{line}{starting line of the content.}

  \item{n}{number of lines to show on a page.}

  \item{search}{regular expression chracter string. If
  specified, the content is searched for the first
  occurence (see examples).}

  \item{file}{\code{"out"} or \code{"log"}, which file to
  show.}
}
\value{
an object of class \code{"out"}, essentially a character
vector with attributes. The print method for \code{"out"}
objects is adapted to the large size of the \code{.out}
output. It allows for pagination and search (see examples).
}
\description{
The \code{out} function shows the full content of the
\code{.out}, the \code{.log} or the \code{.err} file form
X-13ARIMA-SEATS.
}
\details{
To keep the size of \code{"seas"} objects small, "seas"
does not save the content of the \code{.out} by default.
Instead, the \code{out} function re-evaluates the model.
}
\examples{
\dontrun{
m <- seas(AirPassengers)

# exit from the viewer with [q]
out(m)
out(m, search = "regARIMA model residuals")
out(m, search = "Normality Statistics for regARIMA")

# on some platforms, this may be more useful
edit(out(m))

m <- seas(AirPassengers, slidingspans = list())
out(m, search = "Sliding spans analysis", n = 300)

# showing the log file
out(m, file = "log")

# showing the error file
out(m, file = "err")
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
\code{\link{seas}} for the main function of seasonal.
}

