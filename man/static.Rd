\name{static}
\alias{static}
\title{Static Call of a seas Object}
\usage{
static(x, coef = FALSE, test = TRUE)
}
\arguments{
  \item{x}{an object of class \code{seas}}

  \item{coef}{logical, if \code{TRUE}, the coefficients are
  treated as fixed, instead of beeing estimated.}

  \item{name}{character string, optionally specify the name
  of the input time series}

  \item{test}{logical. By default the static call is
  executed and compared to the input call. If the final
  series is not identical, an error is returned.  If
  \code{FALSE}, the option is disabled.}
}
\value{
Static call of an object of class \code{seas}. Can be
copy/pasted into an R script.
}
\description{
A static call is a static replication of a call. Automatic
procedures are subistuted by the automatically selected
spec-argument options.
}
\details{
By default, the static call is tested. It is executed and
compared to the input call. If the final series is not
identical, an error is returned.

If \code{coef = TRUE}, the coefficients are fixed as well.
}
\examples{
\dontrun{

m <- seas(AirPassengers)
static(m)
static(m, test = FALSE)
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

