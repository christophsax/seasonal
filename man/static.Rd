\name{static}
\alias{static}
\title{Static Call of a seas Object}
\usage{
static(x, coef = FALSE, test = TRUE, verbose = FALSE)
}
\arguments{
  \item{x}{an object of class \code{seas}}

  \item{coef}{logical, if \code{TRUE}, the coefficients are
  treated as fixed, instead of beeing estimated.}

  \item{test}{logical. By default the static call is
  executed and compared to the input call. If the final
  series is not identical, a warning is returned. If
  \code{FALSE}, the option is disabled.}

  \item{verbose}{logical, if \code{TRUE}, droped and kept
  series are listed.}
}
\value{
Object of class \code{"call"}. Static call of an object of
class \code{seas}. Can be copy/pasted into an R script.
}
\description{
A static call is a static replication of a call. Automatic
procedures are substituted by the automatically chosen
spec-argument options. The call can be copy/pasted to a
script and used for further manipulations or future
evaluation of the same model.
}
\details{
By default, the static call is tested. It is executed and
compared to the input call. If the final series is not
identical, a warning is returned.

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

