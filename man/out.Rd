\name{out}
\alias{out}
\alias{revisions}
\alias{slidingspans}
\title{Diagnostical Re-Evaluation}
\usage{
out(x, line = 1, n = 100, search = NULL, ...)

revisions(x, ...)

slidingspans(x, ...)
}
\arguments{
  \item{x}{an object of class \code{"seas"} to
  re-evaluate.}

  \item{line}{starting line of the content.}

  \item{n}{number of lines to show at once.}

  \item{search}{chracter string. If specified, the content
  is searched for the first occurence of the string (see
  examples).}

  \item{...}{aditional spec-arguments options (see
  details).}
}
\value{
invisible, the full content or parts of the .out file as a
(large) character vector. The content of the vector is
displayed by a viewer function in the R console.
}
\description{
For diagnostical purposes, these functions re-evaluate an
object of class seas and capture the full content or parts
of the .out file from X-13ARIMA-SEATS.
}
\details{
The \code{out} function shows the full content of the
\code{.out} file form X-13ARIMA-SEATS. The
\code{slidingspans} and \code{revisions} function call the
\code{slidingspans} and \code{history} spec of
X-13ARIMA-SEATS and show the respective parts of the
\code{.out} file. Note that against the convention, the
\code{history} spec is called by the function
\code{revision}, in order to avoid a naming collision with
the function from the \code{utils} pacakge. For a
description of the \code{slidingsspan} and \code{history}
spec, consider the X-13ARIMA-SEATS manual.

For how to enter spec-arguments options, see the details in
\code{"\link{seas}"}. In the \code{out} function,
\code{...} are useful to add additional outputs to the
\code{.out} file. In the \code{slidingspans} and
\code{revisions} function, \code{...} are used to add
additional options to the \code{slidingspans} and
\code{history} spec.
}
\examples{
\dontrun{
m <- seas(AirPassengers)

# exit from the viewer with [q]
out(m)
out(m, search = "regARIMA model residuals")

slidingspans(m)
revisions(m)

# plot method for slingspans and revisions (see ?plot.seas)
plot(slidingspans(m))
plot(revisions(m))

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

\code{\link{plot.seas}}, for plot methods for
\code{slidingspans} and \code{revisions}.
}

