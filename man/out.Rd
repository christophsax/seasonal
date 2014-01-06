\name{out}
\alias{out}
\alias{revisions}
\alias{slidingspans}
\title{Diagnostical Re-Evaluation}
\usage{
out(x, view = TRUE, line = 1, n = 500, search = NULL, ...)

revisions(x, view = TRUE, ...)

slidingspans(x, view = TRUE, ...)
}
\arguments{
  \item{x}{an object of class "seas" to re-evaluate.}

  \item{view}{logical, should the content be shown in the
  console.}

  \item{line}{if \code{view = TRUE}, starting line of the
  content.}

  \item{n}{if \code{view = TRUE}, number of lines to show
  at once.}

  \item{search}{chracter string. If \code{view = TRUE}, the
  \code{.out} content is searched for the first occurence
  of the string (see examples).}

  \item{...}{aditional spec-arguments options (see
  details).}
}
\value{
invisible, the full content or parts of the .out file as a
(large) character vector. If \code{viewer = TRUE}, the
content of the character vector is shown as a side effect.
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
\code{...} are useful to add adtional outputs to the
\code{.out} file. In the \code{slidingspans} and
\code{revisions} function, \code{...} are used to add
aditional options to the \code{slidingspans} and
\code{history} spec.
}
\examples{
\dontrun{
x <- seas(AirPassengers)

# exit from the viewer with [q]
out(x)
out(x, search = "regARIMA model residuals")

slidingspans(x)
revisions(x)

}
}
\references{
Vignette with a more detailed description:
\url{http://cran.r-project.org/web/packages/seasonal/vignettes/seas.pdf}

Wiki page with R examples from the X-13ARIMA-SEATS:
\url{https://github.com/christophsax/seasonal/wiki/Examples-of-X-13ARIMA-SEATS-in-R}

X-13ARIMA-SEATS manual:
\url{http://www.census.gov/ts/x13as/docX13AS.pdf}
}

