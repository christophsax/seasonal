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
an object of class \code{"out"}, \code{"revisions"} or
\code{"slidingspans"}, which is basically a \code{"seas"}
object that contatins the \code{out} element from the
X-13ARIMA-SEATS \code{.out} file.  \code{"revisions"} and
\code{"slidingspans"} also contain some additional time
series.

If printed, the objects show the content of the \code{.out}
file.

There are \code{plot} methods for \code{"revisions"} and
\code{"slidingspans"} that visualize the spec.
}
\description{
For diagnostical purposes, these functions re-evaluate an
object of class \code{"seas"} and capture the full content
or parts of the .out file from X-13ARIMA-SEATS.
}
\details{
The \code{out} function shows the full content of the
\code{.out} file form X-13ARIMA-SEATS. The
\code{slidingspans} and \code{revisions} function call the
'slidingspans' and 'history' spec of X-13ARIMA-SEATS and
show the respective parts of the \code{.out} file. Note
that against the convention, the 'history' spec is called
by the function \code{revision}, in order to avoid a naming
collision with the function from the preloaded \code{utils}
package. For a description of the 'slidingsspans' and
'history' spec, consider the X-13ARIMA-SEATS manual.

For how to enter spec-arguments options, see the details in
\code{"\link{seas}"}. In the \code{out} function,
\code{...} are useful to add additional outputs to the
\code{.out} file. In the \code{slidingspans} and
\code{revisions} function, \code{...} are used to add
additional options to the 'slidingspans' and 'history'
spec.
}
\examples{
\dontrun{
m <- seas(AirPassengers)

# exit from the viewer with [q]
out(m)
out(m, search = "regARIMA model residuals")

slidingspans(m)
revisions(m)

# passing options
revisions(m, history.start = "1959.Aug")
slidingspans(m, slidingspans.fixreg = "td")

# plot method for slingspans and revisions (see ?plot.seas)
plot(slidingspans(m))
rev <- revisions(m)
plot(rev)
plot(rev, series = "trendestimates")

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

