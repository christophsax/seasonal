\name{slidingspans}
\alias{regressioneffects}
\alias{revisions}
\alias{slidingspans}
\title{Deprecated Functions}
\usage{
slidingspans(x, ...)

regressioneffects(x)

revisions(x, ...)
}
\arguments{
  \item{x}{object of class \code{"seas"}}

  \item{...}{further arguments}
}
\description{
Functions are deprecated and will be defunct soon. Use the
more univeral series function.
}
\examples{
\dontrun{
# history spec (replaces the deprecated 'revisions' function)
series(m, "history.trendestimates")
series(m, "history.sfestimates")
series(m, "history.saestimates")
series(m, c("history.sfestimates", "history.trendestimates"))

# slidingspans spec (replaces the deprecated 'slidingspans' function)
series(m, "slidingspans.sfspans")
series(m, "slidingspans.tdspans")

# regressioneffects
series(m, "estimate.regressioneffects")
}
}
\seealso{
\code{\link{series}}, for universal X-13 output extraction.
}

