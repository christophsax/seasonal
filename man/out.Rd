\name{out}
\alias{out}
\title{Diagnostical Re-Evaluation}
\usage{
out(x, viewer = TRUE, line = 1, n = 25, search = NULL, ...)
}
\arguments{
  \item{x}{an object of class "seas" to re-evaluate}

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
the function from the \code{utils} pacakge.

For how to enter spec-arguments options, see the details in
\code{"\link{seas}"}. In the \code{out} function,
\code{...} are useful to add adtional outputs to the
\code{.out} file. In the \code{slidingspans} and
\code{revisions} function, \code{...} are used to add
aditional options to the \code{slidingspans} and
\code{history} spec.
}

