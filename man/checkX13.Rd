\name{checkX13}
\alias{checkX13}
\title{Check the installation of X-13ARIMA-SEATS}
\usage{
  checkX13(fail = FALSE, confirmation = TRUE)
}
\arguments{
  \item{fail}{logical, wether an error should interrupt the
  process. If \code{FALSE}, only a message will be
  returned.}

  \item{confirmation}{logical, a message is returned if
  everything works.}
}
\description{
  The function checks the installation of the binary
  executables of X-13ARIMA-SEATS. For installation details,
  consider Section 2 of the package vignette:
  \code{vignette("seas")}
}
\examples{
\dontrun{
old.path <- Sys.getenv("X13_PATH")
Sys.setenv(X13_PATH = "")  # its broken now
checkX13()

Sys.setenv(X13_PATH = old.path)  # fix it (provided it worked in the first place)
checkX13()
}
}

