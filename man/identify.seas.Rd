\name{identify.seas}
\alias{identify.seas}
\title{Identify Outliers (Experimental)}
\usage{
\method{identify}{seas}(x, type = c("ao", "tc", "ls"))
}
\arguments{
  \item{x}{an object of class \code{"seas"}.}
}
\value{
an object of class \code{"seas"}, containing the static
call of the selected model.
}
\description{
Select or unselect outliers by clicking. To quit and return
the call, press ESC. Clicking several times loops throug
the list of outlier types. The default list can be modified
by the user.
}
\examples{
\dontrun{
 m <- seas(AirPassengers)
 identify(m)
}
}

