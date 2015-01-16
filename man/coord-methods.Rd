\name{coord-methods}
\docType{methods}

\alias{coord}
\alias{coord<-}
\alias{coord-methods}
\alias{coordinates}
\alias{coordinates<-}
\alias{coordinates-methods}
\alias{coordLabels}
\alias{coordLabels<-}
\alias{coordLabels-methods}

\title{Retrieve Pixel Coordinates from iSets}
\description{
	These generic functions accesses pixel coordinates stored in an object derived from \code{\linkS4class{iSet}}. The \code{coordinates} method is an \emph{alias} for \code{coord}.
}
\usage{
coord(object)
coord(object) <- value

coordinates(object)
coordinates(object) <- value

coordLabels(object)
coordLabels(object) <- value
}
\arguments{
    \item{object}{An object, possible derived from \code{\linkS4class{iSet}}.}
    \item{value}{Value to be assigned to the corresponding object.}
}
\value{
    \code{coord} returns a \code{data.frame} with each row containing coordinates for an individual pixel. \code{coordLabels} retrieves the coordinate labels.
}
\author{
	Kyle D. Bemis
}
\seealso{
    \code{\linkS4class{iSet}},
    \code{\linkS4class{SImageSet}},
    \code{\linkS4class{MSImageSet}}
}
\keyword{manip}
