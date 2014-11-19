\name{pixelNames-methods}
\docType{methods}

\alias{pixelNames}
\alias{pixelNames<-}
\alias{pixelNames-methods}

\title{Retrieve Pixel Names from iSets}
\description{
	This generic function accesses pixel names (typically image coordinates) stored in an object derived from \code{\linkS4class{iSet}}.
}
\usage{
pixelNames(object)
pixelNames(object) <- value
}
\arguments{
    \item{object}{An object, possible derived from \code{\linkS4class{iSet}}.}
    \item{value}{Value to be assigned to the corresponding object.}
}
\value{
    \code{pixelNames} returns an object containing information on pixel names.
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
