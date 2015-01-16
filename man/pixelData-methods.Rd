\name{pixelData-methods}
\docType{methods}

\alias{pixelData}
\alias{pixelData<-}
\alias{pixelData-methods}
\alias{pData}
\alias{pData<-}
\alias{pData-methods}

\title{Retrieve Information on Pixels in iSet-derived Classes}
\description{
	This generic function accesses pixel data (experiment specific information about pixels) and pixel metadata (e.g., coordinates or experimental conditions).
}
\usage{
pixelData(object)
pixelData(object) <- value
pData(object)
pData(object) <- value
}
\arguments{
    \item{object}{An object, possible derived from \code{\linkS4class{iSet}}.}
    \item{value}{Value to be assigned to the corresponding object.}
}
\value{
    \code{pixelData} returns an object containing information on pixel variables and pixel metadata. \code{pixelData} returns an object containing information on pixel variables and pixel metadata. \code{pData} returns a \code{data.frame} with pixels as rows and variables as columns.
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
