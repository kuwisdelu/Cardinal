\name{processingData-methods}
\docType{methods}

\alias{processingData}
\alias{processingData<-}
\alias{processingData-methods}

\title{Retrieve Pre-Processing Information from MSImageSets}
\description{
	This generic function accesses pre-processing information from \code{\linkS4class{MSImageSet}} objects.
}
\usage{
processingData(object)
processingData(object) <- value
}
\arguments{
    \item{object}{A \code{MSImageSet} object.}
    \item{value}{Value to be assigned to the corresponding object.}
}
\value{
    \code{processingData} returns pre-processing information.
}
\author{
	Kyle D. Bemis
}
\seealso{
	\code{\linkS4class{MSImageProcess}},
    \code{\linkS4class{MSImageSet}}
}
\keyword{manip}
