\name{sliceBind-methods}
\docType{methods}
\alias{sliceBind}
\alias{sliceBind-methods}
\alias{sliceBind,MSImageSet-method}
\title{Combine mass spectrometry imaging datasets by slice}
\description{
	Combines multiple \code{\linkS4class{MSImageSet}} data objects into a single object.
}
\usage{
    \S4method{sliceBind}{MSImageSet}(object, \dots, newDim=TRUE, newDimName)
}
\arguments{
    \item{object}{\code{\linkS4class{MSImageSet}} data.}
    \item{\dots}{additional \code{\linkS4class{MSImageSet}} objects.}
    \item{newDim}{\code{logical}, whether the objects should be combined along a new dimension.}
    \item{newDimName}{\code{character}, giving the name of the new dimension.}
}
\value{
    An \code{\linkS4class{MSImageSet}} object combining all the given objects.
}
\author{
	Kyle D. Bemis
}
\seealso{
    \code{\linkS4class{MSImageSet}},
}
\examples{
    ## write examples to put here
}
\keyword{methods}

