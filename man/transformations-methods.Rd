\name{transformations-methods}
\docType{methods}
\alias{flipHorizontal}
\alias{flipHorizontal-methods}
\alias{flipHorizontal,MSImageSet-method}
\alias{flipVertical}
\alias{flipVertical-methods}
\alias{flipVertical,MSImageSet-method}
\alias{rotateLeft}
\alias{rotateLeft-methods}
\alias{rotateLeft,MSImageSet-method}
\alias{rotateRight}
\alias{rotateRight-methods}
\alias{rotateRight,MSImageSet-method}
\title{Flip or rotate a mass spectrometry imaging dataset}
\description{
	Flips or rotates the coordinates of an \code{\linkS4class{MSImageSet}} dataset along a given set of sliced dimensions.
}
\usage{
    \S4method{flipHorizontal}{MSImageSet}(object, sliceDimNames=metaData(object)[["coordDimNames"]][c(1,2)], \dots)

    \S4method{flipVertical}{MSImageSet}(object, sliceDimNames=metaData(object)[["coordDimNames"]][c(1,2)], \dots)

    \S4method{rotateLeft}{MSImageSet}(object, sliceDimNames=metaData(object)[["coordDimNames"]][c(1,2)], \dots)

    \S4method{rotateRight}{MSImageSet}(object, sliceDimNames=metaData(object)[["coordDimNames"]][c(1,2)], \dots)
}
\arguments{
    \item{object}{\code{\linkS4class{MSImageSet}} data.}
    \item{sliceDimNames}{\code{character}, giving the name of the sliced dimensions.}
    \item{\dots}{ignored.}
}
\value{
    An \code{\linkS4class{MSImageSet}} with flipped or rotated coordinates along the given dimensions.
}
\author{
	Kyle D. Bemis
}
\seealso{
    \code{\linkS4class{MSImageSet}}
}
\keyword{methods}

