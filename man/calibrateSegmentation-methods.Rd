\name{calibrateSegmentation-methods}
\docType{methods}
\alias{calibrateSegmentation}
\alias{calibrateSegmentation-methods}
\alias{calibrateSegmentation,MSImageSegmentation,missing-method}
\alias{calibrateSegmentation,MSImageSegmentation,MSImageSegmentation-method}
\title{Calibrates the colors between two image segmentations}
\description{
	Attempts to match colors in one image segmentation to the colors in another image segmentation, to more easily allow visual comparison of different segmentations.
}
\usage{
    \S4method{calibrateSegmentation}{MSImageSegmentation,MSImageSegmentation}(object, reference, which.object=NULL, which.reference=NULL,
    correlation=c("spatial", "spectral"), \dots)

    \S4method{calibrateSegmentation}{MSImageSegmentation,missing}(object,
    correlation=c("spatial", "spectral"), \dots)
}
\arguments{
    \item{object}{the segmentation object to calibrate.}
    \item{reference}{the segmentation to use as a reference.}
    \item{which.object}{which segmentation in the first object to calibrate.}
    \item{which.reference}{which segmentation in the reference object to use for the calibratation.}
    \item{correlation}{whether spatial correlation of the classes or spectral correlation of the centroids should be used to calibrate the segmentations.}
    \item{\dots}{ignored.}
}
\details{
    This method is automatically called at the end of any call to \code{spatialCluster} or \code{spatialSparseCluster}.

    It is assumed that the \code{object} and \code{reference} segmentations were both generated from the same dataset. If \code{which.object} or \code{which.reference} are \code{NULL}, then the first segmentation map for each unique initialized number of clusters (k) are used. If the \code{object} and \code{reference} are not from the same dataset, \code{correlation} must be set to 'spectral' or the method will fail.
}
\author{
	Kyle D. Bemis
}
\seealso{
    \code{\linkS4class{MSImageSet}},
    \code{\link[Cardinal]{spatialCluster-methods}}
}
\examples{
    ## write examples to put here
}
\keyword{methods}

