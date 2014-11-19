\name{coregister-methods}
\docType{methods}

\alias{coregister}
\alias{coregister-methods}
\alias{coregister,SpatialKMeans,missing-method}
\alias{coregister,SpatialShrunkenCentroids,missing-method}

\title{Coregister Images}
\description{
	Coregister images of an imaging dataset. Currently this is only used to coregister the class assignments for clustering methods, but additional functionality may be added in the future for 3D experiments and registration of optical images.
}
\usage{
\S4method{coregister}{SpatialShrunkenCentroids,missing}(object, ref, \dots)

\S4method{coregister}{SpatialKMeans,missing}(object, ref, \dots)
}
\arguments{
    \item{object}{An imaging dataset.}
    \item{ref}{A reference for the coregistration.}
    \item{\dots}{Ignored.}
}
\value{
    A new imaging dataset of the same class with coregistered images.
}
\author{
	Kyle D. Bemis
}
\seealso{
    \code{\link[Cardinal]{spatialShrunkenCentroids}}
}
\keyword{methods}
