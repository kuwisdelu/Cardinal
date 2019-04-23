\name{spatialFastmap-methods}
\docType{methods}
\alias{spatialFastmap}
\alias{class:spatialFastmap}
\alias{spatialFastmap-class}
\alias{spatialFastmap-methods}

\alias{spatialFastmap,SparseImagingExperiment-method}
\alias{summary,SpatialFastmap2-method}

\alias{spatialFastmap,SImageSet-method}
\alias{summary,SpatialFastmap-method}
\alias{print.summary.SpatialFastmap}
\alias{plot.summary.SpatialFastmap}

\title{Spatially-aware FastMap projection}
\description{
	Performs spatially-aware FastMap projection.
}
\usage{
\S4method{spatialFastmap}{SparseImagingExperiment}(x, r = 1, ncomp = 3,
    method = c("gaussian", "adaptive"),
    dist = "chebyshev", tol.dist = 1e-9,
    iter.max = 1, BPPARAM = bpparam(), \dots)

\S4method{summary}{SpatialFastmap2}(object, \dots)

\S4method{spatialFastmap}{SImageSet}(x, r = 1, ncomp = 3,
    method = c("gaussian", "adaptive"),
    iter.max = 1, \dots)
}
\arguments{
    \item{x}{The imaging dataset for which to calculate the FastMap components.}
    \item{r}{The neighborhood spatial smoothing radius.}
    \item{ncomp}{The number of FastMap components to calculate.}
    \item{method}{The method to use to calculate the spatial smoothing kernels for the embedding. The 'gaussian' method refers to spatially-aware (SA) weights, and 'adaptive' refers to spatially-aware structurally-adaptive (SASA) weights.}
    \item{dist}{The type of distance metric to use when calculating neighboring pixels based on \code{r}. The options are `radial', `manhattan', `minkowski', and `chebyshev' (the default).}
    \item{tol.dist}{The distance tolerance used for matching pixels when calculating pairwise distances between neighborhoods. This parameter should only matter when the data is not gridded. (Only considers `radial' distance.)}
    \item{iter.max}{The number of iterations to perform when choosing the pivot vectors for each dimension.}
    \item{\dots}{Ignored.}
    \item{object}{A fitted model object to summarize.}
    \item{BPPARAM}{An optional instance of \code{BiocParallelParam}. See documentation for \code{\link{bplapply}}.}
}
\value{
    An object of class \code{SpatialFastmap2}, which is a \code{ResultImagingExperiment}, or an object of class \code{SpatialFastmap}, which is a \code{ResultSet}. Each element of the \code{resultData} slot contains at least the following components:

    \describe{
        \item{\code{scores}:}{A matrix with the FastMap component scores.}
        \item{\code{correlation}:}{A matrix with the feature correlations with each FastMap component.}
        \item{\code{sdev}:}{The standard deviations of the FastMap scores.}
    }
}
\author{
	Kylie A. Bemis
}
\seealso{
    \code{\link{PCA}},
    \code{\link{spatialKMeans}},
    \code{\link{spatialShrunkenCentroids}}
}
\examples{
register(SerialParam())

set.seed(1)
data <- simulateImage(preset=2, npeaks=20, dim=c(6,6),
    representation="centroid")

# project to FastMap components
fm <- spatialFastmap(data, r=1, ncomp=2, method="adaptive")

# visualize first 2 components
image(fm, superpose=FALSE)
}
\keyword{spatial}

