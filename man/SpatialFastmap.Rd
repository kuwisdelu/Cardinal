\name{SpatialFastmap}

\alias{SpatialFastmap}
\alias{class:SpatialFastmap}
\alias{SpatialFastmap-class}

\alias{spatialFastmap}
\alias{spatialFastmap,ANY-method}
\alias{spatialFastmap,SpectralImagingExperiment-method}
\alias{predict,SpatialFastmap-method}
\alias{plot,SpatialFastmap,missing-method}
\alias{image,SpatialFastmap-method}

\title{Spatially-aware FastMap projection}

\description{
	Compute spatially-aware FastMap projection.
}

\usage{
\S4method{spatialFastmap}{ANY}(x, coord, r = 1, ncomp = 3,
    weights = c("gaussian", "adaptive"),
    neighbors = findNeighbors(coord, r=r),
    transpose = TRUE, niter = 10L,
    verbose = getCardinalVerbose(), chunkopts = list(),
    BPPARAM = getCardinalBPPARAM(), \dots)

\S4method{spatialFastmap}{SpectralImagingExperiment}(x, r = 1, ncomp = 3,
    weights = c("gaussian", "adaptive"),
    neighbors = findNeighbors(x, r=r), \dots)

\S4method{predict}{SpatialFastmap}(object, newdata,
    weights = object$weights,
    neighbors = findNeighbors(newdata, r=object$r),
    BPPARAM = getCardinalBPPARAM(), \dots)

\S4method{plot}{SpatialFastmap,missing}(x, type = c("scree", "x"), \dots, xlab, ylab)

\S4method{image}{SpatialFastmap}(x, type = "x", \dots)
}
\arguments{
    \item{x}{A spatial dataset in P x N matrix format.}

    \item{coord}{The spatial coordinates of the rows/columns of \code{x}. Ignored if \code{neighbors} is provided.}
    
    \item{r}{The spatial maximum distance for an observation to be considered a neighbor. Ignored if \code{neighbors} is provided.}

    \item{ncomp}{The number of FastMap components.}

    \item{weights}{The type of spatial weights to use for the smoothing. Gaussian weights are weighted only by distance, while adaptive weights also consider the dissimilarity between neighboring observations.}

    \item{neighbors}{A \code{factor} giving which observations should be treated as spatially-independent. Observations in the same group are assumed to have a spatial relationship.}

    \item{transpose}{Should \code{x} be considered P x N?}
    
    \item{niter}{The number of iterations used to calculate the pivots for each FastMap component.}

    \item{verbose}{Should progress messages be printed?}

    \item{chunkopts}{Chunk processing options. See \code{\link{chunkApply}} for details.}
    
    \item{BPPARAM}{An optional instance of \code{BiocParallelParam}. See documentation for \code{\link{bplapply}}.}

    \item{\dots}{Additional arguments passed to the next method.}

    \item{object}{A \code{SpatialFastmap} object.}

    \item{newdata}{A new \code{SpectralImagingExperiment} for which to calculate the scores.}

    \item{type}{The type of plot to display.}

    \item{xlab, ylab}{Plotting labels.}
}

\value{
    An object of class \code{SpatialFastmap} derived from \code{SpatialResults}, containing the fitted \code{\link[matter]{fastmap}} object and the spatial metadata.
}

\author{
	Kylie A. Bemis
}

\references{
    Alexandrov, T., & Kobarg, J. H. (2011). Efficient spatial segmentation of large imaging mass spectrometry datasets with spatially aware clustering. Bioinformatics, 27(13), i230-i238. doi:10.1093/bioinformatics/btr246

    Faloutsos, C., & Lin, D. (1995). FastMap: A Fast Algorithm for Indexing, Data-Mining and Visualization of Traditional and Multimedia Datasets. Presented at the Proceedings of the 1995 ACM SIGMOD international conference on Management of data.
}

\seealso{
    \code{\link{PCA}},
    \code{\link{NMF}},
    \code{\link{spatialKMeans}}
}

\examples{
set.seed(1)
mse <- simulateImage(preset=2, npeaks=20, dim=c(10,10),
    representation="centroid")

# project to FastMap components
fm <- spatialFastmap(mse, r=1, ncomp=2, weights="adaptive")

# visualize first 2 components
image(fm)
}

\keyword{multivariate}
\keyword{spatial}

