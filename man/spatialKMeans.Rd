\name{spatialKMeans}

\alias{SpatialKMeans}
\alias{class:SpatialKMeans}
\alias{SpatialKMeans-class}

\alias{spatialKMeans,ANY-method}
\alias{spatialKMeans,SpectralImagingExperiment-method}
\alias{predict,SpatialKMeans-method}
\alias{plot,SpatialKMeans,missing-method}
\alias{plot,image-method}

\title{Spatially-aware K-means clustering}

\description{
    Perform spatially-aware k-means clustering. First the data is projected to a reduced dimension space using \code{\link{spatialFastmap}}. Then ordinary k-means clustering is applied to the projected data.
}

\usage{
\S4method{spatialKMeans}{ANY}(x, coord, r = 1, k = 2, ncomp = max(k),
        weights = c("gaussian", "adaptive"),
        neighbors = findNeighbors(coord, r=r),
        transpose = TRUE, niter = 2L,
        centers = TRUE, correlation = TRUE,
        nchunks = getCardinalNChunks(),
        verbose = getCardinalVerbose(),
        BPPARAM = getCardinalBPPARAM(), \dots)

\S4method{SpectralImagingExperiment}{ANY}(x, r = 1, k = 2, ncomp = max(k),
        weights = c("gaussian", "adaptive"),
        neighbors = findNeighbors(x, r=r), \dots)

\S4method{topFeatures}{SpatialKMeans}(object, n = Inf, sort.by = "correlation", \dots)

\S4method{plot}{SpatialKMeans,missing}(x, type = c("correlation", "centers"), \dots, xlab, ylab)

\S4method{image}{SpatialKMeans}(x, type = "cluster", \dots)
}
\arguments{
    \item{x}{A spatial dataset in P x N matrix format.}

    \item{coord}{The spatial coordinates of the rows/columns of \code{x}. Ignored if \code{neighbors} is provided.}
    
    \item{r}{The spatial maximum distance for an observation to be considered a neighbor. Ignored if \code{neighbors} is provided.}

    \item{k}{The number of clusters.}

    \item{ncomp}{The number of FastMap components.}

    \item{weights}{The type of spatial weights to use for the smoothing. Gaussian weights are weighted only by distance, while adaptive weights also consider the dissimilarity between neighboring observations.}

    \item{neighbors}{A \code{factor} giving which observations should be treated as spatially-independent. Observations in the same group are assumed to have a spatial relationship.}

    \item{transpose}{Should \code{x} be considered P x N?}
    
    \item{niter}{The number of iterations used to calculate the pivots for each FastMap component.}

    \item{centers}{Should the cluster centers be re-calculated on the original data?}

    \item{correlation}{Should the correlations between features and the clusters be calculated?}

    \item{nchunks}{The number of chunks to use for processing.}

    \item{verbose}{Should progress messages be printed?}
    
    \item{BPPARAM}{An optional instance of \code{BiocParallelParam}. See documentation for \code{\link{bplapply}}.}

    \item{\dots}{Additional arguments passed to the next method.}

    \item{n, sort.by}{For \code{topFeatures}, the number of top features to return and how to sort them.}

    \item{type}{The type of plot to display.}

    \item{xlab, ylab}{Plotting labels.}
}

\value{
    An object of class \code{SpatialKMeans} derived from \code{SpatialResults}, containing the fitted \code{\link[stats]{kmeans}} object and the spatial metadata.
}

\author{
    Kylie A. Bemis
}

\references{
    Alexandrov, T., & Kobarg, J. H. (2011). Efficient spatial segmentation of large imaging mass spectrometry datasets with spatially aware clustering. Bioinformatics, 27(13), i230-i238. doi:10.1093/bioinformatics/btr246

    Faloutsos, C., & Lin, D. (1995). FastMap: A Fast Algorithm for Indexing, Data-Mining and Visualization of Traditional and Multimedia Datasets. Presented at the Proceedings of the 1995 ACM SIGMOD international conference on Management of data.
}

\seealso{
    \code{\link{spatialKMeans}}
    \code{\link{spatialShrunkenCentroids}}
}

\examples{
set.seed(1)
mse <- simulateImage(preset=3, dim=c(10,10), npeaks=20,
    peakheight=c(3,6,9), representation="centroid")

# fit spatial k-means
skm <- spatialKMeans(mse, r=1, k=4, weights="adaptive")

# visualize clusters
image(skm)
}

\keyword{spatial}
\keyword{clustering}
