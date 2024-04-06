\name{spatialFastmap-methods}
\docType{methods}
\alias{spatialFastmap}
\alias{class:spatialFastmap}
\alias{spatialFastmap-class}
\alias{spatialFastmap-methods}

\alias{spatialFastmap,SparseImagingExperiment-method}
\alias{summary,SpatialFastmap2-method}

\title{Spatially-aware FastMap projection}
\description{
	Performs spatially-aware FastMap projection.
}
\usage{
\S4method{spatialFastmap}{SparseImagingExperiment}(x, r = 1, ncomp = 3,
    method = c("gaussian", "adaptive"),
    metric = c("average", "correlation", "neighborhood"),
    dist = "chebyshev", tol.dist = 1e-9,
    iter.max = 1, BPPARAM = getCardinalBPPARAM(), \dots)

\S4method{summary}{SpatialFastmap2}(object, \dots)
}
\arguments{
    \item{x}{The imaging dataset for which to calculate the FastMap components.}
    \item{r}{The neighborhood spatial smoothing radius.}
    \item{ncomp}{The number of FastMap components to calculate.}
    \item{method}{The method to use to calculate the spatial smoothing kernels for the embedding. The 'gaussian' method refers to spatially-aware (SA) weights, and 'adaptive' refers to spatially-aware structurally-adaptive (SASA) weights.}
    \item{metric}{The dissimilarity metric to use when comparing spectra, where `average' (the default) means to use the differences of spatially-smoothed spectra, `correlation' means to use the correlations of spatially-smoothed spectra, and `neighborhood' means to use pairwise differences of each spectrum in the neighborhoods. Previous versions used `neighborhood', which is the algorithm of Alexandrov & Kobarg; `average' is the current default, because it handles non-gridded pixels better than `neighborhood'.}
    \item{dist}{The type of distance metric to use when calculating neighboring pixels based on \code{r}. The options are `radial', `manhattan', `minkowski', and `chebyshev' (the default).}
    \item{tol.dist}{The distance tolerance used for matching pixels when calculating pairwise distances between neighborhoods. This parameter should only matter when the data is not gridded. (Only considers `radial' distance.)}
    \item{iter.max}{The number of iterations to perform when choosing the pivot vectors for each dimension.}
    \item{\dots}{Ignored.}
    \item{object}{A fitted model object to summarize.}
    \item{BPPARAM}{An optional instance of \code{BiocParallelParam}. See documentation for \code{\link{bplapply}}.}
}
\value{
    An object of class \code{SpatialFastmap2}, which is a \code{ImagingResult}, or an object of class \code{SpatialFastmap}, which is a \code{ResultSet}. Each element of the \code{resultData} slot contains at least the following components:

    \describe{
        \item{\code{scores}:}{A matrix with the FastMap component scores.}
        \item{\code{correlation}:}{A matrix with the feature correlations with each FastMap component.}
        \item{\code{sdev}:}{The standard deviations of the FastMap scores.}
    }
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
    \code{\link{spatialKMeans}},
    \code{\link{spatialShrunkenCentroids}}
}
\examples{
setCardinalBPPARAM(SerialParam())

set.seed(1)
data <- simulateImage(preset=2, npeaks=20, dim=c(6,6),
    representation="centroid")

# project to FastMap components
fm <- spatialFastmap(data, r=1, ncomp=2, method="adaptive")

# visualize first 2 components
image(fm, superpose=FALSE)
}
\keyword{spatial}

