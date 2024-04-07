\name{SpatialShrunkenCentroids}

\alias{SpatialShrunkenCentroids}
\alias{class:SpatialShrunkenCentroids}
\alias{SpatialShrunkenCentroids-class}

\alias{spatialShrunkenCentroids}
\alias{spatialShrunkenCentroids,ANY,ANY-method}
\alias{spatialShrunkenCentroids,SpectralImagingExperiment,ANY-method}
\alias{spatialShrunkenCentroids,ANY,missing-method}
\alias{spatialShrunkenCentroids,SpectralImagingExperiment,missing-method}
\alias{fitted,SpatialShrunkenCentroids-method}
\alias{predict,SpatialShrunkenCentroids-method}
\alias{logLik,SpatialShrunkenCentroids-method}
\alias{topFeatures}
\alias{topFeatures,SpatialShrunkenCentroids-method}
\alias{plot,SpatialShrunkenCentroids,missing-method}
\alias{image,SpatialShrunkenCentroids-method}

\title{Spatially-aware shrunken centroid clustering and classification}

\description{
    Perform spatially-aware nearest shrunken centroid clustering or classification. These methods use statistical regularization to shrink the t-statistics of the features toward 0 so that unimportant features are removed from the model. The dissimilarity to class centroids are spatially smoothed.
}

\usage{
\S4method{spatialShrunkenCentroids}{ANY,ANY}(x, y, coord, r = 1, s = 0,
    weights = c("gaussian", "adaptive"),
    neighbors = findNeighbors(coord, r=r),
    priors = table(y), center = NULL, transpose = TRUE,
    nchunks = getCardinalNChunks(),
    verbose = getCardinalVerbose(),
    BPPARAM = getCardinalBPPARAM(), \dots)

\S4method{spatialShrunkenCentroids}{SpectralImagingExperiment,ANY}(x, y, r = 1, s = 0,
    weights = c("gaussian", "adaptive"),
    neighbors = findNeighbors(x, r=r), \dots)

\S4method{spatialShrunkenCentroids}{ANY,missing}(x, coord, r = 1, k = 2, s = 0,
    weights = c("gaussian", "adaptive"),
    neighbors = findNeighbors(coord, r=r),
    init = NULL, threshold = 0.01, niter = 10L,
    center = NULL, transpose = FALSE,
    nchunks = getCardinalNChunks(),
    verbose = getCardinalVerbose(),
    BPPARAM = getCardinalBPPARAM(), \dots)

\S4method{spatialShrunkenCentroids}{SpectralImagingExperiment,missing}(x, r = 1, k = 2, s = 0,
    weights = c("gaussian", "adaptive"),
    neighbors = findNeighbors(x, r=r), \dots)

\S4method{fitted}{SpatialShrunkenCentroids}(object, type = c("response", "class"), \dots)

\S4method{predict}{SpatialShrunkenCentroids}(object, newdata,
        type = c("response", "class"),
        neighbors = findNeighbors(newdata, r=object$r),
        nchunks = getCardinalNChunks(),
        BPPARAM = getCardinalBPPARAM(), \dots)

\S4method{logLik}{SpatialShrunkenCentroids}(object, \dots)

\S4method{topFeatures}{SpatialShrunkenCentroids}(object, n = Inf, sort.by = c("statistic", "centers"), \dots)

\S4method{plot}{SpatialShrunkenCentroids,missing}(x, type = c("statistic", "centers"), \dots, xlab, ylab)

\S4method{image}{SpatialShrunkenCentroids}(x, type = c("probability", "class"), \dots)
}
\arguments{
    \item{x}{A spatial dataset in P x N matrix format.}

    \item{y}{The response variable.}

    \item{coord}{The spatial coordinates of the rows/columns of \code{x}. Ignored if \code{neighbors} is provided.}
    
    \item{r}{The spatial maximum distance for an observation to be considered a neighbor. Ignored if \code{neighbors} is provided.}

    \item{k}{The number of classes for clustering.}

    \item{s}{The sparsity parameter.}

    \item{weights}{The type of spatial weights to use for the smoothing. Gaussian weights are weighted only by distance, while adaptive weights also consider the dissimilarity between neighboring observations.}

    \item{neighbors}{A \code{factor} giving which observations should be treated as spatially-independent. Observations in the same group are assumed to have a spatial relationship.}

    \item{priors}{The (unnormalized) prior probabilities for each class.}

    \item{center}{The global centroid (if known).}

    \item{transpose}{Should \code{x} be considered P x N?}

    \item{init}{A list of initial cluster configurations. (Should resemble the output of \code{kmeans}.)}

    \item{threshold}{Stop iteration when the proportion of cluster assignment updates is less than this threshold.}
    
    \item{niter}{The maximum number of iterations.}

    \item{nchunks}{The number of chunks to use for processing.}

    \item{verbose}{Should progress messages be printed?}
    
    \item{BPPARAM}{An optional instance of \code{BiocParallelParam}. See documentation for \code{\link{bplapply}}.}

    \item{\dots}{Additional arguments passed to the next method.}

    \item{object}{A \code{SpatialPCA} object.}

    \item{newdata}{A new \code{SpectralImagingExperiment} for which to make predictions.}

    \item{type}{The type of fitted values to extract or the type of predictions to make.}

    \item{n, sort.by}{For \code{topFeatures}, the number of top features to return and how to sort them.}

    \item{xlab, ylab}{Plotting labels.}
}

\value{
    An object of class \code{SpatialShrunkenCentroids} derived from \code{SpatialResults}, containing the fitted \code{\link[matter]{nscentroids}} object and the spatial metadata.
}

\author{
    Kylie A. Bemis
}

\references{
    Bemis, K., Harry, A., Eberlin, L. S., Ferreira, C., van de Ven, S. M., Mallick, P., Stolowitz, M., and Vitek, O. (2016.) Probabilistic segmentation of mass spectrometry images helps select important ions and characterize confidence in the resulting segments. Molecular & Cellular Proteomics. doi:10.1074/mcp.O115.053918

    Tibshirani, R., Hastie, T., Narasimhan, B., & Chu, G. (2003). Class Prediction by Nearest Shrunken Centroids, with Applications to DNA Microarrays. Statistical Science, 18, 104-117.

    Alexandrov, T., & Kobarg, J. H. (2011). Efficient spatial segmentation of large imaging mass spectrometry datasets with spatially aware clustering. Bioinformatics, 27(13), i230-i238. doi:10.1093/bioinformatics/btr246
}

\seealso{
    \code{\link{spatialKMeans}}
}

\examples{
set.seed(1)
mse <- simulateImage(preset=3, dim=c(10,10), npeaks=20,
    peakheight=c(3,6,9), representation="centroid")

# fit spatial shrunken centroids
ssc <- spatialShrunkenCentroids(mse, r=1, k=4, s=c(0,3,6,9), weights="adaptive")

# visualize classes
image(ssc, i=1:4)

# visualize t-statistics
plot(ssc, i=1:4)
}

\keyword{spatial}
\keyword{classif}
\keyword{clustering}
