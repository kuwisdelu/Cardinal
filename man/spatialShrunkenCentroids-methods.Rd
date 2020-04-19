\name{spatialShrunkenCentroids-methods}
\docType{methods}
\alias{spatialShrunkenCentroids}
\alias{class:SpatialShrunkenCentroids}
\alias{SpatialShrunkenCentroids-class}
\alias{spatialShrunkenCentroids-methods}

\alias{spatialShrunkenCentroids,SparseImagingExperiment,missing-method}
\alias{spatialShrunkenCentroids,SparseImagingExperiment,ANY-method}
\alias{predict,SpatialShrunkenCentroids2-method}
\alias{fitted,SpatialShrunkenCentroids2-method}
\alias{summary,SpatialShrunkenCentroids2-method}
\alias{logLik,SpatialShrunkenCentroids2-method}

\alias{spatialShrunkenCentroids,SImageSet,missing-method}
\alias{spatialShrunkenCentroids,SImageSet,factor-method}
\alias{spatialShrunkenCentroids,SImageSet,character-method}
\alias{predict,SpatialShrunkenCentroids-method}
\alias{logLik,SpatialShrunkenCentroids-method}
\alias{summary,SpatialShrunkenCentroids-method}
\alias{print.summary.SpatialShrunkenCentroids}
\alias{plot.summary.SpatialShrunkenCentroids}

\title{Spatially-aware shrunken centroid clustering and classification}
\description{
    Performs spatially-aware nearest shrunken centroid clustering or classification on an imaging dataset. These methods use statistical regularization to shrink the t-statistics of the features toward 0 so that unimportant features are removed from the analysis. A Gaussian spatial kernel or an adaptive kernel based on bilateral filtering are used for spatial smoothing.
}
\usage{
\S4method{spatialShrunkenCentroids}{SparseImagingExperiment,missing}(x, r = 1, k = 3, s = 0,
    method = c("gaussian", "adaptive"),
    dist = "chebyshev", init = NULL,
    iter.max = 10, BPPARAM = getCardinalBPPARAM(), \dots)

\S4method{spatialShrunkenCentroids}{SparseImagingExperiment,ANY}(x, y, r = 1, s = 0,
    method = c("gaussian", "adaptive"),
    dist = "chebyshev", priors = table(y),
    BPPARAM = getCardinalBPPARAM(), \dots)

\S4method{predict}{SpatialShrunkenCentroids2}(object, newx, newy, BPPARAM = getCardinalBPPARAM(), \dots)

\S4method{fitted}{SpatialShrunkenCentroids2}(object, \dots)

\S4method{summary}{SpatialShrunkenCentroids2}(object, \dots)

\S4method{spatialShrunkenCentroids}{SImageSet,missing}(x, r = 1, k = 3, s = 0,
    method = c("gaussian", "adaptive"),
    iter.max=10, \dots)

\S4method{spatialShrunkenCentroids}{SImageSet,factor}(x, y, r = 1, s = 0,
    method = c("gaussian", "adaptive"),
    priors = table(y), \dots)

\S4method{spatialShrunkenCentroids}{SImageSet,character}(x, y, \dots)

\S4method{predict}{SpatialShrunkenCentroids}(object, newx, newy, \dots)
}
\arguments{
    \item{x}{The imaging dataset to segment or classify.}
    \item{y}{A \code{factor} or \code{character} response.}
    \item{r}{The spatial neighborhood radius of nearby pixels to consider. This can be a vector of multiple radii values.}
    \item{k}{The maximum number of segments (clusters). This can be a vector to try initializing the clustering with different numbers of maximum segments. The final number of segments may differ.}
    \item{s}{The sparsity thresholding parameter by which to shrink the t-statistics.}
    \item{method}{The method to use to calculate the spatial smoothing weights. The 'gaussian' method refers to spatially-aware (SA) weights, and 'adaptive' refers to spatially-aware structurally-adaptive (SASA) weights.}
    \item{dist}{The type of distance metric to use when calculating neighboring pixels based on \code{r}. The options are `radial', `manhattan', `minkowski', and `chebyshev' (the default).}
    \item{init}{Initial cluster configuration. This may either be the result of a call to \code{spatialKMeans}, or a list of factors giving the initial cluster configurations.}
    \item{iter.max}{The maximum number of clustering iterations.}
    \item{priors}{Prior probabilities on the classes for classification. Improper priors will be normalized automatically.}
    \item{\dots}{Passed to internal methods.}
    \item{BPPARAM}{An optional instance of \code{BiocParallelParam}. See documentation for \code{\link{bplapply}}.}
    \item{object}{The result of a previous call to \code{\link{spatialShrunkenCentroids}}.}
    \item{newx}{An imaging dataset for which to calculate the predicted response from shrunken centroids.}
    \item{newy}{Optionally, a new response from which residuals should be calculated.}
}
\value{
    An object of class \code{SpatialShrunkenCentroids2}, which is a \code{ImagingResult}, or an object of class \code{SpatialShrunkenCentroids}, which is a \code{ResultSet}. Each element of the \code{resultData} slot contains at least the following components:

    \describe{
        \item{\code{class, classes}:}{A factor indicating the predicted class for each pixel in the dataset.}
        \item{\code{probability, probabilities}:}{A matrix of class probabilities.}
        \item{\code{centers}:}{A matrix of shrunken class centers.}
        \item{\code{statistic, tstatistics}:}{A matrix of shrunken t-statistics of the features.}
        \item{\code{scores}:}{A matrix of discriminant scores.}
        \item{\code{sd}:}{The pooled within-class standard deviations for each feature.}
    }
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
register(SerialParam())

set.seed(1)
x <- simulateImage(preset=2, dim=c(10,10), npeaks=10,
    peakheight=c(4,6,8), representation="centroid")

res <- spatialShrunkenCentroids(x, r=1, k=5, s=c(0,3,6), method="adaptive")

summary(res)

image(res, model=list(s=6))
}

\keyword{spatial}
\keyword{classif}
\keyword{clustering}
