\name{spatialShrunkenCentroids-methods}
\docType{methods}
\alias{spatialShrunkenCentroids}
\alias{class:SpatialShrunkenCentroids}
\alias{SpatialShrunkenCentroids-class}
\alias{spatialShrunkenCentroids-methods}
\alias{spatialShrunkenCentroids,SImageSet,missing-method}
\alias{spatialShrunkenCentroids,SImageSet,factor-method}
\alias{spatialShrunkenCentroids,SImageSet,character-method}
\alias{predict,SpatialShrunkenCentroids-method}
\alias{logLik,SpatialShrunkenCentroids-method}
\alias{summary,SpatialShrunkenCentroids-method}
\alias{print.summary.SpatialShrunkenCentroids}
\alias{plot.summary.SpatialShrunkenCentroids}

\title{Spatially-Aware Shrunken Centroid Clustering and Classification}
\description{
    Performs spatially-aware nearest shrunken centroid clustering or classification on an imaging dataset. These methods use statistical regularization to shrink the t-statistics of the features toward 0 so that unimportant features are removed from the analysis. A Gaussian spatial kernel or an adaptive kernel based on bilateral filtering are used for spatial smoothing.
}
\usage{
\S4method{spatialShrunkenCentroids}{SImageSet,missing}(x, y, r = 1, k = 2, s = 0,
    method = c("gaussian", "adaptive"),
    iter.max=10, \dots)

\S4method{spatialShrunkenCentroids}{SImageSet,factor}(x, y, r = 1, s = 0,
    method = c("gaussian", "adaptive"),
    priors = table(y), ...)

\S4method{spatialShrunkenCentroids}{SImageSet,character}(x, y, \dots)

\S4method{predict}{SpatialShrunkenCentroids}(object, newx, newy, \dots)
}
\arguments{
    \item{x}{The imaging dataset to cluster.}
    \item{y}{A \code{factor} or \code{character} response.}
    \item{r}{The spatial neighborhood radius of nearby pixels to consider. This can be a vector of multiple radii values.}
    \item{k}{The number of clusters. This can be a vector to try different numbers of clusters.}
    \item{s}{The sparsity thresholding parameter by which to shrink the t-statistics.}
    \item{method}{The method to use to calculate the spatial smoothing kernels for the embedding. The 'gaussian' method refers to spatially-aware (SA) weights, and 'adaptive' refers to spatially-aware structurally-adaptive (SASA) weights.}
    \item{iter.max}{The maximum number of clustering iterations.}
    \item{priors}{Prior probabilities on the classes for classification. Improper priors will be normalized automatically.}
    \item{\dots}{Ignored.}
    \item{object}{The result of a previous call to \code{\link[Cardinal]{spatialShrunkenCentroids}}.}
    \item{newx}{An imaging dataset for which to calculate the predicted response from shrunken centroids.}
    \item{newy}{Optionally, a new response from which residuals should be calculated.}
}
\value{
    An object of class \code{SpatialShrunkenCentroids}, which is a \code{ResultSet}, where each component of the \code{resultData} slot contains at least the following components:

    \describe{
        \item{\code{classes}:}{A factor indicating the predicted class for each pixel in the dataset.}
        \item{\code{centers}:}{A matrix of shrunken class centers.}
        \item{\code{time}:}{The amount of time the algorithm took to run.}
        \item{\code{r}:}{The neighborhood spatial smoothing radius.}
        \item{\code{k}:}{The number of clusters.}
        \item{\code{s}:}{The sparsity parameter.}
        \item{\code{method}:}{The type of spatial kernel used.}
        \item{\code{scores}:}{A matrix of discriminant scores.}
        \item{\code{probabilities}:}{A matrix of class probabilities.}
        \item{\code{tstatistics}:}{A matrix of shrunken t-statistics of the features.}
        \item{\code{sd}:}{The pooled within-class standard deviations for each feature.}
        \item{\code{iter}:}{The number of iterations performed.}
    }
}
\author{
    Kyle D. Bemis
}
\references{
    Tibshirani, R., Hastie, T., Narasimhan, B., & Chu, G. (2003). Class Prediction by Nearest Shrunken Centroids, with Applications to DNA Microarrays. Statistical Science, 18, 104-117.

    Alexandrov, T., & Kobarg, J. H. (2011). Efficient spatial segmentation of large imaging mass spectrometry datasets with spatially aware clustering. Bioinformatics, 27(13), i230-i238. doi:10.1093/bioinformatics/btr246
}
\seealso{
    \code{\link[Cardinal]{spatialKMeans}}
}
\examples{
set.seed(1)
data <- matrix(c(NA, NA, 1, 1, NA, NA, NA, NA, NA, NA, 1, 1, NA, NA, 
    NA, NA, NA, NA, NA, 0, 1, 1, NA, NA, NA, NA, NA, 1, 0, 0, 1, 
    1, NA, NA, NA, NA, NA, 0, 1, 1, 1, 1, NA, NA, NA, NA, 0, 1, 1, 
    1, 1, 1, NA, NA, NA, NA, 1, 1, 1, 1, 1, 1, 1, NA, NA, NA, 1, 
    1, NA, NA, NA, NA, NA, NA, 1, 1, NA, NA, NA, NA, NA), nrow=9, ncol=9)

sset <- generateImage(data, range=c(200, 300), step=1)

clust1 <- spatialShrunkenCentroids(sset, r=c(1,2), k=c(2,3), s=c(0,1), method="gaussian")

clust2 <- spatialShrunkenCentroids(sset, r=c(1,2), k=c(2,3), s=c(0,1), method="adaptive")

y <- factor(data[!is.na(data)], labels=c("black", "red"))

class1 <- spatialShrunkenCentroids(sset, y, r=c(1,2), s=c(0,1), method="gaussian")

class1 <- spatialShrunkenCentroids(sset, y, r=c(1,2), s=c(0,1), method="adaptive")
}

\keyword{spatial}
\keyword{classif}
\keyword{clustering}
