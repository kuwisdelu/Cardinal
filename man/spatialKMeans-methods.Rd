\name{spatialKMeans-methods}
\docType{methods}
\alias{spatialKMeans}
\alias{class:SpatialKMeans}
\alias{SpatialKMeans-class}
\alias{spatialKMeans-methods}
\alias{spatialKMeans,SImageSet-method}
\alias{summary,SpatialKMeans-method}
\alias{print.summary.SpatialKMeans}
\alias{plot.summary.SpatialKMeans}

\title{Spatially-Aware K-Means Clustering}
\description{
    Performs spatially-aware (SA) or spatially-aware structurally-adaptive (SASA) clustering of imaging data. The data are first projected into an embedded feature space where spatial structure is maintained using the Fastmap algorithm, and then ordinary k-means clustering is performed on the projected dataset.
}
\usage{
\S4method{spatialKMeans}{SImageSet}(x, r = 1, k = 2,
    method = c("gaussian", "adaptive"),
    weights = 1, iter.max = 100, nstart = 100,
    algorithm = c("Hartigan-Wong", "Lloyd", "Forgy",
        "MacQueen"),
    ncomp = 20, \dots)
}
\arguments{
    \item{x}{The imaging dataset to cluster.}
    \item{r}{The spatial neighborhood radius of nearby pixels to consider. This can be a vector of multiple radii values.}
    \item{k}{The number of clusters. This can be a vector to try different numbers of clusters.}
    \item{method}{The method to use to calculate the spatial smoothing kernels for the embedding. The 'gaussian' method refers to spatially-aware (SA) clustering, and 'adaptive' refers to spatially-aware structurally-adaptive (SASA) clustering.}
    \item{weights}{An optional vector of feature weights to be applied to the features during the clustering.}
    \item{iter.max}{The maximum number of k-means iterations.}
    \item{nstart}{The number of restarts for the k-means algorithm.}
    \item{algorithm}{The k-means algorithm to use. See \code{\link[stats]{kmeans}} for details.}
    \item{ncomp}{The number of fastmap components to calculate.}
    \item{\dots}{Ignored.}
}
\value{
    An object of class \code{SpatialKMeans}, which is a \code{ResultSet}, where each component of the \code{resultData} slot contains at least the following components:

    \describe{
        \item{\code{cluster}:}{A vector of integers indicating the cluster for each pixel in the dataset.}
        \item{\code{centers}:}{A matrix of cluster centers.}
        \item{\code{time}:}{The amount of time the algorithm took to run.}
        \item{\code{r}:}{The neighborhood spatial smoothing radius.}
        \item{\code{k}:}{The number of clusters.}
        \item{\code{method}:}{The method for calculating spatial distances.}
        \item{\code{weights}:}{The feature weights (defaults to 1s).}
        \item{\code{fastmap}:}{A list with components giving details of the Fastmap projection.}
    }
}
\author{
    Kyle D. Bemis
}
\references{
    Alexandrov, T., & Kobarg, J. H. (2011). Efficient spatial segmentation of large imaging mass spectrometry datasets with spatially aware clustering. Bioinformatics, 27(13), i230-i238. doi:10.1093/bioinformatics/btr246

    Faloutsos, C., & Lin, D. (1995). FastMap: A Fast Algorithm for Indexing, Data-Mining and Visualization of Traditional and Multimedia Datasets. Presented at the Proceedings of the 1995 ACM SIGMOD international conference on Management of data.
}
\seealso{
    \code{\link[Cardinal]{spatialShrunkenCentroids}}
}
\examples{
set.seed(1)
data <- matrix(c(NA, NA, 1, 1, NA, NA, NA, NA, NA, NA, 1, 1, NA, NA, 
    NA, NA, NA, NA, NA, 0, 1, 1, NA, NA, NA, NA, NA, 1, 0, 0, 1, 
    1, NA, NA, NA, NA, NA, 0, 1, 1, 1, 1, NA, NA, NA, NA, 0, 1, 1, 
    1, 1, 1, NA, NA, NA, NA, 1, 1, 1, 1, 1, 1, 1, NA, NA, NA, 1, 
    1, NA, NA, NA, NA, NA, NA, 1, 1, NA, NA, NA, NA, NA), nrow=9, ncol=9)

sset <- generateImage(data, range=c(200, 300), step=1)

clust1 <- spatialKMeans(sset, r=c(1,2), k=c(2,3), method="gaussian")

clust2 <- spatialKMeans(sset, r=c(1,2), k=c(2,3), method="adaptive")
}

\keyword{spatial}
\keyword{clustering}
