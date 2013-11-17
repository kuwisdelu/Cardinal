\name{spatialKMeans-methods}
\docType{methods}
\alias{spatialKMeans}
\alias{spatialKMeans-methods}
\alias{spatialKMeans,MSImageSet-method}
\title{Spatially-aware k-means clustering}
\description{
	Perform spatially-aware and spatially-aware structually-adaptive clustering using Fastmap projection followed by k-means clustering of the component scores.
}
\usage{
    \S4method{spatialKMeans}{MSImageSet}(object, r, k, method=c("gaussian", "adaptive"), ncomp=20, standardize=TRUE, neighbors, w, alpha, beta, autoDimNames=NULL, initialize=NULL, iter.max=10, nstart=1, \dots)
}
\arguments{
    \item{object}{the \linkS4class{MSImageSet} to cluster.}
    \item{r}{the spatial radius in pixels.}
    \item{k}{the number of clusters, which can be a vector.}
    \item{method}{\code{character}, where "gaussian" uses Gaussian weights, and "adaptive" uses structually-adaptive weights as in bilateral filtering.}
    \item{ncomp}{the number of Fastmap components to use.}
    \item{standardize}{indicates whether the variables should be standardized beforehand.}
    \item{neighbors}{a \code{matrix} of pixel neighbors, must be a matrix with number of rows equal to the number of pixels and number of columns equal to the number of neighbors, giving the pixel ids, usually generated automatically.}
    \item{w}{a vector of variable weights for initialization, usually generated automatically.}
    \item{alpha}{a vector of weights for the mass spectra in a pixel's radius, must be a vector of length equal to the number of neighbors, usually generated automatically.}
    \item{beta}{a \code{matrix} of structurally-adaptive weights for the mass spectra in a pixel's radius, with number of rows equal to the number of pixels and number of columns equal to the number of neighbors, usually generated automatically.}
    \item{autoDimNames}{the names of which dimensions to treat autonomously in the clustering; most commonly the dimension corresponding to which pixels belong to different slides.}
    \item{initialize}{an \code{integer} vector giving an initial clustering configuration.}
    \item{iter.max}{the maximum number of iterations.}
    \item{nstart}{the number of restarts in the k-means algorithm.}
    \item{\dots}{ignored.}
}
\value{
    A \code{KMeans} object with componenets:
    \item{clusters}{a \code{list} of vectors with the output clusters.}
    \item{centers}{a \code{list} of \code{matrix} objects with the cluster centers.}
    \item{sizes}{a \code{list} of vectors with the cluster sizes.}
    \item{WCSS}{a \code{list} of vectors with the within-cluster sums-of-squares.}
    \item{BCSS}{a \code{list} of vectors with the between-cluster sums-of-squares.}
    \item{fastmap}{a \code{list} containing components from the Fastmap projection.}
}
\author{
	Kyle D. Bemis
}
\seealso{
    \code{\linkS4class{MSImageSet}},
    \code{\link[Cardinal]{spatialClassify-methods}},
    \code{\link[Cardinal]{spatialCluster-methods}},
    \code{\link[Cardinal]{spatialSparseKMeans-methods}}
}
\examples{
    ## write examples to put here
}
\keyword{methods}

