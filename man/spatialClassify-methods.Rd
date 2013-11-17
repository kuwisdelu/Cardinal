\name{spatialClassify-methods}
\docType{methods}
\alias{spatialClassify}
\alias{spatialClassify-methods}
\alias{spatialClassify,MSImageSet-method}
\alias{spatialSparseClassify}
\alias{spatialSparseClassify-methods}
\alias{spatialSparseClassify,MSImageSet-method}
\title{Spatially-aware feature-sparse clustering}
\description{
	Perform spatially-aware and spatially-aware structually-adaptive classification using nearest shrunken centroids.
}
\usage{
    \S4method{spatialClassify}{MSImageSet}(object, labels, r, method=c("gaussian", "adaptive"), autoDimNames=NULL, \dots)
    
    \S4method{spatialSparseClassify}{MSImageSet}(object, labels, r, s, method=c("gaussian", "adaptive"), autoDimNames=NULL, \dots)
}
\arguments{
    \item{object}{the \linkS4class{MSImageSet} to cluster.}
    \item{labels}{the class labels, given as an \code{integer} or a \code{factor.}}
    \item{r}{the spatial radius in pixels.}
    \item{s}{the sparsity parameter, which can be a vector.}
    \item{method}{\code{character}, where "gaussian" uses Gaussian weights, and "adaptive" uses structually-adaptive weights as in bilateral filtering.}
    \item{autoDimNames}{the names of which dimensions to treat autonomously in the clustering; most commonly the dimension corresponding to which pixels belong to different slides.}
    \item{\dots}{ignored.}
}
\details{
    The method \code{\link[Cardinal]{spatialClassify}} is just a wrapper for \code{\link[Cardinal]{spatialSparseClassify}} with 's' = 0. Running either method with 'r' = 0 reduces to ordinary nearest shrunken centroid classification.
}
\value{
    An object of class \code{MSImageSegmentation} with componenets:
    \item{classes}{a \code{list} of vectors with the predicted classes.}
    \item{probabilities}{a \code{list} of \code{matrix} objects with the class probabilities for each pixel.}
    \item{scores}{a \code{list} of \code{matrix} objects with the discriminant scores for each class for each pixel}
    \item{centroids}{a \code{list} of \code{matrix} objects with the class shrunken centroids.}
    \item{tstatistics}{a \code{list} of \code{matrix} objects with the variable t-statistics for each of the classes.}
    \item{sd}{a \code{list} of \code{matrix} objects with the within-class pooled standard deviations for each variable.}
    \item{dof}{an upper limit on the approximate degrees of freedom of each model (calculated as the number of features with non-zero t-statistics plus the number of pooled standard errors estimated).}
    \item{nclasses}{the number of classes in each model.}
    \item{nfeatures}{a \code{list} of vectors with the numbers of features with non-zero t-statistics for each class in each model.}
    \item{priors}{a \code{list} of vectors with the class prior probabilities.}
    \item{fastmap}{a \code{list} containing components from the Fastmap projection.}
}
\author{
	Kyle D. Bemis
}
\seealso{
    \code{\linkS4class{MSImageSet}},
    \code{\link[Cardinal]{spatialKMeans-methods}},
    \code{\link[Cardinal]{spatialSparseKMeans-methods}},
    \code{\link[Cardinal]{spatialCluster-methods}}
}
\examples{
    ## write examples to put here
}
\keyword{methods}

