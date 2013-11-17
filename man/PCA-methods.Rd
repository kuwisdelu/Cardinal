\name{PCA-methods}
\docType{methods}
\alias{PCA}
\alias{PCA-methods}
\alias{PCA,matrix-method}
\alias{PCA,MSImageSet-method}
\title{Principal components analysis}
\description{
	Performs principal components analysis efficiently on large datasets using implicitly restarted Lanczos bi-diagonalization (IRLBA) algorithm for approximate singular value decomposition of the data matrix.
}
\usage{
    \S4method{PCA}{MSImageSet}(x, ncomp=2, standardize=FALSE, method=c("irlba", "svd", "nipals"), \dots)
    
    \S4method{PCA}{matrix}(x, ncomp=2, isNxP=TRUE, standardize=FALSE, method=c("irlba", "svd", "nipals"), \dots)
}
\arguments{
    \item{x}{the data on which to perform principal components analysis.}
    \item{ncomp}{the number of principal components to calculate.}
    \item{isNxP}{whether the data matrix is n x p, i.e., whether the each row corresponds to an observation.}
    \item{standardize}{whether the data should be standardized.}
    \item{method}{the method to use to calculate the components}
    \item{\dots}{additional arguments to pass to specified method.}
}
\value{
    An object of class \code{PCA} with componets:
    \item{scores}{a \code{matrix} with the principal component scores.}
    \item{loadings}{a \code{matrix} with the principal component loadings.}
}
\author{
	Kyle D. Bemis
}
\seealso{
    \code{\linkS4class{MSImageSet}},
    \code{\link[irlba]{irlba}}
}
\examples{
    ## write examples to put here
}
\keyword{methods}

