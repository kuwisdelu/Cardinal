\name{PCA-methods}
\docType{methods}
\alias{PCA}
\alias{class:PCA}
\alias{PCA-class}
\alias{PCA-methods}
\alias{PCA,SImageSet-method}
\alias{predict,PCA-method}
\alias{predict,PCA-method}
\alias{summary,PCA-method}
\alias{print.summary.PCA}
\alias{plot.summary.PCA}

\title{Principal Components Analysis}
\description{
	Performs principal components analysis efficiently on large datasets using implicitly restarted Lanczos bi-diagonalization (IRLBA) algorithm for approximate singular value decomposition of the data matrix.
}
\usage{
    \S4method{PCA}{SImageSet}(x, ncomp = 20,
        method = c("irlba", "nipals", "svd"),
        center = TRUE,
        scale = FALSE,
        iter.max = 100, \dots)
    
    \S4method{predict}{PCA}(object, newx, \dots)
}
\arguments{
    \item{x}{The imaging dataset for which to calculate the principal components.}
    \item{ncomp}{The number of principal components to calculate.}
    \item{method}{The function used to calculate the singular value decomposition.}
    \item{center}{Should the data be centered first? This is passed to \code{scale}.}
    \item{scale}{Shoud the data be scaled first? This is passed to \code{scale}.}
    \item{iter.max}{The number of iterations to perform for the NIPALS algorithm.}
    \item{\dots}{Ignored.}
    \item{object}{The result of a previous call to \code{\link[Cardinal]{PCA}}.}
    \item{newx}{An imaging dataset for which to calculate the principal components scores based on the aleady-calculated principal components loadings.}
}
\value{
    An object of class \code{PCA}, which is a \code{ResultSet}, where each component of the \code{resultData} slot contains at least the following components:

    \describe{
        \item{\code{scores}:}{A matrix with the principal component scores.}
        \item{\code{loadings}:}{A matrix with the principal component loadings.}
        \item{\code{sdev}:}{The standard deviations of the principal components.}
        \item{\code{method}:}{The method used to calculate the principal components.}
        \item{\code{ncomp}:}{The number of principal components calculated.}
        \item{\code{center}:}{The center of the dataset. Used for calculating principal components scores on new data.}
        \item{\code{scale}:}{The scaling factors for the dataset. Used for calculating principal components scores on new data.}
    }
}
\author{
	Kyle D. Bemis
}
\seealso{
    \code{\link[Cardinal]{OPLS}},
    \code{\link[Cardinal]{PLS}},
    \code{\link[irlba]{irlba}},
    \code{\link[base]{svd}}
}
\examples{
sset <- generateImage(diag(4), range=c(200, 300), step=1)

pca <- PCA(sset, ncomp=2)
}
\keyword{multivariate}

