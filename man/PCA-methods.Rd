\name{PCA-methods}
\docType{methods}

\alias{PCA}
\alias{class:PCA}
\alias{PCA-class}
\alias{PCA-methods}

\alias{PCA,SparseImagingExperiment-method}
\alias{predict,PCA2-method}
\alias{summary,PCA2-method}

\alias{PCA,SImageSet-method}
\alias{predict,PCA-method}
\alias{predict,PCA-method}
\alias{summary,PCA-method}
\alias{print.summary.PCA}
\alias{plot.summary.PCA}

\title{Principal components analysis}
\description{
	Performs principal components analysis efficiently on large datasets using implicitly restarted Lanczos bi-diagonalization (IRLBA) algorithm for approximate singular value decomposition of the data matrix.
}
\usage{
\S4method{PCA}{SparseImagingExperiment}(x, ncomp = 3, center = TRUE, scale = FALSE, \dots)

\S4method{predict}{PCA2}(object, newx, ncomp, \dots)

\S4method{summary}{PCA2}(object, \dots)

\S4method{PCA}{SImageSet}(x, ncomp = 3,
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
    \item{object}{The result of a previous call to \code{\link{PCA}}.}
    \item{newx}{An imaging dataset for which to calculate the principal components scores based on the aleady-calculated principal components loadings.}
}
\value{
    An object of class \code{PCA2}, which is a \code{ImagingResult}, or an object of class \code{PCA}, which is a \code{ResultSet}. Each elemnt of \code{resultData} slot contains at least the following components:

    \describe{
        \item{\code{loadings}:}{A matrix with the principal component loadings.}
        \item{\code{scores}:}{A matrix with the principal component scores.}
        \item{\code{sdev}:}{The standard deviations of the principal components.}
    }
}
\author{
	Kylie A. Bemis
}
\seealso{
    \code{\link{OPLS}},
    \code{\link{PLS}},
    \code{\link{irlba}},
    \code{\link{svd}}
}
\examples{
register(SerialParam())

set.seed(1)
data <- simulateImage(preset=2, npeaks=20, dim=c(6,6),
    representation="centroid")

# project to FastMap components
pca <- PCA(data, ncomp=2)

# visualize first 2 components
image(pca, superpose=FALSE)
}
\keyword{multivariate}

