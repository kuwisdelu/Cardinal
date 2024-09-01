\name{SpatialPCA}

\alias{SpatialPCA}
\alias{class:SpatialPCA}
\alias{SpatialPCA-class}

\alias{PCA}
\alias{PCA,ANY-method}
\alias{PCA,SpectralImagingExperiment-method}
\alias{predict,SpatialPCA-method}
\alias{plot,SpatialPCA,missing-method}
\alias{image,SpatialPCA-method}

\title{Principal components analysis}

\description{
	Compute principal components efficiently using implicitly restarted Lanczos bi-diagonalization (IRLBA) algorithm for approximate singular value decomposition.
}

\usage{
\S4method{PCA}{ANY}(x, ncomp = 3,
    center = TRUE, scale = FALSE,
    verbose = getCardinalVerbose(), chunkopts = list(),
    BPPARAM = getCardinalBPPARAM(), \dots)

\S4method{PCA}{SpectralImagingExperiment}(x, ncomp = 3,
    center = TRUE, scale = FALSE, \dots)

\S4method{predict}{SpatialPCA}(object, newdata,
    BPPARAM = getCardinalBPPARAM(), \dots)

\S4method{plot}{SpatialPCA,missing}(x, type = c("rotation", "scree", "x"), \dots, xlab, ylab)

\S4method{image}{SpatialPCA}(x, type = "x", \dots)
}
\arguments{
    \item{x}{A dataset in P x N matrix format.}

    \item{ncomp}{The number of principal components to calculate.}

    \item{center}{Should the data be centered?}

    \item{scale}{Shoud the data be scaled?}

    \item{verbose}{Should progress messages be printed?}

    \item{chunkopts}{Chunk processing options. See \code{\link{chunkApply}} for details.}

    \item{BPPARAM}{An optional instance of \code{BiocParallelParam}. See documentation for \code{\link{bplapply}}.}

    \item{\dots}{Options passed to \code{\link[irlba]{irlba}}.}

    \item{object}{A \code{SpatialPCA} object.}

    \item{newdata}{A new \code{SpectralImagingExperiment} for which to calculate the scores.}

    \item{type}{The type of plot to display.}

    \item{xlab, ylab}{Plotting labels.}
}

\value{
   An object of class \code{SpatialPCA} derived from \code{SpatialResults}, containing the fitted \code{\link[matter]{prcomp_lanczos}} object and the spatial metadata.
}

\author{
	Kylie A. Bemis
}

\seealso{
    \code{\link[matter]{prcomp_lanczos}},
    \code{\link{NMF}},
    \code{\link{spatialFastmap}},
    \code{\link{irlba}},
    \code{\link{svd}}
}

\examples{
set.seed(1, kind="L'Ecuyer-CMRG")
mse <- simulateImage(preset=2, npeaks=20, dim=c(10,10),
    centroided=TRUE)

# project to principal components
pc <- PCA(mse, ncomp=2)

# visualize first 2 components
image(pc, superpose=FALSE, scale=TRUE)
}

\keyword{multivariate}

