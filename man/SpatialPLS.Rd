\name{SpatialPLS}

\alias{SpatialPLS}
\alias{class:SpatialPLS}
\alias{SpatialPLS-class}

\alias{PLS}
\alias{PLS,ANY-method}
\alias{PLS,SpectralImagingExperiment-method}
\alias{fitted,SpatialPLS-method}
\alias{predict,SpatialPLS-method}
\alias{topFeatures,SpatialPLS-method}
\alias{plot,SpatialPLS,missing-method}
\alias{image,SpatialPLS-method}

\alias{SpatialOPLS}
\alias{class:SpatialOPLS}
\alias{SpatialOPLS-class}

\alias{OPLS}
\alias{OPLS,ANY-method}
\alias{OPLS,SpectralImagingExperiment-method}
\alias{coef,SpatialOPLS-method}
\alias{residuals,SpatialOPLS-method}
\alias{fitted,SpatialOPLS-method}
\alias{predict,SpatialOPLS-method}
\alias{topFeatures,SpatialOPLS-method}
\alias{plot,SpatialOPLS,missing-method}
\alias{image,SpatialOPLS-method}

\title{Partial least squares (projection to latent structures)}

\description{
	Compute partial least squares (also called projection to latent structures or PLS). This will also perform discriminant analysis (PLS-DA) if the response is a factor. Orthogonal partial least squares options (O-PLS and O-PLS-DA) is also supported; in this case, O-PLS step is a pre-processing step to remove noise orthogonal to the response, before fitting a PLS model with a single component.
}

\usage{
\S4method{PLS}{ANY}(x, y, ncomp = 3,
    method = c("nipals", "simpls", "kernel1", "kernel2"),
    center = TRUE, scale = FALSE, bags = NULL,
    verbose = getCardinalVerbose(), chunkopts = list(),
    BPPARAM = getCardinalBPPARAM(), \dots)

\S4method{PLS}{SpectralImagingExperiment}(x, y, ncomp = 3,
    method = c("nipals", "simpls", "kernel1", "kernel2"),
    center = TRUE, scale = FALSE, \dots)

\S4method{fitted}{SpatialPLS}(object, type = c("response", "class"), \dots)

\S4method{predict}{SpatialPLS}(object, newdata, ncomp,
        type = c("response", "class"), simplify = TRUE, \dots)

\S4method{topFeatures}{SpatialPLS}(object, n = Inf, sort.by = c("vip", "coefficients"), \dots)

\S4method{plot}{SpatialPLS,missing}(x, type = c("coefficients", "vip", "scores"), \dots, xlab, ylab)

\S4method{image}{SpatialPLS}(x, type = c("response", "class"), \dots)

\S4method{OPLS}{ANY}(x, y, ncomp = 3, retx = TRUE,
    center = TRUE, scale = FALSE, bags = NULL,
    verbose = getCardinalVerbose(), chunkopts = list(),
    BPPARAM = getCardinalBPPARAM(), \dots)

\S4method{OPLS}{SpectralImagingExperiment}(x, y, ncomp = 3, retx = FALSE,
    center = TRUE, scale = FALSE, \dots)

\S4method{coef}{SpatialOPLS}(object, \dots)

\S4method{residuals}{SpatialOPLS}(object, \dots)

\S4method{fitted}{SpatialOPLS}(object, type = c("response", "class"), \dots)

\S4method{predict}{SpatialOPLS}(object, newdata, ncomp,
        type = c("response", "class"), simplify = TRUE, \dots)

\S4method{topFeatures}{SpatialOPLS}(object, n = Inf, sort.by = c("vip", "coefficients"), \dots)

\S4method{plot}{SpatialOPLS,missing}(x, type = c("coefficients", "vip", "scores"), \dots, xlab, ylab)

\S4method{image}{SpatialOPLS}(x, type = c("response", "class"), \dots)
}

\arguments{
    \item{x}{A dataset in P x N matrix format.}

    \item{y}{The response variable.}

    \item{ncomp}{The number of principal components to calculate.}

    \item{method}{The method used for calculating the principal components. See \code{\link[matter]{pls}} for details.}

    \item{center}{Should the data be centered?}

    \item{scale}{Shoud the data be scaled?}

    \item{bags}{Bags for multiple instance learning. If provided, then it is assumed all observations within a bag have the same label, and if a single observation is "positive" then all observations in the bag are "positive". Multiple instance learning is performed using \code{\link[matter]{mi_learn}}.}

    \item{retx}{Should the (potentially large) processed data matrix be included in the result?}

    \item{verbose}{Should progress messages be printed?}

    \item{chunkopts}{Chunk processing options. See \code{\link{chunkApply}} for details.}

    \item{BPPARAM}{An optional instance of \code{BiocParallelParam}. See documentation for \code{\link{bplapply}}.}

    \item{\dots}{Options passed to \code{\link[irlba]{irlba}}.}

    \item{object}{A \code{SpatialPLS} or \code{SpatialOPLS} object.}

    \item{newdata}{A new \code{SpectralImagingExperiment} for which to make predictions.}

    \item{type}{The type of fitted values to extract or the type of predictions to make.}

    \item{simplify}{If predictions are made using multiple numbers of components, should they be returned as a list, or simplified to an array?}

    \item{n, sort.by}{For \code{topFeatures}, the number of top features to return and how to sort them.}

    \item{xlab, ylab}{Plotting labels.}
}

\value{
    An object of class \code{SpatialPLS} or \code{SpatialOPLS} derived from \code{SpatialResults}, containing the fitted \code{\link[matter]{pls}} or \code{\link[matter]{opls}} model and the spatial metadata.
}

\author{
	Kylie A. Bemis
}

\references{
    Trygg, J., & Wold, S. (2002). Orthogonal projections to latent structures (O-PLS). Journal of Chemometrics, 16(3), 119-128. doi:10.1002/cem.695
}

\seealso{
    \code{\link{PCA}},
    \code{\link{spatialShrunkenCentroids}},
}

\examples{
set.seed(1, kind="L'Ecuyer-CMRG")
mse <- simulateImage(preset=2, npeaks=20, dim=c(10,10), centroided=TRUE)
cls <- makeFactor(circle=pData(mse)$circle, square=pData(mse)$square)

# fit a PLS model with 3 components
pls <- PLS(mse, cls, ncomp=1:3)
plot(pls, type="coefficients", annPeaks="circle")

# visualize predictions
image(pls)
}

\keyword{multivariate}
\keyword{classif}
