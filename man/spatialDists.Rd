\name{spatialDists}

\alias{spatialDists}
\alias{spatialDists,ANY-method}
\alias{spatialDists,SpectralImagingExperiment-method}
\alias{spatialDists,PositionDataFrame-method}

\title{Calculate spatially-smoothed distances}

\description{
	Calculate distances between observations with smoothing based on their spatial structure.
}

\usage{
\S4method{spatialDists}{ANY}(x, y, coord, r = 1, byrow = TRUE,
        metric = "euclidean", p = 2, weights = NULL,
        neighbors = findNeighbors(coord, r=r),
        neighbors.weights = spatialWeights(coord, r=r),
        verbose = getCardinalVerbose(), chunkopts = list(),
        BPPARAM = getCardinalBPPARAM(), \dots)

\S4method{spatialDists}{SpectralImagingExperiment}(x, y, r = 1,
        neighbors = findNeighbors(x, r=r),
        neighbors.weights = spatialWeights(x, r=r), \dots)

\S4method{spatialDists}{PositionDataFrame}(x, y, r = 1,
        neighbors = findNeighbors(x, r=r),
        neighbors.weights = spatialWeights(x, r=r), ...)
}

\arguments{
    \item{x}{A data matrix with rows or columns located at the coordinates given by \code{coord}.}

    \item{y}{A data matrix from which to calculate distances with the observations in \code{x}.}

    \item{coord}{The spatial coordinates of the rows/columns of \code{x}. Ignored if \code{neighbors} is provided.}
    
    \item{r}{The spatial maximum distance for an observation to be considered a neighbor. Ignored if \code{neighbors} is provided.}

    \item{byrow}{Are the distances calculated based on the dissimilarity between the rows (TRUE) or the columns (FALSE) of \code{x} and \code{y}.}
    
    \item{metric}{Distance metric to use when finding the nearest neighbors. Supported metrics include "euclidean", "maximum", "manhattan", and "minkowski".}

    \item{p}{The power for the Minkowski distance.}

    \item{weights}{A numeric vector of \emph{feature} weights for the distance components if calculating weighted distances. For example, the weighted Euclidean distance is \code{sqrt(sum(w * (x - y)^2))}.}

    \item{neighbors}{A list of numeric vectors giving the row or column indices of the spatial neighbors for the rows or columns of \code{x}.}

    \item{neighbors.weights}{A list of numeric vectors giving the spatial weights corresponding to \code{neighbors}.}

    \item{verbose}{Should progress messages be printed?}

    \item{chunkopts}{Chunk processing options. See \code{\link{chunkApply}} for details.}
    
    \item{BPPARAM}{An optional instance of \code{BiocParallelParam}. See documentation for \code{\link{bplapply}}.}

    \item{\dots}{Additional arguments passed to the next method.}
}

\value{
    A matrix of distances with rows equal to the number of observations in \code{x} and columns equal to the number of observations in \code{y}.
}

\author{
	Kylie A. Bemis
}

\seealso{
    \code{\link{findNeighbors}},
    \code{\link{spatialWeights}}
}

\examples{
set.seed(1, kind="L'Ecuyer-CMRG")
mse <- simulateImage(preset=1, dim=c(10,10))

# calculate spatially-aware distances from first 5 spectra
spatialDists(mse, spectra(mse)[,1:5], r=1)
}

\keyword{spatial}
