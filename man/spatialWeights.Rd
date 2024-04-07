\name{spatialWeights}

\alias{spatialWeights}
\alias{spatialWeights,ANY-method}
\alias{spatialWeights,SpectralImagingData-method}
\alias{spatialWeights,PositionDataFrame-method}

\title{Calculate spatial weights}

\description{
	Calculate weights for neighboring observations based on either the spatial distance between the neighbors or the dissimilarity between the observations.
}

\usage{
\S4method{spatialWeights}{ANY}(x, coord = x, r = 1, byrow = TRUE,
    neighbors = findNeighbors(coord, r=r),
    weights = c("gaussian", "adaptive"),
    sd = ((2 * r) + 1) / 4, matrix = FALSE,
    nchunks = getCardinalNChunks(),
    verbose = getCardinalVerbose(),
    BPPARAM = getCardinalBPPARAM(), \dots)

\S4method{spatialWeights}{SpectralImagingData}(x, r = 1,
        neighbors = findNeighbors(x, r=r), \dots)

\S4method{spatialWeights}{PositionDataFrame}(x, r = 1,
        neighbors = findNeighbors(x, r=r),
        sd = ((2 * r) + 1) / 4, \dots)
}

\arguments{
    \item{x}{Either  matrix or data frame of spatial coordinates, or a data matrix with rows or columns located at the coordinates given by \code{coord}.}

    \item{coord}{The spatial coordinates of the rows/columns of \code{x}. Ignored if \code{neighbors} is provided.}
    
    \item{r}{The spatial maximum distance for an observation to be considered a neighbor. Ignored if \code{neighbors} is provided.}

    \item{byrow}{If \code{x} is a data matrix, then are the weights calculated based on the dissimilarity between the rows (TRUE) or the columns (FALSE).}
    
    \item{neighbors}{A \code{factor} giving which observations should be treated as spatially-independent. Observations in the same group are assumed to have a spatial relationship.}
    
    \item{weights}{The type of weights to calculate. Either Gaussian weights with a constant standard deviation, or adaptive weights with a standard deviation based on the dissimilarity between the neighboring observations.}

    \item{sd}{The standard deviation for the Gaussian weights. Ignored with \code{weights="adaptive"}.}

    \item{matrix}{Should the weights be returned as a sparse adjacency matrix instead of a list?}

    \item{nchunks}{The number of chunks to use for processing.}

    \item{verbose}{Should progress messages be printed?}
    
    \item{BPPARAM}{An optional instance of \code{BiocParallelParam}. See documentation for \code{\link{bplapply}}.}

    \item{\dots}{Additional arguments passed to the next method.}
}

\value{
    Either a list of weights of neighbors or a sparse adjacency matrix (\code{\linkS4class{sparse_mat}}).
}

\author{
	Kylie A. Bemis
}

\seealso{
    \code{\link{findNeighbors}}
}

\examples{
set.seed(1)
mse <- simulateImage(preset=1, dim=c(10,10))

# calculate weights based on distance
spatialWeights(pixelData(mse), r=1)

# calculate weights based on spectral dissimilarity
spatialWeights(mse, r=1)
}

\keyword{spatial}
