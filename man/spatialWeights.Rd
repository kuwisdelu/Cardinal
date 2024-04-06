\name{findNeighbors}

\alias{findNeighbors}

\alias{findNeighbors,ANY-method}
\alias{findNeighbors,SpectralImagingData-method}
\alias{findNeighbors,PositionDataFrame-method}

\title{Find spatial neighbors}

\description{
	Methods for calculating the spatial neighbors (pixels within a certain distance) or spatial weights for all pixels in a dataset.
}

\usage{
\S4method{findNeighbors}{ANY}(x, r = 1, groups = NULL,
        metric = "maximum", p = 2, matrix = FALSE, \dots)

\S4method{findNeighbors}{SpectraImagingData}(x, r = 1, groups = run(x), \dots)

\S4method{findNeighbors}{PositionDataFrame}(x, r = 1, groups = run(x), \dots)
}

\arguments{
    \item{x}{An imaging dataset or data frame with spatial dimensions.}
    
    \item{r}{The spatial maximum distance for an observation to be considered a neighbor.}
    
    \item{groups}{A \code{factor} giving which observations should be treated as spatially-independent. Observations in the same group are assumed to have a spatial relationship.}
    
    \item{metric}{Distance metric to use when finding the nearest neighbors. Supported metrics include "euclidean", "maximum", "manhattan", and "minkowski".}

    \item{p}{The power for the Minkowski distance.}

    \item{matrix}{Should the neighbors be returned as a sparse adjacency matrix instead of a list?}

    \item{\dots}{Additional arguments passed to the next method.}
    
    \item{BPPARAM}{An optional instance of \code{BiocParallelParam}. See documentation for \code{\link{bplapply}}.}
}

\value{
    Either a list of indices of neighbors or a sparse adjacency matrix (\code{\linkS4class{sparse_mat}}).
}

\author{
	Kylie A. Bemis
}

\seealso{
    \code{\link{spatialWeights}}
}

\examples{
coord <- expand.grid(x=1:9, y=1:9)
values <- rnorm(nrow(coord))
pdata <- PositionDataFrame(coord=coord, values=values)

# find spatial neighbors
findNeighbors(pdata, r=1)

# calculate distance-based weights
spatialWeights(pdata, r=1)

# visualize weight matrix
W <- spatialWeights(pdata, r=1, matrix=TRUE)
image(as.matrix(W), col=bw.colors(100))
}
\keyword{spatial}
