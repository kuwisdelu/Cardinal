\name{findNeighbors-methods}
\docType{methods}

\alias{findNeighbors}
\alias{findNeighbors-methods}

\alias{findNeighbors,ImagingExperiment-method}
\alias{findNeighbors,PositionDataFrame-method}

\alias{findNeighbors,iSet-method}
\alias{findNeighbors,IAnnotatedDataFrame-method}

\alias{spatialWeights}
\alias{spatialWeights-methods}

\alias{spatialWeights,ImagingExperiment-method}
\alias{spatialWeights,PositionDataFrame-method}

\alias{spatialWeights,iSet-method}
\alias{spatialWeights,IAnnotatedDataFrame-method}

\title{Find spatial neighbors and spatial weightst}
\description{
	Methods for calculating the spatial neighbors (pixels within a certain distance) or spatial weights for all pixels in a dataset.
}
\usage{
#### Methods for Cardinal >= 2.x classes ####

\S4method{findNeighbors}{ImagingExperiment}(x, r, groups = run(x), \dots)

\S4method{findNeighbors}{PositionDataFrame}(x, r, groups = run(x), dist = "chebyshev", 
    offsets = FALSE, matrix = FALSE, \dots)

\S4method{spatialWeights}{ImagingExperiment}(x, r, method = c("gaussian", "adaptive"),
    dist = "chebyshev", matrix = FALSE, BPPARAM = bpparam(), \dots)

\S4method{spatialWeights}{PositionDataFrame}(x, r, matrix = FALSE, \dots)

#### Methods for Cardinal 1.x classes ####

\S4method{findNeighbors}{iSet}(x, r, groups = x$sample, \dots)

\S4method{findNeighbors}{IAnnotatedDataFrame}(x, r, groups = x$sample, dist = "chebyshev",
    offsets = FALSE, matrix = FALSE, \dots)

\S4method{spatialWeights}{iSet}(x, r, method = c("gaussian", "adaptive"),
    matrix = FALSE, \dots)

\S4method{spatialWeights}{IAnnotatedDataFrame}(x, r, matrix = FALSE, \dots)
}
\arguments{
    \item{x}{An imaging dataset or data frame with spatial dimensions.}
    \item{r}{The spatial radius or distance.}
    \item{groups}{A \code{factor} giving which pixels should be treated as spatially-independent. Pixels in the same group are assumed to have a spatial relationship.}
    \item{dist}{The type of distance metric to use. The options are `radial', `manhattan', `minkowski', and `chebyshev' (the default).}
    \item{offsets}{Should the coordinate offsets from the center of each neighborhood be returned?}
    \item{matrix}{Should the result be returned as a sparse matrix instead of a list?}
    \item{method}{The method to use to calculate the spatial weights. The 'gaussian' method refers to Gaussian-distributed distance-based weights (\code{alpha} weights), and 'adaptive' refers to structurally-adaptive weights for bilaterla filtering (\code{beta} weights).}
    \item{\dots}{Addtional arguments to be passed to next method.}
    \item{BPPARAM}{An optional instance of \code{BiocParallelParam}. See documentation for \code{\link{bplapply}}.}
}
\value{
    Either a list of neighbors/weights or a sparse matrix (\code{\linkS4class{sparse_mat}}) giving the neighbors and weights for each pixel.

    For \code{spatialWeights}, two types of weights are calculated and returned as a list. The \code{alpha} weights are distance-based, following a Gaussian distributed that produces smaller weights for larger distances. The \code{beta} weights are adaptive weights used for bilateral filtering, which are based on the difference in the feature-vectors between pixels. If \code{method="gaussian"} only the \code{alpha} weights are calcualated and the \code{beta} weights are all set to 1. If \code{matrix=TRUE}, the \code{alpha} and \code{beta} weights are multiplied together to produce the weights for the matrix; otherwise, both are returned separately.
}
\author{
	Kylie A. Bemis
}
\seealso{
    \code{\link{image}}
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
