\name{spatialDGMM}

\alias{SpatialDGMM}
\alias{class:SpatialDGMM}
\alias{SpatialDGMM-class}

\alias{spatialDGMM}
\alias{spatialDGMM,ANY-method}
\alias{spatialDGMM,SpectralImagingExperiment-method}
\alias{plot,SpatialOPLS-method}
\alias{image,SpatialOPLS-method}

\title{Spatially-aware Dirichlet Gaussian mixture model}

\description{
    Fit a spatially-aware Gaussian mixture models to each feature. The model uses Dirichlet prior is used to achieve spatial smoothing. The means and standard deviations of the Gaussian components are estimated using gradient descent. Simulated annealing is used to avoid local optimia and achieve better parameter estimates.
}

\usage{
\S4method{spatialDGMM}{ANY}(x, coord, i, r = 1, k = 2, groups = NULL,
    weights = c("gaussian", "adaptive"),
    neighbors = findNeighbors(coord, r=r, groups=groups),
    annealing = TRUE, compress = TRUE, byrow = FALSE,
    nchunks = getCardinalNChunks(),
    verbose = getCardinalVerbose(),
    BPPARAM = getCardinalBPPARAM(), \dots)

\S4method{spatialDGMM}{SpectralImagingExperiment}(x, i, r = 1, k = 2, groups = NULL,
    weights = c("gaussian", "adaptive"),
    neighbors = findNeighbors(coord, r=r, groups=groups), \dots)

\S4method{plot}{SpatialDGMM,missing}(x, i = 1L, type = "density",
        layout = NULL, free = "", \dots)

\S4method{image}{SpatialDGMM}(x, i = 1L, type = "class",
        layout = NULL, free = "", \dots)
}
\arguments{
    \item{x}{A spatial dataset in P x N matrix format.}

    \item{i}{The rows/columns of \code{x} to segment (if not all of them).}

    \item{coord}{The spatial coordinates of the rows/columns of \code{x}. Ignored if \code{neighbors} is provided.}
    
    \item{r}{The spatial maximum distance for an observation to be considered a neighbor. Ignored if \code{neighbors} is provided.}

    \item{k}{The number of Gaussian components.}

    \item{groups}{Observations belonging to the different groups will be segmented independently. This should be set to the samples if statistic testing (via \code{\link{meansTest}} is to be performed.)}

    \item{weights}{The type of spatial weights to use for the smoothing. Gaussian weights are weighted only by distance, while adaptive weights also consider the dissimilarity between neighboring observations.}

    \item{neighbors}{A \code{factor} giving which observations should be treated as spatially-independent. Observations in the same group are assumed to have a spatial relationship.}

    \item{annealing}{Should simulated annealing be used?}
    
    \item{compress}{Should the results be compressed? The results can be larger than the original dataset, so compressing them is useful. If this option is used, then the class probabilities are not returned, and the class assignments are compressed using \code{\link[matter]{drle}}.}

    \item{byrow}{Should the rows or columns of \code{x} be segmented?}

    \item{nchunks}{The number of chunks to use for processing.}

    \item{verbose}{Should progress messages be printed?}
    
    \item{BPPARAM}{An optional instance of \code{BiocParallelParam}. See documentation for \code{\link{bplapply}}.}

    \item{\dots}{Additional arguments passed to the next method.}

    \item{type}{The type of plot to display.}

    \item{layout}{A vector of the form \code{c(nrow, ncol)} specifying the number of rows and columns in the facet grid.}

    \item{free}{A string specifying the free spatial dimensions during faceting. E.g., \code{""}, \code{"x"}, \code{"y"}, \code{"xy"}, \code{"yx"}.}
}

\value{
    An object of class \code{SpatialDGMM} derived from \code{SpatialResults}, containing the fitted \code{\link[matter]{sgmixn}} object and the spatial metadata.
}

\author{
    Dan Guo and Kylie A. Bemis
}

\references{
    Guo, D., Bemis, K., Rawlins, C., Agar, J., and Vitek, O. (2019.) Unsupervised segmentation of mass spectrometric ion images characterizes morphology of tissues. Proceedings of ISMB/ECCB, Basel, Switzerland, 2019.
}

\examples{
set.seed(1)
mse <- simulateImage(preset=3, dim=c(10,10), npeaks=9,
    peakheight=c(3,6,9), representation="centroid")

gmm <- spatialDGMM(mse, r=1, k=4, weights="adaptive")

image(gmm, i=1:9)
}

\keyword{spatial}
\keyword{clustering}
