\name{spatialDGMM-methods}
\docType{methods}
\alias{spatialDGMM}
\alias{class:SpatialDGMM}
\alias{SpatialDGMM-class}
\alias{SpatialDGMM-methods}

\alias{spatialDGMM,SparseImagingExperiment-method}

\title{Spatially-aware Dirichlet Gaussian mixture model}
\description{
    Fits spatially-aware Dirichlet Gaussian mixture models to each feature and each run in an imaging experiment. Each image is segmented and the means and variances of all Gaussian components are estimated. A linear filter with a spatially-aware kernel is applied to the component probabilities to achieve spatial smoothing. Simulated annealing is used in the EM-algorithm to avoid local optimia and achieve more accurate parameter estimates.
}
\usage{
\S4method{spatialDGMM}{SparseImagingExperiment}(x, r = 1, k = 3,
    method = c("gaussian", "adaptive"),
    dist = "chebyshev", annealing = TRUE,
    iter.max = 100, tol = 1e-9,
    collate.results = TRUE,
    BPPARAM = bpparam(), \dots)
}
\arguments{
    \item{x}{The imaging dataset to segment or classify.}
    \item{r}{The spatial neighborhood radius of nearby pixels to consider. This can be a vector of multiple radii values.}
    \item{k}{The maximum number of segments (clusters). This can be a vector to try initializing the clustering with different numbers of maximum segments. The final number of segments may differ.}
    \item{method}{The method to use to calculate the spatial smoothing kernels for the embedding. The 'gaussian' method refers to spatially-aware (SA) weights, and 'adaptive' refers to spatially-aware structurally-adaptive (SASA) weights.}
    \item{dist}{The type of distance metric to use when calculating neighboring pixels based on \code{r}. The options are `radial', `manhattan', `minkowski', and `chebyshev' (the default).}
    \item{annealing}{Should simulated annealing be used during the optimization process to improve parameter estimates?}
    \item{iter.max}{The maximum number of EM-algorithm iterations.}
    \item{tol}{The tolerance convergence criterion for the EM-algorithm. Corresponds to the chance in log-likelihood.}
    \item{collate.results}{Should the results of different groups be re-combined for compatibility with downstream methods? For advanced users only.}
    \item{\dots}{Passed to internal methods.}
    \item{BPPARAM}{An optional instance of \code{BiocParallelParam}. See documentation for \code{\link{bplapply}}.}
}
\value{
    An object of class \code{SpatialDGMM}, which is a \code{ResultImagingExperiment}, where each component of the \code{resultData} slot contains at least the following components:

    \describe{
        \item{\code{params}:}{A list giving the parameter estimates for the means (mu) and variances (sigma) for each Gaussian component, and the alpha and beta Dirichlet parameters.}
        \item{\code{class}:}{The predicted Gaussian component.}
        \item{\code{probability}:}{The probability of membership for each Gaussian component.}
    }
}
\author{
    Kylie A. Bemis
}
\references{
    Guo, D., Bemis, K., Rawlins, C., Agar, J., and Vitek, O. (2019.) Unsupervised segmentation of mass spectrometric ion images characterizes morphology of tissues. Proceedings of ISMB/ECCB, Basel, Switzerland, 2019.
}
\examples{
set.seed(1)
x <- simulateImage(preset=3, dim=c(10,10), sdnoise=0.5,
    npeaks=10, peakheight=c(4,8), peakdiff=4,
    representation="centroid")

res <- spatialDGMM(x, r=1, k=5)
}

\keyword{spatial}
\keyword{classif}
\keyword{clustering}
