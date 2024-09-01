\name{reduceBaseline}

\alias{reduceBaseline}
\alias{reduceBaseline,SpectralImagingData-method}

\title{Reduce baselines in spectra}

\description{
    Apply deferred baseline reduction to spectra.
}

\usage{
\S4method{reduceBaseline}{SpectralImagingData}(object,
    method = c("locmin", "hull", "snip", "median"), \dots)
}

\arguments{
    \item{object}{A spectral imaging dataset.}

    \item{method}{The baseline estimation method to use. See \code{\link[matter]{estbase}} for details.}

    \item{\dots}{Additional arguments passed to the baseline estimation function.}
}

\details{
    The supported baseline estimation methods are:

    \itemize{

        \item{"locmin": Interpolate from local minima using \code{\link{estbase_loc}}.}

        \item{"hull": Convex hull estimation using \code{\link{estbase_hull}}.}

        \item{"snip": Sensitive nonlinear iterative peak (SNIP) clipping using \code{\link{estbase_snip}}.}

        \item{"median": Running medians using \code{\link{estbase_med}}.}
    }
}

\note{
    The baseline reduction is deferred until \code{process()} is called.
}

\value{
    An object of the same class with the processing step queued.
}

\author{
    Kylie A. Bemis
}

\seealso{
    \code{\link{normalize}},
    \code{\link{smooth}},
    \code{\link{reduceBaseline}},
    \code{\link{peakPick}},
    \code{\link{process}}
}

\examples{
set.seed(1, kind="L'Ecuyer-CMRG")
mse <- simulateImage(preset=1, npeaks=10, dim=c(3,3), baseline=1)

# queue baseline reduction
mse2 <- reduceBaseline(mse, method="locmin")
plot(mse2, i=4)

# apply baseline reduction
mse2 <- process(mse2)
}

\keyword{ts}
