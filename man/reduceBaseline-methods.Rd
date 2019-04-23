\name{reduceBaseline-methods}
\docType{methods}

\alias{reduceBaseline}
\alias{reduceBaseline-methods}
\alias{reduceBaseline,MSImageSet-method}
\alias{reduceBaseline,SparseImagingExperiment-method}

\alias{reduceBaseline.median}
\alias{reduceBaseline.locmin}

\title{Reduce the baseline for an imaging dataset}
\description{
	Apply baseline reduction to the feature vectors of an imaging dataset.
}
\usage{
\S4method{reduceBaseline}{SparseImagingExperiment}(object, method = c("median", "locmin"), \dots)

\S4method{reduceBaseline}{MSImageSet}(object, method = "median",
    \dots,
    pixel = pixels(object),
    plot = FALSE)

## Interpolated median baseline reduction
reduceBaseline.median(x, blocks=500, fun=median, spar=1, \dots)

## Local minima baseline reduction
reduceBaseline.locmin(x, window=5, \dots)
}
\arguments{
    \item{object}{An imaging dataset.}
    \item{method}{The baseline reduction method to use.}
    \item{pixel}{The pixels to baseline subtract. If less than the extent of the dataset, this will result in a subset of the data being processed.}
    \item{plot}{Plot each pixel while it is being processed?}
    \item{\dots}{Additional arguments passed to the baseline reduction method.}
    \item{x}{The signal to be baseline-corrected.}
    \item{blocks}{The number of intervals to break the mass spectrum into in order to choose minima or medians from which to interpolate the baseline.}
    \item{fun}{Function used to determine the points from which the baseline will be interpolated.}
    \item{spar}{Smoothing parameter for the spline smoothing applied to the spectrum in order to decide the cutoffs for throwing away baseline references that might occur inside peaks.}
    \item{window}{The sliding window (number of data points) to consider when determining the local minima.}
}
\details{
    Baseline reduction is usually performed using the provided functions, but a user-created function can also be passed to \code{method}. In this case it should take the following arguments:

    \itemize{
        \item{\code{x}: A \code{numeric} vector of intensities.}
        \item{\code{\dots}: Additional arguments.}
    }

    A user-created function should return a \code{numeric} vector of the same length. with the baseline-subtracted intensities.

    Internally, \code{\link{pixelApply}} is used to apply the baseline reduction. See its documentation page for more details on additional objects available to the environment installed to the baseline reduction function.
}
\value{
    An object of the same class with the baseline-subtracted spectra.
}
\author{
	Kylie A. Bemis
}
\seealso{
    \code{\linkS4class{MSImagingExperiment}},
    \code{\linkS4class{MSImageSet}},
    \code{\link{pixelApply}},
    \code{\link{process}}
}
\examples{
register(SerialParam())

set.seed(1)
data <- simulateImage(preset=3, npeaks=9, dim=c(3,3), baseline=2)

# queue baseline reduction
data <- reduceBaseline(data, method="median", blocks=100)

# apply baseline reduction
data_nobaseline <- process(data, plot=interactive())
}
\keyword{methods}
