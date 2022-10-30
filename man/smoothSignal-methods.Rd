\name{smoothSignal-methods}
\docType{methods}

\alias{smooth}
\alias{smooth-methods}
\alias{smooth,SparseImagingExperiment-method}

\alias{smoothSignal}
\alias{smoothSignal-methods}
\alias{smoothSignal,SparseImagingExperiment-method}

\alias{smoothSignal.gaussian}
\alias{smoothSignal.sgolay}
\alias{smoothSignal.ma}

\title{Smooth the signals of a imaging dataset}
\description{
	Apply smoothing to the feature vectors of an imaging dataset.
}
\usage{
\S4method{smooth}{SparseImagingExperiment}(x, \dots)

\S4method{smoothSignal}{SparseImagingExperiment}(object, method = c("gaussian", "sgolay", "ma"), \dots)

## Gaussian smoothing
smoothSignal.gaussian(x, sd=window/4, window=5, \dots)

## Savitsky-Golay smoothing
smoothSignal.sgolay(x, order=3, window=order + 3 - order \%\% 2, \dots)

## Moving average smoothing
smoothSignal.ma(x, coef=rep(1, window + 1 - window \%\% 2), window=5, \dots)
}
\arguments{
    \item{object}{An imaging dataset.}
    \item{method}{The smoothing method to use.}
    \item{\dots}{Additional arguments passed to the smoothing method.}
    \item{x}{The signal or dataset to be smoothed.}
    \item{sd}{The standard deviation for the Gaussian kernel.}
    \item{window}{The smoothing window.}
    \item{order}{The order of the smoothing filter.}
    \item{coef}{The coefficients for the moving average filter.}
}
\details{
    Smoothing is usually performed using the provided functions, but a user-created function can also be passed to \code{method}. In this case it should take the following arguments:

    \itemize{
        \item{\code{x}: A \code{numeric} vector of intensities.}
        \item{\code{\dots}: Additional arguments.}
    }

    A user-created function should return a \code{numeric} vector of the same length.

    Internally, \code{\link{pixelApply}} is used to apply the smooothing. See its documentation page for more details on additional objects available to the environment installed to the smoothing function.
}
\value{
    An object of the same class with the smoothed spectra.
}
\author{
	Kylie A. Bemis
}
\seealso{
    \code{\linkS4class{MSImagingExperiment}},
    \code{\link{pixelApply}},
    \code{\link{process}}
}
\examples{
setCardinalBPPARAM(SerialParam())

set.seed(2)
data <- simulateImage(preset=1, npeaks=10, dim=c(3,3), baseline=1)
data <- data[,pData(data)$circle]

# queue smoothing
data <- smoothSignal(data, method="ma", window=9)

# apply smoothing
data_smooth <- process(data, plot=interactive())
}
\keyword{methods}
