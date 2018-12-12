\name{peakFilter-methods}
\docType{methods}

\alias{peakFilter}
\alias{peakFilter-methods}
\alias{peakFilter,MSImageSet-method}
\alias{peakFilter,MSImagingExperiment-method}

\alias{peakFilter.freq}

\title{Peak filter an imaging dataset}
\description{
	Apply peak filtering to a mass spectrometry imaging dataset.
}
\usage{
\S4method{peakFilter}{MSImagingExperiment}(object, freq.min = 0.01, \dots)

\S4method{peakFilter}{MSImageSet}(object, method = "freq", \dots, pixel, plot)

## Filter based on the frequency of a peak
peakFilter.freq(x, freq.min=0.01, \dots)
}
\arguments{
    \item{object}{An object of class \code{\linkS4class{MSImageSet}}.}
    \item{freq.min}{Minimum frequency density; peaks that occur in the dataset in lesser proportion than this will be dropped.}
    \item{\dots}{Additional arguments passed to the peak filtering method, or conditions evaluating to logical vectors where only those conditions that are TRUE are retained.}
    \item{method}{The peak filtering method to use.}
    \item{pixel}{Deprecated.}
    \item{plot}{Deprecated. (Never did anything anyway.)}
    \item{x}{The vector of ion image intensities to filter.}
}
\details{
    When applied to a \code{MSImagingExperiment} object, \code{peakFilter} uses the \code{summarize()} to generate useful statistics about the detected peaks. These include the `min', `max', `mean', `sum', `sd', and `var' of the detected peaks. These can be used in logical expressions to filter the peaks.

    When applied to a \code{MSImageSet} object, unlike most other processing methods, \code{peakFilter} operates on the feature space (ion images) of the dataset.

    Peak filtering is usually performed using the provided functions, but a user-created function can also be passed to \code{method}. In this case it should take the following arguments:

    \itemize{
        \item{\code{x}: The vector of ion image intensities to filter.}
        \item{\code{\dots}: Additional arguments.}
    }

    A user-created function should return a logical: \code{TRUE} means keep the peak, and \code{FALSE} means remove the peak.

    Internally, \code{\link{featureApply}} is used to apply the filtering. See its documentation page for more details on additional objects available to the environment installed to the peak filtering function.
}
\value{
    An object of the same class with the filtered peaks.
}
\author{
	Kylie A. Bemis
}
\seealso{
    \code{\linkS4class{MSImagingExperiment}},
    \code{\linkS4class{MSImageSet}},
    \code{\link{peakPick}},
    \code{\link{peakAlign}},
    \code{\link{peakBin}},
    \code{\link{reduceDimension}},
    \code{\link{featureApply}},
    \code{\link{process}}
}
\examples{
register(SerialParam())

set.seed(1)
data <- simulateImage(preset=3, npeaks=9, dim=c(3,3))

# queue peak picking, alignment, and filtering
data <- peakPick(data, method="simple", SNR=6)
data <- peakAlign(data, tolerance=200, units="ppm")
data <- peakFilter(data, freq.min=0.2)

# apply peak picking, alignment, and filtering
data_peaks <- process(data, plot=interactive())
}
\keyword{methods}
