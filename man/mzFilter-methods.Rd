\name{mzFilter-methods}
\docType{methods}

\alias{mzFilter}
\alias{mzFilter-methods}
\alias{mzFilter,MSImagingExperiment-method}

\alias{peakFilter}
\alias{peakFilter-methods}
\alias{peakFilter,MSImageSet-method}
\alias{peakFilter,MSImagingExperiment-method}

\alias{peakFilter.freq}

\title{Filter the features of an imaging dataset by intensity}
\description{
	Apply filtering to a mass spectrometry imaging dataset based on the intensities of each peak or mass feature.
}
\usage{
\S4method{mzFilter}{MSImagingExperiment}(object, \dots, freq.min = NA, rm.zero = TRUE)

\S4method{peakFilter}{MSImagingExperiment}(object, \dots, freq.min = 0.01, rm.zero = TRUE)

\S4method{peakFilter}{MSImageSet}(object, method = "freq", \dots)

## Filter based on the frequency of a peak
peakFilter.freq(x, freq.min=0.01, \dots)
}
\arguments{
    \item{object}{An object of class \code{\linkS4class{MSImageSet}}.}
    \item{freq.min}{Minimum frequency; peaks that occur in the dataset in lesser proportion than this will be dropped.}
    \item{rm.zero}{Remove features with mean intensities of zero.}
    \item{\dots}{Additional arguments passed to the peak filtering method, or conditions evaluating to logical vectors where only those conditions that are TRUE are retained.}
    \item{method}{The peak filtering method to use.}
    \item{x}{The vector of ion image intensities to filter.}
}
\details{
    When applied to a \code{MSImagingExperiment} object, \code{mzFilter} and \code{peakFilter} uses the \code{summarize()} to generate useful summary statistics about the mass features or detected peaks. These include the `min', `max', `mean', `sum', `sd', and `var' of the intensities for each mass feature or peak. These can be used in logical expressions to filter the features of the dataset.

    Note that \code{peakFilter} is an alias for \code{mzFilter}, with different default parameters that are more appropriate for peak-picked data rather than profile spectra.

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
setCardinalBPPARAM(SerialParam())

set.seed(2)
data <- simulateImage(preset=1, npeaks=10, dim=c(3,3))
data <- data[,pData(data)$circle]

# filter m/z features
process(mzFilter(data))

# queue peak picking, alignment, and filtering
data <- peakPick(data, method="simple", SNR=6)
data <- peakAlign(data, tolerance=200, units="ppm")
data <- peakFilter(data, freq.min=0.5)

# apply peak picking, alignment, and filtering
data_peaks <- process(data, plot=interactive())
}
\keyword{methods}
