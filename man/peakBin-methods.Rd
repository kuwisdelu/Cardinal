\name{peakBin-methods}
\docType{methods}

\alias{peakBin}
\alias{peakBin-methods}
\alias{peakBin,MSImagingExperiment,numeric-method}
\alias{peakBin,MSImagingExperiment,missing-method}

\title{Peak bin an imaging dataset}
\description{
	Apply peak binning to a mass spectrometry imaging dataset.
}
\usage{
\S4method{peakBin}{MSImagingExperiment,numeric}(object, ref, type=c("area", "height"),
        tolerance = NA, units = c("ppm", "mz"), \dots)

\S4method{peakBin}{MSImagingExperiment,missing}(object, type=c("area", "height"),
        tolerance = NA, units = c("ppm", "mz"), \dots)
}
\arguments{
    \item{object}{An imaging dataset.}
    \item{ref}{A reference to which the peaks are binned.}
    \item{type}{Should the summarized intensity of the peak by the maximum height of the peak or the area under the curve?}
    \item{tolerance}{The tolerance to be used when matching the m/z features in the dataset to the reference. If this is NA, then automatically guess a resolution from the data.}
    \item{units}{The units to use for the \code{tolerance}.}
    \item{\dots}{Ignored.}
}
\details{
    Peak binning is performed by first matching the m/z-values in the dataset to those in the reference, and then finding the boundaries of the peak by detecting the nearest local minima. Then either the maximum height or the area under the curve of the peak are returned.

    Internally, \code{\link{pixelApply}} is used to apply the binning. See its documentation page for more details.
}
\value{
    An object of the same class with the binned peaks.
}
\author{
	Kylie A. Bemis
}
\seealso{
    \code{\linkS4class{MSImagingExperiment}},
    \code{\link{peakPick}},
    \code{\link{peakAlign}},
    \code{\link{peakFilter}},
    \code{\link{pixelApply}},
    \code{\link{process}}
}
\examples{
setCardinalBPPARAM(SerialParam())

set.seed(2)
data <- simulateImage(preset=1, npeaks=10, dim=c(3,3))
data <- data[,pData(data)$circle]
ref <- mz(metadata(data)$design$featureData)

# queue peak binning
data <- peakBin(data, ref=ref, type="height")

# apply peak binning
data_peaks <- process(data, plot=interactive())
}
\keyword{methods}
