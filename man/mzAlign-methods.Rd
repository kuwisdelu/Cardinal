\name{mzAlign-methods}
\docType{methods}

\alias{mzAlign}
\alias{mzAlign-methods}
\alias{mzAlign,MSImagingExperiment,numeric-method}
\alias{mzAlign,MSImagingExperiment,missing-method}

\title{Mass align an imaging dataset}
\description{
	Apply spectral alignment to a mass spectrometry imaging dataset.
}
\usage{
\S4method{mzAlign}{MSImagingExperiment,numeric}(object, ref, tolerance = 1000, units = c("ppm", "mz"),
        quantile = 0.2, span = 0.75, \dots)

\S4method{mzAlign}{MSImagingExperiment,missing}(object, tolerance = 1000, units = c("ppm", "mz"),
        quantile = 0.2, span = 0.75, \dots)
}
\arguments{
    \item{object}{An imaging dataset.}
    \item{ref}{A reference spectrum to use for alignment.}
    \item{tolerance}{The tolerance to be used when matching the peaks in the unaligned spectra to the reference spectrum.}
    \item{units}{The units to use for the \code{tolerance}.}
    \item{quantile}{The top quantile of reference points (peaks detected via local maxima) to use from the reference spectrum.}
    \item{span}{The smoothing parameter for the local polynomial regression used to determine the warping function.}
    \item{\dots}{Ignored.}
}
\details{
    Mass binning is performed by first selecting reference points in the reference spectrum by detecting local maxima. Some number of these reference points with the highest intensities (determined by \code{quantile}) are then used for alignment. The nearest local maxima to the reference points are detected in each unaligned spectrum (within \code{tolerance}), and then the unaligned spectra are warped to maximize correlation with the reference spectrum.

    Internally, \code{\link{pixelApply}} is used to perform the alignment. See its documentation page for more details.
}
\value{
    An object of the same class with the aligned spectra.
}
\author{
	Kylie A. Bemis
}
\seealso{
    \code{\linkS4class{MSImagingExperiment}},
    \code{\link{mzBin}},
    \code{\link{peakAlign}},
    \code{\link{pixelApply}},
    \code{\link{process}}
}
\examples{
register(SerialParam())

set.seed(2)
data <- simulateImage(preset=1, npeaks=10, dim=c(3,3), sdmz=500)
data <- data[,pData(data)$circle]

# queue spectral alignment
data <- mzAlign(data, tolerance=1, units="mz")

# apply spectral alignment
data_aligned <- process(data, plot=interactive())
}
\keyword{methods}
