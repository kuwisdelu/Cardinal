\name{peakPick-methods}
\docType{methods}

\alias{peakPick}
\alias{peakPick-methods}
\alias{peakPick,MSImageSet-method}
\alias{peakPick,MSImagingExperiment-method}

\alias{peakPick.mad}
\alias{peakPick.simple}
\alias{peakPick.adaptive}
\alias{peakPick.limpic}

\title{Peak pick an imaging dataset}
\description{
	Apply peak picking to a mass spectrometry imaging dataset.
}
\usage{
\S4method{peakPick}{MSImagingExperiment}(object, method = c("mad", "simple", "adaptive"), \dots)

\S4method{peakPick}{MSImageSet}(object, method = c("simple", "adaptive", "limpic"),
    \dots,
    pixel = pixels(object),
    plot = FALSE)

## Local maxima and SNR with noise based on local MAD
peakPick.mad(x, SNR=6, window=5, blocks=1, fun=mean, tform=diff, \dots)

## Local maxima and SNR with constant noise based on SD
peakPick.simple(x, SNR=6, window=5, blocks=100, \dots)

## Local maxima and SNR with adaptive noise based on SD
peakPick.adaptive(x, SNR=6, window=5, blocks=100, spar=1, \dots)

## LIMPIC peak detection
peakPick.limpic(x, SNR=6, window=5, blocks=100, thresh=0.75, \dots)
}
\arguments{
    \item{object}{An imaging dataset.}
    \item{method}{The peak picking method to use.}
    \item{pixel}{The pixels to peak pick. If less than the extent of the dataset, this will result in a subset of the data being processed.}
    \item{plot}{Plot the mass spectrum for each pixel while it is being processed?}
    \item{\dots}{Additional arguments passed to the peak picking method.}
    \item{x}{The mass spectrum to be peak picked.}
    \item{SNR}{The minimum signal-to-noise ratio to be considered a peak.}
    \item{window}{The window width for seeking local maxima.}
    \item{blocks}{The number of blocks in which to divide the mass spectrum in order to calculate the noise.}
    \item{fun}{The function used to estimate centrality and average absolute deviation.}
    \item{tform}{A transformation to be applied to the mass spectrum before estimating noise.}
    \item{spar}{Smoothing parameter for the spline smoothing applied to the spectrum in order to decide the cutoffs for throwing away false noise spikes that might occur inside peaks.}
    \item{thresh}{The thresholding quantile to use when comparing slopes in order to throw away peaks that are too flat.}
}
\details{
    Peak picking is usually performed using the provided functions, but a user-created function can also be passed to \code{method}. In this case it should take the following arguments:

    \itemize{
        \item{\code{x}: A \code{numeric} vector of intensities.}
        \item{\code{\dots}: Additional arguments.}
    }

    When applied to an \code{MSImagingExperiment} object, a user-created function should return a \code{integer} vector giving the indices of the detected peaks.

    When applied to an \code{MSImageSet} object, a user-created function should return a \code{list} with two vectors of the same length as \code{x}:

    \itemize{
        \item{\code{peaks}: A \code{logical} vector indicating peaks.}
        \item{\code{noise}: A \code{numeric} vector with the estimated noise.}
    }

    Internally, \code{\link{pixelApply}} is used to apply the peak picking. See its documentation page for more details on additional objects available to the environment installed to the peak picking function.
}
\value{
    An object of the same class with the peak picked spectra. Note that the full mass range is retained and the peaks are unaligned, so \code{\link{peakAlign}} should be called before applying further methods.
}
\author{
	Kylie A. Bemis
}
\references{
    Mantini, D., Petrucci, F., Pieragostino, D., Del Boccio, P., Di Nicola, M., Di Ilio, C., et al. (2007). LIMPIC: a computational method for the separation of protein MALDI-TOF-MS signals from noise. BMC Bioinformatics, 8(101), 101. doi:10.1186/1471-2105-8-101
}
\seealso{
    \code{\linkS4class{MSImagingExperiment}},
    \code{\linkS4class{MSImageSet}},
    \code{\link{peakAlign}},
    \code{\link{peakFilter}},
    \code{\link{peakBin}},
    \code{\link{reduceDimension}},
    \code{\link{pixelApply}},
    \code{\link{process}}
}
\examples{
setCardinalBPPARAM(SerialParam())

set.seed(2)
data <- simulateImage(preset=1, npeaks=10, dim=c(3,3))
data <- data[,pData(data)$circle]

# queue peak picking
data <- peakPick(data, method="simple", SNR=6)

# apply peak picking
data_peaks <- process(data, plot=interactive())
}
\keyword{methods}
