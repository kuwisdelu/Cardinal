\name{peakPick}

\alias{peakPick}
\alias{peakPick,MSImagingExperiment-method}
\alias{peakPick,MSImagingArrays-method}
\alias{peakPick,SpectralImagingData-method}

\title{Peak pick spectra}

\description{
    Apply deferred peak picking to spectra.
}

\usage{
\S4method{peakPick}{MSImagingExperiment}(object, ref,
    method = c("diff", "sd", "mad", "quantile", "filter", "cwt"),
    SNR = 2, type = c("height", "area"),
    tolerance = NA, units = c("ppm", "mz"), \dots)

\S4method{peakPick}{MSImagingArrays}(object, ref,
    method = c("diff", "sd", "mad", "quantile", "filter", "cwt"),
    SNR = 2, type = c("height", "area"),
    tolerance = NA, units = c("ppm", "mz"), \dots)

\S4method{peakPick}{SpectralImagingData}(object, ref,
    method = c("diff", "sd", "mad", "quantile", "filter", "cwt"),
    SNR = 2, type = c("height", "area"),
    tolerance = NA, units = c("relative", "absolute"), \dots)

estimateReferencePeaks(object, SNR = 2,
    method = c("diff", "sd", "mad", "quantile", "filter", "cwt"),
    nchunks = getCardinalNChunks(),
    verbose = getCardinalVerbose(),
    BPPARAM = getCardinalBPPARAM(), \dots)
}

\arguments{
    \item{object}{A spectral imaging dataset.}

    \item{ref}{Optional vector giving locations of reference peaks to extract from the dataset.}

    \item{method}{The peak picking method to use. See \code{\link[matter]{findpeaks}} for details.}

    \item{SNR}{The signal-to-noise threshold to use to determine a peak.}

    \item{type}{The type of value to use to summarize the peak.}

    \item{tolerance}{If \code{ref} is specified, the tolerance to use when deciding if a local peak in a spectrum matches a reference peak. If \code{NA}, then the tolerance is automatically determined as half the minimum distance between peaks in the reference.}

    \item{units}{The units for the above tolerance.}

    \item{nchunks}{The number of chunks to use for processing.}

    \item{verbose}{Should progress messages be printed?}

    \item{BPPARAM}{An optional instance of \code{BiocParallelParam}. See documentation for \code{\link{bplapply}}.}

    \item{\dots}{Additional arguments passed to the peak picking function.}
}

\details{
    Unless otherwise specified, peaks are detected as local maxima which are then compared to the estimated noise level to determine a signal-to-noise ratio for each peak. Most of the peak detection methods below are differentiated by how they estimate the noise in the specturm.

    The supported peak picking methods are:

    \itemize{

        \item{"diff": Estimate noise based on the derivative of the signal using \code{\link{estnoise_diff}}.}

        \item{"sd": Estimate noise from standard deviation using \code{\link{estnoise_sd}}.}

        \item{"mad": Estimate noise from mean absolute deviation using \code{\link{estnoise_mad}}.}

        \item{"quantile": Estimate noise from a rolling quantile of the difference between the raw signal and a smoothed signal using \code{\link{estnoise_quant}}.}

        \item{"filter": Estimate noise using dynamic filtering of the local peaks using \code{\link{estnoise_filt}}.}

        \item{"cwt": Detect peaks based on continuous wavelet transform (CWT) using \code{\link{findpeaks_cwt}}.}
    }

    If \code{ref} is provided, then the signal-to-noise ratio is not determined, and any detected peaks are returned as long as they match to a reference peak.

    The \code{estimateReferencePeaks} function calculates the mean spectrum (or looks for a "mean" column in \code{featureData()}) and performs peak picking on the mean spectrum. It can be used to create a set of reference peaks if all relevant peaks appear in the mean spectrum.
}

\note{
    The peak picking is deferred until \code{process()} is called.
}

\value{
    An object of the same class with the processing step queued.
}

\author{
    Kylie A. Bemis
}

\seealso{
    \code{\link{process}},
    \code{\link{peakAlign}},
    \code{\link{peakProcess}}
}

\examples{
set.seed(1)
mse <- simulateImage(preset=1, npeaks=10, dim=c(3,3))

# queue peak picking
mse2 <- peakPick(mse, method="diff", SNR=6)
plot(mse2, i=4)

# apply peak picking
mse2 <- process(mse2)
}

\keyword{ts}
