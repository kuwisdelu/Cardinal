\name{estimateDomain}

\alias{estimateDomain}
\alias{estimateReferenceMz}
\alias{estimateReferencePeaks}

\title{Estimate shared domain}

\description{
	For unaligned spectral data, it is often necessary to estimate a suitable shared domain in order to calculate statistical summaries like the mean spectrum.
}

\usage{
estimateDomain(xlist,
    units = c("relative", "absolute"),
    verbose = getCardinalVerbose(), chunkopts = list(),
    BPPARAM = getCardinalBPPARAM(), \dots)

estimateReferenceMz(object,
    units = c("ppm", "mz"),
    verbose = getCardinalVerbose(), chunkopts = list(),
    BPPARAM = getCardinalBPPARAM(), \dots)

estimateReferencePeaks(object, SNR = 2,
    method = c("diff", "sd", "mad", "quantile", "filter", "cwt"),
    verbose = getCardinalVerbose(), chunkopts = list(),
    BPPARAM = getCardinalBPPARAM(), \dots)
}

\arguments{
    \item{xlist}{A list of the domain values (e.g., m/z values) for each spectrum.}

    \item{object}{A mass spectral imaging dataset.}

    \item{units}{Should the spacing between domain values use absolute or relative units?}

    \item{method}{The peak picking method to use. See \code{\link[matter]{findpeaks}} for details.}

    \item{SNR}{The signal-to-noise threshold to use to determine a peak.}

    \item{verbose}{Should progress messages be printed?}

    \item{chunkopts}{Chunk processing options. See \code{\link{chunkApply}} for details.}

    \item{BPPARAM}{An optional instance of \code{BiocParallelParam}. See documentation for \code{\link{bplapply}}.}

    \item{\dots}{Options passed to \code{\link{chunkLapply}}.}
}

\details{
    For \code{estimateDomain}, the domain is estimated by first finding the resolution of each spectrum's individual domain values (e.g., the spacing between m/z values), and then creating a sequence of domain values using the median resolution of all spectra.

    The \code{estimateReferenceMz} function simply calls \code{estimateDomain} on the appropriate components of a mass spectral imaging dataset to estimate profile m/z bins.

    The \code{estimateReferencePeaks} function calculates the mean spectrum (or looks for a "mean" column in \code{featureData()}) and performs peak picking on the mean spectrum. It can be used to create a set of reference peaks if all relevant peaks appear in the mean spectrum.
}

\value{
    A vector of domain values, m/z values, or peaks.
}

\author{
	Kylie A. Bemis
}

\seealso{
    \code{\link{summarizeFeatures}},
    \code{\link{recalibrate}},
    \code{\link{peakAlign}},
    \code{\link{peakProcess}}
}

\keyword{utilities}
\keyword{ts}
