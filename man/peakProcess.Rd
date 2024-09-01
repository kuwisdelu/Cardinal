\name{peakProcess}

\alias{peakProcess}
\alias{peakProcess,MSImagingExperiment_OR_Arrays-method}

\title{Process peaks in mass spectra}

\description{
	Apply peak picking and alignment to a mass spectrometry imaging dataset.
}

\usage{
\S4method{peakProcess}{MSImagingExperiment_OR_Arrays}(object, ref,
    spectra = "intensity", index = "mz",
    method = c("diff", "sd", "mad", "quantile", "filter", "cwt"),
    SNR = 2, type = c("height", "area"),
    tolerance = NA, units = c("ppm", "mz"),
    sampleSize = NA, filterFreq = TRUE, outfile = NULL,
    verbose = getCardinalVerbose(), chunkopts = list(),
    BPPARAM = getCardinalBPPARAM(), \dots)
}

\arguments{
    \item{object}{A spectral imaging dataset.}
    
    \item{ref}{The locations of reference peaks to use for the alignment.}

    \item{spectra}{The name of the array in \code{spectraData()} to use for the peak intensities.}

    \item{index}{The name of the array in \code{spectraData()} (for \code{MSImagingArrays}) or column in \code{featureData()} (for \code{MSImagingExperiment}) to use for the peak locations.}

    \item{method}{The peak picking method to use. See \code{\link[matter]{findpeaks}} for details.}

    \item{SNR}{The signal-to-noise threshold to use to determine a peak.}

    \item{type}{The type of value to use to summarize the peak.}
    
    \item{tolerance}{The tolerance for matching a detected peak to the reference peaks or the shared m/z values. Passed to \code{\link{peakPick}} and \code{\link{peakAlign}}.}
    
    \item{units}{The units for the above tolerance.}

    \item{sampleSize}{The count or proportion giving a subset of spectra to use to determine reference peaks.}

    \item{filterFreq}{Either a logical value indicating whether singleton peaks should be removed, or a count or frequency used as a threshold to filter the peaks.}

    \item{outfile}{Optional. The name of a file to write the resulting dataset as imzML.}
    
    \item{verbose}{Should progress messages be printed?}

    \item{chunkopts}{Chunk processing options. See \code{\link{chunkApply}} for details.}

    \item{BPPARAM}{An optional instance of \code{BiocParallelParam}. See documentation for \code{\link{bplapply}}.}

    \item{\dots}{Options passed to \code{process()}.}
}

\details{
    This method provides a combined interface for \code{\link{peakPick}} and \code{\link{peakAlign}} for the most common approaches to peak processing.

    If \code{peakPick()} has been queued already, then it will be applied. Otherwise, it will be called internally with the provided arguments.

    There are two main paths depending on whether (1) peaks should be extracted based on a reference or (2) peak picking should be performed on the full dataset and then aligned.

    If either \code{ref} is provided or \code{sampleSize} is finite, then (1) is chosen and peaks are extracted based on the reference. If the reference is not provided, then peak picking and alignment performed on a subset of spectra (according to \code{sampleSize}) to create the reference peaks. The reference peaks are then used to extract peaks from the full dataset.

    Otherwise, (2) is chosen and peaks are picked and aligned across all spectra.

    The advantage of (1) is that all reference peaks will be summarized even they would not have a high enough signal-to-noise ratio to be detected in some spectra. The disadvantage is that rare peaks that do not appear in the sampled subset of spectra will not be included in the process peaks.

    The advantage of (2) is that rare peaks will be included because peak detection is performed on all spectra. The disadvantage is that some peaks may be missing from some spectra despite having nonzero intensities, because they did not have a high enough signal-to-noise ratio to be detected as peaks.

    Setting \code{sampleSize} to 1 will balance these advantages and disadvantages because the reference will be based on all spectra. However, this means the full dataset must be processed at least twice (possibly more if intermediate calculations are necessary), so it will be more time-consuming.
}

\value{
    A new object derived from \code{MSImagingExperiment} with the processed peaks.
}

\author{
	Kylie A. Bemis
}

\seealso{
    \code{\link{process}}
    \code{\link{peakPick}},
    \code{\link{peakAlign}}
}

\examples{
set.seed(1, kind="L'Ecuyer-CMRG")
mse <- simulateImage(preset=1, npeaks=10, dim=c(3,3))

# process peaks
mse2 <- peakProcess(mse, method="diff", SNR=3)
plot(mse2, i=4)
}

\keyword{ts}
