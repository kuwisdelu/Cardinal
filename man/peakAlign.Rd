\name{peakAlign}

\alias{peakAlign}
\alias{peakAlign,MSImagingExperiment-method}
\alias{peakAlign,MSImagingArrays-method}
\alias{peakAlign,SpectralImagingExperiment-method}
\alias{peakAlign,SpectralImagingArrays-method}

\title{Align peaks across spectra}

\description{
	Align peaks across spectra in a spectral imaging dataset.
}

\usage{
\S4method{peakAlign}{MSImagingExperiment}(object, ref,
    spectra = "intensity", index = "mz",
    tolerance = NA, units = c("ppm", "mz"), \dots)

\S4method{peakAlign}{MSImagingArrays}(object, ref,
    spectra = "intensity", index = "mz",
    tolerance = NA, units = c("ppm", "mz"), \dots)

\S4method{peakAlign}{SpectralImagingExperiment}(object, ref,
    spectra = "intensity", index = NULL,
    tolerance = NA, units = c("relative", "absolute"),
    verbose = getCardinalVerbose(), chunkopts = list(),
    BPPARAM = getCardinalBPPARAM(), \dots)

\S4method{peakAlign}{SpectralImagingArrays}(object, ref,
    spectra = "intensity", index = NULL,
    tolerance = NA, units = c("relative", "absolute"),
    verbose = getCardinalVerbose(), chunkopts = list(),
    BPPARAM = getCardinalBPPARAM(), \dots)
}

\arguments{
    \item{object}{A spectral imaging dataset.}
    
    \item{ref}{The locations of reference peaks to use for the alignment.}

    \item{spectra}{The name of the array in \code{spectraData()} to use for the peak intensities.}

    \item{index}{The name of the array in \code{spectraData()} (for \code{SpectralImagingArrays}) or column in \code{featureData()} (for \code{SpectralImagingExperiment}) to use for the peak locations.}
    
    \item{tolerance}{The tolerance for matching a detected peak to the reference. If \code{NA}, then the tolerance is automatically determined as half the minimum distance between locations in the estimated spectral domain (see "Details").}
    
    \item{units}{The units for the above tolerance.}
    
    \item{verbose}{Should progress messages be printed?}

    \item{chunkopts}{Chunk processing options. See \code{\link{chunkApply}} for details.}

    \item{BPPARAM}{An optional instance of \code{BiocParallelParam}. See documentation for \code{\link{bplapply}}.}

    \item{\dots}{Options passed to \code{process()}.}
}

\details{
    Before peak alignment, \code{process()} is called to apply any queued pre-processing steps. It is assumed that \code{peakPick()} has either been queued or already applied to the data.

    If \code{ref} is provided, then the aligned peaks are returned immediately without additional processing. (Peaks are binned on-the-fly to the reference peak locations.)

    If \code{ref} is not provided, then the shared peaks must be determined automatically. This starts with creation of a shared domain giving a list of possible peak locations.

    For \code{SpectralImagingArrays}, \code{estimateDomain()} is used to create the shared domain from the \code{index} array. For \code{SpectralImagingExperiment}, the \code{index} column of \code{featureData()} is used directly.

    Next, \code{\link[matter]{binpeaks}} is used to bin the observed peaks to the shared domain. Then, \code{\link[matter]{mergepeaks}} is used to merge peaks that are separated by a distance less than the given \code{tolerance}.

    The averaged locations of the merged peaks in each bin are used as the shared peaks for the full dataset, and the aligned peaks are returned. (Peaks are binned on-the-fly to the shared peak locations.)
}

\value{
    A new object derived from \code{SpectralImagingExperiment} with the aligned peaks.
}

\author{
	Kylie A. Bemis
}

\seealso{
    \code{\link{process}}
    \code{\link{peakPick}},
    \code{\link{peakProcess}}
}

\examples{
set.seed(1)
mse <- simulateImage(preset=1, npeaks=10, dim=c(3,3))

# queue peak picking
mse2 <- peakPick(mse, method="diff", SNR=6)

# align peaks
mse2 <- peakAlign(mse2)
plot(mse2, i=4)
}

\keyword{ts}
