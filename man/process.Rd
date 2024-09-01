\name{process}

\alias{process}
\alias{process,MSImagingExperiment-method}
\alias{process,MSImagingArrays-method}
\alias{process,SpectralImagingExperiment-method}
\alias{process,SpectralImagingArrays-method}
\alias{addProcessing,SpectralImagingData-method}
\alias{reset}

\title{Apply queued processing to spectra}

\description{
	Queue pre-processing steps on an imaging dataset and apply them, possibly writing out the processed data to a file.
}

\usage{
\S4method{process}{MSImagingExperiment}(object, spectra = "intensity", index = "mz",
    domain = NULL, outfile = NULL, \dots)

\S4method{process}{MSImagingArrays}(object, spectra = "intensity", index = "mz",
    domain = NULL, outfile = NULL, \dots)

\S4method{process}{SpectralImagingExperiment}(object, spectra = "intensity", index = NULL,
    domain = NULL, outfile = NULL,
    verbose = getCardinalVerbose(), chunkopts = list(),
    BPPARAM = getCardinalBPPARAM(), \dots)

\S4method{process}{SpectralImagingArrays}(object, spectra = "intensity", index = NULL,
    domain = NULL, outfile = NULL,
    verbose = getCardinalVerbose(), chunkopts = list(),
    BPPARAM = getCardinalBPPARAM(), \dots)

\S4method{addProcessing}{SpectralImagingData}(object, FUN, label, verbose = getCardinalVerbose(), \dots)

reset(object, \dots)
}

\arguments{
    \item{object}{A spectral imaging dataset.}

    \item{spectra}{The name of the array in \code{spectraData()} to use for the peak intensities.}

    \item{index}{The name of the array in \code{spectraData()} (for \code{MSImagingArrays}) or column in \code{featureData()} (for \code{MSImagingExperiment}) to use for the peak locations.}

    \item{domain}{Optional. The name of the array in \code{spectraData()} (for \code{MSImagingArrays}) or column in \code{featureData()} (for \code{MSImagingExperiment}) to use for output domain (if known).}

    \item{outfile}{Optional. The name of a file to write the resulting dataset. Creates an imzML file for \code{MSImagingExperiment} or \code{MSImagingArrays}. The "continuous" format will be written if \code{domain} is specified; otherwise the "processed" format will be used in most cases.}
    
    \item{verbose}{Should progress messages be printed?}

    \item{chunkopts}{Chunk processing options. See \code{\link{chunkApply}} for details.}

    \item{BPPARAM}{An optional instance of \code{BiocParallelParam}. See documentation for \code{\link{bplapply}}.}

    \item{\dots}{Options passed to \code{\link[matter]{chunk_mapply}} or \code{\link[matter]{chunk_colapply}}.}

    \item{FUN}{A user-specified processing function.}

    \item{label}{The name of the processing step.}
}

\details{
    This method allows queueing of delayed processing to an imaging dataset. All of the queued processing steps will be applied in sequence whenever \code{process()} is called next. Use \code{reset()} to remove all queued processing steps.

    Typically, processing steps are queued using methods like \code{normalize}, \code{smooth}, \code{peakPick}, etc.

    However, a processing step can be queued manually with \code{addProcessing}.

    In this case, the user-specified function \emph{must} accept (1) a first argument giving the spectral intensities as a numeric vector and (2) a second argument giving the intensity locations (e.g., m/z values) as a numeric vector.

    The value returned by a user-specified function \code{must} return either (1) a numeric vector of the same length as the input intensities or (2) a 2-column matrix where the first column is the new locations (e.g., m/z values of peaks) and the second column is the new intensities.
}

\value{
    An object of the same class as the original object, with all processing steps applied.
}

\author{
	Kylie A. Bemis
}

\seealso{
    \code{\link{normalize}},
    \code{\link{smooth}},
    \code{\link{recalibrate}},
    \code{\link{reduceBaseline}},
    \code{\link{peakPick}}
}

\examples{
set.seed(1, kind="L'Ecuyer-CMRG")
mse <- simulateImage(preset=1, dim=c(3,3), baseline=1)

mse2 <- smooth(mse, width=11)
mse2 <- reduceBaseline(mse2)
plot(mse2, i=4)

mse2 <- process(mse2)
}

\keyword{manip}
\keyword{ts}
