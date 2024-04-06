\name{spectrapply}

\alias{spectrapply}
\alias{spectrapply,MSImagingExperiment-method}
\alias{spectrapply,MSImagingArrays-method}
\alias{spectrapply,SpectralImagingExperiment-method}
\alias{spectrapply,SpectralImagingArrays-method}

\title{Apply functions over spectra}

\description{
	Apply a user-specified function over all spectra in a spectral imaging dataset.
}

\usage{
\S4method{spectrapply}{MSImagingExperiment}(object, FUN, \dots,
    spectra = "intensity", index = "mz")

\S4method{spectrapply}{MSImagingArrays}(object, FUN, \dots,
    spectra = "intensity", index = "mz")

\S4method{spectrapply}{SpectralImagingExperiment}(object, FUN, \dots,
        spectra = "intensity", index = NULL,
        simplify = TRUE, outpath = NULL,
        nchunks = getCardinalNChunks(),
        verbose = getCardinalVerbose(),
        BPPARAM = getCardinalBPPARAM())

\S4method{spectrapply}{SpectralImagingArrays}(object, FUN, \dots,
        spectra = "intensity", index = NULL,
        simplify = TRUE, outpath = NULL,
        nchunks = getCardinalNChunks(),
        verbose = getCardinalVerbose(),
        BPPARAM = getCardinalBPPARAM())
}

\arguments{
    \item{object}{A spectral imaging dataset.}

    \item{FUN}{A function to be applied. The function \emph{must} accept (1) a first argument giving the spectral intensities as a numeric vector and (2) a second argument giving the intensity locations (e.g., m/z values) as a numeric vector.}

    \item{\dots}{Options passed to \code{\link[matter]{chunkMapply}} or \code{\link[matter]{chunkApply}}.}

    \item{spectra}{The name of the array in \code{spectraData()} to use for the spectral intensities.}

    \item{index}{The name of the array in \code{spectraData()} (for \code{SpectralImagingArrays}) or column in \code{featureData()} (for \code{SpectralImagingExperiment}) to use for the spectral locations.}

    \item{simplify}{Should the result be simplified to an array if possible?}

    \item{outpath}{Optional. The name of a file to write the resulting data.}
    
    \item{nchunks}{The number of chunks to use for processing.}

    \item{verbose}{Should progress messages be printed?}

    \item{BPPARAM}{An optional instance of \code{BiocParallelParam}. See documentation for \code{\link{bplapply}}.}
}

\value{
    A list if \code{simplify=FALSE}. Otherwise, a vector or matrix, or a higher-dimensional array if the attempted simplification is successful.
}

\author{
	Kylie A. Bemis
}

\seealso{
    \code{\link{summarizeFeatures}},
    \code{\link{summarizePixels}}
}

\examples{
set.seed(1)
mse <- simulateImage(preset=1, npeaks=10, dim=c(10,10))

# find m/z locations of peaks in each spectrum
peaks <- spectrapply(data, function(x, mz) mz[matter::findpeaks(x)])
}

\keyword{manip}

