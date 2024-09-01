\name{spectrapply}

\alias{spectrapply}
\alias{spectrapply,SpectralImagingExperiment-method}
\alias{spectrapply,SpectralImagingArrays-method}

\title{Apply functions over spectra}

\description{
	Apply a user-specified function over all spectra in a spectral imaging dataset.
}

\usage{
\S4method{spectrapply}{SpectralImagingExperiment}(object, FUN, \dots,
        spectra = "intensity", index = NULL,
        simplify = TRUE, outpath = NULL,
        verbose = getCardinalVerbose(), chunkopts = list(),
        BPPARAM = getCardinalBPPARAM())

\S4method{spectrapply}{SpectralImagingArrays}(object, FUN, \dots,
        spectra = "intensity", index = NULL,
        simplify = TRUE, outpath = NULL,
        verbose = getCardinalVerbose(), chunkopts = list(),
        BPPARAM = getCardinalBPPARAM())
}

\arguments{
    \item{object}{A spectral imaging dataset.}

    \item{FUN}{A function to be applied. The first argument will be the \code{spectra} elements. Additional arguments are passed for each \code{index} component.}

    \item{\dots}{Options passed to \code{\link[matter]{chunkMapply}} or \code{\link[matter]{chunkApply}}.}

    \item{spectra}{The name of the array in \code{spectraData()} to use for the spectral intensities.}

    \item{index}{The name of the array in \code{spectraData()} (for \code{SpectralImagingArrays}) or column in \code{featureData()} (for \code{SpectralImagingExperiment}) to use for the spectral locations.}

    \item{simplify}{Should the result be simplified to an array if possible?}

    \item{outpath}{Optional. The name of a file to write the resulting data.}
    
    \item{verbose}{Should progress messages be printed?}

    \item{chunkopts}{Chunk processing options. See \code{\link{chunkApply}} for details.}

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
set.seed(1, kind="L'Ecuyer-CMRG")
mse <- simulateImage(preset=1, npeaks=10, dim=c(10,10))

# find m/z locations of peaks in each spectrum
peaks <- spectrapply(mse, index="mz",
    function(x, mz) mz[matter::findpeaks(x)])

head(peaks[[1L]])
head(peaks[[2L]])
}

\keyword{manip}

