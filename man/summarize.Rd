\name{summarizeFeatures}

\alias{summarizeFeatures}
\alias{summarizePixels}
\alias{rowStats,SpectralImagingExperiment-method}
\alias{colStats,SpectralImagingExperiment-method}
\alias{rowSums,SpectralImagingExperiment-method}
\alias{colSums,SpectralImagingExperiment-method}
\alias{rowMeans,SpectralImagingExperiment-method}
\alias{colMeans,SpectralImagingExperiment-method}

\title{Summarize a spectral imaging dataset}

\description{
	Summarizes over the rows or columns of the dataset.
}

\usage{
summarizeFeatures(x, stat = "mean", groups = NULL,
    verbose = getCardinalVerbose(), chunkopts = list(),
    BPPARAM = getCardinalBPPARAM(), \dots)

summarizePixels(x, stat = c(tic="sum"), groups = NULL,
    verbose = getCardinalVerbose(), chunkopts = list(),
    BPPARAM = getCardinalBPPARAM(), \dots)

\S4method{rowStats}{SpectralImagingExperiment}(x, stat, \dots,
    verbose = getCardinalVerbose(), chunkopts = list(),
    BPPARAM = getCardinalBPPARAM())

\S4method{colStats}{SpectralImagingExperiment}(x, stat, \dots,
    verbose = getCardinalVerbose(), chunkopts = list(),
    BPPARAM = getCardinalBPPARAM())

\S4method{rowSums}{SpectralImagingExperiment}(x, na.rm = FALSE, dims = 1, \dots)

\S4method{colSums}{SpectralImagingExperiment}(x, na.rm = FALSE, dims = 1, \dots)

\S4method{rowMeans}{SpectralImagingExperiment}(x, na.rm = FALSE, dims = 1, \dots)

\S4method{colMeans}{SpectralImagingExperiment}(x, na.rm = FALSE, dims = 1, \dots)
}

\arguments{
    \item{x}{A spectral imaging dataset.}

    \item{stat}{The name of summary statistics to compute over the rows or columns of a matrix. Allowable values include: "min", "max", "prod", "sum", "mean", "var", "sd", "any", "all", and "nnzero".}

    \item{groups}{A vector coercible to a factor giving groups to summarize.}

    \item{na.rm}{If \code{TRUE}, remove \code{NA} values before summarizing.}

    \item{dims}{Ignored.}

    \item{verbose}{Should progress messages be printed?}

    \item{chunkopts}{Chunk processing options. See \code{\link{chunkApply}} for details.}

    \item{BPPARAM}{An optional instance of \code{BiocParallelParam}. See documentation for \code{\link{bplapply}}.}

    \item{\dots}{Additional arguments passed to \code{\link[matter]{rowStats}} or \code{\link[matter]{colStats}}, such as the number of chunks.}
}

\value{
    For \code{summarizeFeatures} and \code{summarizePixels}, an object of the same class as \code{x} with the statistical summaries added as columns in the \code{featureData()} or \code{pixelData()}, respectively.

    For \code{rowStats}, \code{colStats}, etc., a vector, matrix, or array with the summary statistics.
}

\author{
	Kylie A. Bemis
}

\examples{
set.seed(1, kind="L'Ecuyer-CMRG")
mse <- simulateImage(preset=1, npeaks=10, dim=c(10,10))

# summarize mean spectrum
mse <- summarizeFeatures(mse, stat="mean")
plot(mse, "mean")

# summarize total ion current
mse <- summarizePixels(mse, stat=c(TIC="sum"))
image(mse, "TIC")
}

\keyword{univar}
