\name{writeMSIData}

\alias{writeMSIData}

\alias{writeImzML}
\alias{writeImzML,MSImagingExperiment_OR_Arrays-method}

\alias{writeAnalyze}
\alias{writeAnalyze,MSImagingExperiment-method}
\alias{writeAnalyze,SpectralImagingExperiment-method}

\title{Write mass spectrometry imaging data files}

\description{
	Write supported mass spectrometry imaging data files, including imzML and Analyze 7.5.
}

\usage{
writeMSIData(object, file, \dots)

\S4method{writeImzML}{MSImagingExperiment_OR_Arrays}(object, file, bundle = TRUE,
	verbose = getCardinalVerbose(), chunkopts = list(),
	BPPARAM = getCardinalBPPARAM(), \dots)

\S4method{writeAnalyze}{MSImagingExperiment}(object, file,
	verbose = getCardinalVerbose(), chunkopts = list(),
	BPPARAM = getCardinalBPPARAM(), \dots)

\S4method{writeAnalyze}{SpectralImagingExperiment}(object, file,
	verbose = getCardinalVerbose(), chunkopts = list(),
	BPPARAM = getCardinalBPPARAM(), \dots)
}

\arguments{
	\item{object}{A spectral imaging dataset.}

	\item{file}{The absolute or relative file path. The file extension must be included for \code{writeMSIData}.}

	\item{bundle}{Should the ".imzML" and ".ibd" files be bundled into a new directory of the same name?}

	\item{verbose}{Should progress messages be printed?}

	\item{chunkopts}{Chunk processing options. See \code{\link{chunkApply}} for details.}

	\item{BPPARAM}{An optional instance of \code{BiocParallelParam}. See documentation for \code{\link{bplapply}}.}

	\item{\dots}{Additional arguments passed to \code{\link[CardinalIO]{writeImzML}} or \code{\link[CardinalIO]{writeAnalyze}}.}
}
\details{
	The \code{writeImzML} function supports writing both the "continuous" and "processed" formats.

	Exporting the experimental metadata to \code{cvParam} tags is lossy, and not all metadata will be preserved. If exporting an object that was originally imported from an imzML file, only metadata that appears in \code{experimentData()} will be preserved when writing.

	Datasets with multiple experimental runs will be merged into a single file. The object's \code{pixelData()} and \code{featureData()} will also be written to tab-delimted files if appropriate. These will be read back in by \code{readImzML()}.

	The imzML files can be modified after writing (such as to add additional experimental metadata) using the Java-based imzMLValidator application: \url{https://gitlab.com/imzML/imzMLValidator/}.
}

\value{
	\code{TRUE} if the file was written successfully, with the output file paths and data objects attached as attributes.
}

\author{
	Kylie A. Bemis
}

\references{
    Schramm T, Hester A, Klinkert I, Both J-P, Heeren RMA, Brunelle A, Laprevote O, Desbenoit N, Robbe M-F, Stoeckli M, Spengler B, Rompp A (2012) imzML - A common data format for the flexible exchange and processing of mass spectrometry imaging data. Journal of Proteomics 75 (16):5106-5110. doi:10.1016/j.jprot.2012.07.026
}

\seealso{
	\code{\link{readMSIData}}
}

\keyword{IO}
