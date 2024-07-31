\name{readMSIData}

\alias{readMSIData}
\alias{readImzML}
\alias{readAnalyze}

\alias{convertMSImagingExperiment2Arrays}
\alias{convertMSImagingArrays2Experiment}

\alias{coerce,MSImagingExperiment,MSImagingArrays-method}
\alias{coerce,MSImagingArrays,MSImagingExperiment-method}

\title{Read mass spectrometry imaging data files}

\description{
	Read supported mass spectrometry imaging data files, including imzML and Analyze 7.5.
}

\usage{
## Read any supported MS imaging file
readMSIData(file, \dots)

## Read imzML file
readImzML(file, memory = FALSE, check = FALSE,
	mass.range = NULL, resolution = NA, units = c("ppm", "mz"),
	guess.max = 1000L, as = "auto", parse.only=FALSE,
	verbose = getCardinalVerbose(), chunkopts = list(),
	BPPARAM = getCardinalBPPARAM(), \dots)

## Read Analyze 7.5 file
readAnalyze(file, memory = FALSE, as = "auto",
	verbose = getCardinalVerbose(), \dots)

## Convert from MSImagingExperiment to MSImagingArrays
convertMSImagingExperiment2Arrays(object)

## Convert from MSImagingArrays to MSImagingExperiment
convertMSImagingArrays2Experiment(object, mz = NULL,
	mass.range = NULL, resolution = NA, units = c("ppm", "mz"),
	guess.max = 1000L, tolerance = 0.5 * resolution,
	verbose = getCardinalVerbose(), chunkopts = list(),
	BPPARAM = getCardinalBPPARAM(), \dots)
}
\arguments{
	\item{file}{The absolute or relative file path. The file extension must be included for \code{readMSIData}.}

	\item{memory}{Should the spectra be loaded into memory? If FALSE, the spectra are attached as an out-of-memory matrix.}

	\item{check}{Should the UUID and checksum of the binary data file be checked against the corresponding imzML tags?}

	\item{mass.range}{The mass range to use when converting the data to an \code{MSImagingExperiment}.}

	\item{resolution}{The mass resolution to use when converting the data to an \code{MSImagingExperiment}. This is the \emph{inverse} of the instrument resolution, if known. It is the width of the m/z bins when converting the data to an \code{MSImagingExperiment}.}

	\item{tolerance}{If the spectra have been centroided but the peaks are unaligned, then this is passed to \code{\link{peakAlign}}.}

	\item{units}{The units for the above resolution.}

	\item{guess.max}{The number of spectra to use when guessing the mass range and resolution, if they are not provided.}

	\item{as}{After reading in the data, what class of object should be returned? The data is initially loaded as an \code{MSImagingArrays} object. It may be converted to an \code{MSImagingExperiment} object. Setting to "auto" means to determine whichever is more appropriate depending on whether the spectra appear to have been processed and centroided.}

	\item{parse.only}{If TRUE, return only the parsed imzML metadata without creating a new \code{MSImagingArrays} or \code{MSImagingExperiment} object.}

	\item{verbose}{Should progress messages be printed?}

	\item{chunkopts}{Chunk processing options. See \code{\link{chunkApply}} for details.}

	\item{BPPARAM}{An optional instance of \code{BiocParallelParam}. See documentation for \code{\link{bplapply}}.}

	\item{\dots}{Additional arguments passed to \code{\link[CardinalIO]{parseImzML}} or \code{\link[CardinalIO]{parseAnalyze}}.}

	\item{object}{A mass spectrometry imaging dataset to convert from one class to another.}

	\item{mz}{A vector of shared m/z values for converting to \code{MSImagingExperiment}, if not to be determined automatically.}
}

\details{
	The spectra are initially loaded into a \code{MSImagingArrays} object before conversion to \code{MSImagingExperiment} (if applicable).

	This conversion can be sped up by specifying the \code{mass.range} and \code{resolution} so they do not have to be determined from the spectra directly. Using a larger value of \code{guess.max} can improve the accuracy of the m/z binning for downstream analysis at the expense of a longer conversion time.

	If greater control is desired, spectra should be imported as \code{MSImagingArrays}, and processing to \code{MSImagingExperiment} can be performed manually.

	If problems are encountered while trying to import imzML files, the files should be verified and fixed with imzMLValidator.

	A Java version of imzML validator can be found at: \url{https://gitlab.com/imzML/imzMLValidator}.

	A web-based version of imzML validator can be found at: \url{https://imzml.github.io}.
}

\value{
	A \code{\linkS4class{MSImagingExperiment}} or \code{\linkS4class{MSImagingArrays}} object.
}

\author{
	Kylie A. Bemis
}

\references{
	Schramm T, Hester A, Klinkert I, Both J-P, Heeren RMA, Brunelle A, Laprevote O, Desbenoit N, Robbe M-F, Stoeckli M, Spengler B, Rompp A (2012) imzML - A common data format for the flexible exchange and processing of mass spectrometry imaging data. Journal of Proteomics 75 (16):5106-5110. doi:10.1016/j.jprot.2012.07.026
}

\seealso{
	\code{\link[CardinalIO]{parseImzML}},
	\code{\link[CardinalIO]{parseAnalyze}}
	\code{\link{writeMSIData}}
}

\keyword{IO}
