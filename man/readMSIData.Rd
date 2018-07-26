\name{readMSIData}
\alias{readMSIData}
\alias{readImzML}
\alias{readAnalyze}

\title{Read mass spectrometry imaging data files}
\description{
	Read supported mass spectrometry imaging data files. Supported formats include imzML and Analyze 7.5.
}
\usage{
## Read any supported MS imaging file
readMSIData(file, \dots)

## Read imzML files
readImzML(name, folder=getwd(), attach.only=FALSE,
	mass.range=NULL, mass.accuracy=200, units.accuracy=c("ppm", "mz"),
	as=c("MSImageSet", "MSImagingExperiment"), \dots)

## Read Analyze 7.5 files
readAnalyze(name, folder=getwd(), attach.only=FALSE,
	as=c("MSImageSet", "MSImagingExperiment"), \dots)
}
\arguments{
	\item{file}{A description of the data file to be read. This may be either an absolute or relative path. The file extension must be included.}
	\item{name}{The common file name for the '.imzML' and '.ibd' files for imzML or for the '.hdr', '.t2m', and '.img' files for Analyze 7.5.}
	\item{folder}{The path to the folder containing the data files.}
	\item{attach.only}{Attach the file as a \code{\linkS4class{Binmat}} on-disk matrix for reading on-demand, rather than loading the data into memory.}
	\item{mass.range}{For 'processed' imzML files, the mass range to use for the imported data. If known, providing this can improve the loading time, as otherwise it is read and calculated from the dataset directly.}
	\item{mass.accuracy}{For 'processed' imzML files, the accuracy to which the m/z values will be binned after reading. This should be set to the native accuracy of the mass spectrometer, if known.}
	\item{units.accuracy}{The units for 'mass.accuracy'.}
	\item{as}{After reading in the data, what class of object should be returned (i.e., 'MSImageSet' or 'MSImagingExperiment')?}
	\item{\dots}{Additional arguments passed to read functions.}
}
\details{
	In the current implementation, the file extensions must match exactly: '.imzML' and '.ibd' for imzML and '.hdr', '.t2m', and '.img' for Analyze 7.5.

	The \code{readImzML} function supports reading and returning both the 'continuous' and 'processed' formats.
}
\value{
	A \code{\linkS4class{MSImageSet}} object.
}
\author{
	Kylie A. Bemis
}
\references{
    Schramm T, Hester A, Klinkert I, Both J-P, Heeren RMA, Brunelle A, Laprevote O, Desbenoit N, Robbe M-F, Stoeckli M, Spengler B, Rompp A (2012) imzML - A common data format for the flexible exchange and processing of mass spectrometry imaging data. Journal of Proteomics 75 (16):5106-5110. doi:10.1016/j.jprot.2012.07.026
}
\seealso{
	\code{\link{writeMSIData}}
}
\keyword{IO}
