\name{writeMSIData}

\alias{writeMSIData}
\alias{writeImzML}
\alias{writeAnalyze}

\alias{writeMSIData,MSImageSet,character-method}
\alias{writeImzML,MSImageSet-method}
\alias{writeAnalyze,MSImageSet-method}

\title{Write Mass Spectrometry Imaging Data Files}
\description{
	Write supported mass spectrometry imaging data files. Supported formats include imzML and Analyze 7.5.
}
\usage{
\S4method{writeMSIData}{MSImageSet,character}(object, file, outformat=c("imzML", "Analyze"), \dots)

\S4method{writeImzML}{MSImageSet}(object, name, folder=getwd(), merge=FALSE,
	mz.type="32-bit float", intensity.type="32-bit float", \dots)

\S4method{writeAnalyze}{MSImageSet}(object, name, folder=getwd(),
	intensity.type="16-bit integer", \dots)
}
\arguments{
	\item{object}{An imaging dataset to be written to file.}
	\item{file}{A description of the data file to be write. This may be either an absolute or relative path. Any file extension will be ignored and replaced with an appropriate one.}
	\item{name}{The common file name for the '.imzML' and '.ibd' files for imzML or for the '.hdr', '.t2m', and '.img' files for Analyze 7.5.}
	\item{folder}{The path to the folder containing the data files.}
	\item{outformat}{The file format to write. Currently, the supported formats are "imzML" or "Analyze".}
	\item{merge}{Whether the samples/runs should be written to the same file (\code{TRUE}) or split into multiple files (\code{FALSE}). Currently, only \code{FALSE} is supported.}
	\item{mz.type}{The data type for the m/z values. Acceptable values are "32-bit float" and "64-bit float".}
	\item{intensity.type}{The data type for the intensity values. Acceptable values are "16-bit integer", "32-bit integer", "64-bit integer", "32-bit float" and "64-bit float".}
	\item{\dots}{Additional arguments passed to write functions.}
}
\details{
	The \code{writeImzML} function currently only supports writing the 'continuous' format. Exporting the metadata is lossy, and not all metadata will be preserved.
}
\value{
	\code{TRUE} if the file was written successfully.
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
