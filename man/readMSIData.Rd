\name{readMSIData}
\alias{readMSIData}
\alias{readImzML}
\alias{readAnalyze}

\title{Read Mass Spectrometry Imaging Data Files}
\description{
	Read supported mass spectrometry imaging data files. Supported formats include imzML and Analyze 7.5.
}
\usage{
## Read any supported MS imaging file
readMSIData(file)

## Read imzML files
readImzML(name, folder=getwd())

## Read Analyze 7.5 files
readAnalyze(name, folder=getwd())
}
\arguments{
	\item{file}{A description of the data file to be read. This may be either an absolute or relative path. The file extension must be included.}
	\item{name}{The common file name for the '.imzML' and '.ibd' files for imzML or for the '.hdr', '.t2m', and '.img' files for Analyze 7.5.}
	\item{folder}{The path to the folder containing the data files.}
}
\details{
	In the current implementation, the file extensions must match exactly: '.imzML' and '.ibd' for imzML and '.hdr', '.t2m', and '.img' for Analyze 7.5.

	The \code{readImzML} function currently only supports reading and returning the 'continuous' format. Support for 'processed' format is in development.
}
\value{
	A \code{\linkS4class{MSImageSet}} object.
}
\author{
	Kyle D. Bemis
}
\references{
    Schramm T, Hester A, Klinkert I, Both J-P, Heeren RMA, Brunelle A, Laprevote O, Desbenoit N, Robbe M-F, Stoeckli M, Spengler B, Rompp A (2012) imzML - A common data format for the flexible exchange and processing of mass spectrometry imaging data. Journal of Proteomics 75 (16):5106-5110. doi:10.1016/j.jprot.2012.07.026
}
\seealso{
	\code{\linkS4class{MSImageSet}}
}
\keyword{IO}
