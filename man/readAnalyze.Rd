\name{readAnalyze}
\alias{readAnalyze}
\title{Read Analyze 7.5 Data}
\description{
	Read data files stored in the Analyze 7.5 format.
}
\usage{
	readAnalyze(name, folder=getwd())
}
\arguments{
	\item{name}{The common file name for the '.hdr', '.t2m', and '.img' files.}
	\item{folder}{The file path to the folder containing the data files.}
}
\value{
	A \code{\linkS4class{MSImageSet}} object.
}
\author{
	Kyle D. Bemis
}
\seealso{
	\code{\link[Cardinal]{readMSIData}},
	\code{\link[Cardinal]{readImzML}},
	\code{\linkS4class{MSImageSet}}
}
