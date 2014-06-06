\name{readMSIData}
\alias{readMSIData}
\title{Read MSIData Data}
\description{
	Read any supporting mass spectrometry imaging data files.
}
\usage{
	readMSIData(file)
}
\arguments{
	\item{file}{A description of the data file to be read. This may be either an absolute or relative path. The file extension must be included.}
}
\value{
	A \code{\linkS4class{MSImageSet}} object.
}
\author{
	Kyle D. Bemis
}
\seealso{
	\code{\link[Cardinal]{readAnalyze}},
	\code{\link[Cardinal]{readImzML}},
	\code{\linkS4class{MSImageSet}}
}
