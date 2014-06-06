\name{readImzML}
\alias{readImzML}
\title{Read imzML Data}
\description{
	Read data files stored in the imzML format.
}
\usage{
	readImzML(name, folder=getwd())
}
\arguments{
	\item{name}{The common file name for the '.imzML' and '.ibd' files.}
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
	\code{\link[Cardinal]{readAnalyze}},
	\code{\linkS4class{MSImageSet}}
}
