\name{poolPeaks-methods}
\docType{methods}
\alias{poolPeaks}
\alias{poolPeaks-methods}
\alias{poolPeaks,MSPeakFrame-method}
\alias{poolPeaks,MSPeakList-method}
\title{Pools mass spectrometry peak lists}
\description{
	Pools together multiple peak lists and lists of peak lists.
}
\usage{
	\S4method{poolPeaks}{MSPeakFrame}(peaks, \dots)
	
	\S4method{poolPeaks}{MSPeakList}(peaks, \dots)
}
\arguments{
	\item{peaks}{the first peaks object to pool.}
	\item{\dots}{additional peaks objects.}
}
\details{
	For a \code{MSPeakFrame}, pools together multiple objects of \code{MSPeakFrame} into a single \code{MSPeakFrame}.

	For a \code{MSPeakList}, pools together multiple objects of \code{MSPeakList} into a single \code{MSPeakList}.
}
\value{
	An \code{MSPeakFrame} or \code{MSPeakList} with pooled peaks.
}
\author{
	Kyle D. Bemis
}
\seealso{
    \code{\linkS4class{MSImageSet}},
    \code{\link[Cardinal]{alignPeaks-methods}},
    \code{\link[Cardinal]{detectPeaks-methods}},
    \code{\link[Cardinal]{mergePeaks-methods}}
}
\examples{
    ## write examples to put here
}
\keyword{methods}

