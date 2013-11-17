\name{mergePeaks-methods}
\docType{methods}
\alias{mergePeaks}
\alias{mergePeaks-methods}
\alias{mergePeaks,MSPeakFrame-method}
\alias{mergePeaks,MSPeakList-method}
\title{Merge the peaks in a peak list}
\description{
	Merge mass spectrometry peaks in an \code{MSPeakFrame} or an \code{MSPeakList} object.
}
\usage{
	\S4method{mergePeaks}{MSPeakFrame}(peaks, ...)
	
	\S4method{mergePeaks}{MSPeakList}(peaks, length.min=1, ...)
}
\arguments{
	\item{peaks}{the peaks to merge.}
	\item{length.min}{the minimum number of allowed peaks}
	\item{...}{ignored.}
}
\details{
	For an \code{MSPeakFrame}, merges all of the peaks in the \code{MSPeakFrame} into a single peak.

	For an \code{MSPeakList}, merges the peaks in each of the \code{MSPeakFrame} objects in the \code{MSPeakFrame} into a single peak, resulting in a list of merged peaks. Use \code{length.min} to filter peaks that do not occur frequently in the dataset.

	This should be used on \code{MSPeakAlignment} objects that are created from a call to \code{\link[Cardinal]{alignPeaks}} to create a useful \code{MSPeakFrame} that can be used with \code{\link[Cardinal]{resampleSpectra}} or \code{\link[Cardinal]{binSpectra}} to reduce the mass spectra to peaks.
}
\value{
	An \code{MSPeakFrame} with merged peaks.
}
\author{
	Kyle D. Bemis
}
\seealso{
    \code{\linkS4class{MSImageSet}},
    \code{\link[Cardinal]{alignPeaks-methods}},
    \code{\link[Cardinal]{detectPeaks-methods}},
    \code{\link[Cardinal]{poolPeaks-methods}}
}
\examples{
    ## write examples to put here
}
\keyword{methods}

