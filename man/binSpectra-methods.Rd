\name{binSpectra-methods}
\docType{methods}
\alias{binSpectra}
\alias{binSpectra-methods}
\alias{binSpectra,MSImageSet,missing-method}
\alias{binSpectra,MSImageSet,MSPeakFrame-method}
\title{Bin mass spectra}
\description{
	Method for binning mass spectra.
}
\usage{
    \S4method{binSpectra}{MSImageSet,missing}(object, pixel, coord, width=1, offset=0, filter=c("none", "gaussian", "sgolay", "kaiser", "ma"), plot=FALSE, \dots)

    \S4method{binSpectra}{MSImageSet,MSPeakFrame}(object, peaks, pixel, coord, sum=TRUE, plot=FALSE, \dots)
}
\arguments{
    \item{object}{\code{\linkS4class{MSImageSet}} data.}
    \item{peaks}{\code{MSPeakFrame} giving a list of peaks.}
    \item{pixel}{\code{numeric}, the pixel or vector of pixels giving the mass
    	spectra on which to assess the quality.}
    \item{coord}{a coordinate or list of coordinates giving the mass spectra
    	on which to assess the quality.}
    \item{width}{\code{numeric}, giving the bin width, in Da or appropriate mass units}
    \item{offset}{\code{numeric}, giving the offset for the bin}
    \item{filter}{a predefined or user-defined filter to be applied before binning.}
    \item{sum}{\code{logical}, indicating to take the area under the curve as the value.}
    \item{plot}{\code{logical}, indicating whether the results of the processing
    	on each mass spectrum should be plotted or not.}
    \item{\dots}{ignored.}
}
\details{
    Running this method without specifying \code{pixel} or \code{coord} will process the whole dataset.

    Binning to an \code{MSPeakFrame} will take the area under the curve of the peak.
}
\value{
    An \code{\linkS4class{MSImageSet}} with the resulting binned spectra.
}
\author{
	Kyle D. Bemis
}
\seealso{
    \code{\linkS4class{MSImageSet}},
    \code{\link[Cardinal]{resampleSpectra-methods}},
    \code{\link[Cardinal]{removeNoise-methods}}
}
\examples{
    ## write examples to put here
}
\keyword{methods}

