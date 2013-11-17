\name{resampleSpectra-methods}
\docType{methods}
\alias{resampleSpectra}
\alias{resampleSpectra-methods}
\alias{resampleSpectra,MSImageSet,missing-method}
\alias{resampleSpectra,MSImageSet,MSPeakFrame-method}
\title{Resample mass spectra to peaks}
\description{
	Method for resampling mass spectra.
}
\usage{
    \S4method{resampleSpectra}{MSImageSet,missing}(object, pixel, coord, step=1, offset=0, filter=c("none", "gaussian", "sgolay", "kaiser", "ma"), plot=FALSE, \dots)

    \S4method{resampleSpectra}{MSImageSet,MSPeakFrame}(object, peaks, pixel, coord, plot=FALSE, \dots)
}
\arguments{
    \item{object}{\code{\linkS4class{MSImageSet}} data.}
    \item{peaks}{\code{MSPeakFrame} giving a list of peaks.}
    \item{pixel}{\code{numeric}, the pixel or vector of pixels giving the mass spectra to bin to peaks.}
    \item{coord}{a coordinate or list of coordinates giving the mass spectra to bin to peaks.}
    \item{step}{\code{numeric}, giving the mass step, in Da or appropriate mass units}
    \item{offset}{\code{numeric}, giving the offset for the resampling}
    \item{filter}{a predefined or user-defined filter to be applied before binning.}
    \item{plot}{\code{logical}, indicating whether the results of the processing on each mass spectrum should be plotted or not.}
    \item{\dots}{additional arguments passed to the interpolation function, currently \code{\link[signal]{interp1}}.}
}
\details{
    Running this method without specifying \code{pixel} or \code{coord} will process the whole dataset.

    Resampling to an \code{MSPeakFrame} will take the maximum height of the peak.
}

\value{
    An \code{\linkS4class{MSImageSet}} with the resulting resampled mass spectra.
}
\author{
	Kyle D. Bemis
}
\seealso{
    \code{\linkS4class{MSImageSet}},
    \code{\link[signal]{interp1}},
    \code{\link[Cardinal]{binSpectra-methods}}
}
\examples{
    ## write examples to put here
}
\keyword{methods}

