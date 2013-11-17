\name{removeBaseline-methods}
\docType{methods}
\alias{removeBaseline}
\alias{removeBaseline-methods}
\alias{removeBaseline,MSImageSet-method}
\title{Remove the baseline from mass spectra}
\description{
	Method for removing the baseline from a mass spectrum or set of mass spectra.
}
\usage{
    \S4method{removeBaseline}{MSImageSet}(object, pixel, coord, method="interp", non.negative=TRUE, plot=FALSE, \dots)
}
\arguments{
    \item{object}{\code{\linkS4class{MSImageSet}} data.}
    \item{pixel}{\code{numeric}, the pixel or vector of pixels giving the mass spectra on which to estimate the baseline.}
    \item{coord}{a coordinate or list of coordinates giving the mass spectra on which to estimate the baseline.}
    \item{method}{the name of a predefined or user-defined function for estimating the baseline of a single mass spectrum.}
    \item{non.negative}{\code{logical}, indicating whether the resulting spectra should be restrained to non-negative intensities.}
    \item{plot}{\code{logical}, indicating whether the results of the processing on each mass spectrum should be plotted or not.}
    \item{\dots}{arguments to be passed to \code{method}.}
}
\details{
    Running this method without specifying \code{pixel} or \code{coord} will process the whole dataset.
    
    \code{method}: The name of a predefined or user-defined function to detect peaks on a single mass spectrum, taking three arguments:
    \itemize{
        \item{\code{x}: \code{numeric}, a vector of intensities
        }
        \item{\code{t}: \code{numeric}, a vector of masses
        }
        \item{\code{\dots}: additional arguments
        }
    }
    A valid baseline estimation function returns a numeric vector of length equal to the length of \code{x}, giving the approximated baseline.
}
\value{
    An \code{\linkS4class{MSImageSet}} with baseline removed.
}
\author{
	Kyle D. Bemis
}
\seealso{
    \code{\linkS4class{MSImageSet}},
    \code{\link[Cardinal]{estimateBaselineInterp1}}
    \code{\link[Cardinal]{estimateBaseline-methods}}
}
\examples{
    # write examples to put here
}
\keyword{methods}

