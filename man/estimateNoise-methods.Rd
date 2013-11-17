\name{estimateNoise-methods}
\docType{methods}
\alias{estimateNoise}
\alias{estimateNoiseSimple}
\alias{estimateNoiseAdaptive}
\alias{estimateNoiseLIMPIC}
\alias{estimateNoise-methods}
\alias{estimateNoise,MSImageSet-method}
\title{Estimate the noise in a mass spectra}
\description{
	Method for estimating the characteristic noise profile in a mass spectrum or set of mass spectra.
}
\usage{
    \S4method{estimateNoise}{MSImageSet}(object, pixel, coord, method=c("sd", "mad", "adaptive-sd", "adaptive-mad", "limpic", "supersmoother"), snr=6, plot=FALSE, \dots)
}
\arguments{
    \item{object}{\code{\linkS4class{MSImageSet}} data.}
    \item{pixel}{\code{numeric}, the pixel or vector of pixels giving the mass
    	spectra on which to estimate the baseline.}
    \item{coord}{a coordinate or list of coordinates giving the mass spectra
    	on which to estimate the baseline.}
    \item{method}{the name of a predefined or user-defined function for smoothing
        a single mass spectrum.}
    \item{snr}{regions of the spectrum with signal-to-noise range higher than this value
        will be highlighted if plotting}
    \item{plot}{\code{logical}, indicating whether the results of the processing
        on each mass spectrum should be plotted or not.}
    \item{\dots}{arguments to be passed to \code{method}.}
}
\details{
    Running this method without specifying \code{pixel} or \code{coord} will process the whole dataset.
    
    \code{method}: The name of a predefined or user-defined function to estimate the noise in a single mass spectrum, taking three arguments:
    \itemize{
        \item{\code{x}: \code{numeric}, a vector of intensities
        }
        \item{\code{t}: \code{numeric}, a vector of masses
        }
        \item{\code{\dots}: additional arguments
        }
    }
    A valid noise estimation function returns a numeric vector of length equal to the length of \code{x}, giving the estimated noise level at each mass feature.
}
\value{
    A matrix where each column gives the estimated noise for a given pixel.
}
\author{
	Kyle D. Bemis
}
\seealso{
    \code{\linkS4class{MSImageSet}},
    \code{\link[Cardinal]{estimateNoiseSimple}},
    \code{\link[Cardinal]{estimateNoiseAdaptive}},
    \code{\link[Cardinal]{estimateNoiseLIMPIC}},
    \code{\link[Cardinal]{detectPeaks-methods}},
    \code{\link[Cardinal]{assessQuality-methods}}
}
\examples{
    ## write examples to put here
}
\keyword{methods}

