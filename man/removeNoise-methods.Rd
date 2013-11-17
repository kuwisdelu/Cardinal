\name{removeNoise-methods}
\docType{methods}
\alias{removeNoise}
\alias{removeNoise-methods}
\alias{removeNoise,MSImageSet-method}
\alias{gaussianFilter}
\alias{savitskyGolayFilter}
\alias{kaiserFilter}
\alias{movingAverageFilter}
\title{Remove the noise from a mass spectra}
\description{
	Method for removing the noise in a mass spectrum or set of mass spectra, by applying a filter to smooth a raw, noisy spectrum.
}
\usage{
    \S4method{removeNoise}{MSImageSet}(object, pixel, coord, method=c("gaussian", "sgolay", "kaiser", "ma"), plot=FALSE, \dots)
}
\arguments{
    \item{object}{\code{\linkS4class{MSImageSet}} data.}
    \item{pixel}{\code{numeric}, the pixel or vector of pixels giving the mass
    	spectra on which to estimate the baseline.}
    \item{coord}{a coordinate or list of coordinates giving the mass spectra
    	on which to estimate the baseline.}
    \item{method}{the name of a predefined or user-defined function for smoothing
        a single mass spectrum.}
    \item{plot}{\code{logical}, indicating whether the results of the processing
        on each mass spectrum should be plotted or not.}
    \item{\dots}{arguments to be passed to \code{method}.}
}
\details{
    Running this method without specifying \code{pixel} or \code{coord} will process the whole dataset.
    
    \code{method}: The name of a predefined or user-defined function to smooth a single mass spectrum, taking three arguments:
    \itemize{
        \item{\code{x}: \code{numeric}, a vector of intensities
        }
        \item{\code{t}: \code{numeric}, a vector of masses
        }
        \item{\code{\dots}: additional arguments
        }
    }
    A valid mass spectrum smoothing function returns a numeric vector of length equal to the length of \code{x}, giving the smoothed spectrum.
}
\value{
    An \code{\linkS4class{MSImageSet}} with noise removed.
}
\author{
	Kyle D. Bemis
}
\seealso{
    \code{\linkS4class{MSImageSet}},
    \code{\link[Cardinal]{gaussianFilter}},
    \code{\link[Cardinal]{savitskyGolayFilter}},
    \code{\link[Cardinal]{kaiserFilter}},
    \code{\link[Cardinal]{movingAverageFilter}}
}
\examples{
    ## write examples to put here
}
\keyword{methods}

