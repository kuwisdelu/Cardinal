\name{detectPeaks-methods}
\docType{methods}
\alias{detectPeaks}
\alias{detectPeaks-methods}
\alias{detectPeaks,MSImageSet-method}
\alias{detectPeaksSNR}
\alias{detectPeaksSNR}
\alias{detectPeaksLIMPIC}
\title{Detect peaks in mass spectra}
\description{
	Method for detecting peaks in a mass spectrum or set of mass spectra.
}
\usage{
    \S4method{detectPeaks}{MSImageSet}(object, pixel, coord, method=c("snr", "limpic"), plot=FALSE, \dots)
}
\arguments{
    \item{object}{\code{\linkS4class{MSImageSet}} data.}
    \item{pixel}{\code{numeric}, the pixel or vector of pixels giving the mass
    	spectra on which to detect peaks.}
    \item{coord}{a coordinate or list of coordinates giving the mass spectra
    	on which to detect peaks.}
    \item{method}{the name of a predefined or user-defined function for assessing
    	the quality of a single mass spectrum.}
    \item{plot}{\code{logical}, indicating whether the results of the processing
        on each mass spectrum should be plotted or not.}
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
    A valid peak detection function returns a logical vector of length equal to the length of \code{x}, with TRUE for each index that corresponds to a peak, and FALSE otherwise.
}
\value{
    An object or list of objects of class \code{MSPeakFrame}, giving the selected peaks for each given pixel.
}
\author{
	Kyle D. Bemis
}
\seealso{
    \code{\linkS4class{MSImageSet}},
    \code{\link[Cardinal]{selectPeaks-methods}},
    \code{\link[Cardinal]{detectPeaksSNR}},
    \code{\link[Cardinal]{detectPeaksLIMPIC}},
    \code{\link[Cardinal]{alignPeaks-methods}},
    \code{\link[Cardinal]{mergePeaks-methods}},
    \code{\link[Cardinal]{poolPeaks-methods}},
    \code{\link[Cardinal]{estimateNoise-methods}},
}
\examples{
    ## write examples to put here
}
\keyword{methods}

