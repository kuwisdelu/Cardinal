\name{assessQuality-methods}
\docType{methods}
\alias{assessQuality}
\alias{assessQuality-methods}
\alias{assessQuality,MSImageSet-method}
\alias{assessQualitySNR}
\title{Assess the quality of mass spectra}
\description{
	Method for assessing the quality of a mass spectrum or set of mass spectra
}
\usage{
    \S4method{assessQuality}{MSImageSet}(object, pixel, coord, method="snr", plot=FALSE, \dots)
}
\arguments{
    \item{object}{\code{\linkS4class{MSImageSet}} data.}
    \item{pixel}{\code{numeric}, the pixel or vector of pixels giving the mass spectra on which to assess the quality.}
    \item{coord}{a coordinate or list of coordinates giving the mass spectra on which to assess the quality.}
    \item{method}{the name of a predefined or user-defined function for assessing the quality of a single mass spectrum.}
    \item{plot}{\code{logical}, indicating whether the results of the processing on each mass spectrum should be plotted or not}
    \item{\dots}{arguments to be passed to \code{method}.}
}
\details{
    Running this method without specifying \code{pixel} or \code{coord} will process the whole dataset.
    
    \code{method}: The name of a predefined or user-defined function to assess the quality of a single mass spectrum, taking three arguments:
    \itemize{
        \item{\code{x}: \code{numeric}, a vector of intensities
        }
        \item{\code{t}: \code{numeric}, a vector of masses
        }
        \item{\code{\dots}: additional arguments
        }
    }
    A valid quality assessment function returns a single numeric value where higher values indicate a higher-quality and more informative mass spectrum.
}
\value{
    A numeric value where higher values indicate a higher-quality and more informative mass spectrum.
}
\author{
	Kyle D. Bemis
}
\seealso{
    \code{\linkS4class{MSImageSet}},
    \code{\link[Cardinal]{assessQualitySNR}},
    \code{\link[Cardinal]{estimateNoise-methods}}
}
\examples{
    ## write examples to put here
}
\keyword{methods}

