\name{alignPeaks-methods}
\docType{methods}
\alias{alignPeaks}
\alias{alignPeaks-methods}
\alias{alignPeaks,MSPeakList,MSImageSet-method}
\alias{alignPeaks,MSPeakList,MSPeakFrame-method}
\title{Alignment of mass spectrometry peaks}
\description{
	Aligns peak lists from mass spectrometry data for later binning or resampling.
}
\usage{
    \S4method{alignPeaks}{MSPeakList,MSImageSet}(peaks, reference,
    method=c("diff", "ztest", "dp"), fun=mean, plot=FALSE, \dots)
	
    \S4method{alignPeaks}{MSPeakList,MSPeakFrame}(peaks, reference,
    method=c("diff", "ztest", "dp"), plot=FALSE, \dots)
}
\arguments{
    \item{peaks}{the peaks to align.}
    \item{reference}{the reference to which the peaks should be aligned.}
    \item{method}{the name of a predefined or user-defined function for aligning two \code{MSPeakFrame} objects.}
    \item{fun}{the function used to create a summary spectrum if aligning using an \code{\linkS4class{MSImageSet}} as the reference.}
    \item{plot}{\code{logical}, indicating whether the alignment should be plotted.}
    \item{\dots}{arguments to be passed to \code{method}.}
}
\details{
    \code{method}: The name of a predefined or user-defined function to align two \code{MSPeakFrame} objects, taking three arguments:
    \itemize{
        \item{\code{peaks}: \code{MSPeakFrame}, supplying the observed peaks
        }
        \item{\code{reference}: \code{MSPeakFrame}, supplying the reference peaks
        }
        \item{\code{\dots}: additional arguments
        }
    }
    A valid alignment function returns a \code{data.frame} object with two variables, named \code{peaks} and \code{reference}. The two variables contain the matching peak ids (i.e. their row numbers in the original \code{MSPeakFrame}).
}
\value{
    An \code{MSPeakAlignment} object where each element is a \code{MSPeakFrame} storing the matched peaks.
}
\author{
	Kyle D. Bemis
}
\seealso{
    \code{\linkS4class{MSImageSet}},
    \code{\link[Cardinal]{detectPeaks-methods}},
    \code{\link[Cardinal]{mergePeaks-methods}},
    \code{\link[Cardinal]{poolPeaks-methods}}
}
\examples{
    ## write examples to put here
}
\keyword{methods}


