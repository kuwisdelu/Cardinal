\name{reduceBaseline-methods}
\docType{methods}

\alias{reduceBaseline}
\alias{reduceBaseline-methods}
\alias{reduceBaseline,MSImageSet-method}

\alias{reduceBaseline.median}

\title{Reduce the Baseline for an Imaging Dataset}
\description{
	Apply baseline reduction to a mass spectrometry imaging dataset.
}
\usage{
\S4method{reduceBaseline}{MSImageSet}(object, method = "median",
    \dots,
    pixel = pixels(object),
    plot = FALSE)

## Median baseline reduction
reduceBaseline.median(x, blocks=500, fun=min, spar=1, \dots)
}
\arguments{
    \item{object}{An object of class \code{\linkS4class{MSImageSet}}.}
    \item{method}{The baseline reduction method to use.}
    \item{pixel}{The pixels to baseline subtract. If less than the extent of the dataset, this will result in a subset of the data being processed.}
    \item{plot}{Plot the mass spectrum for each pixel while it is being processed?}
    \item{\dots}{Additional arguments passed to the baseline reduction method.}
    \item{x}{The mass spectrum to be baseline subtracted.}
    \item{blocks}{The number of intervals to break the mass spectrum into in order to choose minima or medians from which to interpolate the baseline.}
    \item{fun}{Function used to determine the points from which the baseline will be interpolated.}
    \item{spar}{Smoothing parameter for the spline smoothing applied to the spectrum in order to decide the cutoffs for throwing away baseline references that might occur inside peaks.}
}
\details{
    Baseline reduction is usually performed using the provided functions, but a user-created function can also be passed to \code{method}. In this case it should take the following arguments:

    \itemize{
        \item{\code{x}: A \code{numeric} vector of intensities.}
        \item{\code{\dots}: Additional arguments.}
    }

    A user-created function should return a \code{numeric} vector of the same length. with the baseline-subtracted intensities.

    Internally, \code{\link[Cardinal]{pixelApply}} is used to apply the baseline reduction. See its documentation page for more details on additional objects available to the environment installed to the baseline reduction function.
}
\value{
    An object of class \code{\linkS4class{MSImageSet}} with the baseline-subtracted spectra.
}
\author{
	Kyle D. Bemis
}
\seealso{
    \code{\linkS4class{MSImageSet}},
    \code{\link[Cardinal]{pixelApply}}
}
\examples{
data <- generateImage(as="MSImageSet")
reduceBaseline(data, method="median", plot=interactive())
}
\keyword{methods}
