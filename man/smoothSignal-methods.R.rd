\name{smoothSignal-methods}
\docType{methods}

\alias{smoothSignal}
\alias{smoothSignal-methods}
\alias{smoothSignal,MSImageSet-method}

\alias{smoothSignal.gaussian}
\alias{smoothSignal.sgolay}
\alias{smoothSignal.ma}

\title{Smooth the Feature-Vectors of an Imaging Dataset}
\description{
	Apply smoothing to a mass spectrometry imaging dataset.
}
\usage{
\S4method{smoothSignal}{MSImageSet}(object, method = c("gaussian", "sgolay", "ma"),
    \dots,
    pixel = pixels(object),
    plot = FALSE)

## Gaussian smoothing
smoothSignal.gaussian(x, sd=window/4, window=5, \dots)

## Savitsky-Golay smoothing
smoothSignal.sgolay(x, order=3, window=order + 3 - order \%\% 2, \dots)

## Moving average smoothing
smoothSignal.ma(x, coef=rep(1, window + 1 - window \%\% 2), window=5, \dots)
}
\arguments{
    \item{object}{An object of class \code{\linkS4class{MSImageSet}}.}
    \item{method}{The smoothing method to use.}
    \item{pixel}{The pixels to smooth. If less than the extent of the dataset, this will result in a subset of the data being processed.}
    \item{plot}{Plot the mass spectrum for each pixel while it is being processed?}
    \item{\dots}{Additional arguments passed to the smoothing method.}
    \item{x}{The mass spectrum to be smoothed.}
    \item{sd}{The standard deviation for the Gaussian kernel.}
    \item{window}{The smoothing window.}
    \item{order}{The order of the smoothing filter.}
    \item{coef}{The coefficients for the moving average filter.}
}
\details{
    Smoothing is usually performed using the provided functions, but a user-created function can also be passed to \code{method}. In this case it should take the following arguments:

    \itemize{
        \item{\code{x}: A \code{numeric} vector of intensities.}
        \item{\code{\dots}: Additional arguments.}
    }

    A user-created function should return a \code{numeric} vector of the same length.

    Internally, \code{\link[Cardinal]{pixelApply}} is used to apply the smooothing. See its documentation page for more details on additional objects available to the environment installed to the smoothing function.
}
\value{
    An object of class \code{\linkS4class{MSImageSet}} with the smoothed spectra.
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
smoothSignal(data, method="gaussian", plot=interactive())
}
\keyword{methods}
