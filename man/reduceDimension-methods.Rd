\name{reduceDimension-methods}
\docType{methods}

\alias{reduceDimension}
\alias{reduceDimension-methods}
\alias{reduceDimension,MSImageSet,missing-method}
\alias{reduceDimension,MSImageSet,numeric-method}
\alias{reduceDimension,MSImageSet,MSImageSet-method}

\alias{reduceDimension.bin}
\alias{reduceDimension.resample}
\alias{reduceDimension.peaks}

\title{Reduce the Dimension of an Imaging Dataset}
\description{
	Apply dimension reduction to a mass spectrometry imaging dataset.
}
\usage{
\S4method{reduceDimension}{MSImageSet,missing}(object, method = c("bin", "resample"),
    \dots,
    pixel = pixels(object),
    plot = FALSE)

\S4method{reduceDimension}{MSImageSet,numeric}(object, ref, method = "peaks", \dots)

\S4method{reduceDimension}{MSImageSet,MSImageSet}(object, ref, method = "peaks", \dots)

## Bin the signal
reduceDimension.bin(x, t, tout, width=200, offset=0, units=c("ppm","mz"), fun=sum, ...)

## Resample the signal
reduceDimension.resample(x, t, tout, step=1, offset=0, ...)

## Reduce the signal to peaks
reduceDimension.peaks(x, t, tout, peaklist, type=c("height", "area"), ...)
}
\arguments{
    \item{object}{An object of class \code{\linkS4class{MSImageSet}}.}
    \item{ref}{A reference to use to reduce the dimension, usually a peak list of m/z values or a peak-picked and aligned \code{\linkS4class{MSImageSet}}.}
    \item{method}{The method to use to reduce the dimensions of the signal.}
    \item{pixel}{The pixels to process. If less than the extent of the dataset, this will result in a subset of the data being processed.}
    \item{plot}{Plot the mass spectrum for each pixel while it is being processed?}
    \item{\dots}{Additional arguments passed to the dimension reduction method.}
    \item{x}{The mass spectrum to be reduced.}
    \item{t}{The corresponding m/z values.}
    \item{tout}{The m/z values to output.}
    \item{width}{The width of a bin.}
    \item{step}{The step size.}
    \item{offset}{Offset from the nearest integer.}
    \item{units}{Either parts-per-million or the raw m/z values.}
    \item{fun}{The function to be applied to each bin.}
    \item{peaklist}{A \code{vector} of m/z values corresponding to peaks.}
    \item{type}{Should the peak height or area under the curve be taken as the intensity value?}
}
\details{
    Dimension reduction is usually performed using the provided functions, but a user-created function can also be passed to \code{method}. In this case it should take the following arguments:

    \itemize{
        \item{\code{x}: A \code{numeric} vector of intensities.}
        \item{\code{t}: A \code{numeric} vector of m/z values.}
        \item{\code{tout}: A \code{numeric} vector of m/z values to output.}
        \item{\code{\dots}: Additional arguments.}
    }

    The optional argument \code{tout} was added in version 1.3.1 to avoid cases where the output m/z values may be costly and inefficient to re-calculate for every spectrum.

    A user-created function should return a \code{list} with two vectors of equal length, where the new length \emph{must} be shorter than \code{x} and \code{t}:

    \itemize{
        \item{\code{x}: A \code{numeric} vector of new intensities.}
        \item{\code{t}: A \code{numeric} vector of new m/z values.}
    }

    Internally, \code{\link[Cardinal]{pixelApply}} is used to apply the dimension reduction. See its documentation page for more details on additional objects available to the environment installed to the dimension reduction function.
}
\value{
    An object of class \code{\linkS4class{MSImageSet}} with the dimension-reduced spectra.
}
\author{
	Kyle D. Bemis
}
\seealso{
    \code{\linkS4class{MSImageSet}},
    \code{\link[Cardinal]{peakPick}},
    \code{\link[Cardinal]{peakAlign}},
    \code{\link[Cardinal]{pixelApply}}
}
\examples{
data <- generateImage(as="MSImageSet")
reduceDimension(data, method="resample", step=100, plot=interactive())
}
\keyword{methods}
