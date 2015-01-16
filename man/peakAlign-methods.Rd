\name{peakAlign-methods}
\docType{methods}

\alias{peakAlign}
\alias{peakAlign-methods}
\alias{peakAlign,MSImageSet,numeric-method}
\alias{peakAlign,MSImageSet,MSImageSet-method}
\alias{peakAlign,MSImageSet,missing-method}

\alias{peakAlign.diff}
\alias{peakAlign.DP}

\title{Peak Align an Imaging Dataset}
\description{
	Apply peak alignment to a mass spectrometry imaging dataset.
}
\usage{
\S4method{peakAlign}{MSImageSet,numeric}(object, ref, method = c("diff", "DP"),
    \dots,
    pixel = pixels(object),
    plot = FALSE)

\S4method{peakAlign}{MSImageSet,MSImageSet}(object, ref, \dots)

\S4method{peakAlign}{MSImageSet,missing}(object, ref, \dots)

## Absolute difference alignment
peakAlign.diff(x, y, diff.max=200, units=c("ppm", "mz"), \dots)

## Dynamic programming alignment
peakAlign.DP(x, y, gap=0, \dots)
}
\arguments{
    \item{object}{An object of class \code{\linkS4class{MSImageSet}}.}
    \item{ref}{A reference to which to align the peaks.}
    \item{method}{The peak alignment method to use.}
    \item{pixel}{The pixels to align. If less than the extent of the dataset, this will result in a subset of the data being processed.}
    \item{plot}{Plot the mass spectrum for each pixel while it is being processed?}
    \item{\dots}{Additional arguments passed to the peak alignment method.}
    \item{x}{The vector of m/z values to be aligned.}
    \item{y}{The vector of reference m/z values.}
    \item{diff.max}{Peaks that differ less than this value will be aligned together.}
    \item{units}{Either parts-per-million of the raw m/z values.}
    \item{gap}{The gap penalty for the dynamic programming sequence alignment.}
}
\details{
    If a \code{MSImageSet} object is used as the reference, then the local maxima in its mean spectrum will be calculated and used as the reference m/z values. If the reference is missing, the method will use the object itself as the reference.

    Peak alignment is usually performed using the provided functions, but a user-created function can also be passed to \code{method}. In this case it should take the following arguments:

    \itemize{
        \item{\code{x}: The vector of m/z values to be aligned.}
        \item{\code{y}: The vector of reference m/z values.}
        \item{\code{\dots}: Additional arguments.}
    }

    A user-created function should return a vector of the same length as \code{x} and \code{y} where \code{NA} values indicate no match, and non-missing values give the index of the matched peak in the reference set.

    Internally, \code{\link[Cardinal]{pixelApply}} is used to apply the peak alignment. See its documentation page for more details on additional objects available to the environment installed to the peak alignment function.
}
\value{
    An object of class \code{\linkS4class{MSImageSet}} with the peak aligned spectra.
}
\author{
	Kyle D. Bemis
}
\seealso{
    \code{\linkS4class{MSImageSet}},
    \code{\link[Cardinal]{peakPick}},
    \code{\link[Cardinal]{peakFilter}},
    \code{\link[Cardinal]{reduceDimension}},
    \code{\link[Cardinal]{pixelApply}}
}
\examples{
data <- generateImage(diag(2), as="MSImageSet")
peaks <- peakPick(data, method="simple", plot=interactive())
peaks <- peakAlign(peaks, data, method="diff", plot=interactive())
}
\keyword{methods}
