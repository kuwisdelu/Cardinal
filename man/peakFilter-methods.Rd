\name{peakFilter-methods}
\docType{methods}

\alias{peakFilter}
\alias{peakFilter-methods}
\alias{peakFilter,MSImageSet-method}

\alias{peakFilter.freq}

\title{Peak Filter an Imaging Dataset}
\description{
	Apply peak filtering to a mass spectrometry imaging dataset.
}
\usage{
\S4method{peakFilter}{MSImageSet}(object, method = "freq",
    \dots,
    pixel = pixels(object),
    plot = FALSE)

## Filter based on the frequency of a peak
peakFilter.freq(x, freq.min=length(x) / 100, \dots)
}
\arguments{
    \item{object}{An object of class \code{\linkS4class{MSImageSet}}.}
    \item{method}{The peak filtering method to use.}
    \item{pixel}{The pixels to filter. If less than the extent of the dataset, this will result in a subset of the data being processed.}
    \item{plot}{Not supported for this processing method.}
    \item{\dots}{Additional arguments passed to the peak filtering method.}
    \item{x}{The vector of ion image intensities to filter.}
    \item{freq.min}{Peaks that occur in the dataset fewer times than this will be removed.}
}
\details{
    Unlike most other processing methods, \code{peakFilter} operates on the feature space (ion images) of the dataset.

    Peak filtering is usually performed using the provided functions, but a user-created function can also be passed to \code{method}. In this case it should take the following arguments:

    \itemize{
        \item{\code{x}: The vector of ion image intensities to filter.}
        \item{\code{\dots}: Additional arguments.}
    }

    A user-created function should return a logical: \code{TRUE} means keep the peak, and \code{FALSE} means remove the peak.

    Internally, \code{\link[Cardinal]{featureApply}} is used to apply the filtering. See its documentation page for more details on additional objects available to the environment installed to the peak filtering function.
}
\value{
    An object of class \code{\linkS4class{MSImageSet}} with the filtered peaks.
}
\author{
	Kyle D. Bemis
}
\seealso{
    \code{\linkS4class{MSImageSet}},
    \code{\link[Cardinal]{peakPick}},
    \code{\link[Cardinal]{peakAlign}},
    \code{\link[Cardinal]{reduceDimension}},
    \code{\link[Cardinal]{featureApply}}
}
\examples{
data <- generateImage(diag(2), as="MSImageSet")
peaks <- peakPick(data, method="simple", plot=interactive())
peaks <- peakAlign(peaks, method="diff", plot=interactive())
peaks <- peakFilter(peaks, method="freq")
}
\keyword{methods}
