\name{batchProcess-methods}
\docType{methods}

\alias{batchProcess}
\alias{batchProcess-methods}
\alias{batchProcess,MSImageSet-method}

\title{Batch Pre-Processing on an Imaging Dataset}
\description{
	Batch apply multiple pre-processing steps on an imaging dataset.
}
\usage{
\S4method{batchProcess}{MSImageSet}(object,
    normalize = NULL,
    smoothSignal = NULL,
    reduceBaseline = NULL,
    peakPick = NULL,
    \dots,
    layout,
    pixel = pixels(object),
    plot = FALSE)

}
\arguments{
    \item{object}{An object of class \code{\linkS4class{MSImageSet}}.}
    \item{normalize}{Either 'TRUE' or a \code{list} of arguments to be passed to the \code{normalize} method. Use 'FALSE' or 'NULL' to skip this pre-processing step.}
    \item{smoothSignal}{Either 'TRUE' or a \code{list} of arguments to be passed to the \code{smoothSignal} method. Use 'FALSE' or 'NULL' to skip this pre-processing step.}
    \item{reduceBaseline}{Either 'TRUE' or a \code{list} of arguments to be passed to the \code{reduceBaseline} method. Use 'FALSE' or 'NULL' to skip this pre-processing step.}
    \item{peakPick}{Either 'TRUE' or a \code{list} of arguments to be passed to the \code{peakPick} method. Use 'FALSE' or 'NULL' to skip this pre-processing step.}
    \item{layout}{The layout of the plots, given by a length 2 numeric as \code{c(ncol, nrow)}.}
    \item{pixel}{The pixels to process. If less than the extent of the dataset, this will result in a subset of the data being processed.}
    \item{plot}{Plot the pre-processing step for each pixel while it is being processed?}
    \item{\dots}{Ignored.}
}
\details{
    When performing batch pre-processing, the mean spectrum is also calculated and returned as part of the 'featureData' of the result.

    Internally, \code{\link[Cardinal]{pixelApply}} is used to apply the pre-processing steps, as with other pre-processing methods.
}
\value{
    An object of class \code{\linkS4class{MSImageSet}} with the processed spectra.
}
\author{
	Kyle D. Bemis
}
\seealso{
    \code{\linkS4class{MSImageSet}},
    \code{\link[Cardinal]{normalize}},
    \code{\link[Cardinal]{smoothSignal}},
    \code{\link[Cardinal]{reduceBaseline}},
    \code{\link[Cardinal]{peakPick}},
    \code{\link[Cardinal]{pixelApply}}
}
\examples{
data <- generateImage(as="MSImageSet")

batchProcess(data, normalize=TRUE, smoothSignal=TRUE,
    reduceBaseline=TRUE, peakPick=TRUE,
    layout=c(2,2), plot=interactive())

batchProcess(data, normalize=TRUE,
    reduceBaseline=list(blocks=200), peakPick=list(SNR=12),
    layout=c(1,3), plot=interactive())
}
\keyword{methods}
