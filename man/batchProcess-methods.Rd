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
    reduceDimension = NULL,
    peakPick = NULL,
    peakAlign = NULL,
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
    \item{reduceDimension}{Either 'TRUE' or a \code{list} of arguments to be passed to the \code{reduceDimension} method. Use 'FALSE' or 'NULL' to skip this pre-processing step.}
    \item{peakPick}{Either 'TRUE' or a \code{list} of arguments to be passed to the \code{peakPick} method. Use 'FALSE' or 'NULL' to skip this pre-processing step.}
    \item{peakAlign}{Either 'TRUE' or a \code{list} of arguments to be passed to the \code{peakAlign} method. Use 'FALSE' or 'NULL' to skip this pre-processing step.}
    \item{layout}{The layout of the plots, given by a length 2 numeric as \code{c(ncol, nrow)}.}
    \item{pixel}{The pixels to process. If less than the extent of the dataset, this will result in a subset of the data being processed.}
    \item{plot}{Plot the pre-processing step for each pixel while it is being processed?}
    \item{\dots}{Ignored.}
}
\details{
    One of the primary purposes of this method (besides streamlining pre-processing steps) is to allow single-step reduction of larger-than-memory on-disk datasets to a smaller peak picked form without fully loading the data into memory. Therefore, the behavior for \code{peakPick} differs somewhat from when the \code{peakPick} method is called on its own. Typically, the spectra are preserved until \code{peakAlign} is called. However, to save memory, only the peaks are returned by \code{batchProcess}.

    Additionally, when performing batch pre-processing, the mean spectrum is also calculated and returned as part of the 'featureData' of the result, to be used by subsequent calls to \code{peakAlign}.

    Internally, \code{\link[Cardinal]{pixelApply}} is used to apply the pre-processing steps, as with other pre-processing methods.

    Note that \code{reduceDimension} and \code{peakPick} cannot appear in the same \code{batchProcess} call together, and \code{peakAlign} cannot appear in a \code{batchProcess} call without \code{peakPick}.

    The \code{peakAlign} step is performed separately from every other step.
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
    reduceBaseline=TRUE, peakPick=TRUE, peakAlign=TRUE,
    layout=c(2,2), plot=interactive())

batchProcess(data, normalize=TRUE,
    reduceBaseline=list(blocks=200), peakPick=list(SNR=12),
    layout=c(1,3), plot=interactive())
}
\keyword{methods}
