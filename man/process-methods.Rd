\name{process-methods}
\docType{methods}

\alias{process}
\alias{process-methods}
\alias{process,SparseImagingExperiment-method}

\title{Delayed Processing of Imaging Datasets}
\description{
	Queue pre-processing steps on an imaging dataset and apply them.
}
\usage{
\S4method{process}{SparseImagingExperiment}(object, fun, \dots,
    kind = c("pixel", "feature", "global"),
    prefun, preargs,
    postfun, postargs,
    plotfun,
    label = "",
    delay = FALSE,
    plot = FALSE,
    par = NULL,
    outpath = NULL,
    BPPARAM = bpparam())
}
\arguments{
    \item{object}{An imaging dataset.}
    \item{fun}{A function to apply to each feature-vector or image-vector.}
    \item{\dots}{Additional arguments to \code{fun}.}
    \item{kind}{What kind of processing to perform? Over pixels, over features, or global processing of the dataset as a single unit.}
    \item{prefun}{A pre-processing function to be applied to the entire dataset, taking the dataset as its first argument. This should return another object of the same class.}
    \item{preargs}{Additional arguments to \code{prefun}, as a list.}
    \item{postfun}{A post-processing function to be applied to the output, taking the result as its first argument, and the original dataset as its second argument. This should return another object of the same class as the original dataset.}
    \item{postargs}{Additional arguments to \code{postfun}, as a list.}
    \item{plotfun}{A function to be used to plot the output of \code{fun}, taking at least two arguments: (1) the resulting vector and (2) the input vector.}
    \item{label}{The label of the processing step. This is used to identify it in the queue, and is printed as it is being processed.}
    \item{delay}{Should the function \code{fun} be applied now, or queued and delayed until \code{process()} is called again?}
    \item{plot}{Plot the function for each pixel or feature while it is being processed?}
    \item{par}{Plotting parameters to be passed to \code{plotfun}.}
    \item{outpath}{The path to a file where the results will be written by \code{pixelApply} or \code{featureApply}. If NULL, then the results are returned in-memory.}
    \item{BPPARAM}{An optional instance of \code{BiocParallelParam}. See documentation for \code{\link{bplapply}}.}
    
}
\details{
    This method allows queueing of delayed processing to an imaging dataset. All of the registered processing steps will be applied in sequence whenever \code{process()} is called next with \code{delay=FALSE}. The processing can be over feature-vectors (e.g., mass spectra), over image-vectors, or over the entire dataset as a unit. The processing is performed in parallel using the current registered parallel backend.
}
\value{
    An object of the same class (or subclass) as the original imaging dataset, with the data processing queued or applied.
}
\author{
	Kylie A. Bemis
}
\seealso{
    \code{\linkS4class{SparseImagingExperiment}},
    \code{\linkS4class{MSImagingExperiment}},
    \code{\link{pixelApply}},
    \code{\link{featureApply}},
    \code{\link{normalize}},
    \code{\link{smoothSignal}},
    \code{\link{reduceBaseline}},
    \code{\link{peakPick}},
    \code{\link{peakAlign}},
    \code{\link{peakFilter}},
    \code{\link{peakBin}}
}
\examples{
data <- matrix(c(NA, NA, 1, 1, NA, NA, NA, NA, NA, NA, 1, 1, NA, NA, 
 NA, NA, NA, NA, NA, 0, 1, 1, NA, NA, NA, NA, NA, 1, 0, 0, 1, 
 1, NA, NA, NA, NA, NA, 0, 1, 1, 1, 1, NA, NA, NA, NA, 0, 1, 1, 
 1, 1, 1, NA, NA, NA, NA, 1, 1, 1, 1, 1, 1, 1, NA, NA, NA, 1, 
 1, NA, NA, NA, NA, NA, NA, 1, 1, NA, NA, NA, NA, NA), nrow=9, ncol=9)

set.seed(1)
msset <- generateImage(data, range=c(1000,5000), centers=c(3000,4000), resolution=100, as="MSImageSet")

msset <- as(msset, "MSImagingExperiment")

l2msset <- process(msset, log2, label="log2")
}
\keyword{methods}
