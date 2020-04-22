\name{aggregate-methods}
\docType{methods}

\alias{aggregate,SparseImagingExperiment-method}
\alias{aggregate,MSImagingExperiment-method}

\alias{summarizeFeatures}
\alias{summarizePixels}

\title{Calculating summary statistics}

\description{
	These methods calculate summary statistics over subsets of an imaging experiment.
}
\usage{
\S4method{aggregate}{SparseImagingExperiment}(x, by = c("feature", "pixel"), FUN,
        groups = NULL, tform = identity, as = "ImagingExperiment",
        BPPARAM = getCardinalBPPARAM(), \dots)

summarizeFeatures(x, FUN = "mean", \dots)

summarizePixels(x, FUN = "mean", \dots)
}
\arguments{
    \item{x}{An imaging dataset.}
    \item{by}{Should the summarization be performed over pixels or features?}
    \item{FUN}{A function or list of functions that return statistical summaries. Known statistics can be provided to be computed more efficiently than providing the equivalent function. Known statistics include "min", "max", "mean", "sum", "sd", and "var".}
    \item{groups}{A grouping variable for summarization. The summary functions will be applied within each group.}    
    \item{tform}{A unary transformation that should each row or column be transformed before summarization is applied.}
    \item{as}{What class of object should be returned (\code{ImagingExperiment} or \code{DataFrame})?}
    \item{BPPARAM}{An optional \code{BiocParallelParam} instance to be passed to \code{bplapply()}.}
    \item{\dots}{Additional arguments to be passed to \code{FUN}.}
}
\value{
    An \code{ImagingExperiment} subclass if \code{as=="ImagingExperiment"} or a \code{DataFrame} subclass otherwise.
}
\author{
	Kylie A. Bemis
}
\examples{

set.seed(1)
mse <- simulateImage(preset=1, npeaks=10, dim=c(10,10))

# calculate median spectrum
aggregate(mse, by="feature", FUN=median, as="DataFrame")

# summarize mean spectrum
summarizeFeatures(mse, FUN="mean", as="DataFrame")

# summarize image by TIC
summarizePixels(mse, FUN=c(tic="sum"), as="DataFrame")

# summarize mean spectrum grouped by pixels in/out of circle
summarizeFeatures(mse, FUN="mean", groups=mse$circle, as="DataFrame")
}
\keyword{manip}
