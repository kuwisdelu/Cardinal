\name{filter-methods}
\docType{methods}

\alias{filter,ImagingExperiment-method}
\alias{select,ImagingExperiment-method}
\alias{mutate,ImagingExperiment-method}
\alias{summarize,SparseImagingExperiment-method}

\title{Data transformation and summarization for imaging experiments}
\description{
	These methods provide analogs of data manipulation verbs from the \code{dplyr} package, with appropriate semantics for imaging experiments. Due to the differences between imaging datasets and standard data frames, they do not always work identically.

    See the descriptions below for details.
}
\usage{
\S4method{filter}{ImagingExperiment}(.data, \dots, .id)

\S4method{select}{ImagingExperiment}(.data, \dots, .id)

\S4method{mutate}{ImagingExperiment}(.data, \dots)

\S4method{summarize}{SparseImagingExperiment}(.data, \dots,
    .by = c("feature", "pixel"),
    .stat = c("min", "max", "mean", "sum", "sd", "var"),
    .tform = identity,
    BPPARAM = bpparam())
}
\arguments{
    \item{.data}{An imaging dataset.}
    \item{\dots}{Conditions describing rows or columns to be retained, name-value pairs to be added as metadata columns, or name-value pairs of summary functions. See Details.}
    \item{.id}{Select rows (features) or columns (pixels) by index.}
    \item{.by}{Should the summarization be performed over pixels or features?}
    \item{.stat}{Summary statistics to be computed in an efficient manner.}
    \item{.tform}{How should each feature-vector of image-vector be transformed before summarization?}
    \item{BPPARAM}{An optional \code{BiocParallelParam} instance to be passed to \code{bplapply()}.}
}
\details{
    \code{filter()} keeps only the rows (features) where the conditions are TRUE. Columns of \code{featureData(.data)} can be referred to literally in the logical expressions.

    \code{select()} keeps only the columns (pixels) where the conditions are TRUE. Columns of \code{pixelData(.data)} can be referred to literally in the logical expressions.

    \code{mutate()} adds new columns to the pixel metadata columns (\code{pixelData(.data)}).

    \code{summarize()} calculates statistical summaries over either features or pixels using \code{pixelApply()} or \code{featureApply()}. Several statistical summaries can be chosen via the \code{.stat} argument, which will be efficiently calculated according to the format of the data.
}
\value{
    An \code{ImagingExperiment} (or subclass) instance for \code{filter()}, \code{select()}, and \code{mutate()}. An \code{XDataFrame} (or subclass) instance for \code{summarize()}.
}
\author{
	Kylie A. Bemis
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

filter(msset, mz > 2000, mz < 3000)

select(msset, x <= 4, y <= 4)

sm1 <- summarize(msset, .stat="mean")

sm2 <- summarize(msset, .stat=c(tic="sum"), .by="pixel")

mutate(msset, tic=sm2$tic)
}
\keyword{manip}
