\name{dplyr-methods}
\docType{methods}

\alias{filter}
\alias{select}
\alias{mutate}
\alias{summarize}

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
\S4method{filter}{ImagingExperiment}(.data, \dots, .preserve=FALSE)

\S4method{select}{ImagingExperiment}(.data, \dots)

\S4method{mutate}{ImagingExperiment}(.data, \dots)

\S4method{summarize}{SparseImagingExperiment}(.data, \dots,
    .by = c("feature", "pixel"), .group_by,
    .stat = c("min", "max", "mean", "sum", "sd", "var"),
    .tform = identity,
    BPPARAM = bpparam())
}
\arguments{
    \item{.data}{An imaging dataset.}
    \item{\dots}{Conditions describing rows or columns to be retained, name-value pairs to be added as metadata columns, or name-value pairs of summary functions. See Details.}
    \item{.preserve}{Ignored, provided for compatibility with dplyr.}
    \item{.by}{Should the summarization be performed over pixels or features?}
    \item{.group_by}{A grouping variable for summarization. The summary functions will be applied within each group.}
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
register(SerialParam())

set.seed(1)
msset <- simulateImage(preset=1, npeaks=9, dim=c(9,9))

# filter features to mass range 1000 - 1500
filter(msset, 1000 < mz, mz < 1500)

# select pixels to coordinates x = 1..3, y = 1..3
select(msset, x <= 3, y <= 3)

# summarize mean spectrum
sm1 <- summarize(msset, .stat="mean")

# summarize image by TIC
sm2 <- summarize(msset, .stat=c(tic="sum"), .by="pixel")

# add a TIC column
mutate(msset, tic=sm2$tic)

# summarize mean spectrum grouped by pixels in/out of circle
sm3 <- summarize(msset, .stat="mean", .group_by=msset$circle)
}
\keyword{manip}
