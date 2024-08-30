\name{colocalized}

\alias{colocalized}
\alias{colocalized,MSImagingExperiment-method}
\alias{colocalized,SpectralImagingExperiment-method}
\alias{colocalized,SpatialDGMM-method}
\alias{coregister}

\title{Colocalized features}

\description{
    Find colocalized features in an imaging dataset.
}

\usage{
\S4method{colocalized}{MSImagingExperiment}(object, mz, \dots)

\S4method{colocalized}{SpectralImagingExperiment}(object, i, ref,
    threshold = median, n = Inf,
    sort.by = c("cor", "MOC", "M1", "M2", "Dice", "none"),
    verbose = getCardinalVerbose(), chunkopts = list(),
    BPPARAM = getCardinalBPPARAM(), \dots)

\S4method{colocalized}{SpatialDGMM}(object, ref,
    threshold = median, n = Inf,
    sort.by = c("MOC", "M1", "M2", "Dice", "none"),
    verbose = getCardinalVerbose(), chunkopts = list(),
    BPPARAM = getCardinalBPPARAM(), \dots)
}

\arguments{
    \item{object}{An imaging experiment.}
    
    \item{mz}{An m/z value of a feature in \code{object} to use as a reference.}

    \item{i}{The index of a feature in \code{object} to use as a reference.}

    \item{ref}{Either a flattened image (i.e., a numeric vector) or a logical mask of a region-of-interest to use as a reference.}

    \item{threshold}{Either a function that returns the cutoff to use for creating logical masks of numeric references, or a numeric threshold to use.}

    \item{n}{The number of top-ranked colocalized features to return.}

    \item{sort.by}{The colocalization measure used to rank colocalized features. Possible options include Pearson's correlation ("cor"), Manders overlap coefficient ("MOC"), Manders colocalization coefficients ("M1" and "M2"), and Dice similarity coefficient ("Dice".}

    \item{verbose}{Should progress messages be printed?}

    \item{chunkopts}{Chunk processing options. See \code{\link{chunkApply}} for details.}

    \item{BPPARAM}{An optional instance of \code{BiocParallelParam}. See documentation for \code{\link{bplapply}}.}

	\item{\dots}{Options passed to \code{\link{chunkApply}}.}
}

\value{
    A data frame with the colocalized features, or a list of data frames if multiple references are given.
}

\author{Kylie A. Bemis}

\examples{
set.seed(1)
x <- simulateImage(preset=1, dim=c(10,10), centroided=TRUE)

# find features colocalized with first feature
colocalized(x, i=1)
}

\keyword{methods}
