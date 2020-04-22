\name{colocalized-methods}
\docType{methods}

\alias{colocalized}

\alias{colocalized,SparseImagingExperiment,ANY-method}
\alias{colocalized,MSImagingExperiment,missing-method}
\alias{colocalized,SpatialDGMM,ANY-method}

\title{Colocalized features}

\description{
    Find colocalized features in an imaging dataset.
}

\usage{
\S4method{colocalized}{MSImagingExperiment,missing}(object, mz, \dots)

\S4method{colocalized}{SparseImagingExperiment,ANY}(object, ref, n = 10,
		sort.by = c("correlation", "M1", "M2"),
		threshold = median,
		BPPARAM = getCardinalBPPARAM(), \dots)

\S4method{colocalized}{SpatialDGMM,ANY}(object, ref, n = 10,
		sort.by = c("Mscore", "M1", "M2"),
		threshold = median,
		BPPARAM = getCardinalBPPARAM(), \dots)
}

\arguments{
    \item{object}{An imaging experiment.}
    
    \item{mz}{An m/z value giving the image to use as a reference.}

    \item{ref}{Either a numeric vector or logical mask of a region-of-interest, or the feature to use as a reference.}

    \item{n}{The number of top-ranked colocalized features to return.}

    \item{sort.by}{The colocalization measure used to rank colocalized features. Possible options include Pearson's correlation ("correlation"), match score ("Mscore"), and Manders' colocalization coefficients ("M1" and "M2").}

    \item{threshold}{A function that returns the cutoff to use for creating logical masks of numeric references.}

    \item{BPPARAM}{An optional instance of \code{BiocParallelParam}. See documentation for \code{\link{bplapply}}.}

	\item{\dots}{ignored.}
}

\value{
    A data frame with the colocalized features.
}

\author{Kylie A. Bemis}

\seealso{
    \code{\link{topFeatures}}
}

\examples{
setCardinalBPPARAM(SerialParam())

set.seed(1)
data <- simulateImage(preset=2, npeaks=10, representation="centroid")

# find features colocalized with first feature
colocalized(data, ref=1)
}

\keyword{methods}
