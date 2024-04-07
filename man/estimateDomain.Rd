\name{estimateDomain}

\alias{estimateDomain}

\title{Estimate shared domain}

\description{
	For unaligned spectral data, it is often necessary to estimate a suitable shared domain in order to calculate statistical summaries like the mean spectrum.
}

\usage{
estimateDomain(xlist,
    units = c("relative", "absolute"),
    nchunks = getCardinalNChunks(),
    verbose = getCardinalVerbose(),
    BPPARAM = getCardinalBPPARAM(), \dots)
}

\arguments{
    \item{xlist}{A list of the domain values (e.g., m/z values) for each spectrum.}

    \item{units}{Should the spacing between domain values use absolute or relative units?}

    \item{nchunks}{The number of chunks to use for processing.}

    \item{verbose}{Should progress messages be printed?}

    \item{BPPARAM}{An optional instance of \code{BiocParallelParam}. See documentation for \code{\link{bplapply}}.}

    \item{\dots}{Options passed to \code{\link{chunkLapply}}.}
}

\details{
    This domain is estimated by first finding the resolution of each spectrum's individual domain values (e.g., the spacing between m/z values), and then creating a sequence of domain values using the median resolution of all spectra.
}

\value{
    A sequence of domain values.
}

\author{
	Kylie A. Bemis
}

\seealso{
    \code{\link{summarizeFeatures}},
    \code{\link{recalibrate}},
    \code{\link{peakAlign}},
    \code{\link{peakProcess}}
}

\keyword{utilities}
\keyword{ts}
