\name{mzBin-methods}
\docType{methods}

\alias{mzBin}
\alias{mzBin-methods}
\alias{mzBin,MSImagingExperiment,numeric-method}
\alias{mzBin,MSImagingExperiment,missing-method}

\title{Mass bin an imaging dataset}
\description{
	Apply mass binning to a mass spectrometry imaging dataset.
}
\usage{
\S4method{mzBin}{MSImagingExperiment,numeric}(object, ref, width = 400, units = c("ppm", "mz"), fun=sum, \dots)

\S4method{mzBin}{MSImagingExperiment,missing}(object, from=min(mz(object)), to=max(mz(object)), by,
        resolution = 200, units = c("ppm", "mz"), fun=sum, \dots)
}
\arguments{
    \item{object}{An imaging dataset.}
    \item{ref}{A reference to which the m/z values are binned.}
    \item{width}{The width(s) of the bins.}
    \item{from, to}{The starting amd (maximal) end values of the sequence of m/z values.}
    \item{by}{The (approximate) interval between m/z values. For \code{units="ppm"}, rather than an exact step size, this actually corresponds to a binwidth, where each element of the sequence is considered the center of a bin.}
    \item{resolution}{Another way to specify the interval between m/z values. For \code{units="mz"}, this is the same as \code{by}. For \code{units="ppm"}, this is the half-binwdith.}
    \item{units}{The units for \code{by} and \code{resolution}. Either parts-per-million or absolute m/z increments.}
    \item{fun}{The function used to summarize each mass bin.}
    \item{\dots}{Ignored.}
}
\details{
    The reference masses are considered to be the center of each bin. The bin is then expanded on either side according to half the value of \code{width}, and the intensities in the bin are summarized using \code{fun}.

    Internally, \code{\link{pixelApply}} is used to apply the binning. See its documentation page for more details.
}
\value{
    An object of the same class with the binned spectra.
}
\author{
	Kylie A. Bemis
}
\seealso{
    \code{\linkS4class{MSImagingExperiment}},
    \code{\link{mzAlign}},
    \code{\link{peakBin}},
    \code{\link{reduceDimension}},
    \code{\link{pixelApply}},
    \code{\link{process}}
}
\examples{
register(SerialParam())

set.seed(1)
data <- simulateImage(preset=1, npeaks=9, dim=c(3,3))
ref <- mz(metadata(data)$design$featureData)

# queue peak binning
data <- mzBin(data, ref=ref, fun=max)

# apply peak binning
data_binned <- process(data, plot=interactive())
}
\keyword{methods}
