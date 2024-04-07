\name{bin}

\alias{bin}
\alias{bin,MSImagingExperiment-method}
\alias{bin,MSImagingArrays-method}
\alias{bin,SpectralImagingExperiment-method}
\alias{bin,SpectralImagingArrays-method}

\title{Bin spectra}

\description{
	Apply on-the-fly binning to spectra.
}

\usage{
\S4method{bin}{MSImagingExperiment}(x, ref,
    spectra = "intensity", index = "mz",
    method = c("sum", "mean", "max", "min",
        "linear", "cubic", "gaussian", "lanczos"),
    resolution = NA, units = c("ppm", "mz"), \dots)

\S4method{bin}{MSImagingArrays}(x, ref,
    spectra = "intensity", index = "mz",
    method = c("sum", "mean", "max", "min",
        "linear", "cubic", "gaussian", "lanczos"),
    resolution = NA, units = c("ppm", "mz"), \dots)

\S4method{bin}{SpectralImagingExperiment}(x, ref,
    spectra = "intensity", index = NULL,
    method = c("sum", "mean", "max", "min",
        "linear", "cubic", "gaussian", "lanczos"),
    resolution = NA, units = c("relative", "absolute"),
    verbose = getCardinalVerbose(), \dots)

\S4method{bin}{SpectralImagingArrays}(x, ref,
    spectra = "intensity", index = NULL,
    method = c("sum", "mean", "max", "min",
        "linear", "cubic", "gaussian", "lanczos"),
    resolution = NA, units = c("relative", "absolute"),
    verbose = getCardinalVerbose(), \dots)
}

\arguments{
    \item{x}{A spectral imaging dataset.}
    
    \item{ref}{Optional. The bin centers. Created from \code{resolution} if not provided.}

    \item{spectra}{The name of the array in \code{spectraData()} to bin.}

    \item{index}{The name of the array in \code{spectraData()} (for \code{SpectralImagingArrays}) or column in \code{featureData()} (for \code{SpectralImagingExperiment}) to use for the bins.}

    \item{method}{The peak picking method to use. See \code{\link[matter]{approx1}} for details.}
    
    \item{resolution}{The bin widths.}
    
    \item{units}{The units for the above resolution.}

    \item{verbose}{Should progress messages be printed?}

    \item{\dots}{Ignored.}
}

\details{
    The binning is not processed immediately, but is applied on-the-fly whenever the spectra are accessed.
}

\value{
    A new object derived from \code{SpectralImagingExperiment} with the binned spectra.
}

\author{
	Kylie A. Bemis
}

\seealso{
    \code{\link[matter]{approx1}}
}

\examples{
set.seed(1)
mse <- simulateImage(preset=1, npeaks=10, dim=c(3,3))

# bin to unit resolition
mse2 <- bin(mse, resolution=1, units="mz")
}

\keyword{ts}
