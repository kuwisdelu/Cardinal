\name{recalibrate}

\alias{recalibrate}
\alias{recalibrate,MSImagingExperiment_OR_Arrays-method}
\alias{recalibrate,SpectralImagingData-method}

\title{Recalibrate spectra}

\description{
    Apply deferred recalibration to spectra.
}

\usage{
\S4method{recalibrate}{MSImagingExperiment_OR_Arrays}(object, ref,
    method = c("locmax", "dtw", "cow"),
    tolerance = NA, units = c("ppm", "mz"), \dots)

\S4method{recalibrate}{SpectralImagingData}(object, ref,
    method = c("locmax", "dtw", "cow"),
    tolerance = NA, units = c("relative", "absolute"), \dots)
}

\arguments{
    \item{object}{A spectral imaging dataset.}

    \item{ref}{The domain (m/z) values or indices of reference peaks to use for the recalibration.}

    \item{method}{The recalibration method to use. See \code{\link[matter]{warp1}} for details.}

    \item{tolerance}{The tolerance for how much a peak can be shifted in either direction.}

    \item{units}{The units for the above tolerance.}

    \item{\dots}{Additional arguments passed to the recalibration function.}
}

\details{
    The supported recalibration methods are:

    \itemize{

        \item{"locmax": Align to local maxima using \code{\link{warp1_loc}}.}

        \item{"dtw": Dynamic time warping using \code{\link{warp1_dtw}}.}

        \item{"cow": Correlation optimized warping using \code{\link{warp1_cow}}.}
    }
}

\note{
    The recalibration is deferred until \code{process()} is called.
}

\value{
    An object of the same class with the processing step queued.
}

\author{
    Kylie A. Bemis
}

\seealso{
    \code{\link{normalize}},
    \code{\link{smooth}},
    \code{\link{recalibrate}},
    \code{\link{peakPick}},
    \code{\link{process}}
}

\examples{
set.seed(1, kind="L'Ecuyer-CMRG")
mse <- simulateImage(preset=1, npeaks=10, dim=c(3,3), sdmz=250)
plot(mse, i=c(2,4,5), superpose=TRUE, xlim=c(1260,1320))

# queue recalibration
peaks <- estimateReferencePeaks(mse)
mse2 <- recalibrate(mse, ref=peaks, method="locmax", tolerance=500)

# apply recalibration
mse2 <- process(mse2)
plot(mse2, i=c(2,4,5), superpose=TRUE, xlim=c(1260,1320))
}

\keyword{ts}
