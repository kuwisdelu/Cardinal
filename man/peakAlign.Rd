\name{peakAlign-methods}
\docType{methods}

\alias{peakAlign}
\alias{peakAlign-methods}
\alias{peakAlign,MSImagingExperiment,numeric-method}
\alias{peakAlign,MSImagingExperiment,character-method}
\alias{peakAlign,MSImagingExperiment,missing-method}

\alias{peakAlign.diff}
\alias{peakAlign.DP}

\title{Peak align an imaging dataset}
\description{
	Apply peak alignment to a mass spectrometry imaging dataset.
}
\usage{
\S4method{peakAlign}{MSImagingExperiment,missing}(object, tolerance = NA, units = c("ppm", "mz"), \dots)

\S4method{peakAlign}{MSImagingExperiment,character}(object, ref, \dots)

\S4method{peakAlign}{MSImagingExperiment,numeric}(object, ref, \dots)
}
\arguments{
    \item{object}{An imaging dataset.}
    \item{ref}{A reference to which to align the peaks.}
    \item{tolerance}{The tolerance to be used when aligning detected peaks to the reference. If this is NA, then automatically guess a tolerance from the data.}
    \item{units}{The units to use for the \code{tolerance}. Either parts-per-million or the raw m/z values.}
    \item{\dots}{Ignored.}
}
\details{
    When applied to a \code{MSImagingExperiment} object with no other reference, \code{peakAlign} uses \code{summarize()} to calculate the mean spectrum, and then uses the local maxima  of the mean spectrum as the reference. Alternatively, a vector of m/z values or a column name in the featureData that should be used as the reference may be provided. Finally, if the featureData has an numeric vector element named ``reference peaks'' among its \code{metadata()}, this vector is used as the reference.

    Peak alignment is usually performed using the provided functions, but a user-created function can also be passed to \code{method}. In this case it should take the following arguments:

    \itemize{
        \item{\code{x}: The vector of m/z values to be aligned.}
        \item{\code{y}: The vector of reference m/z values.}
        \item{\code{\dots}: Additional arguments.}
    }

    A user-created function should return a vector of the same length as \code{x} and \code{y} where \code{NA} values indicate no match, and non-missing values give the index of the matched peak in the reference set.

    Internally, \code{\link{pixelApply}} is used to apply the peak alignment. See its documentation page for more details on additional objects available to the environment installed to the peak alignment function.
}
\value{
    An object of class \code{\linkS4class{MSImagingExperiment}} with the peak aligned spectra.
}
\author{
	Kylie A. Bemis
}
\seealso{
    \code{\linkS4class{MSImagingExperiment}},
    \code{\link{peakPick}},
    \code{\link{peakFilter}},
    \code{\link{peakBin}},
    \code{\link{pixelApply}},
    \code{\link{process}}
}
\examples{
setCardinalBPPARAM(SerialParam())

set.seed(2)
data <- simulateImage(preset=1, npeaks=10, dim=c(3,3))
data <- data[,pData(data)$circle]

# queue peak picking and alignment
data <- peakPick(data, method="simple", SNR=6)
data <- peakAlign(data, tolerance=200, units="ppm")

# apply peak picking and alignment
data_peaks <- process(data, plot=interactive())
}
\keyword{methods}
