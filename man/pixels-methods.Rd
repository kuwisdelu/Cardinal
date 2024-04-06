\name{pixels-methods}

\alias{pixels}
\alias{features}

\alias{pixels,MSImagingExperiment-method}
\alias{pixels,SpectralImagingExperiment-method}

\alias{features,SpectralImagingExperiment-method}
\alias{features,SpectralImagingArrays-method}
\alias{features,SpectralImagingData-method}

\title{Find pixel or feature indices}

\description{
	Search for the column or row indices of a spectral imaging dataset that correspond to specific pixels or features, based on a set of conditions.
}

\usage{
\S4method{pixels}{SpectralImagingExperiment}(object, \dots, coord, run, tolerance = NA)

\S4method{pixels}{SpectralImagingArrays}(object, \dots, coord, run, tolerance = NA)

\S4method{pixels}{SpectralImagingData}(object, \dots, env = parent.frame(1L))

\S4method{features}{MSImagingExperiment}(object, \dots, mz, tolerance = NA, units = c("ppm", "mz"))

\S4method{features}{SpectralImagingExperiment}(object, \dots, env = parent.frame(1L))
}

\arguments{
    \item{x}{A spectral imaging dataset.}
    
    \item{\dots}{Expressions that evaluate to logical vectors in the environment of \code{pixelData()} or \code{featureData()}, as appropriate.}

    \item{coord}{The coordinates of the pixels to include.}

    \item{run}{The run of the pixels to include.}

    \item{mz}{The m/z values of features to include.}

    \item{tolerance}{The tolerance for matching pixels to coordinates or matching features to m/z values.}

    \item{units}{The units for the above tolerance.}

    \item{env}{The enclosing environment for evaluating \code{\dots}.}
}

\author{
	Kylie A. Bemis
}

\examples{
set.seed(1)
mse <- simulateImage(preset=1, npeaks=10, dim=c(10,10))

pixels(mse, x > 6, y > 6)
pixels(mse, coord=expand.grid(x=1:3, y=1:3))

features(mse, mz > 800, mz < 1800)
features(mse, mz=metadata(mse)$design$featureData$mz)
}

\keyword{utilities}

