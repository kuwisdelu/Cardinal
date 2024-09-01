\name{pixels}

\alias{pixels}
\alias{pixels,SpectralImagingExperiment-method}
\alias{pixels,SpectralImagingArrays-method}
\alias{pixels,SpectralImagingData-method}

\title{Find pixel indices}

\description{
	Search for the column indices of a spectral imaging dataset that correspond to specific pixels, based on a set of conditions.
}

\usage{
\S4method{pixels}{SpectralImagingExperiment}(object, \dots, coord, run, tolerance = NA,
    env = NULL)

\S4method{pixels}{SpectralImagingArrays}(object, \dots, coord, run, tolerance = NA,
    env = NULL)

\S4method{pixels}{SpectralImagingData}(object, \dots, env = NULL)
}

\arguments{
    \item{object}{A spectral imaging dataset.}
    
    \item{\dots}{Expressions that evaluate to logical vectors in the environment of \code{pixelData()}.}

    \item{coord}{The coordinates of the pixels to include.}

    \item{run}{The run of the pixels to include.}

    \item{tolerance}{The tolerance for matching pixels to coordinates.}

    \item{env}{The enclosing environment for evaluating \code{\dots}.}
}

\author{
	Kylie A. Bemis
}

\examples{
set.seed(1, kind="L'Ecuyer-CMRG")
mse <- simulateImage(preset=1, npeaks=10, dim=c(10,10))

pixels(mse, x > 6, y > 6)
pixels(mse, coord=expand.grid(x=1:3, y=1:3))
}

\keyword{utilities}

