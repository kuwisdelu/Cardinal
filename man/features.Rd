\name{features}

\alias{features}
\alias{features,MSImagingExperiment-method}
\alias{features,SpectralImagingExperiment-method}

\title{Find feature indices}

\description{
	Search for the row indices of a spectral imaging dataset that correspond to specificor features, based on a set of conditions.
}

\usage{
\S4method{features}{MSImagingExperiment}(object, \dots, mz, tolerance = NA, units = c("ppm", "mz"),
    env = NULL)

\S4method{features}{SpectralImagingExperiment}(object, \dots, env = NULL)
}

\arguments{
    \item{object}{A spectral imaging dataset.}
    
    \item{\dots}{Expressions that evaluate to logical vectors in the environment of \code{featureData()}.}

    \item{mz}{The m/z values of features to include.}

    \item{tolerance}{The tolerance for matching features to m/z values.}

    \item{units}{The units for the above tolerance.}

    \item{env}{The enclosing environment for evaluating \code{\dots}.}
}

\author{
	Kylie A. Bemis
}

\examples{
set.seed(1)
mse <- simulateImage(preset=1, npeaks=10, dim=c(10,10))

features(mse, mz > 800, mz < 1800)
features(mse, mz=metadata(mse)$design$featureData$mz)
}

\keyword{utilities}

