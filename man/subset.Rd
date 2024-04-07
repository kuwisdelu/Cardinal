\name{subsetFeatures}

\alias{subsetFeatures}
\alias{subsetPixels}
\alias{subset,SpectralImagingExperiment-method}

\title{Subset a spectral imaging dataset}

\description{
	Returns a subset of the dataset that meets the conditions.
}

\usage{
\S4method{subset}{SpectralImagingExperiment}(x, subset, select, \dots)

subsetFeatures(x, \dots)

subsetPixels(x, \dots)
}

\arguments{
    \item{x}{A spectral imaging dataset.}

    \item{subset}{Logical expression to be evaluated in the object's \code{featureData()} indicating which rows (features) to keep.}

    \item{select}{Logical expression to be evaluated in the object's \code{pixelData()} indicating which columns (pixels) to keep.}

    \item{\dots}{Conditions describing rows (features) or columns (pixels) to be retained. Passed to \code{features()} and \code{pixels()} methods to obtain the subset indices.}
}

\value{
    An object of the same class as \code{x} with the appropriate subsetting applied to it.
}

\author{
	Kylie A. Bemis
}

\examples{
set.seed(1)
mse <- simulateImage(preset=1, npeaks=10, dim=c(10,10))

# subset features to mass range 1000 - 1500
subsetFeatures(mse, 1000 < mz, mz < 1500)

# select pixels to coordinates x = 1..3, y = 1..3
subsetPixels(mse, x <= 3, y <= 3)

# subset both features + pixels
subset(mse, 1000 < mz & mz < 1500, x <= 3 & y <= 3)
}

\keyword{manip}
