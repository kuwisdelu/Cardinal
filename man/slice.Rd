\name{slice}

\alias{slice}

\title{Slice an image}

\description{
	Slice a spectral imaging dataset as a "data cube".
}

\usage{
slice(x, i = features(x, ...), ...,
    simplify = TRUE, drop = TRUE)
}

\arguments{
    \item{x}{A spectral imaging dataset.}

    \item{i}{The indices of features to slice for the images.}

    \item{\dots}{Conditions describing features to slice, passed to \code{features()}.}

    \item{simplify}{The image slices be returned as a list, or simplified to an array?}

    \item{drop}{Should redundant array dimensions be dropped? If TRUE, dimensions with only one level are dropped using \code{drop}.}
}

\value{
    A list or array of the sliced image(s). If multiple images are sliced, the first dimension will be the features.
}

\author{
	Kylie A. Bemis
}

\examples{
set.seed(1)
mse <- simulateImage(preset=1, npeaks=10, dim=c(10,10), representation="centroid")
peaks <- mz(metadata(mse)$design$featureData)

# slice image for first feature
slice(mse, 1)

# slice by m/z-value
slice(mse, mz=peaks[1])

# slice multiple
slice(mse, mz=peaks[1:3])
}

\keyword{manip}
