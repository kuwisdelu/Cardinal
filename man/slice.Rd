\name{slice}

\alias{slice}

\title{Slice an image}

\description{
	Slice a spectral imaging dataset as a "data cube".
}

\usage{
slice(x, i = features(x, ...), ..., run = NULL,
    simplify = TRUE, drop = TRUE)
}

\arguments{
    \item{x}{A spectral imaging dataset.}

    \item{i}{The indices of features to slice for the images.}

    \item{\dots}{Conditions describing features to slice, passed to \code{features()}.}

    \item{run}{The names of experimental runs to include, or the index of the levels of the runs to include.}

    \item{simplify}{The image slices be returned as a list, or simplified to an array?}

    \item{drop}{Should redundant array dimensions be dropped? If TRUE, dimensions with only one level are dropped using \code{drop}.}
}

\value{
    A list or array of the sliced image(s). If multiple images are sliced and \code{simplify=TRUE}, then the \emph{last} dimension will be the features.
}

\author{
	Kylie A. Bemis
}

\examples{
set.seed(1, kind="L'Ecuyer-CMRG")
mse <- simulateImage(preset=1, npeaks=10, dim=c(10,10), centroided=TRUE)
peaks <- mz(metadata(mse)$design$featureData)

# slice image for first feature
slice(mse, 1)

# slice by m/z-value
slice(mse, mz=peaks[1])

# slice multiple
slice(mse, mz=peaks[1:3])
}

\keyword{manip}
