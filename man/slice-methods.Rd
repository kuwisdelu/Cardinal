\name{slice-methods}
\docType{methods}

\alias{slice}
\alias{slice,SparseImagingExperiment-method}

\title{Slice an image}
\description{
	Slice an imaging dataset as a "data cube".
}
\usage{
\S4method{slice}{SparseImagingExperiment}(x, \dots, drop=TRUE)
}
\arguments{
    \item{x}{An imaging dataset.}
    \item{\dots}{Conditions describing features to slice, passed to \code{features()}.}
    \item{drop}{Should redundant array dimensions be dropped? If TRUE, dimensions with only one level are dropped using \code{drop}.}
}
\details{
    Because \code{\linkS4class{SparseImagingExperiment}} objects may be pixel-sparse, their data is always internally represented as a matrix rather than an array, where each column is a feature-vector. Only columns for non-missing pixels are retained. This is simpler and more space-efficient if the image is non-rectangular, non-gridded, or has many missing values.

    However, it is often necessary to index into the data as if it were an actual "data cube", with explicit array dimensions for each spatial dimension. \code{slice()} allows this by slicing the object as a "data cube", and returning an image array from the object.

    For non-rectangular data, this may result in missing values. For non-gridded data, images must be projected to an array (with a regular grid), and the result may not represent the underlying values exactly.
}
\value{
    An array representing the sliced image(s).
}
\author{
	Kylie A. Bemis
}
\examples{
register(SerialParam())

set.seed(1)
x <- simulateImage(preset=1, npeaks=10, dim=c(10,10), representation="centroid")
m <- mz(metadata(x)$design$featureData)

# slice image for first feature
slice(x, 1)

# slice by m/z-value
slice(x, mz=m[1])

# slice multiple
slice(x, mz=m[1:3])
}
\keyword{manip}
