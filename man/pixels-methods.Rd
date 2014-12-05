\name{pixels-methods}
\docType{methods}

\alias{pixels}
\alias{pixels-methods}
\alias{pixels,iSet-method}
\alias{pixels,MSImageSet-method}
\alias{features}
\alias{features-methods}
\alias{features,iSet-method}
\alias{features,MSImageSet-method}

\title{Retrieve Pixel or Feature Indices Based on Metadata}
\description{
	These are generic functions to retrieve pixel or feature row indices in an \code{\linkS4class{iSet}}-derived object's \code{pixelData} or \code{featureData} slots based on metadata variables.
}
\usage{
\S4method{pixels}{iSet}(object, \dots)

\S4method{features}{iSet}(object, \dots)

\S4method{pixels}{MSImageSet}(object, \dots, coord)

\S4method{features}{MSImageSet}(object, \dots, mz)
}
\arguments{
    \item{object}{An imaging dataset object.}
    \item{\dots}{Variables that appear in \code{pixelData(object)} or \code{featureData(object)}.}
    \item{mz}{A vector of m/z values.}
    \item{coord}{A list or \code{data.frame} of named pixel coordinates.}
}
\details{
    It is often more convenient to specify a pixel or feature by identifying metadata such as pixel coordinates or m/z-values than by their row indices in the \code{pixelData} and \code{featureData} slots. However, many functions expect indices rather than coordinates or m/z-values. These generic functions make it easy to retrieve indices based on such metadata.

    It is important to note that when passing multiple variables via \dots, the 'AND' operator is used to resolve the query. However, when vectors are passed, all combinations of the given values will be used.

    For convenience, \code{MSImageSet} uses a special implementation for the 'mz' variable, which uses a bisection search so that exact precision is not required when searching based on m/z-values.
}
\value{
    A numeric vector of pixel or feature indices.
}
\author{
	Kyle D. Bemis
}
\seealso{
    \code{\link[Cardinal]{PLS}},
    \code{\link[Cardinal]{OPLS}},
    \code{\link[Cardinal]{spatialShrunkenCentroids}}
}
\examples{
## Create an MSImageSet object
spectra <- matrix(1:27, nrow=3)
mz <- 101:103
coord <- expand.grid(x=1:3, y=1:3)
msset <- MSImageSet(spectra=spectra, mz=mz, coord=coord)

# Find pixel indices
pixels(msset, x=2, y=2)
pixels(msset, coord=list(x=2, y=2))
pixels(msset, coord=list(x=c(2,3), y=c(2,3)))

# Find feature indices
features(msset, mz=102)
features(msset, mz=c(101,103))
features(msset, mz=c(102.2))
}
\keyword{methods}
