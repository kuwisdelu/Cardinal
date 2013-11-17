\name{intensities-methods}
\docType{methods}
\alias{intensities}
\alias{intensities-methods}
\alias{intensities,MSImageSet-method}
\title{Mass spectral intensities}
\description{
	Retrives the spectral intensities an \code{\linkS4class{MSImageSet}} dataset and reconstructs them into a data hypercube.
}
\usage{
    \S4method{intensities}{MSImageSet}(object, pixel, coord, mz, feature,
    drop=FALSE)
}
\arguments{
    \item{object}{\code{\linkS4class{MSImageSet}} data.}
    \item{pixel}{specifies the pixels from which to retrieve intensities.}
    \item{coord}{specifies the coordinates for the pixels from which to retrive intensities; this can be a named vector, \code{list}, or a \code{data.frame}.}
    \item{mz}{specifies the limits of the masses; this must be a \code{numeric} with either one value or two values indicating the range.}
    \item{feature}{specifies the features to retrieve; this must be an \code{integer} vector giving all of the features at which to return intensities}
    \item{drop}{\code{logical}, indicating whether the result should be dropped from an \code{array} to a \code{matrix}, or from a \code{matrix} to a \code{numeric} vector.}
    \item{\dots}{ignored.}
}
\details{
    The primary purpose of this function is to reconstruct the datacube of the original mass spectrometry imaging data. Not specifying a value for either 'pixel' or 'coord' will assume all pixels. Not specifying a value for either 'mz' or 'feature' will assume all features.

   By default, 'drop' is set to \code{FALSE} and will yield an \code{array} with the first dimension as the features, and all subsequent dimensions as the spatial coordinates. If pixels are specified in such a way that the results cannot be represented as an \code{array}, e.g., if they are non-contiguous in the spatial dimensions, a datacube enclosing the requested pixels will be returned.

    If 'drop' is set to \code{TRUE}, a \code{matrix} will be returned with rows as features and columns as pixels. This will be dropped to a \code{numeric} if either dimension of the resulting \code{matrix} would be 1.
}
\value{
    A \code{numeric}, \code{matrix}, or an \code{array}, depending on the value of 'drop', with the mass spectral intensities.
}
\author{
	Kyle D. Bemis
}
\seealso{
    \code{\linkS4class{MSImageSet}}
}
\examples{
    ## write examples to put here
}
\keyword{methods}

