\name{pixelApply-methods}
\docType{methods}

\alias{pixelApply}
\alias{pixelApply-methods}
\alias{pixelApply,SImageSet-method}

\alias{featureApply}
\alias{featureApply-methods}
\alias{featureApply,SImageSet-method}

\title{Apply Functions over Imaging Datasets}
\description{
	Apply an existing or a user-specified function over either all of the features or all of the pixels of an \code{\linkS4class{SImageSet}}. These are provided for convenience by analogy to the 'apply' family of functions, but allowing greater control over how the functions are applied over an imaging dataset.
}
\usage{
\S4method{pixelApply}{SImageSet}(.object, .fun, \dots,
    .pixel,
    .feature,
    .feature.groups,
    .pixel.dependencies,
    .simplify = TRUE,
    .use.names = TRUE,
    .verbose = FALSE)

\S4method{featureApply}{SImageSet}(.object, .fun, \dots,
    .feature,
    .pixel,
    .pixel.groups,
    .feature.dependencies,
    .simplify = TRUE,
    .use.names = TRUE,
    .verbose = FALSE)
}
\arguments{
    \item{.object}{an object of class \code{\linkS4class{SImageSet}}.}
    \item{.fun}{the function to be applied.}
    \item{\dots}{additional arguments passed to \code{.fun}.}
    \item{.pixel}{a subset of pixels to use, given by an \code{integer} vector of numeric indices, a \code{character} vector of pixel names, or a \code{logical} vector indicating which pixels to use.}
    \item{.feature}{a subset of features to use, given in the same manner as pixels.}
    \item{.pixel.groups}{a grouping \code{factor} or a vector that can be coerced into a factor, that indicates groups of pixels over which the function should be applied. Groups pixels are treated as cells in a ragged array, by analogy to the \code{\link[base]{tapply}} function.}
    \item{.feature.groups}{a grouping \code{factor} features, in the same manner as for pixels.}
    \item{.pixel.dependencies}{Not currently used. This may be used in the future to allow caching when applying functions to data on disk.}
    \item{.feature.dependencies}{Not currently used. May be used for caching in the future.}
    \item{.simplify}{Should the result be simplified into a matrix of higher-dimensional array rather than a list, if appropriate?}
    \item{.use.names}{Should the names of elements of \code{.object} (pixels, features, and grouping variables, as appropriate) be used for the names of the result?}
    \item{.verbose}{Used for debugging. Currently ignored.}
    
}
\details{
    The use of \code{.pixel} and \code{.feature} can be used to apply the function over only a subset of pixels or features (or both), allowing faster computation when calculation on only a subset of data is needed.

    For \code{pixelApply}, the function is applied to the feature vector belonging to each pixel. The use of \code{.feature.groups} allows code{\link[base]{tapply}}-like functionality on the feature vectors, applied separately to each pixel.

    For \code{featureApply}, the function is applied to the vector of intensity values (i.e., the flattened image) corresponding to each feature. The use of \code{.feature.groups} allows code{\link[base]{tapply}}-like functionality on the flattened image intensity vectors, applied separately to each feature.

    An important aspect of these functions is that the corresponding row of \code{pixelData} or \code{featureData} is passed as an attribute of the argument of \code{.fun}. This allows, for example, the use of pixel coordinates in the function when using \code{pixelApply}, by accessing the argument's attributes.

    Note that for \code{pixelApply}, arguments are evaluated in an environment that can access the symbol names in \code{featureData}, and for \code{featureApply}, arguments are evaluated in an environment that can access the symbol names in \code{pixelData}. (This may seem backward, but it allows easier reference to the more appropriate names inside \code{.fun}.)

    It is expected that these methods will be expanded in the future for different types of imaging datasets (e.g., data read directly from disk).
}
\value{
    If \code{.simplify = FALSE}, a list. Otherwise, a matrix, or a higher-dimensional array if grouping is specified.
}
\author{
	Kyle D. Bemis
}
\seealso{
    \code{\linkS4class{MSImageSet}}
}
\examples{
data <- matrix(1:256, nrow=4)
coord <- expand.grid(x=1:4, y=1:4, z=1:4)
sset <- SImageSet(data=data, coord=coord)

fData(sset)$flag <- rep(c(TRUE, FALSE), 2)
pixelApply(sset, max, .feature.groups=flag)

pData(sset)$flag <- rep(c(TRUE, FALSE), 32)
featureApply(sset, max, .pixel.groups=flag)
}
\keyword{methods}

