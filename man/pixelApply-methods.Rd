\name{pixelApply-methods}
\docType{methods}

\alias{pixelApply}
\alias{pixelApply-methods}
\alias{pixelApply,SImageSet-method}
\alias{pixelApply,SparseImagingExperiment-method}

\alias{featureApply}
\alias{featureApply-methods}
\alias{featureApply,SImageSet-method}
\alias{featureApply,SparseImagingExperiment-method}

\title{Apply functions over imaging datasets}
\description{
	Apply an existing or a user-specified function over either all of the features or all of the pixels of a \code{\linkS4class{SparseImagingExperiment}} or \code{\linkS4class{SImageSet}}. These are provided by analogy to the 'apply' family of functions, but allowing greater efficiency and convenience when applying functions over an imaging dataset.
}
\usage{
\S4method{pixelApply}{SparseImagingExperiment}(.object, .fun, \dots,
    .blocks = FALSE,
    .simplify = TRUE,
    .use.names = TRUE,
    .outpath = NULL,
    BPREDO = list(),
    BPPARAM = bpparam())

\S4method{featureApply}{SparseImagingExperiment}(.object, .fun, \dots,
    .blocks = FALSE,
    .simplify = TRUE,
    .use.names = TRUE,
    .outpath = NULL,
    BPREDO = list(),
    BPPARAM = bpparam())

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
    \item{.object}{An imaging dataset.}
    \item{.fun}{The function to be applied.}
    \item{\dots}{Additional arguments passed to \code{.fun}.}
    \item{.blocks}{If FALSE (the default), each feature-vector or image-vector will be loaded and processed individually. If TRUE, or a positive integer, the data will be split into that many blocks, and the function (specified by \code{.fun}) will be applied to each block. The number of blocks can be specified as a number, or \code{getOption("Cardinal.nblocks")} will be used.}
    \item{.simplify}{If applying over blocks, then a function to be used to simplify the list of results. Otherwise, a logical value giving whether the results should be simplified into a matrix or array rather than a list.}
    \item{.use.names}{Should the names of elements of \code{.object} (pixels or features, as appropriate) be used for the names of the result?}
    \item{.outpath}{The path to a file where the output data will be written. Results will be kept in-memory if this is NULL. Results will be coerced to a numeric vector before being written to file.}
    \item{BPREDO}{See documentation for \code{\link{bplapply}}.}
    \item{BPPARAM}{An optional instance of \code{BiocParallelParam}. See documentation for \code{\link{bplapply}}.}
    \item{.pixel}{A subset of pixels to use, given by an \code{integer} vector of numeric indices, a \code{character} vector of pixel names, or a \code{logical} vector indicating which pixels to use.}
    \item{.feature}{A subset of features to use, given in the same manner as pixels.}
    \item{.pixel.groups}{A grouping \code{factor} or a vector that can be coerced into a factor, that indicates groups of pixels over which the function should be applied. Groups pixels are treated as cells in a ragged array, by analogy to the \code{\link{tapply}} function.}
    \item{.feature.groups}{A grouping \code{factor} features, in the same manner as for pixels.}
    \item{.pixel.dependencies}{Not currently used. This may be used in the future to allow caching when applying functions to data on disk.}
    \item{.feature.dependencies}{Not currently used. May be used for caching in the future.}
    \item{.verbose}{Used for debugging. Currently ignored.}
}
\details{

    !! FOR \code{\linkS4class{SparseImagingExperiment}} CLASSES !!

    For \code{pixelApply}, the function is applied to the feature vector(s) belonging to pixel(s).

    For \code{featureApply}, the function is applied to the vector(s) of intensity values (i.e., the flattened image) corresponding to the feature(s).

    If \code{.blocks} is provided (either TRUE or a positive integer), then the data is split into blocks beforehand, and entire blocks are loaded and passed to the function as a matrix of column-vectors. Otherwise, single vectors are passed to the function individually.

    Additionally, the following attributes are assigned to the object passed to \code{.fun}, accessible via \code{attr()}:

    \itemize{
        \item{\code{idx}: The indices of the current pixel(s) or feature(s).}
        \item{\code{mcols}: Either the \code{pixelData} for \code{featureApply} or the \code{featureData} for \code{pixelApply}.}
    }

    !! FOR \code{\linkS4class{SImageSet}} CLASSES !!

    The use of \code{.pixel} and \code{.feature} can be used to apply the function over only a subset of pixels or features (or both), allowing faster computation when calculation on only a subset of data is needed.

    For \code{pixelApply}, the function is applied to the feature vector belonging to each pixel. The use of \code{.feature.groups} allows code{\link{tapply}}-like functionality on the feature vectors, applied separately to each pixel.

    For \code{featureApply}, the function is applied to the vector of intensity values (i.e., the flattened image) corresponding to each feature. The use of \code{.feature.groups} allows code{\link{tapply}}-like functionality on the flattened image intensity vectors, applied separately to each feature.

    The \code{fData} from \code{.object} is installed into the environment of \code{.fun} for \code{pixelApply}, and the \code{pData} from \code{.object} is installed into the environment of \code{.fun} for \code{featureApply}. This allows access to the symbols from \code{fData} or \code{pData} during the execution of \code{.fun}. If \code{.fun} already has an environment, it is retained as the parent of the installed environment.

    Additionally, the following objects are made available by installing them into the \code{.fun} environment:

    \itemize{
        \item{\code{.Object}: The passed \code{.object}. (Note the case.)}
        \item{\code{.Index}: The index of the current iteration.}
    }
}
\value{
    If \code{.simplify = FALSE}, a list. Otherwise, a matrix, or a higher-dimensional array if grouping is specified.
}
\author{
	Kylie A. Bemis
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
\keyword{manip}

