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

\alias{spatialApply}
\alias{spatialApply-methods}
\alias{spatialApply,SparseImagingExperiment-method}

\title{Apply functions over imaging datasets}
\description{
	Apply an existing or a user-specified function over either all of the features or all of the pixels of a \code{\linkS4class{SparseImagingExperiment}} or \code{\linkS4class{SImageSet}}. These are provided by analogy to the 'apply' family of functions, but allowing greater efficiency and convenience when applying functions over an imaging dataset.
}
\usage{
#### Methods for Cardinal >= 2.x classes ####

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

\S4method{spatialApply}{SparseImagingExperiment}(.object, .r, .fun, \dots,
    .dist = "chebyshev",
    .blocks = FALSE,
    .simplify = TRUE,
    .use.names = TRUE,
    .outpath = NULL,
    .params = NULL,
    .init = NULL,
    BPREDO = list(),
    BPPARAM = bpparam())

#### Methods for Cardinal 1.x classes ####

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
    \item{.r}{The maximum spatial radius or distance for which pixels are considered to be neighbors.}
    \item{\dots}{Additional arguments passed to \code{.fun}.}
    \item{.dist}{The type of distance metric to use when calculating neighboring pixels based on \code{r}. The options are `radial', `manhattan', `minkowski', and `chebyshev' (the default).}
    \item{.blocks}{If FALSE (the default), each feature-vector or image-vector will be loaded and processed individually. If TRUE, or a positive integer, the data will be split into that many blocks, and the function (specified by \code{.fun}) will be applied to each block. The number of blocks can be specified as a number, or \code{getOption("Cardinal.nblocks")} will be used.}
    \item{.simplify}{If applying over blocks, then a function to be used to simplify the list of results. Otherwise, a logical value giving whether the results should be simplified into a matrix or array rather than a list.}
    \item{.use.names}{Should the names of elements of \code{.object} (either pixel names or feature names, as appropriate) be used for the names of the result?}
    \item{.outpath}{The path to a file where the output data will be written. Results will be kept in-memory if this is NULL. Results will be coerced to a numeric vector before being written to file.}
    \item{.params}{A list of parameters with length equal to \code{length(.object)}. The appropriate elements will be made available to the spatial neighborhoods.}
    \item{.init}{The initialization for the spatial neighbors and spatial blocks. If TRUE, then the result will have an attribute \code{attr(ans, "init")} that can be used as input to \code{.init} in future calls to avoid re-calculating spatial neighbors and spatial blocks.}
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

    #### For \code{\linkS4class{SparseImagingExperiment}}-derived classes ####

    For \code{pixelApply}, the function is applied to the feature vector(s) belonging to pixel(s).

    For \code{featureApply}, the function is applied to the vector(s) of intensity values (i.e., the flattened image) corresponding to the feature(s).

    For \code{spatialApply}, the function is applied to neighborhoods of feature-vectors corresponding to neighboring pixels. The maximum distance in each dimension for a pixel to be considered a neighbor is given by \code{.r}. The first argument to \code{.fun} is a matrix of column-vectors.

    If \code{.blocks} is provided (either TRUE or a positive integer), then the data is split into blocks beforehand, and entire blocks are loaded and passed to the function as a matrix of column-vectors. Otherwise, single vectors are passed to the function individually. If blocks are used, then \code{.simplify} should be a function that simplifies a \code{list} of results. Note that for \code{spatialApply}, if blocks are used, the result is NOT guaranteed to be in the correct order; instead the result will have a \code{attr(ans, "idx")} attribute giving the proper order (pixel IDs) of the results, and the \code{.simplify} function should likely re-order the results.

    The following attributes are assigned to the object passed to \code{.fun}, accessible via \code{attr()}:

    \itemize{
        \item{\code{idx}: The indices of the current pixel(s) or feature(s).}
        \item{\code{mcols}: Either the \code{pixelData} for \code{featureApply} or the \code{featureData} for \code{pixelApply}.}
    }

    Additionally, the following attributes are made available during a call to \code{spatialyApply()}:

    \itemize{
        \item{\code{centers}: A vector indicating which column(s) should be considered the center(s) of the neighborhood(s).}
        \item{\code{neighbors}: A list of vectors indicating which column(s) should be considered the neighborhoods. Only relevant if using \code{.blocks}.}
        \item{\code{offsets}: A matrix where the rows are the spatial offsets of the pixels in the neighborhood(s) from the center pixel(s).}
        \item{\code{params}: The relevant list elements of the \code{.params} argument.}
    }

    #### For \code{\linkS4class{SImageSet}}-derived classes ####

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
    If \code{.simplify = FALSE}, a list. Otherwise, a vector or matrix, or a higher-dimensional array if grouping is specified, or the output of the provided \code{.simplify} function.
}
\author{
	Kylie A. Bemis
}
\seealso{
    \code{\linkS4class{MSImagingExperiment}},
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

