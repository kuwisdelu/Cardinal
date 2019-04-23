\name{normalize-methods}
\docType{methods}

\alias{normalize}
\alias{normalize-methods}
\alias{normalize,MSImageSet-method}
\alias{normalize,SparseImagingExperiment-method}

\alias{normalize.tic}
\alias{normalize.rms}
\alias{normalize.reference}

\title{Normalize an imaging dataset}
\description{
	Apply normalization to the feature vectors of an imaging dataset.
}
\usage{
\S4method{normalize}{SparseImagingExperiment}(object, method = c("tic", "rms", "reference"), \dots)

\S4method{normalize}{MSImageSet}(object, method = "tic",
	\dots,
	pixel = pixels(object),
	plot = FALSE)

## Totial-ion-current normalization
normalize.tic(x, tic=length(x), \dots)

## Root-mean-square normalization
normalize.rms(x, rms=1, \dots)

## Reference normalization
normalize.reference(x, feature, scale=1, \dots)
}
\arguments{
    \item{object}{An imaging dataset.}
    \item{method}{The normalization method to use.}
    \item{pixel}{The pixels to normalize. If less than the extent of the dataset, this will result in a subset of the data being processed.}
    \item{plot}{Plot each pixel while it is being processed?}
    \item{\dots}{Additional arguments passed to the normalization method.}
    \item{x}{The signal to be normalized.}
    \item{tic}{The value to which to normalize the total ion current.}
    \item{rms}{The value to which to normalize the root-mean-square.}
    \item{feature}{The feature to use as a reference for normalization.}
    \item{scale}{The value to which to normalize the reference.}
}
\details{
    Normalization is usually performed using the provided functions, but a user-created function can also be passed to \code{method}. In this case it should take the following arguments:

    \itemize{
        \item{\code{x}: A \code{numeric} vector of intensities.}
        \item{\code{\dots}: Additional arguments.}
    }

    A user-created function should return a \code{numeric} vector of the same length.

    Internally, \code{\link{pixelApply}} is used to apply the normalization. See its documentation page for more details on additional objects available to the environment installed to the normalization function.
}
\value{
    An object of the same class with the normalized spectra.
}
\author{
	Kylie A. Bemis
}
\seealso{
    \code{\linkS4class{MSImagingExperiment}},
    \code{\linkS4class{MSImageSet}},
    \code{\link{pixelApply}},
    \code{\link{process}}
}
\examples{
register(SerialParam())

set.seed(2)
data <- simulateImage(preset=1, npeaks=10, dim=c(3,3))
data <- data[,pData(data)$circle]

# queue normalization
data <- normalize(data, method="tic")

# apply normalization
data_normalized <- process(data)
}
\keyword{methods}
