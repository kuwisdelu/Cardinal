\name{normalize-methods}
\docType{methods}

\alias{normalize}
\alias{normalize-methods}
\alias{normalize,MSImageSet-method}

\alias{normalize.tic}

\title{Normalize an Imaging Dataset}
\description{
	Apply normalization to a mass spectrometry imaging dataset.
}
\usage{
\S4method{normalize}{MSImageSet}(object, method = "tic",
	\dots,
	pixel = pixels(object),
	plot = FALSE)

## TIC normalization
normalize.tic(x, tic=length(x), \dots)
}
\arguments{
    \item{object}{An object of class \code{\linkS4class{MSImageSet}}.}
    \item{method}{The normalization method to use.}
    \item{pixel}{The pixels to normalize. If less than the extent of the dataset, this will result in a subset of the data being processed.}
    \item{plot}{Plot the mass spectrum for each pixel while it is being processed?}
    \item{\dots}{Additional arguments passed to the normalization method.}
    \item{x}{The mass spectrum to be normalized.}
    \item{tic}{The value to which to normalize the total ion current.}
}
\details{
    Normalization is usually performed using the provided functions, but a user-created function can also be passed to \code{method}. In this case it should take the following arguments:

    \itemize{
        \item{\code{x}: A \code{numeric} vector of intensities.}
        \item{\code{\dots}: Additional arguments.}
    }

    A user-created function should return a \code{numeric} vector of the same length.

    Internally, \code{\link[Cardinal]{pixelApply}} is used to apply the normalization. See its documentation page for more details on additional objects available to the environment installed to the normalization function.
}
\value{
    An object of class \code{\linkS4class{MSImageSet}} with the normalized spectra.
}
\author{
	Kyle D. Bemis
}
\seealso{
    \code{\linkS4class{MSImageSet}},
    \code{\link[Cardinal]{pixelApply}}
}
\examples{
data <- generateImage(as="MSImageSet")
normalize(data, method="tic", plot=interactive())
}
\keyword{methods}
