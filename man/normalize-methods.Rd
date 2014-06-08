\name{normalize-methods}
\docType{methods}

\alias{normalize}
\alias{normalize-methods}
\alias{normalize,SImageSet-method}

\alias{normalize.tic}

\title{Normalize an Imaging Dataset}
\description{
	Apply normalization over the feature vectors of an imaging dataset.
}
\usage{
\S4method{normalize}{MSImageSet}(object, method = "tic",
	\dots,
	pixel=pixels(object),
	plot=TRUE)

## TIC normalization
normalize.tic(x, tic=length(x), \dots)
}
\arguments{
    \item{object}{An object of class \code{\linkS4class{MSImageSet}}.}
    \item{method}{The normalization method to use.}
    \item{\dots}{Additional arguments passed to the normalization method.}
    \item{pixel}{The pixels to normalize. This will result in a subset of the data it specified.}
    \item{plot}{Plot the mass spectrum for each pixel while it is being processed.}
    \item{tic}{The value to which to normalize the total ion current.}
}
\details{
    Internally, additional arguments are passed to \code{\link[Cardinal]{pixelApply}} first before the normalization function is applied.
}
\value{
    An object of class \code{\linkS4class{MSImageSet}} with the normalized spectra.
}
\author{
	Kyle D. Bemis
}
\seealso{
    \code{\linkS4class{MSImageSet}}
}
\examples{
data <- generateImage(as="MSImageSet")
normalize(data, method="tic", plot=interactive())
}
\keyword{methods}
