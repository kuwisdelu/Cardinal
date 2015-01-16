\name{standardizeSamples-methods}
\docType{methods}

\alias{standardizeSamples}
\alias{standardizeSamples-methods}
\alias{standardizeSamples,MSImageSet-method}

\alias{standardizeSamples.sum}

\title{Standardize the Samples in an Imaging Dataset}
\description{
	Apply standardization across the samples in a mass spectrometry imaging dataset to correct for between-sample variation.
}
\usage{
\S4method{standardizeSamples}{MSImageSet}(object, method = "sum", \dots)

## TIC normalization
standardizeSamples.sum(x, sum=length(x), \dots)
}
\arguments{
    \item{object}{An object of class \code{\linkS4class{MSImageSet}}.}
    \item{method}{The standardization method to use.}
    \item{\dots}{Additional arguments passed to the standardization method.}
    \item{x}{The flattened ion image to be standardized.}
    \item{sum}{The value to which to standardize the sum of the ion image intensity values.}
}
\details{
    Standardization is usually performed using the provided functions, but a user-created function can also be passed to \code{method}. In this case it should take the following arguments:

    \itemize{
        \item{\code{x}: A \code{numeric} vector of intensities.}
        \item{\code{\dots}: Additional arguments.}
    }

    A user-created function should return a \code{numeric} vector of the same length.

    Internally, \code{\link[Cardinal]{featureApply}} is used to apply the standardization, with \code{.pixel.groups=sample}. See its documentation page for more details on additional objects available to the environment installed to the standardization function.
}
\value{
    An object of class \code{\linkS4class{MSImageSet}} with the ion images standardized across samples.
}
\author{
	Kyle D. Bemis
}
\seealso{
    \code{\linkS4class{MSImageSet}},
    \code{\link[Cardinal]{featureApply}}
}
\examples{
data1 <- generateImage(as="MSImageSet")
data2 <- generateImage(as="MSImageSet")
sampleNames(data2) <- "2"
data3 <- combine(data1, data2)
standardizeSamples(data3, method="sum")
}
\keyword{methods}
