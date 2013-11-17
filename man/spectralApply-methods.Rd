\name{spectralApply-methods}
\docType{methods}
\alias{spectralApply}
\alias{spectralApply-methods}
\alias{spectralApply,MSImageSet-method}
\title{Apply Functions over Mass Spectra}
\description{
	Apply a user-specified function over either all of the features or all of the pixels of an \code{\linkS4class{MSImageSet}}.
}
\usage{
    \S4method{spectralApply}{MSImageSet}(object, MARGIN, FUN, \dots)
}
\arguments{
    \item{object}{an object of class \code{\linkS4class{MSImageSet}}.}
    \item{MARGIN}{a single integer giving the margins of the mass spectra to be applied over, where '1' indicates features and '2' indicates pixels.}
    \item{FUN}{the function to be applied.}
    \item{\dots}{additional arguments passed to \code{FUN}.}
}
\value{
    If possible, a \code{\linkS4class{MSImageSet}} with a 'spectra' slot equivalent to the output of the \code{apply} function. This will be the case whenever the user-specified function returns a vector of equal length for all features or all pixels. If this is not the case, a \code{list} is returned with a component 'spectra' which contains the output.
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

