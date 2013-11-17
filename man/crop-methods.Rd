\name{crop-methods}
\docType{methods}
\alias{crop}
\alias{crop-methods}
\alias{crop,MSImageSet-method}
\title{crop}
\description{
	Subsets an \code{\linkS4class{MSImageSet}} object.
}
\usage{
	\S4method{crop}{MSImageSet}(object, pixel, coord, mz, feature, \dots)
}
\arguments{
    \item{object}{\code{\linkS4class{MSImageSet}} data.}
    \item{pixel}{specifies the pixels from which to retrive intensities.}
    \item{coord}{specifies the coordinates for the pixels from which to retrive intensities; this can be a named vector, \code{list}, or a \code{data.frame}.}
    \item{mz}{specifies the limits of the masses; this must be a \code{numeric} with either one value or two values indicating the range.}
    \item{feature}{specifies the features to retrieve; this must be an \code{integer} vector giving all of the features at which to return intensities}
    \item{\dots}{ignored.}
}
\details{
    Not specifying a value for either 'pixel' or 'coord' will assume all pixels. Not specifying a value for either 'mz' or 'feature' will assume all features.
}
\value{
	A new \code{\linkS4class{MSImageSet}} object keeping only the specified pixels and features.
}
\author{
	Kyle D. Bemis
}
\seealso{
	\code{\linkS4class{MSImageSet}}
}
\examples{
	## write examples to add here
}
\keyword{methods}
