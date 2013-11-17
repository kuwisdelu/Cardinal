\name{unload-methods}
\docType{methods}
\alias{unload}
\alias{unload-methods}
\alias{unload,MSImageSet-method}
\title{Unload an MS imaging dataset}
\description{
	Fully removes an \code{\linkS4class{MSImageSet}} from the environment and frees its memory; if removing a redundant reference to another \code{\linkS4class{MSImageSet}} that should remain loaded, use \code{\link[base]{rm}} or \code{\link[base]{remove}} instead, otherwise it will become unusable.
}
\usage{
    \S4method{unload}{MSImageSet}(object, \dots)
}
\arguments{
    \item{object}{\code{\linkS4class{MSImageSet}} object.}
    \item{\dots}{additional \code{\linkS4class{MSImageSet}} objects to unload.}
}
\details{
	This should especially be used when removing an \code{\linkS4class{MSImageSet}} object from the user environment, otherwise memory may not be freed.
}
\value{
	Invisibly returns \code{TRUE} if all objects are unloaded successfully.
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

