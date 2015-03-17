\name{mz-methods}
\docType{methods}

\alias{mz}
\alias{mz<-}
\alias{mz-methods}

\title{Retrieve m/z-values from MSImageSets}
\description{
	This generic function accesses m/z values from \code{\linkS4class{MSImageSet}} objects.
}
\usage{
mz(object, \dots)
mz(object) <- value
}
\arguments{
    \item{object}{An \code{MSImageSet} object.}
    \item{value}{Value to be assigned to the corresponding object.}
    \item{\dots}{Additional arguments (ignored).}
}
\value{
    \code{mz} returns a numeric vector of m/z values.
}
\author{
	Kyle D. Bemis
}
\seealso{
    \code{\linkS4class{MSImageSet}}
}
\keyword{manip}
