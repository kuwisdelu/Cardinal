\name{mz-methods}
\docType{methods}

\alias{mz}
\alias{mz<-}
\alias{mz-methods}

\alias{mz,missing-method}

\title{Manipulate mass-to-charge-ratio values}

\description{
    This is a generic function for getting or setting 'mz' for an object with associated m/z values, or for generating a sequence of appropriate m/z values for such an object.
}

\usage{
mz(object, \dots)

mz(object) <- value

\S4method{mz}{missing}(from, to, by = 400, units = c("ppm", "mz"), \dots)
}

\arguments{
    \item{object}{An object with m/z values.}
    
    \item{value}{The value to set the m/z values.}

    \item{from, to}{The starting amd (maximal) end values of the sequence of m/z values.}

    \item{by}{The (approximate) interval between m/z values. For \code{units="ppm"}, rather than an exact step size, this actually corresponds to a binwidth, where each element of the sequence is considered the center of a bin.}

	\item{units}{The units for \code{by}. Either parts-per-million or absolute m/z increments.}

	\item{\dots}{Additional arguments (ignored).}
}

\author{Kylie A. Bemis}

\seealso{
    \code{\linkS4class{MassDataFrame}}
}

\examples{
mz(from=200, to=220, by=300, units="ppm")
}

\keyword{methods}
