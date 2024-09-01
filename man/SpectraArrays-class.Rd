\name{SpectraArrays-class}
\docType{class}

\alias{class:SpectraArrays}
\alias{SpectraArrays}
\alias{SpectraArrays-class}

\alias{show,SpectraArrays-method}
\alias{vm_used,SpectraArrays-method}
\alias{fetch,SpectraArrays-method}
\alias{flash,SpectraArrays-method}
\alias{[,SpectraArrays,ANY,ANY,ANY-method}
\alias{[<-,SpectraArrays,ANY,ANY,ANY-method}
\alias{$,SpectraArrays-method}
\alias{$<-,SpectraArrays-method}
\alias{[[,SpectraArrays-method}
\alias{[[<-,SpectraArrays-method}
\alias{names,SpectraArrays-method}
\alias{names<-,SpectraArrays-method}
\alias{length,SpectraArrays-method}
\alias{dim,SpectraArrays-method}
\alias{cbind,SpectraArrays-method}
\alias{rbind,SpectraArrays-method}
\alias{combine,SpectraArrays,ANY-method}
\alias{c,SpectraArrays-method}
\alias{coerce,SpectraArrays,list-method}
\alias{coerce,list,SpectraArrays-method}
\alias{coerce,SpectraArrays,SimpleList-method}
\alias{coerce,SimpleList,SpectraArrays-method}

\title{SpectraArrays: List of spectra arrays}

\description{
   The \code{SpectraArrays} class provides a list-like container for spectra arrays of conformable dimensions.
}

\usage{
## Instance creation
SpectraArrays(arrays = SimpleList())

## Additional methods documented below
}

\arguments{
    \item{arrays}{A list of arrays.}
}

\details{
    The \code{SpectraArrays} class is intended to be flexible and the arrays do not need to be "array-like" (i.e., have non-NULL \code{dim()}.) One dimensional arrays and lists are allowd. Every array must have the same \code{NROW()} and \code{NCOL()}.

    It supports lossless coercion to and from \code{\linkS4class{SimpleList}}.
}

\section{Methods}{
    \describe{
        \item{\code{length(object)}:}{Get the number of spectra in the object.}

        \item{\code{names(object)}, \code{names(object) <- value}:}{Get or set the names of spectra arrays in the object.}

        \item{\code{object[[i]]}, \code{object[[i]] <- value}:}{Get or set an array in the object.}

        \item{\code{object[i, j, ..., drop]}:}{Subset as a list or array, depending on the number of dimensions of the stored spectra arrays. The result is the same class as the original object.}

        \item{\code{rbind(...)}, \code{cbind(...)}:}{Combine \code{SpectraArrays} objects by row or column.}

        \item{\code{c(...)}:}{Combine \code{SpectraArrays} objects as lists.}

        \item{\code{fetch(object, \dots)}:}{Pull data arrays into shared memory.}

        \item{\code{flash(object, \dots)}:}{Push data arrays to a temporary file.}
    }
}

\author{Kylie A. Bemis}

\seealso{
    \code{\linkS4class{SpectralImagingData}},
    \code{\linkS4class{MSImagingArrays}}
}

\examples{
set.seed(1, kind="L'Ecuyer-CMRG")
x <- matrix(rlnorm(128), nrow=16, ncol=8)
y <- matrix(rlnorm(128), nrow=16, ncol=8)

s <- SpectraArrays(list(x=x, y=y))

print(s)
}

\keyword{classes}
