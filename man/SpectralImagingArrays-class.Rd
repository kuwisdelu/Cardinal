\name{SpectralImagingArrays-class}
\docType{class}

\alias{class:SpectralImagingArrays}
\alias{SpectralImagingArrays}
\alias{SpectralImagingArrays-class}

\alias{show,SpectralImagingArrays-method}
\alias{names,SpectralImagingArrays-method}
\alias{names<-,SpectralImagingArrays-method}
\alias{length,SpectralImagingArrays-method}
\alias{[,SpectralImagingArrays,ANY,ANY,ANY-method}
\alias{[<-,SpectralImagingArrays,ANY,ANY,ANY-method}
\alias{dim,SpectralImagingArrays-method}
\alias{cbind,SpectralImagingArrays-method}
\alias{rbind,SpectralImagingArrays-method}
\alias{combine,SpectralImagingArrays,ANY-method}
\alias{c,SpectralImagingArrays-method}

\title{SpectralImagingArrays: Spectral imaging data with arbitrary domain}

\description{
   The \code{SpectralImagingArrays} class provides a list-like container for high-dimensional spectral imaging data where every spectrum may have its own domain values. It is designed to provide easy access to raw individual spectra, but images cannot be easily reconstructed.

   The \code{\linkS4class{MSImagingArrays}} class extends \code{SpectralImagingArrays} for mass spectrometry-based imaging experiments with unaligned mass features.
}

\usage{
## Instance creation
SpectralImagingArrays(spectraData = SimpleList(),
    pixelData = PositionDataFrame(), metadata = list())

## Additional methods documented below
}

\arguments{
    \item{spectraData}{Either a list-like object with lists of individual spectra and lists of their domain values, or a \code{\linkS4class{SpectraArrays}} instance.}

    \item{pixelData}{A \code{\linkS4class{PositionDataFrame}} with pixel metadata, with a row for each spectrum.}

    \item{metadata}{A \code{list} with experimental-level metadata.}
}

\section{Slots}{
    \describe{
        \item{\code{spectraData}:}{A \code{\linkS4class{SpectraArrays}} object storing one or more array-like data elements with conformable dimensions.}

        \item{\code{elementMetadata}:}{A \code{\linkS4class{PositionDataFrame}} containing spectrum-level metadata, including each spectrum's pixel coordinates and experimental run information.}

        \item{\code{processing}:}{A list containing unexecuted \code{\linkS4class{ProcessingStep}} objects.}
    }
}

\section{Methods}{
    All methods for \code{\linkS4class{SpectralImagingData}} also work on \code{SpectralImagingArrays} objects. Additional methods are documented below:

    \describe{
        \item{\code{length(object)}:}{Get the number of spectra in the object.}

        \item{\code{object[i, ..., drop]}:}{Subset as a list based on the spectra. The result is the same class as the original object.}

        \item{\code{rbind(...)}, \code{cbind(...)}:}{Combine \code{SpectralImagingArrays} objects by row or column.}
    }
}

\author{Kylie A. Bemis}

\seealso{
    \code{\linkS4class{SpectralImagingData}},
    \code{\linkS4class{MSImagingArrays}}
}

\examples{
set.seed(1, kind="L'Ecuyer-CMRG")
x <- replicate(9, rlnorm(10), simplify=FALSE)
t <- replicate(9, sort(runif(10)), simplify=FALSE)
coord <- expand.grid(x=1:3, y=1:3)

sa <- SpectralImagingArrays(
    spectraData=list(intensity=x, wavelength=t),
    pixelData=PositionDataFrame(coord))

print(sa)
}

\keyword{classes}
