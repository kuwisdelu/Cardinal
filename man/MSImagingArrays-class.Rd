\name{MSImagingArrays-class}
\docType{class}

\alias{class:MSImagingArrays}
\alias{MSImagingArrays}
\alias{MSImagingArrays-class}

\alias{show,MSImagingArrays-method}
\alias{mz,MSImagingArrays-method}
\alias{mz<-,MSImagingArrays-method}
\alias{intensity,MSImagingArrays-method}
\alias{intensity<-,MSImagingArrays-method}
\alias{c,MSImagingArrays-method}

\title{MSImagingArrays: MS imaging data with arbitrary m/z values}

\description{
   The \code{MSImagingArrays} class provides a list-like container for high-throughput mass spectrometry imaging data where every mass spectrum may have its own m/z values. It is designed for easy access to raw mass spectra for the purposes of pre-processing.

   It can be converted to a \code{\linkS4class{MSImagingExperiment}} object for easier image slicing and for applying statistical models and machine learning methods.
}

\usage{
## Instance creation
MSImagingArrays(spectraData = SimpleList(),
    pixelData = PositionDataFrame(), experimentData = NULL,
    centroided = NA, continuous = NA, metadata = list())

## Additional methods documented below
}

\arguments{
    \item{spectraData}{Either a list-like object with lists of individual spectra and lists of their domain values, or a \code{\linkS4class{SpectraArrays}} instance.}

    \item{pixelData}{A \code{\linkS4class{PositionDataFrame}} with pixel metadata, with a row for each spectrum.}

    \item{experimentData}{Either NULL or a \code{\linkS4class{ImzMeta}} object with MS-specific experiment-level metadata.}

    \item{centroided}{A logical value indicated whether the spectra have been centroided.}

    \item{continuous}{A logical value indicated whether the spectra all have the same m/z values.}

    \item{metadata}{A list of arbitrary metadata.}
}

\section{Slots}{
    \describe{
        \item{\code{spectraData}:}{A \code{\linkS4class{SpectraArrays}} object storing one or more array-like data elements with conformable dimensions.}

        \item{\code{elementMetadata}:}{A \code{\linkS4class{PositionDataFrame}} containing spectrum-level metadata, including each spectrum's pixel coordinates and experimental run information.}

        \item{\code{processing}:}{A list containing unexecuted \code{\linkS4class{ProcessingStep}} objects.}

        \item{\code{experimentData}:}{Either NULL or an \code{\linkS4class{ImzMeta}} object containing experiment-level metadata (necessary for writing the data to imzML).}

        \item{\code{centroided}:}{A logical value indicated whether the spectra have been centroided (if known).}

        \item{\code{continuous}:}{A logical value indicated whether the spectra all have the same m/z values (if known).}
    }
}

\section{Methods}{
    All methods for \code{\linkS4class{SpectralImagingData}} and \code{\linkS4class{SpectralImagingArrays}} also work on \code{MSImagingArrays} objects. Additional methods are documented below:

    \describe{
        \item{\code{mz(object, i = NULL, ...)}, \code{mz(object, i = NULL, ...) <- value}:}{Get or set the m/z arrays in the \code{spectraData} slot.}

        \item{\code{intensity(object, i = NULL, ...)}, \code{intensity(object, i = NULL, ...) <- value}:}{Get or set the intensity arrays in the \code{spectraData} slot.}

        \item{\code{centroided(object, ...)}, \code{centroided(object, ...) <- value}:}{Get or set the \code{centroided} slot.}

        \item{\code{isCentroided(object, ...)}:}{Equivalent to \code{isTRUE(centroided(object))}.}

        \item{\code{experimentData(object)}, \code{experimentData(object) <- value}:}{Get or set the \code{experimentData} slot.}
    }
}

\author{Kylie A. Bemis}

\seealso{
    \code{\linkS4class{SpectralImagingArrays}},
    \code{\linkS4class{MSImagingExperiment}}
}

\examples{
set.seed(1, kind="L'Ecuyer-CMRG")
x <- replicate(9, rlnorm(10), simplify=FALSE)
mz <- replicate(9, 500 * sort(runif(10)), simplify=FALSE)
coord <- expand.grid(x=1:3, y=1:3)

msa <- MSImagingArrays(
    spectraData=list(intensity=x, mz=mz),
    pixelData=PositionDataFrame(coord))

print(msa)
}

\keyword{classes}
