\name{MSImagingExperiment-class}
\docType{class}

\alias{class:MSImagingExperiment}
\alias{MSImagingExperiment}
\alias{MSImagingExperiment-class}

\alias{show,MSImagingExperiment-method}
\alias{mz,missing-method}
\alias{mz,MSImagingExperiment-method}
\alias{mz<-,MSImagingExperiment-method}
\alias{intensity,MSImagingExperiment-method}
\alias{intensity<-,MSImagingExperiment-method}
\alias{centroided,MSImagingExperiment_OR_Arrays-method}
\alias{centroided<-,MSImagingExperiment_OR_Arrays-method}
\alias{isCentroided,MSImagingExperiment_OR_Arrays-method}
\alias{experimentData,MSImagingExperiment_OR_Arrays-method}
\alias{experimentData<-,MSImagingExperiment_OR_Arrays,ANY-method}
\alias{cbind,MSImagingExperiment-method}
\alias{rbind,MSImagingExperiment-method}
\alias{coerce,MSImageSet,MSImagingExperiment-method}

\title{MSImagingExperiment: MS imaging data with shared m/z values}

\description{
   The \code{MSImagingExperiment} class provides a matrix-like container for high-throughput mass spectrometry imaging data where every mass spectrum shares the same m/z values. It is designed to provide easy access to both the spectra (as columns) and sliced images (as rows).

   It can be converted from a \code{\linkS4class{MSImagingArrays}} object which is designed for representing raw mass spectra.
}

\usage{
## Instance creation
MSImagingExperiment(spectraData = SimpleList(),
    featureData = MassDataFrame(), pixelData = PositionDataFrame(),
    experimentData = NULL, centroided = NA, metadata = list())

## Additional methods documented below
}

\arguments{
    \item{spectraData}{Either a matrix-like object with number of rows equal to the number of features and number of columns equal to the number of pixels, a list of such objects, or a \code{\linkS4class{SpectraArrays}} instance.}

    \item{featureData}{A \code{\linkS4class{MassDataFrame}} with feature metadata, with a row for each feature.}

    \item{pixelData}{A \code{\linkS4class{PositionDataFrame}} with pixel metadata, with a row for each spectrum.}

    \item{experimentData}{Either NULL or a \code{\linkS4class{ImzMeta}} object with MS-specific experiment-level metadata.}

    \item{centroided}{A logical value indicated whether the spectra have been centroided.}

    \item{metadata}{A list of arbitrary metadata.}
}

\section{Slots}{
    \describe{
        \item{\code{spectraData}:}{A \code{\linkS4class{SpectraArrays}} object storing one or more array-like data elements with conformable dimensions.}

        \item{\code{featureData}:}{A \code{\linkS4class{MassDataFrame}} containing feature-level metadata.}

        \item{\code{elementMetadata}:}{A \code{\linkS4class{PositionDataFrame}} containing spectrum-level metadata, including each spectrum's pixel coordinates and experimental run information.}

        \item{\code{processing}:}{A list containing unexecuted \code{\linkS4class{ProcessingStep}} objects.}

        \item{\code{experimentData}:}{Either NULL or an \code{\linkS4class{ImzMeta}} object containing experiment-level metadata (necessary for writing the data to imzML).}

        \item{\code{centroided}:}{A logical value indicated whether the spectra have been centroided (if known).}
    }
}

\section{Methods}{
    All methods for \code{\linkS4class{SpectralImagingData}} and \code{\linkS4class{SpectralImagingExperiment}} also work on \code{MSImagingExperiment} objects. Additional methods are documented below:

    \describe{
        \item{\code{mz(object, ...)}, \code{mz(object, ...) <- value}:}{Get or set the m/z column in the \code{featureData} slot.}

        \item{\code{intensity(object, ...)}, \code{intensity(object, ...) <- value}:}{Get or set the intensity matrix in the \code{spectraData} slot.}

        \item{\code{centroided(object, ...)}, \code{centroided(object, ...) <- value}:}{Get or set the \code{centroided} slot.}

        \item{\code{isCentroided(object, ...)}:}{Equivalent to \code{isTRUE(centroided(object))}.}

        \item{\code{experimentData(object)}, \code{experimentData(object) <- value}:}{Get or set the \code{experimentData} slot.}
    }
}

\author{Kylie A. Bemis}

\seealso{
    \code{\linkS4class{SpectralImagingExperiment}},
    \code{\linkS4class{MSImagingArrays}}
}

\examples{
set.seed(1, kind="L'Ecuyer-CMRG")
x <- matrix(rlnorm(81), nrow=9, ncol=9)
mz <- sort(runif(9))
coord <- expand.grid(x=1:3, y=1:3)

mse <- MSImagingExperiment(
    spectraData=x,
    featureData=MassDataFrame(mz=mz),
    pixelData=PositionDataFrame(coord))

print(mse)
}

\keyword{classes}
