\name{SpectralImagingData-class}
\docType{class}

\alias{class:SpectralImagingData}
\alias{SpectralImagingData}
\alias{SpectralImagingData-class}

\alias{show,SpectralImagingData-method}
\alias{spectraData,SpectralImagingData-method}
\alias{spectraData<-,SpectralImagingData-method}
\alias{spectraNames,SpectralImagingData-method}
\alias{spectraNames<-,SpectralImagingData-method}
\alias{spectra,SpectralImagingData-method}
\alias{spectra<-,SpectralImagingData-method}
\alias{spectraVariables,SpectralImagingData-method}
\alias{pData,SpectralImagingData-method}
\alias{pData<-,SpectralImagingData,ANY-method}
\alias{pixelData}
\alias{pixelData<-}
\alias{pixelData,SpectralImagingData-method}
\alias{pixelData<-,SpectralImagingData-method}
\alias{pixelNames}
\alias{pixelNames<-}
\alias{pixelNames,SpectralImagingData-method}
\alias{pixelNames<-,SpectralImagingData-method}
\alias{coord,SpectralImagingData-method}
\alias{coord<-,SpectralImagingData-method}
\alias{coordNames,SpectralImagingData-method}
\alias{coordNames<-,SpectralImagingData-method}
\alias{run,SpectralImagingData-method}
\alias{run<-,SpectralImagingData-method}
\alias{runNames,SpectralImagingData-method}
\alias{runNames<-,SpectralImagingData-method}
\alias{nrun,SpectralImagingData-method}
\alias{is3D,SpectralImagingData-method}
\alias{processingData,SpectralImagingData-method}
\alias{processingData<-,SpectralImagingData-method}

\alias{$,SpectralImagingData-method}
\alias{$<-,SpectralImagingData-method}
\alias{[[,SpectralImagingData-method}
\alias{[[<-,SpectralImagingData-method}

\title{SpectralImagingData: Abstract class for spectral imaging data}

\description{
   The \code{SpectralImagingData} class is an abstract container for high-dimensional spectral imaging data. Every spectrum is associated with spatial coordinates so that an image can be constructed from the spectral intensities.

   The \code{\linkS4class{SpectralImagingArrays}} and \code{\linkS4class{SpectralImagingExperiment}} classes directly extend this class, where \code{\linkS4class{SpectralImagingArrays}} is primarily intended for unprocessed spectra with unaligned features, and \code{\linkS4class{SpectralImagingExperiment}} is intended for processed spectra with aligned features.

   The \code{\linkS4class{MSImagingArrays}} and \code{\linkS4class{MSImagingExperiment}} classes further extend these classes for mass spectrometry imaging data.
}

\section{Slots}{
    \describe{
        \item{\code{spectraData}:}{A \code{\linkS4class{SpectraArrays}} object storing one or more array-like data elements with conformable dimensions.}

        \item{\code{elementMetadata}:}{A \code{\linkS4class{PositionDataFrame}} containing spectrum-level metadata, including each spectrum's pixel coordinates and experimental run information.}

        \item{\code{processing}:}{A list containing unexecuted \code{\linkS4class{ProcessingStep}} objects.}
    }
}

\section{Methods}{
    \describe{
        \item{\code{spectraData(object, ...)}, \code{spectraData(object, ...) <- value}:}{Get or set the \code{spectraData} slot.}

        \item{\code{spectraNames(object, ...)}, \code{spectraNames(object, ...) <- value}:}{Get or set the names of the spectra in the \code{spectraData} slot.}

        \item{\code{spectra(object, i = 1L, ...)}, \code{spectra(object, i = 1L, ...) <- value}:}{Get or set a specific spectra array in the \code{spectraData} slot.}

        \item{\code{pixelData(object)}, \code{pixelData(object) <- value}:}{Get or set the \code{elementMetadata} slot.}

        \item{\code{pData(object)}, \code{pData(object) <- value}:}{Get or set the \code{elementMetadata} slot.}

        \item{\code{pixelNames(object)}, \code{pixelNames(object) <- value}:}{Get or set the pixel names (i.e., the row names of the \code{elementMetadata} slot).}

        \item{\code{spectraVariables(object, ...)}:}{Get the names of the spectrum-level variables (i.e., the columns of the \code{elementMetadata} slot).}

        \item{\code{coord(object)}, \code{coord(object) <- value}:}{Get or set the pixel coordinate columns in \code{pixelData}.}

        \item{\code{coordNames(object)}, \code{coordNames(object) <- value}:}{Get or set the names of the pixel coordinate columns in \code{pixelData}.}

        \item{\code{run(object)}, \code{run(object) <- value}:}{Get or set the experimental run column from \code{pixelData}.}

        \item{\code{runNames(object)}, \code{runNames(object) <- value}:}{Get or set the experimental run levels from \code{pixelData}.}

        \item{\code{nrun(object)}:}{Get the number of experimental runs.}

        \item{\code{is3D(object)}:}{Check if the number of spatial dimensions is greater than 2.}

        \item{\code{processingData(object, ...)}, \code{processingData(object, ...) <- value}:}{Get or set the \code{processing} slot.}
    }
}

\author{Kylie A. Bemis}

\seealso{
    \code{\linkS4class{SpectralImagingExperiment}},
    \code{\linkS4class{SpectralImagingArrays}},
    \code{\linkS4class{MSImagingExperiment}},
    \code{\linkS4class{MSImagingArrays}}
}

\keyword{classes}
