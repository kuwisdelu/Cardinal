\name{SpectralImagingExperiment-class}
\docType{class}

\alias{class:SpectralImagingExperiment}
\alias{SpectralImagingExperiment}
\alias{SpectralImagingExperiment-class}

\alias{show,SpectralImagingExperiment-method}
\alias{fData,SpectralImagingExperiment-method}
\alias{fData<-,SpectralImagingExperiment,ANY-method}
\alias{featureData,SpectralImagingExperiment-method}
\alias{featureData<-,SpectralImagingExperiment,ANY-method}
\alias{featureNames,SpectralImagingExperiment-method}
\alias{featureNames<-,SpectralImagingExperiment-method}
\alias{colnames,SpectralImagingExperiment-method}
\alias{colnames<-,SpectralImagingExperiment-method}
\alias{rownames,SpectralImagingExperiment-method}
\alias{rownames<-,SpectralImagingExperiment-method}
\alias{names,SpectralImagingExperiment-method}
\alias{names<-,SpectralImagingExperiment-method}
\alias{length,SpectralImagingExperiment-method}
\alias{[,SpectralImagingExperiment,ANY,ANY,ANY-method}
\alias{[<-,SpectralImagingExperiment,ANY,ANY,ANY-method}
\alias{dim,SpectralImagingExperiment-method}
\alias{cbind,SpectralImagingExperiment-method}
\alias{rbind,SpectralImagingExperiment-method}
\alias{combine,SpectralImagingExperiment,ANY-method}

\title{SpectralImagingExperiment: Spectral imaging data with shared domain}

\description{
   The \code{SpectralImagingExperiment} class provides a matrix-like container for high-dimensional spectral imaging data where every spectrum shares the same domain values. It is designed to provide easy access to both the spectra (as columns) and sliced images (as rows).

   The \code{\linkS4class{MSImagingExperiment}} class extends \code{SpectralImagingExperiment} for mass spectrometry-based imaging experiments with aligned mass features.
}

\usage{
## Instance creation
SpectralImagingExperiment(spectraData = SimpleList(),
    featureData = DataFrame(), pixelData = PositionDataFrame(),
    metadata = list())

## Additional methods documented below
}

\arguments{
    \item{spectraData}{Either a matrix-like object with number of rows equal to the number of features and number of columns equal to the number of pixels, a list of such objects, or a \code{\linkS4class{SpectraArrays}} instance.}

    \item{featureData}{A \code{\linkS4class{DataFrame}} with feature metadata, with a row for each feature.}

    \item{pixelData}{A \code{\linkS4class{PositionDataFrame}} with pixel metadata, with a row for each spectrum.}

    \item{metadata}{A \code{list} with experimental-level metadata.}
}

\section{Slots}{
    \describe{
        \item{\code{spectraData}:}{A \code{\linkS4class{SpectraArrays}} object storing one or more array-like data elements with conformable dimensions.}

        \item{\code{featureData}:}{A \code{\linkS4class{DataFrame}} containing feature-level metadata (e.g., a color channel, a molecular analyte, or a mass-to-charge ratio).}

        \item{\code{elementMetadata}:}{A \code{\linkS4class{PositionDataFrame}} containing spectrum-level metadata, including each spectrum's pixel coordinates and experimental run information.}

        \item{\code{processing}:}{A list containing unexecuted \code{\linkS4class{ProcessingStep}} objects.}
    }
}

\section{Methods}{
    All methods for \code{\linkS4class{SpectralImagingData}} also work on \code{SpectralImagingExperiment} objects. Additional methods are documented below:

    \describe{
        \item{\code{featureData(object)}, \code{featureData(object) <- value}:}{Get or set the \code{featureData} slot.}

        \item{\code{fData(object)}, \code{fData(object) <- value}:}{Get or set the \code{featureData} slot.}

        \item{\code{featureNames(object)}, \code{featureNames(object) <- value}:}{Get or set the feature names (i.e., the row names of the \code{featureData} slot).}

        \item{\code{length(object)}:}{Get the number of spectra in the object.}

        \item{\code{nrow(object)}, \code{ncol(object)}:}{Get the number of rows (features) or the number of columns (pixels) in the object.}

        \item{\code{object[i, j, ..., drop]}:}{Subset based on the rows (\code{featureData}) and the columns (\code{pixelData}). The result is the same class as the original object.}

        \item{\code{rbind(...)}, \code{cbind(...)}:}{Combine \code{SpectralImagingExperiment} objects by row or column.}
    }
}

\author{Kylie A. Bemis}

\seealso{
    \code{\linkS4class{SpectralImagingData}},
    \code{\linkS4class{MSImagingExperiment}}
}

\examples{
set.seed(1, kind="L'Ecuyer-CMRG")
x <- matrix(rlnorm(81), nrow=9, ncol=9)
index <- 1:9
coord <- expand.grid(x=1:3, y=1:3)

se <- SpectralImagingExperiment(
    spectraData=x,
    featureData=DataFrame(index=1:9),
    pixelData=PositionDataFrame(coord))

print(se)
}

\keyword{classes}
