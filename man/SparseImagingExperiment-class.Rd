\name{SparseImagingExperiment-class}
\docType{class}

\alias{class:SparseImagingExperiment}
\alias{SparseImagingExperiment}
\alias{SparseImagingExperiment-class}

\alias{pixels}
\alias{pixels-methods}
\alias{features}
\alias{features-methods}

\alias{show,SparseImagingExperiment-method}
\alias{run,SparseImagingExperiment-method}
\alias{run<-,SparseImagingExperiment-method}
\alias{runNames,SparseImagingExperiment-method}
\alias{runNames<-,SparseImagingExperiment-method}
\alias{coord,SparseImagingExperiment-method}
\alias{coord<-,SparseImagingExperiment-method}
\alias{coordinates,SparseImagingExperiment-method}
\alias{coordinates<-,SparseImagingExperiment-method}
\alias{coordLabels,SparseImagingExperiment-method}
\alias{coordLabels<-,SparseImagingExperiment-method}
\alias{coordnames,SparseImagingExperiment-method}
\alias{coordnames<-,SparseImagingExperiment,ANY-method}
\alias{gridded,SparseImagingExperiment-method}
\alias{gridded<-,SparseImagingExperiment,ANY-method}
\alias{resolution,SparseImagingExperiment-method}
\alias{resolution<-,SparseImagingExperiment-method}
\alias{pixelData,SparseImagingExperiment-method}
\alias{pixelData<-,SparseImagingExperiment-method}
\alias{pixelNames,SparseImagingExperiment-method}
\alias{pixelNames<-,SparseImagingExperiment-method}
\alias{processingData,SparseImagingExperiment-method}
\alias{processingData<-,SparseImagingExperiment-method}
\alias{preproc,SparseImagingExperiment-method}
\alias{[,SparseImagingExperiment,ANY,ANY,ANY-method}
\alias{[<-,SparseImagingExperiment,ANY,ANY,ANY-method}
\alias{dim,SparseImagingExperiment-method}
\alias{dims,SparseImagingExperiment-method}
\alias{cbind,SparseImagingExperiment-method}
\alias{rbind,SparseImagingExperiment-method}
\alias{combine,SparseImagingExperiment,ANY-method}
\alias{collect,SparseImagingExperiment-method}
\alias{is3D,SparseImagingExperiment-method}

\alias{pixels,SparseImagingExperiment-method}
\alias{features,SparseImagingExperiment-method}

\alias{is3D}


\title{SparseImagingExperiment: Pixel-sparse imaging experiments}

\description{
   The \code{SparseImagingExperiment} class specializes the virtual \code{\linkS4class{ImagingExperiment}} class by assuming that each pixel may be a high-dimensional feature vector (e.g., a spectrum), but the pixels themselves may be sparse. Therefore, the data may be more efficiently stored as a matrix where rows are features and columns are pixels, rather than storing the full, dense datacube.

   Both 2D and 3D data are supported. Non-gridded pixel coordinates are allowed.

   The \code{\linkS4class{MSImagingExperiment}} subclass adds design features for mass spectrometry imaging experiments.
}

\usage{
## Instance creation
SparseImagingExperiment(
    imageData = matrix(nrow=0, ncol=0),
    featureData = XDataFrame(),
    pixelData = PositionDataFrame(),
    metadata = list(),
    processing = SimpleList())

## Additional methods documented below
}

\arguments{
    \item{imageData}{Either a matrix-like object with number of rows equal to the number of features and number of columns equal to the number of pixels, or an \code{\linkS4class{ImageArrayList}}.}

    \item{featureData}{A \code{\linkS4class{DataFrame}} with feature metadata, with a row for each feature.}

    \item{pixelData}{A \code{\linkS4class{PositionDataFrame}} with pixel metadata, with a row for each pixel.}

    \item{metadata}{A \code{list} with experimental-level metadata.}

    \item{processing}{A \code{\linkS4class{SimpleList}} with processing steps. This should typically be empty for new objects.}    
}

\section{Slots}{
    \describe{
        \item{\code{imageData}:}{An object inheriting from \code{\linkS4class{ImageArrayList}}, storing one or more array-like data elements with conformable dimensions.}

        \item{\code{featureData}:}{Contains feature information in a \code{\linkS4class{DataFrame}}. Each row includes the metadata for a single feature (e.g., a color channel, a molecular analyte, or a mass-to-charge ratio).}

        \item{\code{elementMetadata}:}{Contains pixel information in a \code{\linkS4class{PositionDataFrame}}. Each row includes the metadata for a single observation (e.g., a pixel), including specialized slot-columns for tracking pixel coordinates and experimental runs.}

        \item{\code{metadata}:}{A \code{list} containing experiment-level metadata.}

        \item{\code{processing}:}{A \code{\linkS4class{SimpleList}} containing processing steps (including both queued and previously executed processing steps).}
    }
}

\section{Methods}{
    All methods for \code{\linkS4class{ImagingExperiment}} also work on \code{SparseImagingExperiment} objects. Additional methods are documented below:

    \describe{
        \item{\code{pixels(object, ...)}:}{Returns the row indices of \code{pixelData} corresponding to conditions passed via \dots.}

        \item{\code{features(object, ...)}:}{Returns the row indices of \code{featureData} corresponding to conditions passed via \dots.}

        \item{\code{run(object)}, \code{run(object) <- value}:}{Get or set the experimental run slot-column from \code{pixelData}.}

        \item{\code{runNames(object)}, \code{runNames(object) <- value}:}{Get or set the experimental run levels from \code{pixelData}.}

        \item{\code{coord(object)}, \code{coord(object) <- value}:}{Get or set the spatial position slot-columns from \code{pixelData}.}

        \item{\code{coordLabels(object)}, \code{coordLabels(object) <- value}:}{Get or set the names of the spatial position slot-columns from \code{pixelData}.}

        \item{\code{gridded(object)}, \code{gridded(object) <- value}:}{Get or set whether the spatial positions are gridded or not. Typically, this should not be set manually.}

        \item{\code{resolution(object)}, \code{resolution(object) <- value}:}{Get or set the spatial resolution of the spatial positions. Typically, this should not be set manually.}

        \item{\code{dims(object)}:}{Get the gridded dimensions of the spatial positions (i.e., as if projected to an image raster).}

        \item{\code{is3D(object)}:}{Check if the data is 3D or not.}

        \item{\code{slice(object, ...)}:}{Slice the data as a data cube (i.e., as if projected to an multidimensional image raster).}

        \item{\code{processingData(object)}, \code{processingData(object) <- value}:}{Get or set the \code{processing} slot.}

        \item{\code{preproc(object)}:}{List the preprocessing steps queued and applied to the dataset.}

        \item{\code{collect(x, ...)}:}{Pull all data elements of \code{imageData} into memory as matrices.}

        \item{\code{object[i, j, ..., drop]}:}{Subset based on the rows (\code{featureData}) and the columns (\code{pixelData}). The result is the same class as the original object.}

        \item{\code{rbind(...)}, \code{cbind(...)}:}{Combine \code{SparseImagingExperiment} objects by row or column.}
    }
}

\author{Kylie A. Bemis}

\seealso{
    \code{\linkS4class{ImagingExperiment}},
    \code{\linkS4class{MSImagingExperiment}}
}

\examples{
data <- matrix(1:9^2, nrow=9, ncol=9)
t <- seq_len(9)
a <- seq_len(9)
coord <- expand.grid(x=1:3, y=1:3)

idata <- ImageArrayList(data)
fdata <- XDataFrame(t=t)
pdata <- PositionDataFrame(coord=coord, a=a)

x <- SparseImagingExperiment(
        imageData=idata,
        featureData=fdata,
        pixelData=pdata)

print(x)
}

\keyword{classes}
