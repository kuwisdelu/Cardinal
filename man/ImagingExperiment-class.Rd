\name{ImagingExperiment-class}
\docType{class}

\alias{class:ImagingExperiment}
\alias{ImagingExperiment}
\alias{ImagingExperiment-class}

\alias{imageData}
\alias{imageData<-}
\alias{imageData-methods}
\alias{iData}
\alias{iData<-}
\alias{iData-methods}
\alias{phenoData}
\alias{phenoData<-}
\alias{phenoData-methods}
\alias{pixelData}
\alias{pixelData<-}
\alias{pixelData-methods}
\alias{pData}
\alias{pData<-}
\alias{pData-methods}
\alias{featureData}
\alias{featureData<-}
\alias{featureData-methods}
\alias{fData}
\alias{fData<-}
\alias{fData-methods}

\alias{sampleNames}
\alias{sampleNames<-}
\alias{sampleNames-methods}
\alias{pixelNames}
\alias{pixelNames<-}
\alias{pixelNames-methods}
\alias{featureNames}
\alias{featureNames<-}
\alias{featureNames-methods}

\alias{show,ImagingExperiment-method}
\alias{length,ImagingExperiment-method}
\alias{$,ImagingExperiment-method}
\alias{$<-,ImagingExperiment-method}
\alias{[[,ImagingExperiment-method}
\alias{[[<-,ImagingExperiment-method}
\alias{[[,ImagingExperiment,ANY,ANY-method}
\alias{[[<-,ImagingExperiment,ANY,ANY-method}
\alias{[,ImagingExperiment,ANY,ANY,ANY-method}
\alias{[<-,ImagingExperiment,ANY,ANY,ANY-method}
\alias{cbind,ImagingExperiment-method}
\alias{rbind,ImagingExperiment-method}
\alias{combine,ImagingExperiment,ANY-method}

\alias{dim,ImagingExperiment-method}
\alias{dimnames,ImagingExperiment-method}
\alias{imageData,ImagingExperiment-method}
\alias{imageData<-,ImagingExperiment-method}
\alias{iData,ImagingExperiment,ANY-method}
\alias{iData,ImagingExperiment,missing-method}
\alias{iData<-,ImagingExperiment,ANY-method}
\alias{iData<-,ImagingExperiment,missing-method}
\alias{pData,ImagingExperiment-method}
\alias{pData<-,ImagingExperiment,ANY-method}
\alias{phenoData,ImagingExperiment-method}
\alias{phenoData<-,ImagingExperiment,ANY-method}
\alias{sampleNames,ImagingExperiment-method}
\alias{sampleNames<-,ImagingExperiment,ANY-method}
\alias{pixelData,ImagingExperiment-method}
\alias{pixelData<-,ImagingExperiment-method}
\alias{pixelNames,ImagingExperiment-method}
\alias{pixelNames<-,ImagingExperiment-method}
\alias{fData,ImagingExperiment-method}
\alias{fData<-,ImagingExperiment,ANY-method}
\alias{featureData,ImagingExperiment-method}
\alias{featureData<-,ImagingExperiment-method}
\alias{featureData<-,ImagingExperiment,ANY-method}
\alias{featureNames,ImagingExperiment-method}
\alias{featureNames<-,ImagingExperiment-method}
\alias{names,ImagingExperiment-method}
\alias{names<-,ImagingExperiment-method}


\title{ImagingExperiment: Abstract class for imaging experiments}

\description{
   The \code{ImagingExperiment} class is a virtual class for biological imaging experiments. It includes slots for sample/pixel metadata and for feature metadata. The class makes very few assumptions about the structure of the underlying imaging data, including the dimensions.

   For a concrete subclass, see the \code{\linkS4class{SparseImagingExperiment}} class, which assumes that the image data can be represented as a matrix where columns represent pixels and rows represent features. The \code{\linkS4class{MSImagingExperiment}} subclass is further specialized for analysis of mass spectrometry imaging experiments.
}

\section{Slots}{
    \describe{
        \item{\code{imageData}:}{An object inheriting from \code{\linkS4class{ImageList}}, storing one or more array-like data elements. No assumption is made about the shape of the arrays.}

        \item{\code{featureData}:}{Contains feature information in a \code{\linkS4class{DataFrame}}. Each row includes the metadata for a single feature (e.g., a color channel, a molecular analyte, or a mass-to-charge ratio).}

        \item{\code{elementMetadata}:}{Contains sample or pixel information in a \code{\linkS4class{DataFrame}}. Each row includes the metadata for a single observation (e.g., a sample or a pixel).}

        \item{\code{metadata}:}{A \code{list} containing experiment-level metadata.}
    }
}

\section{Methods}{
    \describe{
        \item{\code{imageData(object), imageData(object) <- value}:}{Get and set the \code{imageData} slot.}

        \item{\code{iData(object, i), iData(object, i, ...) <- value}:}{Get or set the element \code{i} from the \code{imageData}. If \code{i} is missing, the first data element is returned.}

        \item{\code{phenoData(object), phenoData(object) <- value}:}{Get and set the \code{elementMetadata} slot.}

        \item{\code{sampleNames(object), sampleNames(object) <- value}:}{Get and set the row names of the \code{elementMetadata} slot.}

        \item{\code{pData(object), pData(object) <- value}:}{A shortcut for \code{phenoData(object)} and \code{phenoData(object)<-}.}

        \item{\code{pixelData(object), pixelData(object) <- value}:}{In subclasses where columns represent pixels, get and set the \code{elementMetadata} slot.}

        \item{\code{pixelNames(object), pixelNames(object) <- value}:}{In subclasses where columns represent pixels, get and set the row names of the \code{elementMetadata} slot.}

        \item{\code{featureData(object), featureData(object) <- value}:}{Get and set the \code{featureData} slot.}

        \item{\code{featureNames(object), featureNames(object) <- value}:}{Get and set the row names of the \code{featureData} slot.}

        \item{\code{fData(object), fData(object) <- value}:}{A shortcut for \code{featureData(object)} and \code{featureData(object)<-}.}

        \item{\code{dim}:}{The dimensions of the object, as determined by the number of features (rows in \code{featureData}) and the number of samples/pixels (rows in \code{elementMetadata}).}

        \item{\code{object$name, object$name <- value}:}{Get and set the \code{name} column in \code{pixelData}.}

        \item{\code{object[[i]], object[[i]] <- value}:}{Get and set the column \code{i} (a string or integer) in \code{pixelData}.}

        \item{\code{object[i, j, ..., drop]}:}{Subset based on the rows (\code{fData}) and the columns (\code{pData}). The result is the same class as the original object.}

        \item{\code{rbind(...)}, \code{cbind(...)}:}{Combine \code{ImagingExperiment} objects by row or column.}
    }
}

\author{Kylie A. Bemis}

\seealso{
    \code{\linkS4class{SparseImagingExperiment}},
    \code{\linkS4class{MSImagingExperiment}}
}

\examples{
## cannot create an ImagingExperiment object
try(new("ImagingExperiment"))

## create an ImagingExperiment derived class
MyImagingExperiment <- setClass("MyImagingExperiment", contains="ImagingExperiment")
MyImagingExperiment()

removeClass("MyImagingExperiment")
}

\keyword{classes}
