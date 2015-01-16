\name{iSet-class}
\docType{class}

\alias{class:iSet}
\alias{iSet}
\alias{iSet-class}

\alias{$,iSet-method}
\alias{$<-,iSet-method}
\alias{[[,iSet-method}
\alias{[[<-,iSet-method}
\alias{[[,iSet,ANY,ANY-method}
\alias{[[<-,iSet,ANY,ANY-method}
\alias{initialize,iSet-method}
\alias{dim,iSet-method}
\alias{dims,iSet-method}
\alias{storageMode,iSet-method}
\alias{storageMode<-,iSet,character-method}
\alias{storageMode<-,iSet,ANY-method}
\alias{combine,iSet,iSet-method}
\alias{protocolData,iSet-method}
\alias{protocolData<-,iSet-method}
\alias{experimentData,iSet-method}
\alias{experimentData<-,iSet-method}
\alias{imageData,iSet-method}
\alias{imageData<-,iSet-method}
\alias{iData,iSet-method}
\alias{iData<-,iSet-method}
\alias{pixelData,iSet-method}
\alias{pixelData<-,iSet-method}
\alias{pData,iSet-method}
\alias{pData<-,iSet-method}
\alias{pData<-,iSet,ANY-method}
\alias{varMetadata,iSet-method}
\alias{varMetadata<-,iSet-method}
\alias{varMetadata<-,iSet,ANY-method}
\alias{varLabels,iSet-method}
\alias{varLabels<-,iSet-method}
\alias{sampleNames,iSet-method}
\alias{sampleNames<-,iSet-method}
\alias{sampleNames<-,iSet,ANY-method}
\alias{pixelNames,iSet-method}
\alias{pixelNames<-,iSet-method}
\alias{coordLabels,iSet-method}
\alias{coordLabels<-,iSet-method}
\alias{coord,iSet-method}
\alias{coord<-,iSet-method}
\alias{featureData,iSet-method}
\alias{featureData<-,iSet-method}
\alias{featureData<-,iSet,ANY-method}
\alias{fData,iSet-method}
\alias{fData<-,iSet-method}
\alias{fData<-,iSet,ANY-method}
\alias{fvarMetadata,iSet-method}
\alias{fvarMetadata<-,iSet-method}
\alias{fvarMetadata<-,iSet,ANY-method}
\alias{fvarLabels,iSet-method}
\alias{fvarLabels<-,iSet-method}
\alias{featureNames,iSet-method}
\alias{featureNames<-,iSet-method}
\alias{summary,iSet-method}
\alias{print.summary.iSet}

\title{Class to Contain High-Throughput Imaging Experiment Data and Metadata}

\description{
    A container class for data from high-throughput imaging experiments and associated metadata. Classes derived from from \code{iSet} contain one or more arrays or array-like objects with an equal number of dimensions as \code{imageData} elements. It is assumed that the first dimension of each such element corresponds to the data features, and all other dimensions are described by associated coordinates in the \code{pixelData} slot. Otherwise, derived classes are responsible for managing how the elements of \code{imageData} behave and their relationship with the rows of \code{pixelData} and \code{featureData}.

    The \code{\linkS4class{MSImageSet}} class for mass spectrometry imaging experiments is the primary derived class of \code{iSet}. Its parent class \code{\linkS4class{SImageSet}} is another derived class for more general images.

    This class is based on the \code{eSet} virtual class from Biobase. However, the \code{iSet} class contains an \code{imageData} slot which is an 'immutableEnvironment' that preserves copy-on-write behavior for \code{iSet} derived classes, but only copying elements of \code{imageData} when that slot specifically is modified. In addition \code{pixelData} is an \code{\linkS4class{IAnnotatedDataFrame}} that stores pixel information such as pixel coordinates in addition to phenotypic data.
}

\section{Slots}{
    \describe{
        \item{\code{imageData}:}{An instance of \code{\linkS4class{ImageData}}, which stores one or more array or array-like objects of equal number of dimensions as elements in an 'immutableEnvironment'. This slot preserves copy-on-write behavior when it is modified specifically, but is pass-by-reference otherwise, for memory efficiency.}

        \item{\code{pixelData}:}{Contains pixel information in an \code{\linkS4class{IAnnotatedDataFrame}}. This includes both pixel coordinates and phenotypic and sample data. Its rows correspond to individual pixels, many of which may belong to the same sample. Apart a requirement on columns describing the pixel coordinates, it is left to derived classes to decide the relationship to elements of \code{imageData}.}

        \item{\code{featureData}:}{Contains variables describing features. It Is left to derived classes to decide the relationship to elements of \code{imageData}.}

        \item{\code{experimentData}:}{Contains details of experimental methods. Should be an object of a derived class of \code{\linkS4class{MIAxE}}.}

        \item{\code{protocolData}:}{Contains variables describing the generation of the samples in \code{pixelData}.}

        \item{\code{.__classVersion__}:}{A \code{Versions} object describing the version of the class used to created the instance. Intended for developer use.}
    }
}

\section{Extends}{
   \code{\linkS4class{VersionedBiobase}}, directly.
   \code{\linkS4class{Versioned}}, by class "VersionedBiobase", distance 2.
}

\section{Creating Objects}{
    \code{iSet} is a virtual class. No instances can be created.
}

\section{Methods}{
    Class-specific methods:
    \describe{
        \item{\code{sampleNames(object), sampleNames(object) <- value}:}{Access and set the sample names in the \code{pixelData} and \code{protocolData} slots.}

        \item{\code{featureNames(object), featureNames(object) <- value}:}{Access and set the feature names in the \code{featureData} slot.}

        \item{\code{pixelNames(object), pixelNames(object) <- value}:}{Access and set the pixel names in the \code{pixelData} slot.}

        \item{\code{coordLabels(object), coordLabels(object) <- value}:}{Access and set the coordinate names described by the coordinate variables in the \code{pixelData} slot. Note that this does \emph{not} set or get coordinate names with a \code{labelType} of \code{sample}, regardless of whether they are currently being used to describe coordinates or not. Therefore, checking \code{coordLabels(object)} versus \code{names(coord(object))} is a simple way of checking whether a dataset is 2D or 3D.}

        \item{\code{coord(object)}, \code{coord(object)<-}:}{Return or set the coodinates. This is a \code{data.frame} containing the subset of columns of \code{data} for which the variables have a 'labelType' of 'dim'.}

        \item{\code{imageData(object), imageData(object) <- value}:}{Access and set the \code{imageData} slot.}

        \item{\code{pixelData(object), pixelData(object) <- value}:}{Access and set the \code{pixelData} slot.}

        \item{\code{pData(object), pData(object) <- value}:}{Access and set the pixel information.}

        \item{\code{varMetadata(object), varMetadata(object) <- value}:}{Access and set the metadata describing the variables in \code{pData}.}

        \item{\code{varLabels(object), varLabels(object) <- value}:}{Access and set the variable labels in \code{pixelData}.}

        \item{\code{featureData(object), featureData(object) <- value}:}{Access and set the \code{featureData} slot.}

        \item{\code{fData(object), fData(object) <- value}:}{Access and set the feature information.}

        \item{\code{fvarMetadata(object), fvarMetadata(object) <- value}:}{Access and set the metadata describing the features in \code{fData}.}

        \item{\code{fvarLabels(object), fvarLabels(object) <- value}:}{Access and set the feature labels in \code{featureData}.}

        \item{\code{features(object, ...)}:}{Access the feature indices (rows in \code{featureData}) corresponding to variables in \code{featureData}.}

        \item{\code{pixels(object, ...)}:}{Access the pixel indices (rows in \code{pixelData}) corresponding to variables in \code{pixelData}.}

        \item{\code{experimentData(object), experimentData(object) <-}:}{Access and set the \code{experimentData} slot.}

        \item{\code{protocolData(object), protocolData(object) <-}:}{Access and set the \code{protocolData} slot.}

        \item{\code{storageMode(object), storageMode(object)<-}:}{Return or set the storage mode of the \code{imageData} slot. See documentation on the \code{storageMode} slot above for more details.}
    }

    Standard generic methods:
    \describe{
        \item{\code{initialize}:}{Initialize a object of an \code{iSet} derived class. Called by \code{new}. Not to be used by the user.}

        \item{\code{validObject}:}{Checks that there exist columns in \code{pixelData} describing the pixel coordinates, cooresponding to the dimensions of the elements of \code{imageData}. For every named dimension of the arrays on \code{imageData} there must be a \code{pData} column describing its pixel coordinates. Also checks that the \code{sampleNames} match between \code{pixelData} and \code{protocolData}.}

        \item{\code{combine(x, y, ...)}:}{Combine two or more \code{iSet} objects. To be combined, iSets must have identical \code{featureData} and distinct \code{pixelNames} and \code{sampleNames}. All elements of \code{imageData} must have matching names. Elements of \code{imageData} are combined by calls for \code{combine}.}

        \item{\code{dim}:}{The dimensions of the object, as determined by the number of features (rows in featureData) and the number of pixels (rows in pixelData). This may differ from the dimensions returned by \code{dims(object)} (which corresponds to the arrays in \code{data}) or returned by \code{dim(imageData(object))}. See \code{\linkS4class{SImageSet}} for an example where this is the case, due to its use of a "virtual" datacube.}

        \item{\code{dims}:}{A matrix with each column corresponding to the dimensions of an element in the \code{data} slot.}

        \item{\code{iSet$name, iSet$name <- value}:}{Access and set the \code{name} column in \code{pixelData}.}

        \item{\code{iSet[[i, ...]], iSet[[i, ...]] <- value}:}{Access and set the column \code{i} (character or numeric index) in \code{pixelData}. The \dots argument can include named variables (especially 'labelDescription') to be added to the \code{varMetadata}.}
    }
}

\author{Kyle D. Bemis}

\seealso{
    \code{\linkS4class{eSet}},
    \code{\linkS4class{SImageSet}},
    \code{\linkS4class{MSImageSet}}
}

\examples{
## Cannot create an iSet object
try(new("iSet"))

## Create an iSet derived class
MyImageSet <- setClass("MyImageSet", contains="iSet")
MyImageSet()

removeClass("MyImageSet")
}

\keyword{classes}
