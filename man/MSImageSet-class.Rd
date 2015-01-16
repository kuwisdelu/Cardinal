\name{MSImageSet-class}
\docType{class}

\alias{class:MSImageSet}
\alias{MSImageSet}
\alias{MSImageSet-class}

\alias{initialize,MSImageSet-method}
\alias{combine,MSImageSet,MSImageSet-method}
\alias{mz,MSImageSet-method}
\alias{mz<-,MSImageSet-method}
\alias{spectra,MSImageSet-method}
\alias{spectra<-,MSImageSet-method}
\alias{peaks,MSImageSet-method}
\alias{peaks<-,MSImageSet-method}
\alias{processingData,MSImageSet-method}
\alias{processingData<-,MSImageSet-method}
\alias{centroided,MSImageSet-method}
\alias{centroided<-,MSImageSet-method}

\title{Class to Contain Mass Spectrometry Imaging Experiment Data}

\description{
    Container for mass spectrometry imaging experimental data and metadata. \code{MSImageSet} is derived from \code{\linkS4class{iSet}} through \code{\linkS4class{SImageSet}}. It extends these classes with information about the processing and analysis, requiring MIAPE-Imaging in its experimentData slot.
}

\usage{
## Instance creation
MSImageSet(
    spectra = Hashmat(nrow=0, ncol=0),
    mz = seq_len(dim(spectra)[1]),
    coord = expand.grid(
        x = seq_len(prod(dim(spectra)[-1])),
        y = seq_len(ifelse(prod(dim(spectra)[-1]) > 0, 1, 0))),
    imageData = MSImageData(data=spectra, coord=coord),
    pixelData = IAnnotatedDataFrame(
        data=coord,
        varMetadata=data.frame(labelType=rep("dim", ncol(coord)))),
    featureData = AnnotatedDataFrame(
        data=data.frame(mz=mz)),
    processingData = new("MSImageProcess"),
    protocolData = AnnotatedDataFrame(
        data=data.frame(row.names=sampleNames(pixelData))),
    experimentData = new("MIAPE-Imaging"),
    \dots)

## Additional methods documented below
}

\arguments{
    
    \item{spectra}{A matrix-like object with number of rows equal to the number of features and number of columns equal to the number of non-missing pixels. Each column should be a mass spectrum. Alternatively, a multidimensional array that represents the datacube with the first dimension as the features (m/z values) can also be supplied. Additional dimensions could be the spatial dimensions of the image, for example.}

    \item{mz}{A numeric vector representing the mass-to-charge ratio features (m/z values) corresponding to the rows in the \code{spectra} matrix. Must be strictly increasing or decreasing.}

    \item{coord}{A \code{data.frame} with columns representing the spatial dimensions. Each row provides a spatial coordinate for the location of a mass spectrum corresponding to a column in \code{spectra}. This argument is ignored if \code{spectra} is a multidimensional array rather than a matrix.}

    \item{imageData}{An object of class \code{\linkS4class{SImageData}} that will contain the imaging mass spectra. Usually constructed through the \code{spectra} and \code{coord} arguments.}

    \item{pixelData}{An object of class \code{\linkS4class{IAnnotatedDataFrame}} giving the information about the pixels including coordinates of the data in \code{imageData}.}

    \item{featureData}{An object of class \code{\linkS4class{AnnotatedDataFrame}} giving information about the data features. Requires a column named "mz".}

    \item{processingData}{An object of class \code{\linkS4class{MSImageProcess}} giving information about the pre-processing steps applied to the spectra.}

    \item{protocolData}{An object of class \code{\linkS4class{AnnotatedDataFrame}} giving information about the samples. It must have one row for each of the \code{sampleNames} in \code{pixelData}.}

    \item{experimentData}{An object derived from class \code{\linkS4class{MIAxE}} giving information about the imaging experiment.}

    \item{\dots}{Additional arguments passed to the initializer.}
    
}

\section{Slots}{
    \describe{
        \item{\code{imageData}:}{An instance of \code{\linkS4class{SImageData}}, which stores one or more matrices of equal number of dimensions as elements in an 'immutableEnvironment'. This slot preserves copy-on-write behavior when it is modified specifically, but is pass-by-reference otherwise, for memory efficiency.}

        \item{\code{pixelData}:}{Contains pixel information in an \code{\linkS4class{IAnnotatedDataFrame}}. This includes both pixel coordinates and phenotypic and sample data. Its rows correspond to the columns in \code{imageData}.}

        \item{\code{featureData}:}{Contains variables describing features. Its rows correspond to the rows in \code{imageData} in an \code{\linkS4class{IAnnotatedDataFrame}}.}

        \item{\code{processingData}:}{Contains details about the pre-processing steps that have been applied to the spectra. An object of class \code{\linkS4class{MSImageProcess}}.}

        \item{\code{experimentData}:}{Contains details of experimental methods. Must be \code{\linkS4class{MIAPE-Imaging}}.}

        \item{\code{protocolData}:}{Contains variables describing the generation of the samples in \code{pixelData} in an \code{\linkS4class{IAnnotatedDataFrame}}.}

        \item{\code{.__classVersion__}:}{A \code{Versions} object describing the version of the class used to created the instance. Intended for developer use.}
    }
}

\section{Extends}{
    \code{\linkS4class{SImageSet}}, directly.
    \code{\linkS4class{iSet}}, by class "SImageSet", distance 1.
    \code{\linkS4class{VersionedBiobase}}, by class "iSet", distance 2.
    \code{\linkS4class{Versioned}}, by class "VersionedBiobase", distance 3.
}

\section{Creating Objects}{
    \code{MSImageSet} instances can be created through \code{MSImageSet()}, but are more commonly created through reading of external data files.
}

\section{Methods}{
    Class-specific methods:
    \describe{
        \item{\code{spectra(object), spectra(object) <- value}:}{Access and set the mass spectra in \code{imageData}. This is a matrix-like object with rows corresponding to features and columns corresponding to pixels, so that each column of the returned object is a mass spectrum.}

        \item{\code{peaks(object), peaks(object) <- value}:}{Access and set the peaks in \code{imageData} if peak picking have been performed. This is a shortcut for \code{peakData(imageData(object))}. These are the unaligned peaks. Aligned peaks (if they exist) are accesed by spectra(object).}

        \item{\code{mz(object), mz(object) <- value}:}{Returns and sets the common m/z values of the mass spectra in the dataset. This is a required column of \code{featureData}.}

        \item{\code{features(object, ..., mz)}:}{Access the feature indices (rows in \code{featureData}) corresponding to variables in \code{featureData}. Bisection search is used for fuzzy matching of m/z values.}

        \item{\code{pixels(object, ..., coord)}:}{Access the pixel indices (rows in \code{pixelData}) corresponding to variables in \code{pixelData}. If specified, \code{coord} should be a \code{data.frame} where each row corresponds to the coordinates of a desired pixel.}

        \item{\code{centroided(object), centroided(object) <- value}:}{Access whether the dataset consists of profile or centroided mass spectra. This is a shortcut for \code{centroided(processingData(object))}. A setter is also provided, and is sometimes necessary for forcing some analysis methods to accept unprocessed spectra. (This is usually a bad idea.)}

        \item{\code{processingData(object), processingData(object) <- value}:}{Access and set the \code{processingData} slot.}
    }

    Standard generic methods:
    \describe{
        \item{\code{combine(x, y, ...)}:}{Combine two or more \code{MSImageSet} objects. Unique 'sample's in \code{pixelData} are treated as a dimension.}

        \item{\code{MSImageSet[i, j, ..., drop]}:}{Subset an \code{SImageSet} based on the rows (featureData components) and the columns (pixelData components). The result is a new \code{MSImageSet}.}
    }

    See \code{\linkS4class{iSet}} and \code{\linkS4class{SImageSet}} for additional methods.
}

\author{Kyle D. Bemis}

\seealso{
    \code{\linkS4class{iSet}},
    \code{\linkS4class{SImageSet}}
}

\examples{
## Create an MSImageSet object
spectra <- matrix(1:27, nrow=3)
mz <- 101:103
coord <- expand.grid(x=1:3, y=1:3)
msset <- MSImageSet(spectra=spectra, mz=mz, coord=coord)

## Access a single image corresponding to the first feature
imageData(msset)[1,,]

## Reconstruct the datacube
imageData(msset)[]

## Access the P x N matrix of column-wise mass spectra
spectra(msset)

## Subset the MSImageSet to the first 2 m/z values and first 6 mass spectra
msset2 <- msset[1:2, 1:6]
imageData(msset2)[]
msset2
}

\keyword{classes}
