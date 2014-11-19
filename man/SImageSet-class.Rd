\name{SImageSet-class}
\docType{class}

\alias{class:SImageSet}
\alias{SImageSet}
\alias{SImageSet-class}

\alias{[,SImageSet-method}
\alias{[,SImageSet,ANY,ANY,ANY-method}
\alias{initialize,SImageSet-method}
\alias{combine,SImageSet,SImageSet-method}
\alias{iData,SImageSet-method}
\alias{iData<-,SImageSet-method}
\alias{regeneratePositions,SImageSet-method}
\alias{coord<-,SImageSet-method}
\alias{coordLabels<-,SImageSet-method}
\alias{featureNames<-,SImageSet-method}
\alias{pixelNames<-,SImageSet-method}

\title{Class to Contain Pixel-Sparse Imaging Data}

\description{
    An \code{\linkS4class{iSet}} derived class for pixel-sparse imaging data. Data is stored to be memory efficient when there are missing pixels or when the the stored images are non-rectangular regions. The data structures remain efficient for non-sparse pixel data as long as the full datacube does not need to be reconstructed often, and single images or feature vectors are of primary interest. This class can be combined with \code{\linkS4class{Hashmat}} to be sparse in both feature space and pixel space. This is useful for datasets with sparse signals, such as processed spectra.

    \code{\linkS4class{MSImageSet}} is a derived class of \code{\linkS4class{SImageSet}} for storing mass spectrometry imaging experiments.
}

\usage{
## Instance creation
SImageSet(
    data = Hashmat(nrow=0, ncol=0),
    coord = expand.grid(
        x = seq_len(prod(dim(data)[-1])),
        y = seq_len(ifelse(prod(dim(data)[-1]) > 0, 1, 0))),
    imageData = SImageData(
        data=data,
        coord=coord),
    pixelData = IAnnotatedDataFrame(
        data=coord,
        varMetadata=data.frame(labelType=rep("dim", ncol(coord)))),
    featureData = AnnotatedDataFrame(
        data=data.frame(row.names=seq_len(nrow(data)))),
    protocolData = AnnotatedDataFrame(
        data=data.frame(row.names=sampleNames(pixelData))),
    experimentData = new("MIAPE-Imaging"),
    \dots)

## Additional methods documented below
}

\arguments{
    
    \item{data}{A matrix-like object with number of rows equal to the number of features and number of columns equal to the number of non-missing pixels. Each column should be a feature vector. Alternatively, a multidimensional array that represents the datacube with the first dimension as the features can also be supplied. Additional dimensions could be the spatial dimensions of the image, for example.}

    \item{coord}{A \code{data.frame} with columns representing the spatial dimensions. Each row provides a spatial coordinate for the location of a feature vector corresponding to a column in \code{data}. This argument is ignored if \code{data} is a multidimensional array rather than a matrix.}

    \item{imageData}{An object of class \code{\linkS4class{SImageData}} that will contain the imaging data. Usually constructed using \code{data} and \code{coord}.}

    \item{pixelData}{An object of class \code{\linkS4class{IAnnotatedDataFrame}} giving the information about the pixels including coordinates of the data in \code{imageData}.}

    \item{featureData}{An object of class \code{\linkS4class{AnnotatedDataFrame}} giving information about the data features.}

    \item{protocolData}{An object of class \code{\linkS4class{AnnotatedDataFrame}} giving information about the samples. It must have one row for each of the \code{sampleNames} in \code{pixelData}.}

    \item{experimentData}{An object derived from class \code{\linkS4class{MIAxE}} giving information about the imaging experiment.}

    \item{\dots}{Additional arguments passed to the initializer.}
    
}

\section{Slots}{
    \describe{
        \item{\code{imageData}:}{An instance of \code{\linkS4class{SImageData}}, which stores one or more matrices of equal number of dimensions as elements in an 'immutableEnvironment'. This slot preserves copy-on-write behavior when it is modified specifically, but is pass-by-reference otherwise, for memory efficiency.}

        \item{\code{pixelData}:}{Contains pixel information in an \code{\linkS4class{IAnnotatedDataFrame}}. This includes both pixel coordinates and phenotypic and sample data. Its rows correspond to the columns in \code{imageData}.}

        \item{\code{featureData}:}{Contains variables describing features in an \code{\linkS4class{IAnnotatedDataFrame}}. Its rows correspond to the rows in \code{imageData}.}

        \item{\code{experimentData}:}{Contains details of experimental methods. Should be an object of a derived class of \code{\linkS4class{MIAxE}}.}

        \item{\code{protocolData}:}{Contains variables in an \code{\linkS4class{IAnnotatedDataFrame}} describing the generation of the samples in \code{pixelData}.}

        \item{\code{.__classVersion__}:}{A \code{Versions} object describing the version of the class used to created the instance. Intended for developer use.}
    }
}

\section{Extends}{
    \code{\linkS4class{iSet}}, directly.
    \code{\linkS4class{VersionedBiobase}}, by class "iSet", distance 1.
    \code{\linkS4class{Versioned}}, by class "VersionedBiobase", distance 2.
}

\section{Creating Objects}{
    \code{SImageSet} instances are usually created through \code{SImageSet()}.
}

\section{Methods}{
    Class-specific methods:
    \describe{
        \item{\code{iData(object), iData(object) <- value}:}{Access and set the sparse image data in \code{imageData}. This is a matrix-like object with rows corresponding to features and columns corresponding to pixels, so that each column of the returned object is a feature vector.}

        \item{\code{regeneratePositions}:}{Regenerates the \code{positionArray} in \code{imageData} used to reconstruct the datacube based on the coordinates in \code{pixelData}. Normally, this should not be called by the user. However, if the coordinates are modified manually, it can be used to re-sync the data structures.}
    }

    Standard generic methods:
    \describe{
        \item{\code{combine(x, y, ...)}:}{Combine two or more \code{SImageSet} objects. Unique 'sample's in \code{pixelData} are treated as a dimension.}

        \item{\code{SImageSet[i, j, ..., drop]}:}{Subset an \code{SImageSet} based on the rows (featureData components) and the columns (pixelData components). The result is a new \code{SImageSet}.}
    }

    See \code{\linkS4class{iSet}} for additional methods.
}

\author{Kyle D. Bemis}

\seealso{
    \code{\linkS4class{iSet}},
    \code{\linkS4class{SImageData}},
    \code{\linkS4class{MSImageSet}}
}

\examples{
## Create an SImageSet object
data <- matrix(1:27, nrow=3)
coord <- expand.grid(x=1:3, y=1:3)
sset <- SImageSet(data=data, coord=coord)

## Access a single image corresponding to the first feature
imageData(sset)[1,,]

## Reconstruct the datacube
imageData(sset)[]

## Access the P x N matrix of column-wise feature vectors
iData(sset)

## Subset the SImageSet to the first 2 features and first 6 pixels
sset2 <- sset[1:2, 1:6]
imageData(sset2)[]
sset2
}

\keyword{classes}
