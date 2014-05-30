\name{SImageData-class}
\docType{class}

\alias{class:SImageData}
\alias{SImageData}
\alias{SImageData-class}

\alias{[,SImageData-method}
\alias{dim,SImageData-method}
\alias{dims,SImageData-method}
\alias{initialize,SImageData-method}
\alias{iData,SImageData-method}
\alias{iData<-,SImageData-method}

\title{Class Containing Sparse Image Data.}

\description{
    A container class for holding pixel-sparse image as a virtual datacube. It is assumed there will be missing pixels, so the feature vectors are stored as a matrix for memory efficiency, and the datacube is reconstructed on-the-fly. The implementation remains efficient even for non-sparse data as long as the full datacube does not need to be reconstructed as often as single images and feature vectors. All elements of \code{data} must have an identical number of rows (features) and columns (pixels).
}

\usage{
## Instance creation
SImageData(
    data = Hashmat(nrow=0, ncol=0),
    coord = expand.grid(
        x = seq_len(ncol(data)),
        y = seq_len(ifelse(ncol(data) > 0, 1, 0))),
    storageMode = c("immutableEnvironment",
        "lockedEnvironment", "environment"),
    positionArray = generatePositionArray(coord),
    \dots)

## Additional methods documented below
}

\arguments{
    
    \item{data}{A matrix-like object with number of rows equal to the number of features and number of columns equal to the number of non-missing pixels. Each column should be a feature vector. Alternatively, a multidimensional array that represents the datacube with the first dimension as the features can also be supplied. Additional dimensions could be the spatial dimensions of the image, for example.}

    \item{coord}{A \code{data.frame} with columns representing the spatial dimensions. Each row provides a spatial coordinate for the location of a feature vector corresponding to a column in \code{data}. This argument is ignored if \code{data} is a multidimensional array rather than a matrix.}

    \item{storageMode}{The storage mode to use for the \code{SImageData} object for the environment in the \code{data} slot. This must be one of \code{"immutableEnvironment"}, \code{"lockedEnvironment"}, or \code{"environment"}. See documentation on the \code{storageMode} slot below for more details.}

    \item{\dots}{Additional Named arguments that are passed to the \code{initialize} method for instantiating the object. These must be matrices or matrix-like objects of equal dimension to \code{data}. They will be assigned into the environment in the \code{data} slot.}

}

\section{Slots}{
    \describe{
        \item{\code{data}:}{An \code{environment} which contains at least one element named \code{"iData"}, which is a matrix-like object with rows equal to the number of features and columns equal to the number of non-missing pixels. Each column is a feature vector.}

        \item{\code{positionArray}:}{An \code{array} with dimensions equal to the spatial dimensions of the image, which stores the column numbers of the feature vectors corresponding to the pixels in the \code{"iData"} element of the \code{data} slot. This allows re-construction of the imaging "datacube" on-the-fly.}

        \item{\code{dim}:}{A length 2 integer vector analogous to the 'dim' attribute of an ordinary R matrix.}

        \item{\code{dimnames}:}{A length 2 \code{list} analogous to the 'dimnames' attribute of an ordinary R matrix.}
        
        \item{\code{storageMode}:}{A \code{character} which is one of \code{"immutableEnvironment"}, \code{"lockedEnvironment"}, or \code{"environment"}. The values \code{"lockedEnvironment"} and \code{"environment"} behave as described in the documentation of \code{\link[Biobase]{AssayData}}. An \code{"immutableEnvironment"} uses a locked environment while retaining R's typical copy-on-write behavior. Whenever an object in an immutable environment is modified, a new environment is created for the \code{data} slot, and all objects copied into it. This allows usual R functional semantics while avoiding copying of large objects when other slots are modified.}

        \item{\code{.__classVersion__}:}{A \code{Versions} object describing the version of the class used to created the instance. Intended for developer use.}
    }
}

\section{Extends}{
   \code{\linkS4class{Versioned}}
}

\section{Creating Objects}{
    \code{SImageData} instances are usually created through \code{SImageData()}.
}

\section{Methods}{
    Class-specific methods:
    \describe{
        \item{\code{iData(object)}, \code{iData(object)<-}:}{Return or set the matrix of column-wise feature vectors; the \code{"iData"} element of the \code{data} slot.}

        \item{\code{positionArray(object)}, \code{positionArray(object)<-}:}{Return or set the \code{positionArray} slot. When setting, this should be an array returned by a call to \code{generatePositionArray}.}

        \item{\code{featureNames(object), featureNames(object) <- value}:}{Access and set row names of the \code{"iData"} element of \code{data}.}

        \item{\code{pixelNames(object), pixelNames(object) <- value}:}{Access and set column names of the \code{"iData"} element of \code{data}.}

        \item{\code{storageMode(object)}, \code{storageMode(object)<-}:}{Return or set the storage mode. See documentation on the \code{storageMode} slot above for more details.}
    }

    Standard generic methods:
    \describe{
        \item{\code{combine(x, y, ...)}:}{Combine two or more \code{SImageData} objects. Elements are combined as for \code{\linkS4class{ImageData}}. The \code{positionArray} slots are combined along a new dimension (padding with NAs if necessary).}

        \item{\code{dim}:}{Return the dimensions of the (virtual) datacube. This is equal to the number of features (the number of rows in the matrix returned by \code{iData}) and the dimensions of the \code{positionArray} slot. For a standard imaging dataset, that is the number features followed by the spatial dimensions of the image.}

        \item{\code{dims}:}{A matrix where each column corresponds to the dimensions of the (virtual) datacubes stored as elements in the \code{data} slot. See above for how the dimensions are calculated.}

        \item{\code{SImageData[i, j, ..., drop]}:}{Access intensities in the (virtual) imaging datacube. The datacube is reconstructed on-the-fly from the \code{"iData"} matrix in the \code{data} environment and the \code{positionArray}. The object can be indexed like any ordinary array with number of dimensions equal to \code{dim(object)}.}
    }
}

\author{Kyle D. Bemis}

\seealso{
    \code{\link{generatePositionArray}},
    \code{\linkS4class{ImageData}},
    \code{\linkS4class{SImageSet}},
    \code{\linkS4class{MSImageSet}}
}

\examples{
## Create an SImageData object
SImageData()

## Using a P x N matrix
data1 <- matrix(1:27, nrow=3)
coord <- expand.grid(x=1:3, y=1:3)
sdata1 <- SImageData(data1, coord)
sdata1[] # extract data as array

## Using a P x X x Y array
data2 <- array(1:27, dim=c(3,3,3))
sdata2 <- SImageData(data2)
sdata2[] # should be identical to above

# Missing data from some pixels
data3 <- matrix(1:9, nrow=3)
sdata3 <- SImageData(data3, coord[c(1,5,9),])

dim(sdata3) # presents as an array
iData(sdata3) # stored as matrix
sdata3[] # recontruct the datacube

iData(sdata3)[,1] <- 101:103 # assign using iData()
sdata3[] # can only assign into matrix representation

## Sparse feature vectors
data4 <- Hashmat(nrow=9, ncol=9)
sdata4 <- SImageData(data4, coord)
iData(sdata4)[] <- diag(9)
sdata4[1,,]
}

\keyword{classes}
