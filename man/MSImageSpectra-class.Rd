\name{MSImageSpectra-class}
\docType{class}

\alias{class:MSImageSpectra}
\alias{MSImageSpectra}
\alias{MSImageSpectra-class}


\alias{[,MSImageSpectra-method}
\alias{dim,MSImageSpectra-method}
\alias{dims,MSImageSpectra-method}
\alias{initialize,MSImageSpectra-method}
\alias{spectra,MSImageSpectra-method}
\alias{spectra<-,MSImageSpectra-method}

\title{Class Containing Mass Spectra Data.}

\description{
    A container class for holding imaging data, designed to contain one or more arrays in an immutable environment.
}

\usage{    
## Instance creation
MSImageSpectra(spectra, coord,
    storageMode = c("immutableEnvironment", "lockedEnvironment", "environment"),
    positionArray = if ( !missing(coord) ) generatePositionArray(coord),
    \dots)

## Additional methods documented below
}

\arguments{
    
    \item{spectra}{A matrix with number of rows equal to the number of features (m/z values) and number of columns equal to the number of non-missing pixels. Each column should be a mass spectrum. Alternatively, a multidimensional array that represents the datacube with the first dimension as the features can also be supplied. Additional dimensions could be the spatial dimensions of the image, for example.}

    \item{coord}{A \code{data.frame} with columns representing the spatial dimensions where each mass spectrum was measured. Each row provides a spatial coordinate for the location of a mass spectrum corresponding to a column in \code{spectra}. This argument is ignored (with warning) if \code{spectra} is a multidimensional array rather than a matrix.}

    \item{storageMode}{The storage mode to use for the \code{MSImageSpectra} object for the environment in the \code{data} slot. This must be one of \code{"immutableEnvironment"}, \code{"lockedEnvironment"}, or \code{"environment"}. See documentation on the \code{storageMode} slot below for more details.}

    \item{\dots}{Additional Named arguments that are passed to the \code{initialize} method for instantiating the object. These must be matrices or matrix-like objects of equal dimension to \code{spectra}. They will be assigned into the environment in the \code{data} slot.}

}

\section{Extends}{
   \code{\linkS4class{Versioned}}
}

\section{Creating Objects}{
    \code{MSImageSpectra} instances are usually created through \code{MSImageSpectra()}.
}

\section{Slots}{
    \describe{
        \item{\code{data}:}{An \code{environment} which contains an element named \code{"spectra"}, which is a matrix with rows equal to the number of features (m/z values) and columns equal to the number of non-missing pixels. Each column is a mass spectrum. Inherited from \code{\linkS4class{ImageData}}}

        \item{\code{positionArray}:}{An \code{array} with dimensions equal to the spatial dimensions of the image, which stores the column numbers of the mass spectrum corresponding to that pixel in the \code{"spectra"} element of the \code{data} slot. This allows re-construction of the imaging ``datacube'' on-the-fly.}
        
        \item{\code{storageMode}:}{A \code{character} which is one of \code{"immutableEnvironment"}, \code{"lockedEnvironment"}, or \code{"environment"}. The values \code{"lockedEnvironment"} and \code{"environment"} behave as described in the documentation of \code{\link[Biobase]{AssayData}}. An \code{"immutableEnvironment"} uses a locked environment while retaining R's typical copy-on-write behavior. Whenever an object in an immutable environment is modified, a new environment is created for the \code{data} slot, and all objects copied into it. This allows usual R functional semantics while avoiding copying of large objects when other slots are modified. Inherited from \code{\linkS4class{ImageData}}.}

        \item{\code{.__classVersion__}:}{A \code{Versions} object describing the version of the class used to created the instance. Intended for developer use.}
    }
}

\section{Methods}{
    Class-specific methods:
    \describe{
        \item{\code{spectra(object)}, \code{spectra(object)<-}:}{Return or set the matrix of mass spectra}
    }

    Standard generic methods:
    \describe{
        \item{\code{combine(x, y, ...)}:}{Combine two or more \code{MSImageSpectra} objects. Elements of the \code{data} slot are combined as for \code{\linkS4class{ImageData}}. The \code{positionArray} slots are combined along a new dimension using \code{\link[abind]{abind}}.}

        \item{\code{dim(object)}:}{Return the dimensions of the (virtual) datacube. This is equal to the number of features (the number of rows in the matrix returned by \code{spectra}) and the dimensions of the \code{positionArray} slot. For a standard single mass spectrometry imaging dataset, it the the number of m/z values and followed by the spatial dimensions of the image.}

        \item{\code{dims(object)}:}{A matrix where each column corresponds to the dimensions of the (virtual) datacubes stored as elements in the \code{data} slot. See above for how the dimensions are calculated.}

        \item{\code{MSImageSpectra[i, j, ..., drop]}:}{Access intensities in the (virtual) datacube of mass spectra. The datacube is reconstructed on-the-fly from the \code{spectra} matrix and the \code{positionArray}. The object can be indexed like any ordinary array with number of dimensions equal to \code{dim(object)}.}
    }
}

\author{Kyle D. Bemis}

\seealso{
    \code{\link{generatePositionArray}},
    \code{\linkS4class{ImageData}},
    \code{\linkS4class{MSImageSet}}
}

\examples{
## Create an MSImageSpectra object
MSImageSpectra()

## Using a P x N matrix
spectra1 <- matrix(1:27, nrow=3)
coord <- expand.grid(x=1:3, y=1:3)
msidata1 <- MSImageSpectra(spectra1, coord)
msidata1[] # extract data as array

## Using a P x X x Y array
spectra2 <- array(1:27, dim=c(3,3,3))
msidata2 <- MSImageSpectra(spectra2)
msidata2[] # should be identical to above

# Missing spectra from some pixels
spectra3 <- matrix(1:9, nrow=3)
msidata3 <- MSImageSpectra(spectra3, coord[c(1,5,9),])

dim(msidata3) # presents as an array
spectra(msidata3) # stored as matrix
msidata3[] # recontruct the datacube

spectra(msidata3)[,1] <- 101:103 # assign using spectra()
msidata3[] # can only assign into matrix representation
}

\keyword{classes}
