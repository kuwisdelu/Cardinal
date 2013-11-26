\name{MSImageSpecta-class}
\docType{class}

\alias{class:MSImageSpecta}
\alias{MSImageSpecta}
\alias{MSImageSpecta-class}


\alias{[,MSImageSpecta-method}
\alias{dim,MSImageSpecta-method}
\alias{initialize,MSImageSpecta-method}
\alias{spectra,MSImageSpecta-method}
\alias{spectra<-,MSImageSpecta-method}

\title{Class containing mass spectra data.}

\description{
    A container class for holding imaging data, designed to contain one or more arrays in an immutable environment.
}

\usage{    
    ## Instance creation
    MSImageSpecta(spectra, coord,
        storageMode = c("immutableEnvironment", "lockedEnvironment", "environment"),
        positionArray = if ( !missing(coord) ) generatePositionArray(coord) else NULL,
        \dots)

    ## Additional methods documented below
}

\arguments{
    
    \item{spectra}{}

    \item{coord}{}

    \item{storageMode}{The storage mode to use for the \code{MSImageSpecta} object for the environment in the \code{data} slot. This must be one of \code{"immutableEnvironment"}, \code{"lockedEnvironment"}, or \code{"environment"}. See documentation on the \code{storageMode} slot below for more details.}

    \item{\dots}{Additional Named arguments that are passed to the \code{initialize} method for instantiating the object. These must be matrices or matrix-like objects of equal dimension to \code{spectra}. They will be assigned into the environment in the \code{data} slot.}

}

\section{Extends}{
   Versioned
}

\section{Creating Objects}{
    \code{MSImageSpecta} instances are usually created through \code{MSImageSpecta()}.
}

\section{Slots}{
    \describe{
        \item{\code{data}:}{An \code{environment} which contains an element named \code{"spectra"}, which is a matrix with rows equal to the number of features (m/z values) and columns equal to the number of non-missing pixels. Each column is a mass spectrum. Inherited from \code{\link{ImageData}}}

        \item{\code{positionArray}:}{An \code{array} with dimensions equal to the spatial dimensions of the image, which stores the column numbers of the mass spectrum corresponding to that pixel in the \code{"spectra"} element of the \code{data} slot. This allows re-construction of the imaging ``datacube'' on-the-fly.}
        
        \item{\code{storageMode}:}{A \code{character} which is one of \code{"immutableEnvironment"}, \code{"lockedEnvironment"}, or \code{"environment"}. The values \code{"lockedEnvironment"} and \code{"environment"} behave as described in the documentation of \code{\link[Biobase]{AssayData}}. An \code{"immutableEnvironment"} uses a locked environment while retaining R's typical copy-on-write behavior. Whenever an object in an immutable environment is modified, a new environment is created for the \code{data} slot, and all objects copied into it. This allows usual R functional semantics while avoiding copying of large objects when other slots are modified. Inherited from \code{\link{ImageData}}.}

        \item{\code{.__classVersion__}:}{A \code{Versions} object describing the version of the class used to created the instance. Intended for developer use.}
    }
}

\section{Methods}{
    Accessors and setters:
    \describe{
        \item{\code{spectra(object)}, \code{spectra(object)<-}:}{Return or set the matrix of mass spectra}
    }

    Standard generic methods:
    \describe{
        \item{\code{dim(object)}:}{Return the dimensions of the (virtual) datacube. This is equal to the number of features (the number of rows in the matrix returned by \code{spectra}) and the dimensions of the \code{positionArray} slot. For a standard single mass spectrometry imaging dataset, it the the number of m/z values and followed by the spatial dimensions of the image.}

        \item{\code{MSImageSpecta[i, j, ..., drop]}:}{Access intensities in the (virtual) datacube of mass spectra. The datacube is reconstructed on-the-fly from the \code{spectra} matrix and the \code{positionArray}. The object can be indexed like any ordinary array with number of dimensions equal to \code{dim(object)}.}
    }
}

\author{Kyle D. Bemis}

\seealso{
    \code{\link[Biobase]{AssayData}},
    \code{\link{ImageData}},
    \code{\link{MSImageSet}}
}

\examples{
    ## Create an MSImageSpecta object
    MSImageSpecta()

    
}

\keyword{classes}
