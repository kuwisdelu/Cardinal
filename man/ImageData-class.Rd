\name{ImageData-class}
\docType{class}

\alias{class:ImageData}
\alias{ImageData}
\alias{ImageData-class}


\alias{[[,ImageData-method}
\alias{[[<-,ImageData-method}
\alias{combine,ImageData,ImageData-method}
\alias{dim,ImageData-method}
\alias{dims,ImageData-method}
\alias{initialize,ImageData-method}
\alias{storageMode,ImageData-method}
\alias{storageMode<-,ImageData,character-method}

\title{Class Containing Arrays of Imaging Data.}

\description{
    A container class for holding imaging data, designed to contain one or more arrays in an immutable environment.
}

\usage{    
## Instance creation
ImageData(\dots, storageMode = c("immutableEnvironment"
    "lockedEnvironment", "environment"))

## Additional methods documented below
}

\arguments{
    
    \item{\dots}{Named arguments that are passed to the \code{initialize} method for instantiating the object. These must be arrays or array-like objects with an equal number of dimensions. They will be assigned into the environment in the \code{data} slot.}

    \item{storageMode}{The storage mode to use for the \code{ImageData} object for the environment in the \code{data} slot. This must be one of \code{"immutableEnvironment"}, \code{"lockedEnvironment"}, or \code{"environment"}. See documentation on the \code{storageMode} slot below for more details.}

}

\section{Slots}{
    \describe{
        \item{\code{data}:}{An \code{environment} which may contain one or more arrays with an equal number of dimensions.}
        
        \item{\code{storageMode}:}{A \code{character} which is one of \code{"immutableEnvironment"}, \code{"lockedEnvironment"}, or \code{"environment"}. The values \code{"lockedEnvironment"} and \code{"environment"} behave as described in the documentation of \code{\link[Biobase]{AssayData}}. An \code{"immutableEnvironment"} uses a locked environment while retaining R's typical copy-on-write behavior. Whenever an object in an immutable environment is modified, a new environment is created for the \code{data} slot, and all objects copied into it. This allows usual R functional semantics while avoiding copying of large objects when other slots are modified.}

        \item{\code{.__classVersion__}:}{A \code{Versions} object describing the version of the class used to created the instance. Intended for developer use.}
    }
}

\section{Extends}{
   \code{\linkS4class{Versioned}}
}

\section{Creating Objects}{
    \code{ImageData} instances are usually created through \code{ImageData()}.
}

\section{Methods}{
    Class-specific methods:
    \describe{
        \item{\code{storageMode(object)}, \code{storageMode(object)<-}:}{Return or set the storage mode. See documentation on the \code{storageMode} slot above for more details.}
    }

    Standard generic methods:
    \describe{
        \item{\code{initialize}:}{Initialize an \code{ImageData} object. Called by \code{new}. Not to be used by the user.}

        \item{\code{validObject}:}{Validity-check that the arrays in the \code{data} slot environment are all of equal number of dimensions, and the storage mode is a valid value.}

        \item{\code{combine(x, y, ...)}:}{Combine two or more \code{ImageData} objects. All elements must have matching names. Arrays are combined along their last dimension, using \code{rbind} for vectors, \code{cbind} for matrices, and \code{\link[abind]{abind}} for higher dimensional arrays.}

        \item{\code{dims}:}{A matrix with each column corresponding to the dimensions of an element in the \code{data} slot.}

        \item{\code{ImageData[[name]]}, \code{ImageData[[name]] <- value}:}{Access or replace an element named \code{"name"} in the environment in the \code{data} slot.}
    }
}

\author{Kyle D. Bemis}

\seealso{
    \code{\linkS4class{MSImageSpectra}},
    \code{\linkS4class{MSImageSet}}
}

\examples{
## Create an ImageData object
ImageData()

idata <- ImageData(data0=matrix(1:4, nrow=2))
idata[["data0"]]

# Immutable environments in ImageData objects
storageMode(idata) <- "lockedEnvironment"
try(idata[["data0"]][,1] <- c(10,11)) # Fails

storageMode(idata) <- "immutableEnvironment"
try(idata[["data0"]][,1] <- c(10,11)) # Succeeds

# Test copy-on-write for immutable environments
idata2 <- idata
idata2[["data0"]] <- matrix(5:8, nrow=2)
idata[["data0"]] == idata2[["data0"]] # False
}

\keyword{classes}
