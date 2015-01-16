\name{ImageData-class}
\docType{class}

\alias{class:ImageData}
\alias{ImageData}
\alias{ImageData-class}

\alias{[[,ImageData-method}
\alias{[[<-,ImageData-method}
\alias{[[,ImageData,character,missing-method}
\alias{[[<-,ImageData,character,missing-method}
\alias{annotatedDataFrameFrom,ImageData-method}
\alias{combine,ImageData,ImageData-method}
\alias{combine,array,array-method}
\alias{dim,ImageData-method}
\alias{dims,ImageData-method}
\alias{initialize,ImageData-method}
\alias{storageMode,ImageData-method}
\alias{storageMode<-,ImageData,character-method}
\alias{names,ImageData-method}
\alias{names<-,ImageData-method}
\alias{show,ImageData-method}

\title{Class Containing Arrays of Imaging Data}

\description{
    A container class for holding imaging data, designed to contain one or more arrays in an immutable environment. It is assumed that the first dimension of each array corresponds to the features.

    Note that only visible objects (names not beginning with '.') are checked for validity; however, \emph{all} objects are copied if any elements in the \code{data} slot are modified when \code{data} is an "immutableEnvironment".
}

\usage{
## Instance creation
ImageData(\dots,
    data = new.env(parent=emptyenv()),
    storageMode = c("immutableEnvironment",
        "lockedEnvironment", "environment"))

## Additional methods documented below
}

\arguments{
    
    \item{\dots}{Named arguments that are passed to the \code{initialize} method for instantiating the object. These must be arrays or array-like objects with an equal number of dimensions. They will be assigned into the environment in the \code{data} slot.}

    \item{data}{An environment in which to assign the previously named variables.}

    \item{storageMode}{The storage mode to use for the \code{ImageData} object for the environment in the \code{data} slot. This must be one of \code{"immutableEnvironment"}, \code{"lockedEnvironment"}, or \code{"environment"}. See documentation on the \code{storageMode} slot below for more details.}

}

\section{Slots}{
    \describe{
        \item{\code{data}:}{An \code{environment} which may contain one or more arrays with an equal number of dimensions. It is assumed that the first dimension corresponds to the features.}
        
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

        \item{\code{combine(x, y, ...)}:}{Combine two or more \code{ImageData} objects. All elements must have matching names, and are combined with calls to \code{combine}. Higher dimensional arrays are combined using the same rules as for matrices. (See \code{\link[BiocGenerics]{combine}} for more details.)}

        \item{\code{annotatedDataFrameFrom(object)}:}{Returns an \code{\linkS4class{IAnnotatedDataFrame}} with columns for the dimensions of the elements of \code{data}. All dimensions must be named (determined by the \code{rownames(dims(object))}). It is assumed that the first dimension corresponds to the features, and is not used as a dimension in the returned \code{\linkS4class{IAnnotatedDataFrame}}. Additional arguments (\code{byrow}, \dots) are ignored.}

        \item{\code{dims}:}{A matrix with each column corresponding to the dimensions of an element in the \code{data} slot.}

        \item{\code{names(x)}, \code{names(x)<-}:}{Access or replace the array names of the elements contained in the \code{data} slot environment.}

        \item{\code{ImageData[[name]]}, \code{ImageData[[name]] <- value}:}{Access or replace an element named \code{"name"} in the environment in the \code{data} slot.}
    }
}

\author{Kyle D. Bemis}

\seealso{
    \code{\link[Biobase]{AssayData}},
    \code{\linkS4class{SImageData}},
    \code{\linkS4class{SImageSet}},
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
