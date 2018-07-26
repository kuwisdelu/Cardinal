\name{ImageList-class}
\docType{class}

\alias{class:ImageList}
\alias{ImageList}
\alias{ImageList-class}

\alias{class:ImageArrayList}
\alias{ImageArrayList}
\alias{ImageArrayList-class}

\alias{class:SimpleImageList}
\alias{SimpleImageList-class}

\alias{class:SimpleImageArrayList}
\alias{SimpleImageArrayList-class}

\alias{[[,ImageList,ANY,ANY-method}
\alias{[[<-,ImageList,ANY,ANY-method}
\alias{dim,ImageList-method}
\alias{dims,ImageList-method}
\alias{names,ImageList-method}
\alias{names<-,ImageList-method}
\alias{length,ImageList-method}
\alias{as.list,ImageList-method}

\alias{[,ImageArrayList,ANY,ANY,ANY-method}
\alias{[<-,ImageArrayList,ANY,ANY,ANY-method}
\alias{cbind,ImageArrayList-method}
\alias{rbind,ImageArrayList-method}

\alias{[,ImageArrayList-method}
\alias{[,ImageArrayList-method}

\alias{class:MSContinuousImagingSpectraList}
\alias{MSContinuousImagingSpectraList}
\alias{MSContinuousImagingSpectraList-class}

\alias{class:MSProcessedImagingSpectraList}
\alias{MSProcessedImagingSpectraList}
\alias{MSProcessedImagingSpectraList-class}

\title{ImageList: Abstract image data list}

\description{
    The \code{ImageList} virtual class provides an formal abstraction for the \code{imageData} slot of \code{\linkS4class{ImagingExperiment}} objects. It is analogous to the \code{Assays} classes from the \code{SummarizedExperiment} package.

    The \code{ImageArrayList} virtual class specializes the \code{ImageList} abstraction by assuming the array-like data elements all have conformable dimensions.

    The \code{SimpleImageList} and \code{SimpleImageArrayList} subclasses are the default implementations.

    The \code{MSContinuousImagingSpectraList} and \code{MSProcessedImagingSpectraList} classes are subclasses of \code{SimpleImageArrayList} that make certain assumptions about how the underlying data elements are stored (i.e., either dense or sparse). They are intended to be used with mass spectrometry imaging data.
}

\usage{
# Create a SimpleImageList
ImageList(data)

# Create a SimpleImageArrayList
ImageArrayList(data)

# ImageArrayList class for 'continuous' (dense) MS imaging data
MSContinuousImagingSpectraList(data)

# ImageArrayList class for 'processed' (sparse) MS imaging data
MSProcessedImagingSpectraList(data)
}

\arguments{
    \item{data}{A \code{\linkS4class{SimpleList}} or \code{list} of array-like data elements, or an array-like object.}
}

\details{
    \code{ImageList} and \code{ImageArrayList} objects have list-like semantics where the elements are array-like (i.e., have \code{dim}), where \code{ImageArrayList} makes the additional assumption that the array-like elements have identical \code{dim}s for at least the first two dimensions.

    The \code{ImageList} class includes:

    \itemize{
        \item{(1) The \code{ImageList()} and \code{ImageArrayList()} constructor functions.}
        
        \item{(2) Lossless back-and-forth coercion from/to \code{\linkS4class{SimpleList}}. The coercion method need not and should not check the validity of the returned object.}

        \item(3) \code{length}, \code{names}, \code{names<-}, and \code{`[[`}, \code{`[[<-`} methods for \code{ImageList}, as well as \code{`[`}, \code{`[<-`}, \code{rbind}, and \code{cbind} methods for \code{ImageArrayList}.
    }

    See the documentation for the \code{Assays} class in the \code{SummarizedExperiment} package for additional details, as the implementation is quite similar, with the main difference being that all assumptions about the dimensions of the array-like data elements is contained in the \code{ImageArrayList} subclass. This is intended to allow subclasses of the \code{ImageList} class to handle images stored as arrays with non-conformable dimensions.

    These classes are intended to eventually replace the \code{\linkS4class{ImageData}} classes.
}

\author{Kylie A. Bemis}

\seealso{
    \code{\linkS4class{SimpleList}}
}

\examples{
## Create an ImageList object
data0 <- matrix(1:9, nrow=3)
data1 <- matrix(10:18, nrow=3)
data2 <- matrix(19:27, nrow=3)
idata <- ImageArrayList(list(d0=data0, d1=data1, d2=data2))

# Subset all arrays at once
idataS <- idata[1:2,1:2]
all.equal(idataS[["d0"]], data0[1:2,1:2])

# Combine over "column" dimension
idataB <- cbind(idata, idata)
all.equal(idataB[["d0"]], cbind(data0, data0))
}

\keyword{classes}
