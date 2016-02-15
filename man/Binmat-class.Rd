\name{Binmat-class}
\docType{class}

\alias{class:Binmat}
\alias{Binmat}
\alias{Binmat-class}

\alias{[,Binmat-method}
\alias{[,Binmat,ANY,ANY,ANY-method}
\alias{dim,Binmat-method}
\alias{dim<-,Binmat-method}
\alias{dimnames,Binmat-method}
\alias{dimnames<-,Binmat-method}
\alias{dimnames<-,Binmat,ANY-method}
\alias{colnames,Binmat-method}
\alias{colnames<-,Binmat-method}
\alias{rownames,Binmat-method}
\alias{rownames<-,Binmat-method}
\alias{cbind,Binmat-method}
\alias{rbind,Binmat-method}
\alias{initialize,Binmat-method}
\alias{show,Binmat-method}

\title{On-Disk Matrix Class Using On-Demand Disk Reads}

\description{
    The Binmat class implements on-disk matrices with efficient access to columns. Values within each column are contiguously stored in a binary file on disk. The columns themselves need not be stored contiguously. Only the accessed elements of the matrix are loaded into memory.
}

\usage{
## Instance creation
Binmat(
    files,
    nrow, ncol,
    offsets = 0,
    extents = rep(nrow, ncol),
    datatype = c("16-bit integer",
        "32-bit integer",
        "64-bit integer",
        "32-bit float",
        "64-bit float"),
    dimnames = NULL,
    \dots)

## Additional methods documented below
}

\arguments{
    
    \item{files}{The file(s) where the matrix is stored.}

    \item{nrow}{The number of rows in the on-disk matrix.}

    \item{ncol}{The number of columns in the on-disk matrix.}

    \item{offsets}{The positions of the first value of each column in number of bytes from the beginning of the file.}

    \item{extents}{The length of each column.}

    \item{datatype}{The binary data type.}

    \item{dimnames}{The 'dimnames' giving the dimension names for the matrix, analogous to the 'dimnames' attribute of an ordinary R matrix. This must be a list of length 2 or \code{NULL}.}

    \item{\dots}{Additional arguments passed to the constructor.}

}

\section{Slots}{
    \describe{
        \item{\code{files}:}{A \code{factor} giving the full file paths of the binary files storing the matrix (or matrices) on disk. Length must be equal to the number of columns.}

        \item{\code{offsets}:}{A \code{numeric} vector giving the positions of the first value of each column in number of bytes from the beginning of the file.}

        \item{\code{extents}:}{A \code{numeric} vector giving the length of each column.}

        \item{\code{datatype}:}{A \code{factor} vector giving the binary data types of each element of the matrix (or matrices) on disk. Length must be equal to the number of columns.}
        
        \item{\code{dim}:}{A length 2 integer vector analogous to the 'dim' attribute of an ordinary R matrix.}

        \item{\code{dimnames}:}{A length 2 \code{list} analogous to the 'dimnames' attribute of an ordinary R matrix.}

        \item{\code{.__classVersion__}:}{A \code{Versions} object describing the version of the class used to created the instance. Intended for developer use.}
    }
}

\section{Extends}{
   \code{\linkS4class{Versioned}}
}

\section{Creating Objects}{
    \code{Binmat} instances are usually created through \code{Binmat()}.
}

\section{Methods}{
    Standard generic methods:
    \describe{
        \item{\code{dim(x), dim(x) <- value}:}{Return or set the dimensions of the on-disk matrix.}

        \item{\code{dimnames(x), dimnames(x) <- value}:}{Return or set the 'dimnames' of the on-disk matrix.}

        \item{\code{colnames(x), colnames(x) <- value}:}{Return or set the column names of the on-disk matrix.}

        \item{\code{rownames(x), rownames(x) <- value}:}{Return or set the row names of the on-disk matrix.}

        \item{\code{ncol}:}{Return the number of columns in the on-disk matrix.}

        \item{\code{nrow}:}{Return the number of columns in the on-disk matrix.}

        \item{\code{cbind}:}{Combine on-disk matrices by columns.}

        \item{\code{rbind}:}{Not allowed for on-disk matrices. (Always returns an error.)}

        \item{\code{Binmat[i, j, ..., drop]}:}{Access elements in the on-disk matrix. A Binmat on-disk matrix can be indexed like an ordinary R matrix. Note however that linear indexing is not supported. Assignment is not currently allowed.}
    }
}

\author{Kyle D. Bemis}

\seealso{
    \code{\link[base]{matrix}},
    \code{\linkS4class{Hashmat}},
    \code{\linkS4class{SImageSet}}
}

\examples{
\dontrun{
## Create an Binmat object
Binmat("path/to/file.extension")
}
}

\keyword{classes}
\keyword{array}
