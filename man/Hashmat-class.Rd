\name{Hashmat-class}
\docType{class}

\alias{class:Hashmat}
\alias{Hashmat}
\alias{Hashmat-class}

\alias{[,Hashmat-method}
\alias{[<-,Hashmat-method}
\alias{[,Hashmat,ANY,ANY,ANY-method}
\alias{[<-,Hashmat,ANY,ANY,ANY-method}
\alias{dim,Hashmat-method}
\alias{dim<-,Hashmat-method}
\alias{dimnames,Hashmat-method}
\alias{dimnames<-,Hashmat-method}
\alias{dimnames<-,Hashmat,ANY-method}
\alias{colnames,Hashmat-method}
\alias{colnames<-,Hashmat-method}
\alias{rownames,Hashmat-method}
\alias{rownames<-,Hashmat-method}
\alias{cbind,Hashmat-method}
\alias{rbind,Hashmat-method}
\alias{pData,Hashmat-method}
\alias{pData<-,Hashmat-method}
\alias{pData<-,Hashmat,ANY-method}
\alias{keys}
\alias{keys<-}
\alias{keys,Hashmat-method}
\alias{keys<-,Hashmat-method}
\alias{keys<-,Hashmat,character-method}
\alias{keys<-,Hashmat,list-method}
\alias{combine,Hashmat,Hashmat-method}
\alias{initialize,Hashmat-method}
\alias{show,Hashmat-method}

\title{Sparse Matrix Class Using Lists as Hash Tables}

\description{
    The Hashmat class implements compressed sparse column (CSC) style matrices using R \code{list} objects as the columns. The implementation is unique in that it allows re-assignment of the keys describing the rows, allowing for arbitrary re-ordering of rows and row-wise elements. This is useful for storing sparse signals, such as processed spectra.
}

\usage{
## Instance creation
Hashmat(data = NA, nrow = 1, ncol = 1, byrow=FALSE,
    dimnames = NULL, \dots)

## Additional methods documented below
}

\arguments{
    
    \item{data}{A \code{matrix} or a \code{vector}. If \code{data} is a \code{matrix}, then a sparse matrix is construced from matrix directly and other arguments (except for \code{dimnames}) are ignored. If \code{data} is a \code{vector}, then the behavior is the same as for ordinary matrix construction.}

    \item{nrow}{The number of rows in the sparse matrix.}

    \item{ncol}{The number of columns in the sparse matrix.}

    \item{byrow}{If 'FALSE', the matrix is filled by columns. If 'TRUE', it is filled by rows.}

    \item{dimnames}{The 'dimnames' giving the dimension names for the matrix, analogous to the 'dimnames' attribute of an ordinary R matrix. This must be a list of length 2 or \code{NULL}.}

    \item{\dots}{Additional arguments passed to the constructor.}

}

\section{Slots}{
    \describe{
        \item{\code{data}:}{A \code{list} with vectors corresponding columns of the sparse matrix, whose elements are its non-zero elements.}

        \item{\code{keys}:}{A \code{character} vector providing the keys that determine the rows of the non-zero elements of the matrix.}
        
        \item{\code{dim}:}{A length 2 integer vector analogous to the 'dim' attribute of an ordinary R matrix.}

        \item{\code{dimnames}:}{A length 2 \code{list} analogous to the 'dimnames' attribute of an ordinary R matrix.}

        \item{\code{.__classVersion__}:}{A \code{Versions} object describing the version of the class used to created the instance. Intended for developer use.}
    }
}

\section{Extends}{
   \code{\linkS4class{Versioned}}
}

\section{Creating Objects}{
    \code{Hashmat} instances are usually created through \code{Hashmat()}.
}

\section{Methods}{
    Class-specific methods:
    \describe{
        \item{\code{pData(object)}, \code{pData(object)<-}:}{Access or set the list of \code{numeric} vectors storing the column-vectors of the sparse matrix directly.}

        \item{\code{keys(object)}, \code{keys(object)<-}:}{Access of set the keys for the row elements. If this is a \code{character}, it sets the \code{keys} slot directly, and hence the 'dim' is also changed. If this is a \code{list}, then the list should have length equal to the number of rows, and each element should be an \code{integer} vector of length equal to the number of non-zero row elements for the respective column. The vectors are used to index the \code{keys} slot and set the key names of the vectors, and hence change or reorder the row elements.}
    }

    Standard generic methods:
    \describe{
        \item{\code{combine(x, y, ...)}:}{Combines two Hashmat objects. See the \code{\link[BiocGenerics]{combine}} method for matrices for details of how the Hashmat sparse matrices are combined. The behavior is identical, except when filling in missing elements in non-shared rows and columns, the resulting Hashmat object will have zeroes instead of NAs.}

        \item{\code{dim(x), dim(x) <- value}:}{Return or set the dimensions of the sparse matrix.}

        \item{\code{dimnames(x), dimnames(x) <- value}:}{Return or set the 'dimnames' of the sparse matrix.}

        \item{\code{colnames(x), colnames(x) <- value}:}{Return or set the column names of the sparse matrix.}

        \item{\code{rownames(x), rownames(x) <- value}:}{Return or set the row names of the sparse matrix.}

        \item{\code{ncol}:}{Return the number of columns in the sparse matrix.}

        \item{\code{nrow}:}{Return the number of columns in the sparse matrix.}

        \item{\code{cbind}:}{Combine sparse matrices by columns. The keys used to resolve the rows must match between matrices.}

        \item{\code{rbind}:}{Not allowed for sparse matrices. (Always returns an error.)}

        \item{\code{Hashmat[i, j, ..., drop], Hashmat[i, j, ...] <- value}:}{Access and assign elements in the sparse matrix. A Hashmat sparse matrix can be indexed like an ordinary R matrix. Note however that linear indexing is not supported. Use \code{drop = NA} to return a subset of the same class as the object.}
    }
}

\author{Kyle D. Bemis}

\seealso{
    \code{\link[base]{matrix}},
    \code{\linkS4class{Binmat}},
    \code{\linkS4class{SImageSet}}
}

\examples{
## Create an Hashmat object
Hashmat()

## Using a list of elements and keys
dmat1 <- diag(3)
smat1 <- Hashmat(dmat1)
all.equal(smat1[], dmat1, check.attr=FALSE)

## Filling an empty sparse matrix
smat2 <- Hashmat(nrow=1000, ncol=1000)
smat2[500,] <- rep(1, 1000)

dmat2 <- matrix(nrow=1000, ncol=1000)
dmat2[500,] <- rep(1, 1000)

print(object.size(dmat2), units="Mb")
print(object.size(smat2), units="Mb") # Much smaller

all.equal(dmat2[500,], smat2[500,], , check.attr=FALSE)
}

\keyword{classes}
\keyword{array}
