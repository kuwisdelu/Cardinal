\name{ResultSet-class}
\docType{class}

\alias{class:ResultSet}
\alias{ResultSet}
\alias{ResultSet-class}

\alias{$,ResultSet-method}
\alias{[,ResultSet-method}
\alias{[,ResultSet,ANY,ANY,ANY-method}
\alias{[[,ResultSet-method}
\alias{[[,ResultSet,ANY,ANY-method}
\alias{resultData}
\alias{resultData<-}
\alias{resultData,ResultSet-method}
\alias{resultData<-,ResultSet-method}
\alias{modelData}
\alias{modelData<-}
\alias{modelData,ResultSet-method}
\alias{modelData<-,ResultSet-method}
\alias{length,ResultSet-method}
\alias{names,ResultSet-method}
\alias{show,ResultSet-method}

\title{Class to Contain Analysis Results for Imaging Experiments}

\description{
    This class is used as a return value by most of the analysis methods provided by Cardinal, including \code{\link[Cardinal]{PCA}}, \code{\link[Cardinal]{PLS}}, \code{\link[Cardinal]{OPLS}}, \code{\link[Cardinal]{spatialKMeans}}, \code{\link[Cardinal]{spatialShrunkenCentroids}}.
}

\section{Slots}{
    \describe{
        \item{\code{imageData}:}{This slot is unused in a \code{\linkS4class{ResultSet}}.}

        \item{\code{pixelData}:}{The \code{pixelData} from the analyzed dataset.}

        \item{\code{featureData}:}{The \code{featureData} from the analyzed dataset.}

        \item{\code{experimentData}:}{The \code{experimentData} from the analyzed dataset.}

        \item{\code{protocolData}:}{The \code{protocolData} from the analyzed dataset.}

        \item{\code{resultData}:}{A \code{list} of analysis results. Each element contains the results from a different parameter set.}

        \item{\code{modelData}:}{An \code{\linkS4class{AnnotatedDataFrame}} containing information about the parameters of the models in \code{resultData}.}

        \item{\code{.__classVersion__}:}{A \code{Versions} object describing the version of the class used to created the instance. Intended for developer use.}
    }
}

\section{Extends}{
    \code{\linkS4class{iSet}}, directly.
    \code{\linkS4class{VersionedBiobase}}, by class "iSet", distance 1.
    \code{\linkS4class{Versioned}}, by class "VersionedBiobase", distance 2.
}

\section{Creating Objects}{
    \code{ResultSet} is a virtual class. No instances can be created.
}

\section{Methods}{
    Class-specific methods:
    \describe{
        \item{\code{resultData(object)}:}{Access and set the results of the analyses.}

        \item{\code{modelData(object)}:}{Access and set the model parameters.}
    }

    Standard generic methods:
    \describe{
        \item{\code{length(x)}:}{Access the number of elements of \code{resultData}.}

        \item{\code{names(x)}:}{Access the names of the components of all of the elements of \code{resultData}.}

        \item{\code{ResultSet$name}:}{Access all of the result components with the name \code{name}.}

        \item{\code{ResultSet[[i, ...]]}:}{Access \code{i}th element of the \code{resultData} slot.}

        \item{\code{ResultSet[i, j, ..., drop]}:}{Subset an \code{ResultSet} based on the model parameters in \code{modelData}.}
    }

    See \code{\linkS4class{iSet}} for additional methods.
}

\author{Kyle D. Bemis}

\seealso{
    \code{\link[Cardinal]{iSet}},
    \code{\link[Cardinal]{PCA}},
    \code{\link[Cardinal]{PLS}},
    \code{\link[Cardinal]{OPLS}},
    \code{\link[Cardinal]{spatialKMeans}},
    \code{\link[Cardinal]{spatialShrunkenCentroids}}
}

\keyword{classes}
