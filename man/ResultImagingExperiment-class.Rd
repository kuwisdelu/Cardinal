\name{ResultImagingExperiment-class}
\docType{class}

\alias{class:ResultImagingExperiment}
\alias{ResultImagingExperiment}
\alias{ResultImagingExperiment-class}

\alias{class:SparseResultImagingExperiment}
\alias{SparseResultImagingExperiment}
\alias{SparseResultImagingExperiment-class}

\alias{modelData}
\alias{modelData<-}
\alias{resultData}
\alias{resultData<-}
\alias{resultNames}
\alias{resultNames<-}

\alias{show,ResultImagingExperiment-method}
\alias{$,ResultImagingExperiment-method}
\alias{[[,ResultImagingExperiment,ANY,ANY-method}
\alias{[[<-,ResultImagingExperiment,ANY,ANY-method}
\alias{[[,SparseResultImagingExperiment,ANY,ANY-method}
\alias{[[<-,SparseResultImagingExperiment,ANY,ANY-method}
\alias{modelData,ResultImagingExperiment-method}
\alias{modelData<-,ResultImagingExperiment-method}
\alias{resultData,ResultImagingExperiment,ANY-method}
\alias{resultData,ResultImagingExperiment,missing-method}
\alias{resultData<-,ResultImagingExperiment,ANY-method}
\alias{resultData<-,ResultImagingExperiment,missing-method}
\alias{resultNames,ResultImagingExperiment-method}

\alias{show,SparseResultImagingExperiment-method}
\alias{[,SparseResultImagingExperiment,ANY,ANY,ANY-method}
\alias{cbind,SparseResultImagingExperiment-method}
\alias{rbind,SparseResultImagingExperiment-method}
\alias{combine,SparseResultImagingExperiment,ANY-method}

\title{ResultImagingExperiment: Results of statistical analysis of imaging experiments}

\description{
   The \code{ResultImagingExperiment} class is a virtual class for containing the results of statistical analyses applied to imaging experiments. It includes the pixel and feature metadata of the original imaging experiment, but the image data may be missing. The results are stored as a list, where each element contains the results of a single model or parameter set. Results from multiple models or parameter sets may be stored together.

   The \code{SparseResultImagingExperiment} subclass inherits from both \code{\linkS4class{SparseImagingExperiment}} and \code{ResultImagingExperiment}.
}

\section{Slots}{
    \describe{
        \item{\code{imageData}:}{An object inheriting from \code{\linkS4class{ImageArrayList}}, storing one or more array-like data elements with conformable dimensions. This may be empty.}

        \item{\code{featureData}:}{Contains feature information in a \code{\linkS4class{XDataFrame}}. Each row includes the metadata for a single feature (e.g., a color channel, a molecular analyte, or a mass-to-charge ratio).}

        \item{\code{elementMetadata}:}{Contains pixel information in a \code{\linkS4class{PositionDataFrame}}. Each row includes the metadata for a single observation (e.g., a pixel), including specialized slot-columns for tracking pixel coordinates and experimental runs.}

        \item{\code{resultData}:}{A \code{List} containing the results of statistical analysis. Each element contains the results of a single model or parameter set.}

        \item{\code{modelData}:}{A \code{DataFrame} providing details about the models or parameters used in the analysis. Must have the same number of rows as the length of \code{resultData}.}

        \item{\code{metadata}:}{A \code{list} containing experiment-level metadata.}
    }
}

\section{Methods}{
    All methods for \code{\linkS4class{ImagingExperiment}} also work on \code{ResultImagingExperiment} objects. Additional methods are documented below:

    \describe{
        \item{\code{modelData(object)}, \code{modelData(object) <- value}:}{Get or set the \code{modelData}.}

        \item{\code{resultData(object, i, j)}, \code{resultData(object, i) <- value}:}{Get or set the corresponding element of \code{resultData}.}

        \item{\code{resultNames(object)}:}{Get the names of the components of \code{resultData}.}
    }
}

\author{Kylie A. Bemis}

\seealso{
    \code{\linkS4class{ImagingExperiment}},
    \code{\linkS4class{SparseImagingExperiment}}
}

\keyword{classes}
