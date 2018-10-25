\name{MSProcessedImagingExperiment-class}
\docType{class}

\alias{class:MSProcessedImagingExperiment}
\alias{MSProcessedImagingExperiment}
\alias{MSProcessedImagingExperiment-class}

\alias{mz<-,MSProcessedImagingExperiment-method}

\alias{iData<-,MSProcessedImagingExperiment,ANY-method}
\alias{iData<-,MSProcessedImagingExperiment,missing-method}
\alias{imageData<-,MSProcessedImagingExperiment-method}

\alias{mzData}
\alias{mzData<-}
\alias{mzData-methods}

\alias{peakData}
\alias{peakData<-}
\alias{peakData-methods}

\alias{mzData,MSProcessedImagingExperiment-method}
\alias{mzData<-,MSProcessedImagingExperiment-method}
\alias{peakData,MSProcessedImagingExperiment-method}
\alias{peakData<-,MSProcessedImagingExperiment-method}

\alias{tolerance,MSProcessedImagingExperiment-method}
\alias{tolerance<-,MSProcessedImagingExperiment-method}

\alias{combiner,MSProcessedImagingExperiment-method}
\alias{combiner<-,MSProcessedImagingExperiment-method}

\alias{[,MSProcessedImagingSpectraList,ANY,ANY,ANY-method}
\alias{[[<-,MSProcessedImagingSpectraList,ANY,ANY-method}

\alias{keys,MSProcessedImagingSpectraList-method}
\alias{keys<-,MSProcessedImagingSpectraList,ANY-method}

\alias{tolerance,MSProcessedImagingSpectraList-method}
\alias{tolerance<-,MSProcessedImagingSpectraList-method}

\alias{combiner,MSProcessedImagingSpectraList-method}
\alias{combiner<-,MSProcessedImagingSpectraList-method}

\title{MSProcessedImagingExperiment: Dense mass spectrometry imaging experiments}

\description{
   The \code{MSProcessedImagingExperiment} class is a simple extension of \code{\linkS4class{MSImagingExperiment}} for sparse spectra. All methods for that class apply. In addition, each data element must be stored as a column-major \code{\linkS4class{sparse_mat}}.
}

\section{Methods}{
    All methods for \code{\linkS4class{ImagingExperiment}} and \code{\linkS4class{SparseImagingExperiment}} also work on \code{MSProcessedImagingExperiment} objects. Additional methods are documented below:

    \describe{
        \item{\code{mzData(object)}, \code{mzData(object) <- value}:}{Get or set the underlying (pre-binned) m/z values associated with the sparse mass spectra.}

        \item{\code{peakData(object)}, \code{peakData(object) <- value}:}{Get or set the underlying (pre-binned) intensity values associated with the sparse mass spectra.}

        \item{\code{tolerance(object)}, \code{tolerance(object) <- value}:}{Get or set the binning tolerance for sparse spectra or peaks.}

        \item{\code{combiner(object)}, \code{combiner(object) <- value}:}{Get or set the binning function for sparse spectra or peaks.}
    }
}

\author{Kylie A. Bemis}

\seealso{
    \code{\linkS4class{MSImagingExperiment}},
    \code{\linkS4class{MSContinuousImagingExperiment}}
}

\keyword{classes}
