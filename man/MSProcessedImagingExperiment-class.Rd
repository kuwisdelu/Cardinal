\name{MSProcessedImagingExperiment-class}
\docType{class}

\alias{class:MSProcessedImagingExperiment}
\alias{MSProcessedImagingExperiment}
\alias{MSProcessedImagingExperiment-class}

\alias{mzData}
\alias{mzData<-}
\alias{mzData-methods}

\alias{intensityData}
\alias{intensityData<-}
\alias{intensityData-methods}

\alias{mzData,MSProcessedImagingExperiment-method}
\alias{mzData<-,MSProcessedImagingExperiment-method}
\alias{intensityData,MSProcessedImagingExperiment-method}
\alias{intensityData<-,MSProcessedImagingExperiment-method}

\alias{mz<-,MSProcessedImagingExperiment-method}
\alias{resolution<-,MSProcessedImagingExperiment-method}

\alias{iData<-,MSProcessedImagingExperiment,ANY-method}
\alias{iData<-,MSProcessedImagingExperiment,missing-method}
\alias{imageData<-,MSProcessedImagingExperiment-method}

\alias{tolerance,MSProcessedImagingExperiment-method}
\alias{tolerance<-,MSProcessedImagingExperiment-method}
\alias{sampler,MSProcessedImagingExperiment-method}
\alias{sampler<-,MSProcessedImagingExperiment-method}
\alias{domain,MSProcessedImagingSpectraList-method}
\alias{domain<-,MSProcessedImagingSpectraList-method}
\alias{domain<-,MSProcessedImagingSpectraList,ANY-method}
\alias{tolerance,MSProcessedImagingSpectraList-method}
\alias{tolerance<-,MSProcessedImagingSpectraList-method}
\alias{sampler,MSProcessedImagingSpectraList-method}
\alias{sampler<-,MSProcessedImagingSpectraList-method}

\alias{[,MSProcessedImagingSpectraList,ANY,ANY,ANY-method}
\alias{[[<-,MSProcessedImagingSpectraList,ANY,ANY-method}

\title{MSProcessedImagingExperiment: "Processed" mass spectrometry imaging experiments}

\description{
   The \code{MSProcessedImagingExperiment} class is a simple extension of \code{\linkS4class{MSImagingExperiment}} for sparse spectra. All methods for that class apply. In addition, each data element must be stored as a column-major \code{\linkS4class{sparse_mat}}.
}

\section{Methods}{
    All methods for \code{\linkS4class{MSImagingExperiment}} also work on \code{MSProcessedImagingExperiment} objects. Additional methods are documented below:

    \describe{
        \item{\code{intensityData(object)}, \code{intensityData(object) <- value}:}{Get or set the underlying (pre-binned) intensity values associated with the sparse mass spectra.}

        \item{\code{mzData(object)}, \code{mzData(object) <- value}:}{Get or set the underlying (pre-binned) m/z values associated with the sparse mass spectra.}

        \item{\code{mz(object) <- value}:}{Setting the m/z values changes the m/z binning scheme for the entire dataset (without modifying the underlying data).}

        \item{\code{resolution(object) <- value}:}{Setting the m/z resolution changes the m/z binning scheme for the entire dataset (without modifying the underlying data).}

        \item{\code{tolerance(object)}, \code{tolerance(object) <- value}:}{Get or set the binning tolerance for sparse spectra or peaks.}

        \item{\code{combiner(object)}, \code{combiner(object) <- value}:}{Get or set the binning function for sparse spectra or peaks.}

        \item{\code{pull(x, ..., as.matrix=FALSE)}:}{Pull all data elements of \code{imageData} into memory as sparse matrices.}
    }
}

\author{Kylie A. Bemis}

\seealso{
    \code{\linkS4class{MSImagingExperiment}},
    \code{\linkS4class{MSContinuousImagingExperiment}}
}

\keyword{classes}
