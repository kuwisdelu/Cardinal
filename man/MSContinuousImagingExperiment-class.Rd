\name{MSContinuousImagingExperiment-class}
\docType{class}

\alias{class:MSContinuousImagingExperiment}
\alias{MSContinuousImagingExperiment}
\alias{MSContinuousImagingExperiment-class}

\alias{iData<-,MSContinuousImagingExperiment,ANY-method}
\alias{iData<-,MSContinuousImagingExperiment,missing-method}
\alias{imageData<-,MSContinuousImagingExperiment-method}

\alias{[,MSContinuousImagingSpectraList,ANY,ANY,ANY-method}
\alias{[[<-,MSContinuousImagingSpectraList,ANY,ANY-method}

\title{MSContinuousImagingExperiment: "Continuous" mass spectrometry imaging experiments}

\description{
   The \code{MSContinuousImagingExperiment} class is a simple extension of \code{\linkS4class{MSImagingExperiment}} for dense spectra. All methods for that class apply. In addition, each data element must be stored as an ordinary R \code{matrix} or a column-major \code{\linkS4class{matter_mat}}.
}

\author{Kylie A. Bemis}

\seealso{
    \code{\linkS4class{MSImagingExperiment}},
    \code{\linkS4class{MSProcessedImagingExperiment}}
}

\keyword{classes}
