\name{MSImagingInfo-class}
\docType{class}

\alias{class:MSImagingInfo}
\alias{MSImagingInfo}
\alias{MSImagingInfo-class}

\alias{length,MSImagingInfo-method}
\alias{as.list,MSImagingInfo-method}
\alias{scans,MSImagingInfo-method}
\alias{mzData,MSImagingInfo-method}
\alias{peakData,MSImagingInfo-method}
\alias{imageData,MSImagingInfo-method}

\alias{msiInfo}
\alias{msiInfo,MSImageSet-method}
\alias{msiInfo,MSImagingExperiment-method}
\alias{msiInfo,MSContinuousImagingExperiment-method}
\alias{msiInfo,MSProcessedImagingExperiment-method}

\alias{normalization}
\alias{normalization<-}
\alias{normalization,Vector-method}
\alias{normalization<-,Vector-method}
\alias{smoothing}
\alias{smoothing<-}
\alias{smoothing,Vector-method}
\alias{smoothing<-,Vector-method}
\alias{baselineReduction}
\alias{baselineReduction<-}
\alias{baselineReduction,Vector-method}
\alias{baselineReduction<-,Vector-method}
\alias{peakPicking}
\alias{peakPicking<-}
\alias{peakPicking,Vector-method}
\alias{peakPicking<-,Vector-method}
\alias{spectrumRepresentation}
\alias{spectrumRepresentation<-}
\alias{spectrumRepresentation,Vector-method}
\alias{spectrumRepresentation<-,Vector-method}

\alias{pixelSize}
\alias{pixelSize,Vector-method}
\alias{instrumentModel}
\alias{instrumentModel,Vector-method}
\alias{instrumentVendor}
\alias{instrumentVendor,Vector-method}
\alias{matrixApplication}
\alias{matrixApplication,Vector-method}
\alias{massAnalyzerType}
\alias{massAnalyzerType,Vector-method}
\alias{ionizationType}
\alias{ionizationType,Vector-method}
\alias{scanPolarity}
\alias{scanPolarity,Vector-method}
\alias{scanType}
\alias{scanType,Vector-method}
\alias{scanPattern}
\alias{scanPattern,Vector-method}
\alias{scanDirection}
\alias{scanDirection,Vector-method}
\alias{lineScanDirection}
\alias{lineScanDirection,Vector-method}

\title{MSImagingInfo: Mass spectrometry imaging metadata for imzML conversion}

\description{
    The \code{MSImagingInfo} class is designed to contain metadata for reading/writing Cardinal objects from/to imzML files.
}

\section{Methods}{
    \describe{
        \item{\code{length(object)}:}{The number of scans (i.e., the number of mass spectra).}

        \item{\code{scans(object)}:}{Access the scan list metadata for writing to imzML.}

        \item{\code{mzData(object)}:}{Access the m/z array list metadata for writing to imzML.}

        \item{\code{peakData(object)}:}{Access the intensity array list metadata for writing to imzML (identical to \code{imageData(object)}).}

        \item{\code{imageData(object)}:}{Access the intensity array list metadata for writing to imzML (identical to \code{peakData(object)}).}

        \item{\code{normalization(object), normalization(object) <- value}:}{Accessor and setter function for the \code{normalization}.}

        \item{\code{smoothing(object), smoothing(object) <- value}:}{Accessor and setter function for the \code{smoothing}.}

        \item{\code{baselineReduction(object), baselineReduction(object) <- value}:}{Accessor and setter function for the \code{baselineReduction}.}

        \item{\code{peakPicking(object), peakPicking(object) <- value}:}{Accessor and setter function for the \code{peakPicking}.}

        \item{\code{spectrumRepresentation(object), spectrumRepresentation(object) <- value}:}{Accessor and setter function for the \code{spectrumRepresentation}.}

        \item{\code{matrixApplication(object)}:}{Accessor function for \code{matrixApplication}.}

        \item{\code{pixelSize(object)}:}{Accessor function for \code{pixelSize}.}

        \item{\code{instrumentModel(object)}:}{Accessor function for \code{instrumentModel}.}
        
        \item{\code{instrumentVendor(object)}:}{Accessor function for \code{instrumentVendor}.}

        \item{\code{massAnalyzerType(object)}:}{Accessor function for \code{massAnalyzerType}.}

        \item{\code{ionizationType(object)}:}{Accessor function for \code{ionizationType}.}

        \item{\code{scanPolarity(object)}:}{Accessor function for \code{scanPolarity}.}
                
        \item{\code{scanType(object)}:}{Accessor function for \code{scanType}.}

        \item{\code{scanPattern(object)}:}{Accessor function for \code{scanPattern}.}

        \item{\code{scanDirection(object)}:}{Accessor function for \code{scanDirection}.}

        \item{\code{lineScanDirection(object)}:}{Accessor function for \code{lineScanDirection}.}
    }
}

\author{Kylie A. Bemis}

\references{
    Schramm T, Hester A, Klinkert I, Both J-P, Heeren RMA, Brunelle A, Laprevote O, Desbenoit N, Robbe M-F, Stoeckli M, Spengler B, Rompp A (2012) imzML - A common data format for the flexible exchange and processing of mass spectrometry imaging data. Journal of Proteomics 75 (16):5106-5110. doi:10.1016/j.jprot.2012.07.026
}

\seealso{
    \code{\linkS4class{MIAxE}},
    \code{\linkS4class{MIAPE-Imaging}}
}

\examples{
mz <- mz(from=200, to=220, by=400)
coord <- expand.grid(x=1:3, y=1:3)
data <- matrix(runif(length(mz) * nrow(coord)),
    nrow=length(mz), ncol=nrow(coord))

x <- MSImagingExperiment(
    imageData=ImageArrayList(data),
    featureData=MassDataFrame(mz=mz),
    pixelData=PositionDataFrame(coord=coord))

msiInfo(x)
}

\keyword{classes}
