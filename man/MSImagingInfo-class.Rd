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

\alias{normalization,Vector-method}
\alias{normalization<-,Vector-method}
\alias{smoothing,Vector-method}
\alias{smoothing<-,Vector-method}
\alias{baselineReduction,Vector-method}
\alias{baselineReduction<-,Vector-method}
\alias{peakPicking,Vector-method}
\alias{peakPicking<-,Vector-method}
\alias{spectrumRepresentation,Vector-method}
\alias{spectrumRepresentation<-,Vector-method}
\alias{pixelSize,Vector-method}
\alias{pixelSize<-,Vector-method}
\alias{instrumentModel,Vector-method}
\alias{instrumentModel<-,Vector-method}
\alias{instrumentVendor,Vector-method}
\alias{instrumentVendor<-,Vector-method}
\alias{matrixApplication,Vector-method}
\alias{matrixApplication<-,Vector-method}
\alias{massAnalyzerType,Vector-method}
\alias{massAnalyzerType<-,Vector-method}
\alias{ionizationType,Vector-method}
\alias{ionizationType<-,Vector-method}
\alias{scanPolarity,Vector-method}
\alias{scanPolarity<-,Vector-method}
\alias{scanType,Vector-method}
\alias{scanType<-,Vector-method}
\alias{scanPattern,Vector-method}
\alias{scanPattern<-,Vector-method}
\alias{scanDirection,Vector-method}
\alias{scanDirection<-,Vector-method}
\alias{lineScanDirection,Vector-method}
\alias{lineScanDirection<-,Vector-method}

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

        \item{\code{spectrumRepresentation(object), spectrumRepresentation(object) <- value}:}{Accessor and setter function for the \code{peakPicking}.}

        \item{\code{matrixApplication(object), matrixApplication(object) <- value}:}{Accessor and setter function for \code{matrixApplication}.}

        \item{\code{pixelSize(object), pixelSize(object) <- value}:}{Accessor and setter function for \code{pixelSize}.}

        \item{\code{instrumentModel(object), instrumentModel(object) <- value}:}{Accessor and setter function for \code{instrumentModel}.}
        
        \item{\code{instrumentVendor(object), instrumentVendor(object) <- value}:}{Accessor and setter function for \code{instrumentVendor}.}

        \item{\code{massAnalyzerType(object), massAnalyzerType(object) <- value}:}{Accessor and setter function for \code{massAnalyzerType}.}

        \item{\code{ionizationType(object), ionizationType(object) <- value}:}{Accessor and setter function for \code{ionizationType}.}

        \item{\code{scanPolarity(object), scanPolarity(object) <- value}:}{Accessor and setter function for \code{scanPolarity}.}
                
        \item{\code{scanType(object), scanType(object) <- value}:}{Accessor and setter function for \code{scanType}.}

        \item{\code{scanPattern(object), scanPattern(object) <- value}:}{Accessor and setter function for \code{scanPattern}.}

        \item{\code{scanDirection(object), scanDirection(object) <- value}:}{Accessor and setter function for \code{scanDirection}.}

        \item{\code{lineScanDirection(object), lineScanDirection(object) <- value}:}{Accessor and setter function for \code{lineScanDirection}.}
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
data <- matrix(1:9^2, nrow=9, ncol=9)
mz <- seq(from=100, to=105, length.out=9)
coord <- expand.grid(x=1:3, y=1:3)

x <- MSImagingExperiment(
    imageData=ImageArrayList(data),
    featureData=MassDataFrame(mz=mz),
    pixelData=PositionDataFrame(coord=coord))

msiInfo(x)
}

\keyword{classes}
