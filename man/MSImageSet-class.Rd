\name{MSImageSet-class}
\Rdversion{1.1}
\docType{class}
\alias{$,MSImageSet-method}
\alias{$<-,MSImageSet-method}
\alias{[,MSImageSet,ANY,ANY,ANY-method}
\alias{[,MSPeakFrame,ANY,ANY,ANY-method}
\alias{[,MSPeakList,ANY,ANY,ANY-method}
\alias{[<-,MSImageSet,ANY,ANY,ANY-method}
\alias{[[,MSImageSet,character,missing-method}
\alias{[[,MSImageSet,numeric,missing-method}
\alias{[[<-,MSImageSet,character,missing-method}
\alias{[[<-,MSImageSet,numeric,missing-method}
\alias{MSImageSet}
\alias{MSImageSet-class}
\alias{coord}
\alias{coord,MSImageSet-method}
\alias{coord<-}
\alias{coord<-,MSImageSet-method}
\alias{featureData}
\alias{featureData,MSImageSet-method}
\alias{featureData<-}
\alias{featureData<-,MSImageSet-method}
\alias{features}
\alias{features,MSImageSet,numeric-method}
\alias{generatePositionArray}
\alias{generatePositionArray,data.frame-method}
\alias{generatePositionArray,matrix-method}
\alias{generatePositionArray,MSImageSet-method}
\alias{isBinned}
\alias{isBinned,MSImageSet-method}
\alias{isBinned<-}
\alias{isBinned<-,MSImageSet-method}
\alias{isPeaks}
\alias{isPeaks,MSImageSet-method}
\alias{isPeaks<-}
\alias{isPeaks<-,MSImageSet-method}
\alias{isResampled}
\alias{isResampled,MSImageSet-method}
\alias{isResampled<-}
\alias{isResampled<-,MSImageSet-method}
\alias{metaData}
\alias{metaData,MSImageSet-method}
\alias{metaData<-}
\alias{metaData<-,MSImageSet-method}
\alias{mz}
\alias{mz,MSImageSet-method}
\alias{mz<-}
\alias{mz<-,MSImageSet-method}
\alias{neighbors}
\alias{neighbors,MSImageSet,numeric-method}
\alias{neighbors,data.frame,numeric-method}
\alias{neighbors,matrix,numeric-method}
\alias{numFeatures}
\alias{numFeatures,MSImageSet-method}
\alias{numPixels}
\alias{numPixels,MSImageSet-method}
\alias{peaks}
\alias{peaks,MSImageSet-method}
\alias{peaks<-}
\alias{peaks<-,MSImageSet-method}
\alias{pixelData}
\alias{pixelData,MSImageSet-method}
\alias{pixelData<-}
\alias{pixelData<-,MSImageSet-method}
\alias{pixels}
\alias{pixels,MSImageSet,data.frame-method}
\alias{pixels,MSImageSet,list-method}
\alias{pixels,MSImageSet,numeric-method}
\alias{regeneratePositions}
\alias{regeneratePositions,MSImageSet-method}
\alias{show,MSImageSet-method}
\alias{spectra}
\alias{spectra,MSImageSet-method}
\alias{spectra<-}
\alias{spectra<-,MSImageSet-method}
\alias{standardizeTotalImageIntensity}
\alias{standardizeTotalImageIntensity,MSImageSet-method}
\alias{standardizeTotalIonCurrent}
\alias{standardizeTotalIonCurrent,MSImageSet-method}

\title{Class \code{"MSImageSet"}}
\description{
    A mass spectrometry imaging data set.
}
\section{Objects from the Class}{
    A new \code{\linkS4class{MSImageSet}} can be created using the function
    \code{\link[Cardinal]{createMSImageSet}}.
}
\section{Slots}{
  \describe{
    \item{\code{spectra}:}{\code{"environment"}, with a single \code{matrix} object named 'spectra' storing the intensities for each pixel}
    \item{\code{peaks}:}{\code{"MSPeakFrame"}, data describing the peaks}
    \item{\code{featureData}:}{\code{"data.frame"}, data describing the mass features}
    \item{\code{pixelData}:}{\code{"data.frame"}, data describing the pixel characteristics}
    \item{\code{metaData}:}{\code{"list"}, metadata about the mass spectra data set}
  }
}
\section{Methods}{
  \describe{
    \item{coord}{\code{signature(object = "MSImageSet")}: 
        Accesses the coordinates of the pixels of an \code{\linkS4class{MSImageSet}}. }
    \item{coord<-}{\code{signature(object = "MSImageSet")}: 
        Replaces the coordinates of the pixels of an\code{\linkS4class{MSImageSet}}. }
    \item{crop}{\code{signature(object = "MSImageSet")}: 
        Return a new \code{\linkS4class{MSImageSet}} retaining only a subset of the
        original pixels and features. }
    \item{featureData}{\code{signature(object = "MSImageSet")}: 
        Accessor function for the \code{featureData} slot. }
    \item{featureData<-}{\code{signature(object = "MSImageSet")}: 
        Replacement function for the \code{featureData} slot. }
    \item{features}{\code{signature(object = "MSImageSet")}: 
        Returns the feature ids of masses in an \code{\linkS4class{MSImageSet}}
        within the \code{spectra} slot given mass-to-charge ratios. }
    \item{flipHorizontal}{\code{signature(object = "MSImageSet")}: 
        Flips an \code{\linkS4class{MSImageSet}} horizontally. }
    \item{flipVertical}{\code{signature(object = "MSImageSet")}: 
        Flips an \code{\linkS4class{MSImageSet}} vertically. }
    \item{generatePositionArray}{\code{signature(object = "MSImageSet")}: 
        Generates an array of pixel position ids. }
    \item{image}{\code{signature(x = "MSImageSet")}: 
        Create an image of an \code{\linkS4class{MSImageSet}}. }
    \item{isBinned}{\code{signature(object = "MSImageSet")}: 
        Whether an \code{\linkS4class{MSImageSet}} has been binned or not. }
    \item{isPeaks}{\code{signature(object = "MSImageSet")}: 
        Whether an \code{\linkS4class{MSImageSet}} has been reduced to peaks or not. }
    \item{isResampled}{\code{signature(object = "MSImageSet")}: 
        Whether an \code{\linkS4class{MSImageSet}} has been resampled or not. }
    \item{intensities}{\code{signature(object = "MSImageSet")}: 
        Extract an array of intensities from an \code{\linkS4class{MSImageSet}}. }
    \item{metaData}{\code{signature(object = "MSImageSet")}: 
        Accessor function for the \code{metaData} slot. }
    \item{metaData<-}{\code{signature(object = "MSImageSet")}: 
        Replacement function for the \code{metaData} slot. }
    \item{mz}{\code{signature(object = "MSImageSet")}: 
        Accesses the masses of the mass spectra in an \code{\linkS4class{MSImageSet}}. }
    \item{mz<-}{\code{signature(object = "MSImageSet")}: 
        Replaces the masses of the mass spectra in an \code{\linkS4class{MSImageSet}}. }
    \item{neighbors}{\code{signature(object = "MSImageSet", r = "numeric")}: 
        Finds and returns the pixel ids of the neighbors of a given pixel within a
        given neighborhood radius. }
    \item{numFeatures}{\code{signature(object = "MSImageSet")}: 
        Returns the number of features in an \code{\linkS4class{MSImageSet}}. }
    \item{numPixels}{\code{signature(object = "MSImageSet")}: 
        Returnst the nubmer of pixels in an \code{\linkS4class{MSImageSet}}. }
    \item{peaks}{\code{signature(object = "MSImageSet")}: 
        Accessor function for the \code{peaks} slot. }
    \item{peaks<-}{\code{signature(object = "MSImageSet")}: 
        Replacement function for the \code{peaks} slot. }
    \item{pixelData}{\code{signature(object = "MSImageSet")}: 
        Accessor function for the \code{pixelData} slot. }
    \item{pixelData<-}{\code{signature(object = "MSImageSet")}: 
        Replacement function for the \code{pixelData} slot. }
    \item{plot}{\code{signature(x = "MSImageSet", y = "missing")}: 
        Create a plot of a mass spectrum from an \code{\linkS4class{MSImageSet}}. }
    \item{pixels}{\code{signature(object = "MSImageSet", coord = "data.frame")}: 
        Returns the pixel ids of mass spectra in an \code{\linkS4class{MSImageSet}}
        within the \code{spectra} slot given input coordinates. }
    \item{pixels}{\code{signature(object = "MSImageSet", coord = "list")}: 
        Returns the pixel ids of mass spectra in an \code{\linkS4class{MSImageSet}}
        within the \code{spectra} slot given input coordinates. }
    \item{pixels}{\code{signature(object = "MSImageSet", coord = "numeric")}: 
        Returns the pixel ids of mass spectra in an \code{\linkS4class{MSImageSet}}
        within the \code{spectra} slot given input coordinates. }
    \item{regeneratePositions}{\code{signature(object = "MSImageSet"{})}: 
        Regenerates the array of pixel position ids. }
    \item{rotateLeft}{\code{signature(object = "MSImageSet")}: 
        Rotates an \code{\linkS4class{MSImageSet}} to the left. }
    \item{rotateRight}{\code{signature(object = "MSImageSet")}: 
        Rotates an \code{\linkS4class{MSImageSet}} to the right. }
    \item{sliceBind}{\code{signature(object = "MSImageSet")}:
        Bind multiple \code{\linkS4class{MSImageSet}} objects into a single one. }
    \item{spectra}{\code{signature(object = "MSImageSet")}: 
        Accessor function for the \code{spectra} slot. }
    \item{spectra<-}{\code{signature(object = "MSImageSet")}: 
        Replacement function for the \code{spectra} slot. }
    \item{spectralApply}{\code{signature(object = "MSImageSet")}: 
        Apply-like functionality for an \code{\linkS4class{MSImageSet}}. }
    \item{standardizeTotalImageIntensity}{\code{signature(object = "MSImageSet")}: 
        Standardize the sum image intensities in an \code{\linkS4class{MSImageSet}} to a common total. }
    \item{standardizeTotalIonCurrent}{\code{signature(object = "MSImageSet")}: 
        Standardize all the mass spectra in an \code{\linkS4class{MSImageSet}} to a common total ion current. }
    \item{unload}{\code{signature(object = "MSImageSet")}: 
        Remove an \code{\linkS4class{MSImageSet}} object and free its memory. }
	 }
}
\author{
    Kyle D. Bemis
}

\seealso{
    \code{\link[Cardinal]{createMSImageSet}}
}
\examples{
    ## write examples to add here
}
\keyword{classes}
