\name{imageData-methods}
\docType{methods}

\alias{imageData}
\alias{imageData<-}
\alias{imageData-methods}
\alias{iData}
\alias{iData<-}
\alias{iData-methods}

\alias{spectra}
\alias{spectra<-}
\alias{spectra-methods}

\alias{peaks}
\alias{peaks<-}
\alias{peaks-methods}

\alias{mzData}
\alias{mzData<-}
\alias{mzData-methods}

\alias{peakData}
\alias{peakData<-}
\alias{peakData-methods}

\title{Retrieve Image Data from iSets}
\description{
	These generic functions image data (typically spectra) stored in an object derived from \code{\linkS4class{iSet}}.
}
\usage{
imageData(object)
imageData(object) <- value
iData(object)
iData(object) <- value

spectra(object)
spectra(object) <- value

peaks(object)
peaks(object) <- value

mzData(object)
mzData(object) <- value

peakData(object)
peakData(object) <- value
}
\arguments{
    \item{object}{An object, possible derived from \code{\linkS4class{iSet}}.}
    \item{value}{Value to be assigned to the corresponding object.}
}
\value{
    \code{imageData} returns an object containing both image data and metadata, usually an object derived from \code{\linkS4class{ImageData}}. \code{iData} returns only the image data in a matrix-like object with the rows corresponding to features and the columns corresponding to pixels. \code{spectra} is an \emph{alias} for \code{iData} for use with \code{\linkS4class{MSImageSet}} objects. \code{mzData} and \code{peakData} are used for retrieving both peak data and metadata from peak-picked objects. \code{peaks} retrieves peak cubes from peak-picked objects.
}
\author{
	Kyle D. Bemis
}
\seealso{
    \code{\linkS4class{iSet}},
    \code{\linkS4class{SImageSet}},
    \code{\linkS4class{MSImageSet}}
}
\keyword{manip}
