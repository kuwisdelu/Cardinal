\name{selectPeaks-methods}
\docType{methods}
\alias{selectPeaks}
\alias{selectPeaks-methods}
\alias{selectPeaks,MSImageSet-method}
\title{Select peaks on a mass spectrum}
\description{
	Uses the \code{\link[graphics]{locator}} function to select peaks by allowing the user to select m/z-values that give the boundaries of peaks on a plot from an \code{\linkS4class{MSImageSet}}.
}
\usage{
    \S4method{selectPeaks}{MSImageSet}(object, pixel, coord, silent=FALSE, \dots)
}
\arguments{
    \item{object}{\code{\linkS4class{MSImageSet}} data.}
    \item{pixel}{the pixel from which to use the mass spectrum.}
    \item{coord}{a named vector or named \code{list}, or a \code{data.frame} giving the coordinates of the pixel to use to plot the mass spectrum.}
    \item{silent}{\code{logical}, indicating whether messages should be sent.}
    \item{\dots}{ignored.}
}
\details{
    When this method is called, it is assumed that an appropriate mass spectrum plot from a call to \code{\link[Cardinal]{plot}} has already been plotted. Since this method relies on \code{\link[graphics]{locator}} functionality, it is incompatible with grid graphics, and thus cannot be used with \code{\link[Cardinal]{trellisPlot}}.
}
\value{
    An object of class \code{MSPeakFrame} giving the selected peaks.
}
\author{
	Kyle D. Bemis
}
\seealso{
    \code{\linkS4class{MSImageSet}},
    \code{\link[Cardinal]{detectPeaks-methods}},
    \code{\link[graphics]{locator}}
}
\examples{
    ## write examples to put here
}
\keyword{methods}

