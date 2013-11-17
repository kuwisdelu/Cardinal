\name{selectPixels-methods}
\docType{methods}
\alias{selectPixels}
\alias{selectPixels-methods}
\alias{selectPixels,MSImageSet-method}
\title{Select pixels belonging to a region of interest}
\description{
	Uses the \code{\link[graphics]{locator}} function to select mass spectra of interest by allowing the user to select pixels on a image from an \code{\linkS4class{MSImageSet}}.
}
\usage{
    \S4method{selectPixels}{MSImageSet}(object, sliceDimNames=metaData(object)[["coordDimNames"]][c(1,2)], fixCoord=list(), silent=FALSE, \dots)
}
\arguments{
    \item{object}{\code{\linkS4class{MSImageSet}} data.}
    \item{sliceDimNames}{\code{character}, giving the name of the sliced dimensions.}
    \item{fixCoord}{\code{list}, giving the fixed coordinates.}
    \item{silent}{\code{logical}, indicating whether messages should be sent.}
    \item{\dots}{ignored.}
}
\details{
    When this method is called, it is assumed that an appropriate image from a call to \code{\link[Cardinal]{image}} has already been plotted. Since this method relies on \code{\link[graphics]{locator}} functionality, it is incompatible with grid graphics, and thus cannot be used with \code{\link[Cardinal]{trellisImage}}.
}
\value{
    A \code{logical} vector giving \code{TRUE} for selected pixels and \code{FALSE} for pixels not selected.
}
\author{
	Kyle D. Bemis
}
\seealso{
    \code{\linkS4class{MSImageSet}},
    \code{\link[Cardinal]{selectROI-methods}},
    \code{\link[Cardinal]{roiBind-methods}},
    \code{\link[graphics]{locator}},
    \code{\link[sp]{point.in.polygon}}
}
\examples{
    ## write examples to put here
}
\keyword{methods}

