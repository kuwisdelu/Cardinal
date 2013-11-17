\name{selectROI-methods}
\docType{methods}
\alias{selectROI}
\alias{selectROI-methods}
\alias{selectROI,MSImageSet-method}
\title{Select pixels belonging to a region of interest}
\description{
	Uses the \code{\link[graphics]{locator}} function to select a region of interest by allowing the user to select pixels that give the vertices of a polygon on a image from an \code{\linkS4class{MSImageSet}}.
}
\usage{
    \S4method{selectROI}{MSImageSet}(object, sliceDimNames=metaData(object)[["coordDimNames"]][c(1,2)], fixCoord=list(), silent=FALSE, \dots)
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
    A \code{logical} vector giving \code{TRUE} for pixels within the selected region and \code{FALSE} for pixels outside the selected region.
}
\author{
	Kyle D. Bemis
}
\seealso{
    \code{\linkS4class{MSImageSet}},
    \code{\link[Cardinal]{selectPixels-methods}},
    \code{\link[Cardinal]{roiBind-methods}},
    \code{\link[graphics]{locator}},
    \code{\link[sp]{point.in.polygon}}
}
\examples{
    ## write examples to put here
}
\keyword{methods}

