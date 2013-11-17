\name{roiBind-methods}
\docType{methods}
\alias{roiBind}
\alias{roiBind-methods}
\alias{roiBind,logical-method}
\title{Combine regions-of-interest}
\description{
	Combines multiple \code{logical} vectors from calls to \code{\link[Cardinal]{selectPixels-methods}} or \code{\link[Cardinal]{selectROI-methods}}, to create a factor for regions-of-interest.
}
\usage{
    \S4method{roiBind}{logical}(object, \dots, labels=NULL, na.replace=FALSE, ordered=FALSE)
}
\arguments{
    \item{object}{\code{logical} vector giving an ROI.}
    \item{\dots}{additional \code{logical} vectors or \code{integer} vectors giving additional ROIs.}
    \item{labels}{\code{character}, giving the names of the ROIs}
    \item{na.replace}{\code{logical}, indicating whether missing values should be treated as missing, or as a separate ROI}
    \item{ordered}{\code{logical}, indicating whether to create an ordered factor}
}
\details{
    If 'na.replace' is \code{TRUE} and 'labels' are specified, a label for the missing values must also be given.
}
\value{
    A \code{factor} with the ROIs as levels.
}
\author{
	Kyle D. Bemis
}
\seealso{
    \code{\link[Cardinal]{selectROI-methods}},
    \code{\link[Cardinal]{selectPixels-methods}}
}
\examples{
    ## write examples to put here
}
\keyword{methods}

