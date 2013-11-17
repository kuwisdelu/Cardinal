\name{predict-methods}
\docType{methods}
\alias{predict}
\alias{predict-methods}
\alias{predict,MSImageSegmentation-method}
\alias{predict,PCA-method}
\alias{predict,PLS-method}
\alias{predict,OPLS-method}
\title{Prediction}
\description{
	Predicts new results using a previously-fitted model on new data.
}
\usage{
    \S4method{predict}{MSImageSegmentation}(object, newx, newy, autoDimNames=metaData(object)[["autoDimNames"]], \dots)
	
    \S4method{predict}{PCA}(object, newx, ncomp=object$ncomp, \dots)
	
    \S4method{predict}{PLS}(object, newx, newy, ncomp=object$ncomp, \dots)

    \S4method{predict}{OPLS}(object, newx, newy, ncomp=object$ncomp, \dots)
}
\arguments{
    \item{object}{the object representing the previously-fit model.}
    \item{newx}{the new explanatory data, usually an \code{\linkS4class{MSImageSet}} object.}
    \item{newy}{optional, the response variable for the new data.}
    \item{autoDimNames}{the names of which dimensions to treat autonomously in the clustering; most commonly the dimension corresponding to which pixels belong to different slides.}
    \item{ncomp}{the number of components to use for prediction.}
    \item{\dots}{arguments to be passed to \code{method}.}
}
\value{
    A new object of the same class as the original \code{object} with the predicted values.
}
\author{
	Kyle D. Bemis
}
\seealso{
    \code{\linkS4class{MSImageSet}},
    \code{\link[Cardinal]{PCA-methods}},
    \code{\link[Cardinal]{PLS-methods}},
    \code{\link[Cardinal]{OPLS-methods}},
    \code{\link[Cardinal]{spatialClassify-methods}},
    \code{\link[Cardinal]{spatialCluster-methods}}
}
\examples{
    ## write examples to put here
}
\keyword{methods}


