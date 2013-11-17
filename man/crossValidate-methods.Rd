\name{crossValidate-methods}
\docType{methods}
\alias{crossValidate}
\alias{crossValidate-methods}
\alias{crossValidate,MSImageSet-method}
\title{Cross Validation}
\description{
	Perform cross-validation on an \code{\linkS4class{MSImageSet}} using one of the classification methods \code{\link[Cardinal]{PLS}}, \code{\link[Cardinal]{OPLS}}, \code{\link[Cardinal]{spatialClassify}}, or \code{\link[Cardinal]{spatialSparseClassify}}.
}
\usage{
    \S4method{crossValidate}{MSImageSet}(x, y, doMethod=c("spatialClassify", "spatialSparseClassify", "PLS", "OPLS"), nfolds, foldid=NULL, \dots)
}
\arguments{
    \item{x}{\code{\linkS4class{MSImageSet}} data.}
    \item{y}{the response variable.}
    \item{doMethod}{the name of a predefined or user-defined method for classification, that can handle missing values and has a \code{predict} method for the results.}
    \item{nfolds}{the number of folds in the cross-validation.}
    \item{foldid}{\code{integer}, giving the ids of which pixels belong to which fold in the cross-validation, most commonly which pixels belong to which slides, subjects, or samples.}
    \item{\dots}{arguments to be passed to \code{method}.}
}
\details{
    When using cross-validation with the \code{\link[Cardinal]{spatialClassify}} or \code{\link{spatialSparseClassify}} methods, it is vitally important to remember to set 'autoDimNames' correctly so that different folds do not influence each other. In practice, 'foldid' should refer to the same coordinate variable as 'autoDimNames' in the classification methods.
}
\value{
    A object of class \code{CrossValidation}, which is a list with components containing the output object for each fold.
}
\author{
	Kyle D. Bemis
}
\seealso{
    \code{\link[Cardinal]{summary}}
    \code{\link[Cardinal]{PLS}}
    \code{\link[Cardinal]{OPLS}}
    \code{\link[Cardinal]{spatialClassify}}
    \code{\link[Cardinal]{spatialSparseClassify}}
}
\examples{
    ## write examples to put here
}
\keyword{methods}

