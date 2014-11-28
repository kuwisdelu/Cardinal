\name{cvApply-methods}
\docType{methods}

\alias{cvApply}
\alias{cvApply-methods}
\alias{cvApply,SImageSet-method}

\alias{summary,CrossValidated-method}
\alias{print.summary.CrossValidated}
\alias{plot.summary.CrossValidated}

\title{Apply Cross-Validated Analysis to Imaging Datasets}
\description{
	Apply an existing or a user-specified function over imaging datasets. 
}
\usage{
\S4method{cvApply}{SImageSet}(.x, .y, .fun, .fold = sample, \dots)
}
\arguments{
    \item{.x}{An object of class \code{\linkS4class{SImageSet}}.}
    \item{.y}{An appropriate response variable.}
    \item{.fun}{The function to be used for the analyses.}
    \item{.fold}{A variable determining the cross-validation folds. By default, this will set to 'sample' from \code{pixelData(.x)}, to ensure that whole samples are left out during the cross-validation. This argument is evaluated in \code{pixelData(.x)}.}
    \item{\dots}{Additional arguments passed to \code{.fun}.}
}
\details{
    This method is designed to be used with the provided classification methods, but can also be used with user-provided functions and methods as long as they fulfill certain expectations.

    The function or method passed to '.fun' must take at least two arguments: the first argument must be a object derived from \code{\linkS4class{SImageSet}}, and the second argument must be the response variable. The function should return an object of a class derived from \code{\linkS4class{ResultSet}}, which should have a \code{predict} method that takes arguments 'newx' and 'newy'. 
}
\value{
    An object of class 'CrossValidated', which is derived from \code{\linkS4class{ResultSet}}.
}
\author{
	Kyle D. Bemis
}
\seealso{
    \code{\link[Cardinal]{PLS}},
    \code{\link[Cardinal]{OPLS}},
    \code{\link[Cardinal]{spatialShrunkenCentroids}}
}
\keyword{manip}
\keyword{classif}
