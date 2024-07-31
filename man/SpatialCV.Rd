\name{SpatialCV}

\alias{SpatialCV}
\alias{class:SpatialCV}
\alias{SpatialCV-class}

\alias{crossValidate}
\alias{fitted,SpatialCV-method}
\alias{image,SpatialCV-method}

\title{Cross-validation for spectral imaging data}

\description{
	Apply cross-validation with an existing or a user-specified modeling function over folds of a spectral imaging dataset.
}

\usage{
crossValidate(fit., x, y, folds = run(x), \dots,
    predict. = predict, keep.models = FALSE,
    trainProcess = peakProcess, trainArgs = list(),
    testProcess = peakProcess, testArgs = list(),
    verbose = getCardinalVerbose(), chunkopts = list(),
    BPPARAM = getCardinalBPPARAM())

\S4method{fitted}{SpatialCV}(object, type = c("response", "class"), \dots)

\S4method{image}{SpatialCV}(x, i = 1L, type = c("response", "class"),
        layout = NULL, free = "", \dots)
}

\arguments{
    \item{fit.}{The function used to fit the model.}

    \item{x, y}{The data and response variable, where \code{x} is assumed to be an P x N dataset such as a \code{SpectralImagingExperiment}}

    \item{folds}{A vector coercible to a factor giving the fold for each row or column of \code{x}.}

    \item{\dots}{Additional arguments passed to \code{fit.} and \code{predict.}.}

    \item{predict.}{The function used to predict on new data from the fitted model. The fitted model is passed as the 1st argument and the test data is passed as the 2nd argument.}

    \item{keep.models}{Should the models be kept and returned?}

    \item{trainProcess, trainArgs}{A function and arguments used for processing the training sets. The training set is passed as the 1st argument to \code{trainProcess}.}

    \item{testProcess, testArgs}{A function and arguments used for processing the test sets. The test set is passed as the 1st argument to \code{trainProcess}, and the processed training set is passed as the 2nd argument.}

    \item{verbose}{Should progress be printed for each iteration?}

    \item{BPPARAM}{An optional instance of \code{BiocParallelParam}. See documentation for \code{\link{bplapply}}. \emph{Passed} to \code{fit.}, \code{predict.}, \code{trainProcess} and \code{testProcess}.}

    \item{object}{An object inheriting from \code{SpatialCV}.}

    \item{type}{The type of prediction, where \code{"response"} means the fitted response matrix and \code{"class"} will be the vector of class predictions (only for classification).}

    \item{i}{If predictions are made for multiple sets of parameters, which set of parameters (i.e., which element of the \code{fitted.values} list) should be plotted?}

    \item{layout}{A vector of the form \code{c(nrow, ncol)} specifying the number of rows and columns in the facet grid.}

    \item{free}{A string specifying the free spatial dimensions during faceting. E.g., \code{""}, \code{"x"}, \code{"y"}, \code{"xy"}, \code{"yx"}.}
}

\details{
    This method is designed to be used with the provided classification methods, but can also be used with user-provided functions and methods as long as they conform to certain expectations. Internally, \code{\link{cv_do}} from the \code{matter} package is used to perform the cross-validation. See \code{?cv_do} for details.
}

\value{
    An object of class \code{SpatialCV} derived from \code{SpatialResults} and containing accuracies for each fold, the predictions for each fold, and (optionally) the fitted models.
}

\author{
	Kylie A. Bemis
}

\seealso{
    \code{\link{cv_do}},
    \code{\link{spatialShrunkenCentroids}},
    \code{\link{PLS}},
    \code{\link{OPLS}}
}

\keyword{utilities}
\keyword{classif}
\keyword{regression}
