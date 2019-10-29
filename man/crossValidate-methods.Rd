\name{cvApply-methods}
\docType{methods}

\alias{cvApply}
\alias{cvApply-methods}

\alias{crossValidate}
\alias{crossValidate-methods}

\alias{cvApply,SImageSet-method}
\alias{cvApply,SparseImagingExperiment-method}
\alias{crossValidate,SparseImagingExperiment-method}
\alias{crossValidate,MSImagingExperiment-method}
\alias{summary,CrossValidated2-method}

\alias{summary,CrossValidated-method}
\alias{print.summary.CrossValidated}
\alias{plot.summary.CrossValidated}

\title{Apply cross-validation to imaging analyses}
\description{
	Apply cross-validation with an existing or a user-specified modeling function over an imaging datasets.
}
\usage{
\S4method{crossValidate}{MSImagingExperiment}(.x, .y, .fun,
        .fold = run(.x),
        .predict = predict,
        .process = FALSE,
        .processControl = list(),
        .peaks = NULL,
        BPPARAM = bpparam(), \dots)

\S4method{crossValidate}{SparseImagingExperiment}(.x, .y, .fun, .fold = run(.x),
        BPPARAM = bpparam(), \dots)

\S4method{cvApply}{SparseImagingExperiment}(.x, .y, .fun,
        .fold = run(.x),
        .predict = predict,
        .fitted = fitted,
        .simplify = FALSE,
        BPREDO = list(),
        BPPARAM = bpparam(), \dots)

\S4method{summary}{CrossValidated2}(object, \dots)

\S4method{cvApply}{SImageSet}(.x, .y, .fun, .fold = sample, \dots)
}
\arguments{
    \item{.x}{An imaging dataset.}
    \item{.y}{The response variable for prediction.}
    \item{.fun}{A function for training a model where the first two arguments are the dataset and the response.}
    \item{.fold}{A variable determining the cross-validation folds. When specifying custom folds, it is important to make sure that data points from the same experimental run are not split among different folds. I.e., all data points from a run should belong to the same CV fold.}
    \item{.predict}{A function for predicting from a trained model. The first two arguments are the model and a new dataset.}
    \item{.fitted}{A function for extracting the predicted values from the result of a call to \code{.predict}.}
    \item{.simplify}{If FALSE (the default), the output of \code{.predict} is returned. If TRUE, then \code{.fitted} is applied to the results to extract fitted values. Only the fitted values, observed values, and basic model information is returned.}
    \item{.process}{Should pre-processing be applied before each training and test step? This includes peak alignment and peak filtering. Peak binning is also performed if \code{.peaks} is given.}
    \item{.processControl}{A list of arguments to be passed to the pre-processing steps.}
    \item{.peaks}{A peak-picked version of the full dataset \code{.x}, for use with pre-processing between training and test steps.}
    \item{BPREDO}{See documentation for \code{\link{bplapply}}.}
    \item{BPPARAM}{An optional instance of \code{BiocParallelParam}. See documentation for \code{\link{bplapply}}.}
    \item{\dots}{Additional arguments passed to \code{.fun}.}
    \item{object}{A fitted model object to summarize.}
}
\details{
    This method is designed to be used with the provided classification methods, but can also be used with user-provided functions and methods as long as they fulfill certain expectations.

    The function or method passed to '.fun' must take at least two arguments: the first argument must be a object derived from \code{\linkS4class{SparseImagingExperiment}} or \code{\linkS4class{SImageSet}}, and the second argument must be the response variable. The function should return an object of a class derived from \code{\linkS4class{ImagingResult}} or \code{\linkS4class{ResultSet}}, which should have a \code{predict} method that takes arguments 'newx' and 'newy' in addition to the fitted model.

    For \code{\linkS4class{MSImagingExperiment}} objects, pre-processing can be performed. This is particularly useful if there is no reference to which to align peaks, except the mean spectrum (which is calculated from the whole dataset, and may invalidate cross-validation results).

    If \code{.process=TRUE} and \code{.peaks=NULL}, then either the data should be profile spectra with no peak picking, or it should be a peak-picked dataset \emph{before peak alignment}. If the data has no peak picking, then the pre-processing will consist of peak picking on the mean spectrum of the training sets, followed by peak alignment and peak filtering. If the data has been peak-picked but not aligned, then the pre-processing will consist of peak alignment to the mean spectrum of the training sets, and peak filtering.

    If \code{.process=TRUE} and \code{.peaks} is given, then the data should be a dataset consisting of profile spectra, and \code{.peaks} should be a peak-picked version of the same dataset \emph{before peak alignment}. The pre-processing will consist of peak alignment to the mean spectrum of the training sets, peak filtering, and peak binning the full data to the aligned peaks.

    The \code{crossValidate} function calls \code{cvApply} internally and then post-processes the result to be more easily-interpretable and space-efficient. Accuracy metrics are reported for each set of modeling parameters.
}
\value{
    An object of class 'CrossValidated', which is derived from \code{\linkS4class{ResultSet}}, or an object of class 'CrossValidated2', which is derived from \code{\linkS4class{ImagingResult}}.
}
\author{
	Kylie A. Bemis
}
\seealso{
    \code{\link{spatialShrunkenCentroids}},
    \code{\link{PLS}},
    \code{\link{OPLS}}    
}
\keyword{manip}
\keyword{classif}
