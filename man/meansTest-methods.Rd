\name{meansTest-methods}
\docType{methods}

\alias{meansTest}
\alias{class:MeansTest}
\alias{MeansTest-class}
\alias{meansTest-methods}

\alias{segmentationTest}
\alias{class:SegmentationTest}
\alias{SegmentationTest-class}
\alias{segmentationTest-methods}

\alias{meansTest,SparseImagingExperiment-method}
\alias{summary,MeansTest-method}

\alias{segmentationTest,SparseImagingExperiment-method}
\alias{segmentationTest,SpatialDGMM-method}
\alias{summary,SegmentationTest-method}

\title{Linear model-based testing for summarized imaging experiments}
\description{
    Performs hypothesis testing for imaging experiments by fitting linear mixed models to summarizations or segmentations.
}
\usage{
\S4method{meansTest}{SparseImagingExperiment}(x, fixed, random, groups = run(x),
        BPPARAM = bpparam(), \dots)

\S4method{segmentationTest}{SparseImagingExperiment}(x, fixed, random, groups = run(x),
        classControl = c("Ymax", "Mscore"),
        BPPARAM = bpparam(), \dots)

\S4method{segmentationTest}{SpatialDGMM}(x, fixed, random, model = modelData(x),
        classControl = c("Ymax", "Mscore"),
        BPPARAM = bpparam(), \dots)

\S4method{summary}{MeansTest}(object, \dots, BPPARAM = bpparam())

\S4method{summary}{SegmentationTest}(object, \dots, BPPARAM = bpparam())
}
\arguments{
    \item{x}{An imaging dataset or segmented/summarized imaging dataset.}
    \item{fixed}{A one-sided formula giving the fixed effects of the model on the RHS. The response will added to the LHS, and the formula will be passed to the underlying modeling function.}
    \item{random}{A one-sided formula giving the random effects of the model on the RHS. See \code{\link{lme}} for the allowed specifications.}
    \item{groups}{The summarization units. Pixels from different groups will be segmented/summarized separately. \emph{Each distinct observational unit (e.g., tissue sample) should be assigned to a unique group}.}
    \item{model}{An integer vector or \code{list} specifying which fitted model to plot. If this is an integer vector, it should give the rows indices of \code{modelData(x)} to use for plotting. Otherwise, it should be a list giving the values of parameters in \code{modelData(x)}.}
    \item{classControl}{Either the method used to match segmented classes to the fixed effects, or a list where each element is a vector of name-value pairs giving the mapping between groups and classes (e.g., group1=class1). For automated matching methods, 'Mscore' means to use a match score quantifying the overlap between classes and fixed effects, while 'Ymax' means to use the classes with the highest mean response for each group.}
    \item{\dots}{Passed to internal linear modeling methods.}
    \item{object}{A fitted model object to summarize.}
    \item{BPPARAM}{An optional instance of \code{BiocParallelParam}. See documentation for \code{\link{bplapply}}.}
}
\value{
    An object of class \code{MeansTest} or \code{SegmentationTest}, which is a \code{ResultImagingExperiment}, where each element of the \code{resultData} slot contains at least the following components:

    \describe{
        \item{\code{model}:}{A linear model fitted using either \code{\link{lm}} or \code{\link{lme}}.}
        \item{\code{data}:}{The summarized data used to fit the model.}
    }
}
\author{
    Dan Guo and Kylie A. Bemis
}
\seealso{
    \code{\link{lm}}, \code{\link{lme}},
    \code{\link{spatialDGMM}}
}
\examples{
set.seed(1)
x <- simulateImage(preset=4, nruns=3, npeaks=10,
    dim=c(10,10), peakheight=5, peakdiff=2,
    representation="centroid")

groups <- replace(run(x), !(x$circleA | x$circleB), NA)

fit <- meansTest(x, ~ condition, groups=groups)

modelData(fit)
}

\keyword{htest}
\keyword{models}
