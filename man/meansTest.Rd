\name{meansTest-methods}

\alias{meansTest}
\alias{class:MeansTest}
\alias{MeansTest-class}

\alias{meansTest,ANY-method}
\alias{meansTest,SpectralImagingExperiment-method}
\alias{meansTest,SpatialDGMM-method}

\alias{topFeatures,MeansTest-method}
\alias{plot,MeansTest,missing-method}

\alias{segmentationTest}

\title{Linear model-based testing for summarized imaging experiments}

\description{
    Performs hypothesis testing for imaging experiments by fitting linear mixed models to summarizations or segmentations.
}

\usage{
\S4method{meansTest}{ANY}(x, data, fixed, random, samples,
    response = "y", reduced = ~ 1, byrow = FALSE,
    nchunks = getCardinalNChunks(),
    verbose = getCardinalVerbose(),
    BPPARAM = getCardinalBPPARAM(), \dots)

\S4method{meansTest}{SpectralImagingExperiment}(x, data, fixed, random, samples = run(x),
    response = "intensity", \dots)

\S4method{meansTest}{SpatialDGMM}(x, data, fixed, random, class = 1L,
    response = "intensity", reduced = ~ 1,
    nchunks = getCardinalNChunks(),
    verbose = getCardinalVerbose(),
    BPPARAM = getCardinalBPPARAM(), \dots)

segmentationTest(x, fixed, random, samples = run(x),
    class = 1L, response = "intensity", reduced = ~ 1, \dots)

\S4method{topFeatures}{MeansTest}(object, n = Inf, sort.by = "statistic", \dots)

\S4method{plot}{MeansTest,missing}(x, i = 1L, type = "boxplot", show.obs = TRUE,
        fill = FALSE, layout = NULL, \dots)
}

\arguments{
    \item{x}{An imaging dataset or segmented/summarized imaging dataset.}
    
    \item{fixed}{A one-sided formula giving the fixed effects of the model on the RHS. The response will added to the LHS, and the formula will be passed to the underlying modeling function.}
    
    \item{random}{A one-sided formula giving the random effects of the model on the RHS. See \code{\link{lme}} for the allowed specifications.}

    \item{samples}{A vector coercible to a factor giving the observational unit (i.e., the samples and replicates).}

    \item{class}{For \code{SpatialDGMM}, the class (segment) from the Gaussian mixture models that should be used for the comparison. By default, compare the classes (segments) with the highest means in each sample.}

    \item{response}{The name of to assign the response variable in the fitted models.}

    \item{reduced}{A one-sided formula specifying the reduced model for the null hypothesis. The default is an intercept-only model.}

    \item{byrow}{For the default method, are the rows or the columns the \code{x} .}

    \item{nchunks}{The number of chunks to use for processing.}

    \item{verbose}{Should progress messages be printed?}
    
    \item{BPPARAM}{An optional instance of \code{BiocParallelParam}. See documentation for \code{\link{bplapply}}.}

    \item{\dots}{Passed to internal linear modeling methods. Either \code{\link{lm}} if only fixed effects are given or \code{\link{lme}} if random effects are given.}

    \item{object}{A fitted model object to summarize.}

    \item{n, sort.by}{For \code{topFeatures}, the number of top features to return and how to sort them.}

    \item{i}{The index of the model(s)/feature(s) to plot.}

    \item{type}{The type of plot to display.}

    \item{show.obs}{Should individual observations (i.e., the summarized mean for each sample) be plotted too?}

    \item{fill}{Should the boxplots be filled?}

    \item{layout}{A vector of the form \code{c(nrow, ncol)} specifying the number of rows and columns in the facet grid.}
}

\value{
    An object of class \code{MeansTest} derived from \code{ResultsList}, where each element contains a linear model.
}

\author{
    Dan Guo and Kylie A. Bemis
}

\seealso{
    \code{\link{lm}},
    \code{\link{lme}},
    \code{\link{spatialDGMM}}
}

\examples{
set.seed(1)
x <- simulateImage(preset=4, nrun=3, npeaks=10,
    dim=c(10,10), peakheight=5, peakdiff=2,
    representation="centroid")

samples <- replace(run(x), !(x$circleA | x$circleB), NA)

fit <- meansTest(x, ~condition, samples=samples)
print(fit)
}

\keyword{htest}
\keyword{models}
\keyword{regression}
