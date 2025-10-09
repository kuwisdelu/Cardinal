\name{MeansTest}

\alias{MeansTest}
\alias{class:MeansTest}
\alias{MeansTest-class}

\alias{meansTest}
\alias{meansTest,ANY-method}
\alias{meansTest,SpectralImagingExperiment-method}
\alias{meansTest,SpatialDGMM-method}

\alias{topFeatures,MeansTest-method}
\alias{plot,MeansTest,missing-method}

\alias{contrastTest}

\alias{segmentationTest}

\title{Linear model-based testing for summarized imaging experiments}

\description{
    Performs hypothesis testing for imaging experiments by fitting linear mixed models to summarizations or segmentations.
}

\usage{
\S4method{meansTest}{ANY}(x, data, fixed, random, samples,
    response = "y", reduced = ~ 1, byrow = FALSE,
    use_lmer = FALSE,
    verbose = getCardinalVerbose(), chunkopts = list(),
    BPPARAM = getCardinalBPPARAM(), \dots)

\S4method{meansTest}{SpectralImagingExperiment}(x, fixed, random, samples = run(x),
    response = "intensity", \dots)

\S4method{meansTest}{SpatialDGMM}(x, fixed, random, class = 1L,
    response = "intensity", reduced = ~ 1,
    use_lmer = FALSE,
    verbose = getCardinalVerbose(), chunkopts = list(),
    BPPARAM = getCardinalBPPARAM(), \dots)

segmentationTest(x, fixed, random, samples = run(x),
    class = 1L, response = "intensity", reduced = ~ 1, \dots)

\S4method{topFeatures}{MeansTest}(object, n = Inf, sort.by = "statistic", \dots)

\S4method{plot}{MeansTest,missing}(x, i = 1L, type = "boxplot", show.obs = TRUE,
        fill = FALSE, layout = NULL, \dots)

contrastTest(object, specs, method = "pairwise", emm_adjust = "none",
    verbose = getCardinalVerbose(), chunkopts = list(),
    BPPARAM = getCardinalBPPARAM(), \dots)
}

\arguments{
    \item{x}{A dataset in P x N matrix format or a set of spatially segmented images.}

    \item{data}{A data frame of additional variables parallel to \code{x}.}
    
    \item{fixed}{A one-sided formula giving the fixed effects of the model on the RHS. The response will added to the LHS, and the formula will be passed to the underlying modeling function.}
    
    \item{random}{A one-sided formula giving the random effects of the model on the RHS. See \code{\link{lme}} for the allowed specifications.}

    \item{samples}{A vector coercible to a factor giving the observational unit (i.e., the samples and replicates).}

    \item{class}{For \code{SpatialDGMM}, the class (segment) from the Gaussian mixture models that should be used for the comparison. By default, compare the classes (segments) with the highest means in each sample.}

    \item{response}{The name of to assign the response variable in the fitted models.}

    \item{reduced}{A one-sided formula specifying the fixed effects in the reduced model for the null hypothesis. The default is an intercept-only model. Random effects are retained.}

    \item{use_lmer}{Logical. If \code{TRUE}, use \code{lmerTest::lmer} instead of \code{nlme::lme} for fitting mixed effects models. This provides Satterthwaite degrees of freedom and is typically faster. When \code{TRUE}, likelihood ratio tests are not performed; use \code{contrastTest()} for post-hoc tests. Requires the \code{lmerTest} package.}

    \item{byrow}{For the default method, are the rows or the columns the \code{x} .}

    \item{verbose}{Should progress messages be printed?}

    \item{chunkopts}{Chunk processing options. See \code{\link{chunkApply}} for details.}
    
    \item{BPPARAM}{An optional instance of \code{BiocParallelParam}. See documentation for \code{\link{bplapply}}.}

    \item{\dots}{For \code{meansTest}, passed to internal linear modeling methods (\code{\link{lm}}, \code{\link{lme}}, or \code{lmerTest::lmer}). For \code{contrastTest}, passed to \code{emmeans::emmeans()} or \code{emmeans::contrast()}.}

    \item{specs}{For \code{contrastTest}, specification for estimated marginal means passed to \code{emmeans::emmeans()}. Can be a character vector of factor names (e.g., \code{"condition"}), a formula (e.g., \code{~ condition}), or more complex specifications. See \code{?emmeans::emmeans} for details.}

    \item{method}{For \code{contrastTest}, the contrast method. Common options include \code{"pairwise"} (all pairwise comparisons), \code{"trt.vs.ctrl"} (treatment vs control), \code{"poly"} (polynomial contrasts), or a named list of custom contrasts. See \code{?emmeans::contrast} for all options.}

    \item{emm_adjust}{For \code{contrastTest}, the p-value adjustment method passed to \code{emmeans::contrast()}. Options include \code{"none"}, \code{"bonferroni"}, \code{"tukey"}, \code{"fdr"}, etc. See \code{?emmeans::summary.emmGrid} for all options. Note: adjustment only affects results when testing multiple contrasts (e.g., 3-level factors produce 3 pairwise contrasts).}

    \item{object}{A fitted model object to summarize.}

    \item{n, sort.by}{For \code{topFeatures}, the number of top features to return and how to sort them.}

    \item{i}{The index of the model(s)/feature(s) to plot.}

    \item{type}{The type of plot to display.}

    \item{show.obs}{Should individual observations (i.e., the summarized mean for each sample) be plotted too?}

    \item{fill}{Should the boxplots be filled?}

    \item{layout}{A vector of the form \code{c(nrow, ncol)} specifying the number of rows and columns in the facet grid.}
}

\details{
    Likelihood ratio tests are used for hypothesis testing when \code{use_lmer = FALSE} (the default). When \code{use_lmer = TRUE}, models are fit with \code{lmerTest::lmer} using REML, but no hypothesis tests are performed. Instead, use the \code{contrastTest()} function for post-hoc comparisons.
    
    The \code{contrastTest()} function provides flexible post-hoc testing for models fit with \code{use_lmer = TRUE}. It uses \code{emmeans::emmeans()} to compute estimated marginal means and \code{emmeans::contrast()} to perform comparisons. The function operates on all m/z features in parallel and returns a \code{ContrastResultsList} object with contrast statistics.
    
    When using \code{contrastTest()}, p-value adjustments (via the \code{adjust} parameter) only show differences from unadjusted values when testing multiple contrasts. For example, a 2-level factor produces only 1 contrast, so Bonferroni adjustment yields \code{p * 1 = p} (no change). A 3-level factor produces 3 pairwise contrasts, demonstrating visible adjustment effects.
}

\value{
    For \code{meansTest}: An object of class \code{MeansTest} derived from \code{ResultsList}, where each element contains a linear model (\code{lm}, \code{lme}, or \code{lmerMod} object).
    
    For \code{contrastTest}: A \code{ContrastResultsList} object (extends \code{ResultsList}) where each element contains an \code{emmeans} contrast object. The \code{mcols} metadata includes contrast estimates and p-values with columns named as \code{"[contrast_name].estimate"} and \code{"[contrast_name].pvalue"}.
}

\author{
    Dan Guo, Kylie A. Bemis, and Ethan Rogers
}

\seealso{
    \code{\link{lm}},
    \code{\link{lme}},
    \code{lmerTest::lmer},
    \code{emmeans::emmeans},
    \code{emmeans::contrast},
    \code{\link{spatialDGMM}}
}

\examples{
set.seed(1, kind="L'Ecuyer-CMRG")
x <- simulateImage(preset=4, nrun=3, npeaks=10,
    dim=c(10,10), peakheight=5, peakdiff=2,
    centroided=TRUE)

samples <- replace(run(x), !(x$circleA | x$circleB), NA)

# Fit with nlme::lme (performs likelihood ratio tests)
fit <- meansTest(x, ~condition, samples=samples)
print(fit)

# Fit with lmerTest::lmer (no tests, use contrastTest() instead)
\dontrun{
fit_lmer <- meansTest(x, ~condition, samples=samples, use_lmer=TRUE)

# Post-hoc contrasts with p-value adjustment
contr <- contrastTest(fit_lmer, specs="condition", 
                      method="pairwise", emm_adjust="bonferroni")
print(contr[[1]])  # First m/z feature
mcols(contr)  # All contrast statistics
}
}

\keyword{htest}
\keyword{models}
\keyword{regression}
\keyword{contrast}
