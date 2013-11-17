\name{likPlot-methods}
\docType{methods}
\alias{likPlot}
\alias{likPlot-methods}
\alias{likPlot,MSImageSegmentation-method}
\title{Plot the log-likelihood or other model criteria}
\description{
	Plots the log-likelihood of a model to help determine the best parameters.
}
\usage{
    \S4method{likPlot}{MSImageSegmentation}(object,
    output=c("loglik", "aic", "bic", "nclasses"),
    col=rainbow(max(metaData(object)[["levels"]])),
    layout, \dots)
}
\arguments{
    \item{object}{the object from which to create the plot.}
    \item{output}{the statistics to plot in the output.}
    \item{col}{a vector of colors to use for the different models.}
    \item{layout}{a pair of numbers giving the number of columns and rows, respectively, in the the resulting plot.}
    \item{\dots}{ignored.}
}
\details{
    Log-likelihood, AIC, and BIC are plotted together, and are best used
    for selection of the sparsity parameter. They are not necessarily comparable
    between different initialization parameters for the number of clusters.
    Because higher sparsity generally results in fewer clusters, it is best to
    initialize with a higher number of clusters than necessary, and use
    the sparsity parameter to select a model.
}
\author{
	Kyle D. Bemis
}
\seealso{
    \code{\linkS4class{MSImageSet}},
    \code{\link[Cardinal]{spatialClassify-methods}},
    \code{\link[Cardinal]{spatialCluster-methods}}
}
\examples{
    ## write examples to put here
}
\keyword{methods}

