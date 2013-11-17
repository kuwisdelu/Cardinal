\name{summaryPlot-methods}
\docType{methods}
\alias{summaryPlot}
\alias{summaryPlot-methods}
\alias{summaryPlot,MSImageSegmentation,MSImageSet-method}
\title{Plot summary ion images for a model}
\description{
	For a given spatial segmentation, plots the spatial mask for a particular
    cluster, along with the top ions that are highly localized with that cluster.
}
\usage{
    \S4method{summaryPlot}{MSImageSegmentation,MSImageSet}(object, reference,
    which=1, class.mask=1:object[["nclasses"]][[which]],
    true.labels=FALSE, mask.missing=FALSE,
    n=6, mode=c("classes", "probabilities"),
    output=c("centroids", "tstatistics"), main=metaData(object)[["name"]],
    sliceDimNames=metaData(reference)[["coordDimNames"]][c(1,2)], fixCoord=list(),
    col=rainbow(max(object[["nclasses"]][which])), layout, \dots)
}
\arguments{
    \item{object}{the \code{MSImageSegmentation} object to use for the plot.}
    \item{reference}{the \code{\linkS4class{MSImageSet}} to use for the ion images.}
    \item{which}{which segmentation(s) to use in an \code{MSImageSegmentation} if multiple segmentations were created from multiple parameters; this can either be an \code{integer}, a \code{character} giving the names of the \code{parnames} in the object's \code{metaData}, or a list specifying the names of parameters and their values.}
    \item{class.mask}{a number indicating the class to be displayed.}
    \item{n}{\code{integer}, giving how many top ions to plot; use \code{Inf} to plot all of the ions with non-zero t-statistics for a given class.}
    \item{mode}{what z-values plot for an \code{MSImageSegmentation}, either the classes, or the probabilities.}
    \item{output}{what statistic output to include as plots.}
    \item{main}{the title for the spatial segmentation.}
    \item{col}{a vector of colors to use for the different models.}
    \item{layout}{a pair of numbers giving the number of columns and rows, respectively, in the the resulting summary plot.}
    \item{true.labels}{whether the true labels should be used for the image instead of the predicted spatial segmentation if the result of classification.}
    \item{mask.missing}{whether predicted classifications with missing true labels should be hidden in the image segmentation.}
    \item{\dots}{arguments past along to the \code{image} method for plotting the individual ion images.}
}
\details{
    This method should be used to give a quick overview of the top ions for a
    given cluster and their spatial distribution. It is recommended to use \code{\link[Cardinal]{trellisImage-methods}} for plotting multiple ions with greater control.
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

