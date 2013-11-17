\name{trellisImage-methods}
\docType{methods}
\alias{trellisImage}
\alias{trellisImage-methods}
\alias{trellisImage,MSImageSet-method}
\alias{trellisImage,MSImageSegmentation-method}
\title{Display an ion image or spatial segmentation with lattice graphics}
\description{
	Plots an ion image for an \code{\linkS4class{MSImageSet}} mass spectrometry imaging dataset or a spatial segmentation for an \code{MSImageSegmentation} object using lattice graphics capabilities to produce a trellis display that allows visualizing multiple images at once.
}
\usage{
    \S4method{trellisImage}{MSImageSet}(x, mz, feature, values,
    sliceDimNames=metaData(x)[["coordDimNames"]][c(1,2)], fixCoord=list(),
    xlab=sliceDimNames[[1]], ylab=sliceDimNames[[2]], main=metaData(x)[["name"]],
    xlim=range(coord(x)[sliceDimNames[[1]]]), ylim=range(coord(x)[sliceDimNames[[2]]]),
    col.regions=intensity.colors(100), colorkey=list(col=col.regions),
    at=seq(from=min(values, na.rm=TRUE), to=max(values, na.rm=TRUE),
    length.out=length(col.regions)), contrast=c("none", "suppress", "histeq"),
    smoothing=c("none", "gaussian", "adaptive"), interpolate=c("none", "bilinear"),
    fun=mean, \dots)
    
    \S4method{trellisImage}{MSImageSegmentation}(x, which=attr(x, "plotnames"),
    mode=c("classes", "probabilities"),  main=metaData(x)[["name"]], colorkey=FALSE,
    col.regions=rainbow(max(x[["nclasses"]][which])),
    at=seq(0, length(col.regions)) + 0.5,
    key=list(text=list(metaData(x)[["labels"]][1:length(col.regions)]),
    rectangles=list(col=col.regions), columns=min(4, length(col.regions))),
    true.labels=FALSE, mask.missing=FALSE, class.mask=NULL, \dots)
}
\arguments{
    \item{x}{the object from which to create the image.}
    \item{mz}{the mass at which to fix the ion image.}
    \item{feature}{the mass feature at which to fix the ion image.}
    \item{values}{alternative values to use to create an image for an \code{\linkS4class{MSImageSet}}; this can be a \code{list} or \code{data.frame} where each named vector will be plotted as a seperate image in the trellis display.}
    \item{sliceDimNames}{\code{character}, giving the name of the sliced dimensions.}
    \item{fixCoord}{a named \code{list}, giving coordinates to fix if the \code{\linkS4class{MSImageSet}} has more than two dimensions; multiple coordinates can be specified to yield multiple images in the trellis display.}
    \item{xlab}{\code{character} giving the label for the x-axis.}
    \item{ylab}{\code{character} giving the label for the y-axis.}
    \item{main}{the overall title for the image.}
    \item{xlim}{the x-axis range.}
    \item{ylim}{the y-axis range.}
    \item{col.regions}{a vector of colors to use in the image.}
    \item{colorkey}{a \code{logical} indicating whether a colorkey should be displayed, or a \code{list} giving the parameters of the colorkey.}
    \item{at}{\code{numeric} giving breakpoints along the range of plotted values where colors will be plotted.}
    \item{contrast}{the contrast enhancement function to apply to the ion image.}
    \item{smoothing}{the smoothing function to apply to the ion image.}
    \item{interpolate}{the interpolation function to apply to the ion image}
    \item{fun}{if multiple masses or mass features are given for an ion image, the function to apply over them to create a single value for each pixel in the image.}
    \item{which}{which segmentation(s) to use in an \code{MSImageSegmentation} if multiple segmentations were created from multiple parameters; this can either be an \code{integer}, a \code{character} giving the names of the \code{parnames} in the object's \code{metaData}, or a list specifying the names of parameters and their values.}
    \item{mode}{what z-values plot for an \code{MSImageSegmentation}, either the classes, or the probabilities.}
    \item{key}{a \code{list} giving the components for the key, or \code{NULL} if no key is desired.}
    \item{true.labels}{whether the true labels should be used for the image instead of the predicted spatial segmentation if the result of classification.}
    \item{mask.missing}{whether predicted classifications with missing true labels should be hidden in the image segmentation.}
    \item{class.mask}{a vector of numbers indicating the classes to be displayed.}
    \item{\dots}{additional arguments passed to the underlying \code{\link[lattice]{levelplot}} functions.}
}
\author{
	Kyle D. Bemis
}
\seealso{
    \code{\linkS4class{MSImageSet}},
    \code{\link[Cardinal]{plot-methods}},
    \code{\link[Cardinal]{image-methods}},
    \code{\link[Cardinal]{trellisPlot-methods}}
}
\examples{
    ## write examples to put here
}
\keyword{methods}

