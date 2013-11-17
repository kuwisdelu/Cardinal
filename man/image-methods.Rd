\name{image-methods}
\docType{methods}
\alias{image}
\alias{image-methods}
\alias{image,MSImageSet-method}
\alias{image,MSImageSegmentation-method}
\title{Display an ion image or spatial segmentation}
\description{
	Plots an ion image for an \code{\linkS4class{MSImageSet}} mass spectrometry imaging dataset or displays a spatial segmentation for an \code{MSImageSegmentation} object.
}
\usage{
    \S4method{image}{MSImageSet}(x, mz, feature, values,
    sliceDimNames=metaData(x)[["coordDimNames"]][c(1,2)], fixCoord=list(),
    xlab=sliceDimNames[[1]], ylab=sliceDimNames[[2]], col=intensity.colors(100),
    sub=formatMZ(mz), col.sub="gray", main=metaData(x)[["name"]], fun=mean,
    contrast=c("none", "suppress", "histeq"),
    smoothing=c("none", "gaussian", "adaptive"),
    interpolate=c("none", "bilinear"),
    xlim=range(coord(x)[sliceDimNames[[1]]]),
    ylim=range(coord(x)[sliceDimNames[[2]]]),
    zlim=NULL, colorkey=list(col=col),
    auto.scale=c("variable", "max"),
    asp=1, add=FALSE, \dots)
    
    \S4method{image}{MSImageSegmentation}(x, which=1,
    mode=c("classes", "probabilities"), main=metaData(x)[["name"]],
    col=rainbow(max(x[["nclasses"]][which])),
    breaks=seq(0, length(col)) + 0.5,
    sub=attr(x, "plotnames")[[which]], true.labels=FALSE,
    mask.missing=FALSE, class.mask=NULL,
    legend=TRUE, add=FALSE, \dots)
}
\arguments{
    \item{x}{the object from which to create the image.}
    \item{mz}{the mass at which to fix the ion image.}
    \item{feature}{the mass feature at which to fix the ion image.}
    \item{values}{alternative values to use to create an image for an \code{\linkS4class{MSImageSet}}.}
    \item{sliceDimNames}{\code{character}, giving the name of the sliced dimensions.}
    \item{fixCoord}{a named \code{list}, giving coordinates to fix if the \code{\linkS4class{MSImageSet}} has more than two dimensions.}
    \item{xlab}{\code{character} giving the label for the x-axis.}
    \item{ylab}{\code{character} giving the label for the y-axis.}
    \item{col}{a vector of colors to use in the image.}
    \item{sub}{the plot subtitle.}
    \item{col.sub}{the color of the subtitle.}
    \item{main}{the overall title for the image.}
    \item{fun}{if multiple masses or mass features are given for an ion image, the function to apply over them to create a single value for each pixel in the image.}
    \item{contrast}{the contrast enhancement function to apply to the ion image.}
    \item{smoothing}{the smoothing function to apply to the ion image.}
    \item{interpolate}{the interpolation function to apply to the ion image}
    \item{xlim}{the x-axis range.}
    \item{ylim}{the y-axis range.}
    \item{zlim}{the z-axis range.}
    \item{colorkey}{a \code{logical} indicating whether a colorkey should be displayed, or a \code{list} giving the parameters of the colorkey.}
    \item{auto.scale}{controls the behavior of automatic re-scaling of intensities when contrast enhancement, smoothing, or interpolation are applied.}
    \item{asp}{the aspect ratio.}
    \item{add}{whether the image should create a new plot or add to a previous plot.}
    \item{which}{which segmentation(s) to use in an \code{MSImageSegmentation} if multiple segmentations were created from multiple parameters; this can either be an \code{integer}, a \code{character} giving the names of the \code{parnames} in the object's \code{metaData}, or a list specifying the names of parameters and their values.}
    \item{mode}{what to z-values plot for an \code{MSImageSegmentation}, either the classes, or the probabilities.}
    \item{breaks}{\code{numeric} giving breakpoints along the range of plotted values where colors will be plotted.}
    \item{true.labels}{whether the true labels should be used for the image instead of the predicted spatial segmentation if the result of classification.}
    \item{mask.missing}{whether predicted classifications with missing true labels should be hidden in the image segmentation.}
    \item{class.mask}{a vector of numbers indicating the classes to be displayed.}
    \item{legend}{whether a legend should be added to the image.}
    \item{\dots}{additional arguments passed to the underlying \code{\link[graphics]{image}} functions.}
}
\author{
	Kyle D. Bemis
}
\seealso{
    \code{\linkS4class{MSImageSet}},
    \code{\link[Cardinal]{plot-methods}},
    \code{\link[Cardinal]{trellisImage-methods}},
    \code{\link[Cardinal]{trellisPlot-methods}}
}
\examples{
    ## write examples to put here
}
\keyword{methods}

