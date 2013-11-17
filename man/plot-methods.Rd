\name{plot-methods}
\docType{methods}
\alias{plot}
\alias{plot-methods}
\alias{plot,MSImageSet,missing-method}
\alias{plot,MSPeakFrame,missing-method}
\alias{plot,MSPeakList,missing-method}
\alias{plot,MSImageSegmentation,missing-method}
\title{Display a mass spectrum or mass peak plot}
\description{
	Plots a mass spectrum or mass peaks for an \code{\linkS4class{MSImageSet}} mass spectrometry imaging dataset, an \code{MSPeakFrame}, \code{MSPeakList}, or an \code{MSImageSegmentation} object.
}
\usage{
    \S4method{plot}{MSImageSet,missing}(x, pixel, coord,
    main=metaData(x)[["name"]], xlab="m/z", ylab="intensity",
    col="black", pch=20, cex=0.6,
    type=if( isPeaks(x) ) c("h", "p") else "l", sub=formatCoord(coord),
    col.sub="gray", fun=mean,
    filter=c("none", "gaussian", "ma", "sgolay", "kaiser"),
    window=5, nlabels=if( isPeaks(x) ) 6 else 0,
    add=FALSE, \dots)

    \S4method{plot}{MSImageSegmentation,missing}(x, which=1,
    mode=c("centroids", "tstatistics"), main=metaData(x)[["name"]],
    xlab="m/z", ylab=mode, col=rainbow(max(x[["nclasses"]][which])),
    type=c("h", "p"), sub=attr(x, "plotnames")[[which]],
    col.sub="gray", pch=20, cex=0.6, climits=FALSE, nlabels=6,
    class.mask=NULL, legend=TRUE, add=FALSE, \dots)
    
    \S4method{plot}{MSPeakFrame,missing}(x, dist=FALSE, xlab="m/z",
    ylab="intensity", nlabels=6, type=c("h", "p"), col="black", pch=20, cex=0.6,
    lty.dist=1, lwd.dist=0.5, col.dist="black", add=FALSE, \dots)
    
    \S4method{plot}{MSPeakList,missing}(x, dist=FALSE, nlabels=0,
    col=sample(rainbow(length(peaks(x)))), col.dist=col, add=FALSE, \dots)
}
\arguments{
    \item{x}{the object from which to create the plot.}
    \item{pixel}{the pixel from which to use the mass spectrum.}
    \item{coord}{a named vector or named \code{list}, or a \code{data.frame} giving the coordinates of the pixel to use to plot the mass spectrum.}
    \item{main}{the overall title for the plot.}
    \item{xlab}{\code{character} giving the label for the x-axis.}
    \item{ylab}{\code{character} giving the label for the y-axis.}
    \item{col}{a vector of colors to use in the plot.}
    \item{type}{the type of plot to make, defaulting to 'l' for a mass spectrum or c('h','p') for mass peaks; this can be a single value or a pair of values.}
    \item{sub}{the plot subtitle.}
    \item{col.sub}{the color of the subtitle.}
    \item{fun}{if multiple masses or mass features are given for an ion plot, the function to apply over them to create a single value for each pixel for the plot.}
    \item{filter}{the filter function to use to smooth the mass spectrum.}
    \item{window}{the filter window.}
    \item{nlabels}{the number of highest-intensity peaks to label when plotting peak-picked mass spectra.}
    \item{add}{whether the plot should create a new plot or add to a previous plot.}
    \item{dist}{\code{logical}, whether to plot the fitted distributions for mass peaks.}
    \item{lty.dist}{the line type to use for the fitted distributions.}
    \item{lwd.dist}{the line width to use for the fitted distributions.}
    \item{col.dist}{the color to use for the fitted distributions}
    \item{which}{which segmentation(s) to use in an \code{MSImageSegmentation} if multiple segmentations were created from multiple parameters; this can either be an \code{integer}, a \code{character} giving the names of the \code{parnames} in the object's \code{metaData}, or a list specifying the names of parameters and their values.}
    \item{mode}{what to y-values plot for an \code{MSImageSegmentation}, either the intensities, or the t-statistics.}
    \item{pch}{the plotting symbol to use}
    \item{cex}{\code{numeric} giving the size of the plotting symbols relative to the default size}
    \item{climits}{\code{logical} giving whether confidence limits calculated from the pooled standard errors should be plotted for an \code{MSImageSegmentation}}
    \item{class.mask}{a vector of numbers indicating the classes to be displayed.}
    \item{legend}{whether a legend should be added to the plot.}
    \item{\dots}{additional arguments passed to the underlying \code{\link[graphics]{plot}} functions.}
}
\author{
	Kyle D. Bemis
}
\seealso{
    \code{\linkS4class{MSImageSet}},
    \code{\link[Cardinal]{image-methods}},
    \code{\link[Cardinal]{trellisImage-methods}},
    \code{\link[Cardinal]{trellisPlot-methods}}
}
\examples{
    ## write examples to put here
}
\keyword{methods}

