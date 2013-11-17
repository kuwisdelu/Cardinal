\name{trellisPlot-methods}
\docType{methods}
\alias{trellisPlot}
\alias{trellisPlot-methods}
\alias{trellisPlot,MSImageSet-method}
\alias{trellisPlot,MSImageSegmentation-method}
\title{Display a mass spectrum or mass peak plot using lattice graphics}
\description{
	Plots a mass spectrum or mass peaks for an \code{\linkS4class{MSImageSet}} mass spectrometry imaging dataset, an \code{MSPeakFrame}, \code{MSPeakList}, or an \code{MSImageSegmentation} object using lattice graphics capabilities to produce a trellis display that allows visualizing multiple mass spectra at once.
}
\usage{
    \S4method{trellisPlot}{MSImageSet}(x, pixel, coord, by,
    main=metaData(x)[["name"]], xlab="m/z", ylab="intensity",
    type=if( isPeaks(x) ) c("h", "p") else "l", fun=mean,
    nlabels=if( isPeaks(x) ) 6 else 0, pch=20, cex=0.6,
    filter=c("none", "gaussian", "ma", "sgolay", "kaiser"), \dots)
    
    \S4method{trellisPlot}{MSImageSegmentation}(x, which=attr(x, "plotnames"),
    mode=c("centroids", "tstatistics"),
    main=metaData(x)[["name"]], xlab="m/z", ylab=mode,
    col=rainbow(max(x[["nclasses"]][which])), type=c("h", "p"),
    key=list(text=list(metaData(x)[["labels"]][1:length(col)]),
    points=list(pch=pch, col=col), columns=min(4, length(col))),
    climits=FALSE, nlabels=6, class.mask=NULL, pch=20, cex=0.6, \dots)
}
\arguments{
    \item{x}{the object from which to create the plot.}
    \item{pixel}{the pixel from which to use the mass spectrum.}
    \item{coord}{a named vector or named \code{list}, or a \code{data.frame} giving the coordinates of the pixel to use to plot the mass spectrum.}
    \item{by}{a \code{logical}, \code{integer}, or \code{factor}, with length equal to the number of pixels in the object; the unique levels will be grouped together and plotted in unique trellis displays.}
    \item{main}{the overall title for the plot.}
    \item{xlab}{\code{character} giving the label for the x-axis.}
    \item{ylab}{\code{character} giving the label for the y-axis.}
    \item{type}{the type of plot to make, defaulting to 'l' for a mass spectrum or 'h' for mass peaks; this can be a single value or a pair of values.}
    \item{fun}{if multiple masses or mass features are given for an ion plot, the function to apply over them to create a single value for each pixel for the plot.}
    \item{nlabels}{the number of highest-intensity peaks to label when plotting peak-picked mass spectra.}
    \item{filter}{the filter function to use to smooth the mass spectrum.}
    \item{which}{which segmentation(s) to use in an \code{MSImageSegmentation} if multiple segmentations were created from multiple parameters; this can either be an \code{integer}, a \code{character} giving the names of the \code{parnames} in the object's \code{metaData}, or a list specifying the names of parameters and their values.}
    \item{mode}{what to y-values plot for an \code{MSImageSegmentation}, either the intensities, or the t-statistics.}
    \item{col}{a vector of colors to use for the different classes in the segmentation.}
    \item{key}{a \code{list} giving the components for the key, or \code{NULL} if no key is desired.}
    \item{climits}{\code{logical} giving whether confidence limits calculated from the pooled standard errors should be plotted for an \code{MSImageSegmentation}}
    \item{class.mask}{a vector of numbers indicating the classes to be displayed.}
    \item{pch}{the plotting symbol to use}
    \item{cex}{\code{numeric} giving the size of the plotting symbols relative to the default size}
    \item{\dots}{additional arguments passed to the underlying \code{\link[lattice]{xyplot}} functions.}
}
\author{
	Kyle D. Bemis
}
\seealso{
    \code{\linkS4class{MSImageSet}},
    \code{\link[Cardinal]{image-methods}},
    \code{\link[Cardinal]{plot-methods}},
    \code{\link[Cardinal]{trellisImage-methods}}
}
\examples{
    ## write examples to put here
}
\keyword{methods}

