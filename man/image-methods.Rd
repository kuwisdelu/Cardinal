\name{image-methods}
\docType{methods}

\alias{image}
\alias{image-methods}
\alias{image,SImageSet-method}
\alias{image,MSImageSet-method}

\title{Plot the Pixel-Space of an Imaging Dataset}

\description{
    Create and display plots in the pixel space of an imaging dataset. This uses a formula interface inspired by the \code{\link[lattice]{lattice}} graphics package.
}

\usage{
\S4method{image}{SImageSet}(x, formula = ~ x * y,
    feature,
    feature.groups,
    groups = NULL,
    superpose = FALSE,
    fun = mean,
    \dots,
    xlab,
    xlim,
    ylab,
    ylim,
    zlim,
    asp = 1,
    col = intensity.colors(100),
    colorkey = TRUE,
    subset = TRUE,
    lattice = FALSE)

\S4method{image}{MSImageSet}(x, formula = ~ x * y,
    feature = features(x, mz=mz),
    mz,
    plusminus,
    \dots,
    sub)
}

\arguments{
    
    \item{x}{An imaging dataset.}
    
    \item{formula}{A formula of the form 'z ~ x * y | g1 * g2 * ...' (or equivalently, 'z ~ x + y | g1 + g2 + ...'), indicating a LHS 'y' (on the y-axis) versus a RHS 'x' (on the x-axis) and conditioning variables 'g1, g2, ...'.

    Usually, the LHS is not supplied, and the formula is of the form '~ x * y | g1 * g2 * ...', and the y-axis is implicityl assumed to be the feature vectors corresponding to each pixel in the imaging dataset specified by the object 'x'. However, a variable evaluating to a vector of pixel values, or a sequence of such variables, can also be supplied.

    The RHS is evaluated in \code{pData(x)} and should provide values for the xy-axes. These must be spatial coordinates.

    The conditioning variables are evaluated in \code{fData(x)}. These can be specified in the formula as 'g1 * g2 * ...'. The argument 'feature.groups' allows an alternate way to specify a single conditioning variable. Conditioning variables specified using the formula interface will always appear on separate plots. This can be combined with 'superpose=TRUE' to both overlay plots based on a conditioning variable and use conditioning variables to create separate plots.}
    
    \item{feature}{The feature or vector of features for which to plot the image. This is an expression that evaluates to a logical or integer indexing vector.}

    \item{feature.groups}{An alternative way to express a single conditioning variable. This is a variable or expression to be evaluated in \code{fData(x)}, expected to act as a grouping variable for the features specified by 'feature', typically used to distinguish different groups or ranges of features. Pixel vectors of images from features in the same feature group will have 'fun' applied over them; 'fun' will be applied to each feature group separately, usually for averaging. If 'superpose = FALSE' then these appear on separate plots.}

    \item{groups}{A variable or expression to be evaluated in \code{pData(x)}, expected to act as a grouping variable for the pixel regions in the image(s) to be plotted, typically used to distinguish different image regions by varying graphical parameters like color and line type. By default, if 'superpose = FALSE', these appear overlaid on the same plot.}

    \item{superpose}{Should feature vectors from different feature groups specified by 'feature.groups' be superposed on the same plot? If 'TRUE' then the 'groups' argument is ignored.}

    \item{fun}{A function to apply over pixel vectors of images grouped together by 'feature.groups'. By default, this is used for averaging over features.}

    \item{xlab}{Character or expression giving the label for the x-axis.}

    \item{ylab}{Character or expression giving the label for the x-axis.}

    \item{xlim}{A numeric vector of length 2 giving the left and right limits for the x-axis.}

    \item{ylim}{A numeric vector of length 2 giving the top and bottom limits for the y-axis.}

    \item{zlim}{A numeric vector of length 2 giving the lower and upper limits for the z-axis (i.e., the range of colors to be plotted).}

    \item{asp}{The aspect ratio of the plot.}

    \item{col}{A specification for the default plotting color(s).}

    \item{colorkey}{Should a coloykey describing the z-axis be drawn with the plot?}

    \item{sub}{The subtitle for the plot.}

    \item{subset}{An expression that evaluates to a logical or integer indexing vector to be evaluated in \code{pData(x)}.}

    \item{lattice}{Should lattice graphics be used to create the plot?}

    \item{mz}{The m/z value for which to plot the ion image.}

    \item{plusminus}{If specified, a window of m/z values surrounding the one given by \code{coord} will be included in the plot with \code{fun} applied over them, and this indicates the range of the window on either side.}
    
    \item{\dots}{additional arguments passed to the underlying \code{\link[graphics]{plot}} functions.}
}

\author{
    Kyle D. Bemis
}

\seealso{
    \code{\linkS4class{MSImageSet}},
    \code{\link[Cardinal]{image-methods}}
}

\examples{
data <- matrix(c(NA, NA, 1, 1, NA, NA, NA, NA, NA, NA, 1, 1, NA, NA, 
 NA, NA, NA, NA, NA, 0, 1, 1, NA, NA, NA, NA, NA, 1, 0, 0, 1, 
 1, NA, NA, NA, NA, NA, 0, 1, 1, 1, 1, NA, NA, NA, NA, 0, 1, 1, 
 1, 1, 1, NA, NA, NA, NA, 1, 1, 1, 1, 1, 1, 1, NA, NA, NA, 1, 
 1, NA, NA, NA, NA, NA, NA, 1, 1, NA, NA, NA, NA, NA), nrow=9, ncol=9)

mycol <- gradient.colors(100, "red", "black")

set.seed(1)
sset <- generateImage(data, range=c(1000,5000), centers=c(3000,4000), resolution=100)

pData(sset)$pg <- factor(data[is.finite(data)], labels=c("black", "red"))
fData(sset)$fg <- factor(rep("bg", nrow(fData(sset))), levels=c("bg", "black", "red"))
fData(sset)$fg[2950 < fData(sset)$t & fData(sset)$t < 3050] <- "black"
fData(sset)$fg[3950 < fData(sset)$t & fData(sset)$t < 4050] <- "red"

image(sset, feature=1, col=mycol)

image(sset, feature=fData(sset)$fg=="black", col=mycol)

image(sset, feature=fData(sset)$fg=="red", col=mycol)

image(sset, ~ x * y | fg, feature=1:nrow(sset), lattice=TRUE, col=mycol)

image(sset, feature=1:nrow(sset), feature.groups=fg, lattice=TRUE, col=mycol)

set.seed(1)
msset <- generateImage(data, range=c(1000,5000), centers=c(3000,4000), resolution=100, as="MSImageSet")

image(msset, mz=3000, col=mycol)

image(msset, mz=4000, col=mycol)

image(msset, mz=3500, plusminus=500, col=mycol)
}

\keyword{methods}

