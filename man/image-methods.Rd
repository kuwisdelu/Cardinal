\name{image-methods}
\docType{methods}

\alias{image}
\alias{image-methods}
\alias{image,SImageSet-method}
\alias{image,MSImageSet-method}
\alias{image,ResultSet-method}
\alias{image,CrossValidated-method}
\alias{image,PCA-method}
\alias{image,PLS-method}
\alias{image,OPLS-method}
\alias{image,SpatialKMeans-method}
\alias{image,SpatialShrunkenCentroids-method}

\alias{image3D}
\alias{image3D-methods}
\alias{image3D,SImageSet-method}

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
    strip = TRUE,
    key = FALSE,
    fun = mean,
    normalize.image = c("none", "linear"),
    contrast.enhance = c("none", "suppression", "histogram"),
    smooth.image = c("none", "gaussian", "adaptive"),
    \dots,
    xlab,
    xlim,
    ylab,
    ylim,
    zlab,
    zlim,
    layout,
    asp = 1,
    col = rainbow(nlevels(feature.groups)),
    col.regions = intensity.colors(100),
    colorkey = !is3d,
    subset = TRUE,
    lattice = FALSE)

\S4method{image}{MSImageSet}(x, formula = ~ x * y,
    feature = features(x, mz=mz),
    feature.groups,
    mz,
    plusminus,
    \dots)

\S4method{image}{ResultSet}(x, formula,
    model = pData(modelData(x)),
    feature,
    feature.groups,
    superpose = TRUE,
    strip = TRUE,
    key = superpose,
    \dots,
    column,
    col = if (superpose) rainbow(nlevels(feature.groups)) else "black",
    lattice = FALSE)

\S4method{image}{CrossValidated}(x, fold = 1:length(x), layout, \dots)

\S4method{image}{PCA}(x, formula = substitute(mode ~ x * y),
    mode = "scores",
    \dots)

\S4method{image}{PLS}(x, formula = substitute(mode ~ x * y),
    mode = c("fitted", "scores", "y"),
    \dots)

\S4method{image}{OPLS}(x, formula = substitute(mode ~ x * y),
    mode = c("fitted", "scores", "Oscores", "y"),
    \dots)

\S4method{image}{SpatialShrunkenCentroids}(x, formula = substitute(mode ~ x * y),
    mode = c("probabilities", "classes", "scores"),
    \dots)

\S4method{image}{SpatialKMeans}(x, formula = substitute(mode ~ x * y),
    mode = "cluster",
    \dots)

\S4method{image3D}{SImageSet}(x, formula = ~ x * y * z, \dots)

}

\arguments{
    
    \item{x}{An imaging dataset.}
    
    \item{formula}{A formula of the form 'z ~ x * y | g1 * g2 * ...' (or equivalently, 'z ~ x + y | g1 + g2 + ...'), indicating a LHS 'y' (on the y-axis) versus a RHS 'x' (on the x-axis) and conditioning variables 'g1, g2, ...'.

    Usually, the LHS is not supplied, and the formula is of the form '~ x * y | g1 * g2 * ...', and the y-axis is implicityl assumed to be the feature vectors corresponding to each pixel in the imaging dataset specified by the object 'x'. However, a variable evaluating to a vector of pixel values, or a sequence of such variables, can also be supplied.

    The RHS is evaluated in \code{pData(x)} and should provide values for the xy-axes. These must be spatial coordinates.

    The conditioning variables are evaluated in \code{fData(x)}. These can be specified in the formula as 'g1 * g2 * ...'. The argument 'feature.groups' allows an alternate way to specify a single conditioning variable. Conditioning variables specified using the formula interface will always appear on separate plots. This can be combined with 'superpose = TRUE' to both overlay plots based on a conditioning variable and use conditioning variables to create separate plots.}

    \item{model}{A vector or \code{list} specifying which fitted model to plot. If this is a vector, it should give a subset of the rows of \code{modelData(x)} to use for plotting. Otherwise, it should be a list giving the values of parameters in \code{modelData(x)}.}
    
    \item{feature}{The feature or vector of features for which to plot the image. This is an expression that evaluates to a logical or integer indexing vector.}

    \item{feature.groups}{An alternative way to express a single conditioning variable. This is a variable or expression to be evaluated in \code{fData(x)}, expected to act as a grouping variable for the features specified by 'feature', typically used to distinguish different groups or ranges of features. Pixel vectors of images from features in the same feature group will have 'fun' applied over them; 'fun' will be applied to each feature group separately, usually for averaging. If 'superpose = FALSE' then these appear on separate plots.}

    \item{groups}{A variable or expression to be evaluated in \code{pData(x)}, expected to act as a grouping variable for the pixel regions in the image(s) to be plotted, typically used to distinguish different image regions by varying graphical parameters like color and line type. By default, if 'superpose = FALSE', these appear overlaid on the same plot.}

    \item{superpose}{Should feature vectors from different feature groups specified by 'feature.groups' be superposed on the same plot? If 'TRUE' then the 'groups' argument is ignored.}

    \item{strip}{Should strip labels indicating the plotting group be plotting along with the each panel? Passed to 'strip' in \code{\link[lattice]{levelplot}} is 'lattice = TRUE'.}

    \item{key}{A logical, or \code{list} containing components to be used as a key for the plot. This is passed to 'key' in \code{\link[lattice]{levelplot}} if 'lattice = TRUE'.}

    \item{fun}{A function to apply over pixel vectors of images grouped together by 'feature.groups'. By default, this is used for averaging over features.}

    \item{normalize.image}{Normalization function to be applied to each image. The function can be user-supplied, of one of 'none' or 'linear'. The 'linear' normalization method normalized each image to the same intensity range using a linear transformation.}

    \item{contrast.enhance}{Contrast enhancement function to be applied to each image. The function can be user-supplied, or one of 'none', 'histogram', or 'suppression'. The 'histogram' equalization method flatterns the distribution of intensities. The hotspot 'suppression' method uses thresholding to reduce the intensities of hotspots.}

    \item{smooth.image}{Image smoothing function to be applied to each image. The function can be user-supplied, or one of 'none', 'gaussian', or 'adaptive'. The 'gaussian' smoothing method smooths images with a simple gaussian kernel. The 'adaptive' method uses bilateral filtering to preserve edges.}

    \item{xlab}{Character or expression giving the label for the x-axis.}

    \item{ylab}{Character or expression giving the label for the y-axis.}

    \item{zlab}{Character or expression giving the label for the z-axis. (Only used for plotting 3D images.)}

    \item{xlim}{A numeric vector of length 2 giving the left and right limits for the x-axis.}

    \item{ylim}{A numeric vector of length 2 giving the top and bottom limits for the y-axis.}

    \item{zlim}{A numeric vector of length 2 giving the lower and upper limits for the z-axis (i.e., the range of colors to be plotted).}

    \item{layout}{The layout of the plots, given by a length 2 numeric as \code{c(ncol, nrow)}. This is passed to \code{\link[lattice]{levelplot}} if 'lattice = TRUE'. For base graphics, this defaults to one plot per page.}

    \item{asp}{The aspect ratio of the plot.}

    \item{col}{A specification for the default plotting color(s) for groups.}

    \item{col.regions}{The default plotting color(s) for the z-axis of image intensities.}

    \item{colorkey}{Should a coloykey describing the z-axis be drawn with the plot?}

    \item{subset}{An expression that evaluates to a logical or integer indexing vector to be evaluated in \code{pData(x)}.}

    \item{lattice}{Should lattice graphics be used to create the plot?}
    
    \item{\dots}{additional arguments passed to the underlying \code{\link[graphics]{plot}} functions.}

    \item{mz}{The m/z value for which to plot the ion image.}

    \item{plusminus}{If specified, a window of m/z values surrounding the one given by \code{coord} will be included in the plot with \code{fun} applied over them, and this indicates the range of the window on either side.}

    \item{fold}{What folds of the cross-validation should be plotted.}

    \item{mode}{What kind of results should be plotted. This is the name of the object to plot in the \code{ResultSet} object.}

    \item{column}{What columns of the results should be plotted. If the results are a matrix, this corresponds to the columns to be plotted, which can be indicated either by numeric index or by name.}

}

\note{
    For objects derived from class \linkS4class{SImageSet}, calling \code{image3D(x)} is equivalent to \code{image(x, ~ x * y * z)}.
}

\author{
    Kyle D. Bemis
}

\seealso{
    \code{\link[Cardinal]{plot-methods}},
    \code{\link[Cardinal]{select-methods}}
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

\keyword{hplot}

