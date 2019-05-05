\name{image-methods}
\docType{methods}

\alias{image}
\alias{image-methods}

\alias{image,formula-method}
\alias{image,PositionDataFrame-method}
\alias{image,SparseImagingExperiment-method}
\alias{image,MSImagingExperiment-method}
\alias{image,SparseResultImagingExperiment-method}
\alias{image,PCA2-method}
\alias{image,PLS2-method}
\alias{image,SpatialFastmap2-method}
\alias{image,SpatialKMeans2-method}
\alias{image,SpatialShrunkenCentroids2-method}
\alias{image,SpatialDGMM-method}
\alias{image,SegmentationTest-method}

\alias{image3D,SparseImagingExperiment-method}
\alias{image3D,PCA2-method}
\alias{image3D,PLS2-method}
\alias{image3D,SpatialFastmap2-method}
\alias{image3D,SpatialKMeans2-method}
\alias{image3D,SpatialShrunkenCentroids2-method}
\alias{image3D,SpatialDGMM-method}
\alias{image3D,SegmentationTest-method}

\alias{image,SImageSet-method}
\alias{image,MSImageSet-method}
\alias{image,ResultSet-method}
\alias{image,CrossValidated-method}
\alias{image,PCA-method}
\alias{image,PLS-method}
\alias{image,OPLS-method}
\alias{image,SpatialFastmap-method}
\alias{image,SpatialKMeans-method}
\alias{image,SpatialShrunkenCentroids-method}

\alias{image3D}
\alias{image3D-methods}
\alias{image3D,SImageSet-method}
\alias{image3D,MSImageSet-method}
\alias{image3D,ResultSet-method}
\alias{image3D,CrossValidated-method}
\alias{image3D,PCA-method}
\alias{image3D,PLS-method}
\alias{image3D,OPLS-method}
\alias{image3D,SpatialFastmap-method}
\alias{image3D,SpatialKMeans-method}
\alias{image3D,SpatialShrunkenCentroids-method}

\title{Plot an image of the pixel data of an imaging dataset}

\description{
    Create and display images for the pixel data of an imaging dataset using a formula interface.
}

\usage{
\S4method{image}{formula}(x, data = environment(x), \dots,
    xlab, ylab, zlab, subset)

#### Methods for Cardinal >= 2.x classes ####

\S4method{image}{PositionDataFrame}(x, formula,
        groups = NULL,
        superpose = FALSE,
        strip = TRUE,
        key = superpose || !is.null(groups),
        normalize.image = c("none", "linear"),
        contrast.enhance = c("none", "suppression", "histogram"),
        smooth.image = c("none", "gaussian", "adaptive"),
        \dots,
        xlab, xlim,
        ylab, ylim,
        zlab, zlim,
        asp = 1,
        layout,
        col = discrete.colors,
        colorscale = viridis,
        colorkey = !key,
        alpha.power = 1,
        subset = TRUE,
        add = FALSE)

\S4method{image}{SparseImagingExperiment}(x, formula,
        feature,
        feature.groups,
        groups = NULL,
        superpose = FALSE,
        strip = TRUE,
        key = superpose || !is.null(groups),
        fun = mean,
        normalize.image = c("none", "linear"),
        contrast.enhance = c("none", "suppression", "histogram"),
        smooth.image = c("none", "gaussian", "adaptive"),
        \dots,
        xlab, xlim,
        ylab, ylim,
        zlab, zlim,
        asp = 1,
        layout,
        col = discrete.colors,
        colorscale = viridis,
        colorkey = !key,
        alpha.power = 1,
        subset = TRUE,
        add = FALSE)

\S4method{image3D}{SparseImagingExperiment}(x, formula, \dots, alpha.power = 2)

\S4method{image}{MSImagingExperiment}(x, formula,
        feature = features(x, mz=mz),
        feature.groups,
        mz,
        plusminus,
        \dots)

\S4method{image}{SparseResultImagingExperiment}(x, formula,
        model = modelData(x),
        superpose = TRUE,
        \dots,
        column,
        colorscale = cividis,
        colorkey = !superpose,
        alpha.power = 2,
        subset = TRUE)

\S4method{image}{PCA2}(x, formula,
    values = "scores", \dots)

\S4method{image}{PLS2}(x, formula,
    values = c("fitted", "scores"), \dots)

\S4method{image}{SpatialFastmap2}(x, formula,
    values = "scores", \dots)

\S4method{image}{SpatialKMeans2}(x, formula,
    values = "cluster", \dots)

\S4method{image}{SpatialShrunkenCentroids2}(x, formula,
    values = c("probability", "class", "scores"), \dots)

\S4method{image}{SpatialDGMM}(x, formula,
    values = c("probability", "class"), \dots)

\S4method{image}{SegmentationTest}(x, formula,
    values = "mapping", \dots)

#### Methods for Cardinal 1.x classes ####

\S4method{image}{SImageSet}(x, formula = ~ x * y,
    feature,
    feature.groups,
    groups = NULL,
    superpose = FALSE,
    strip = TRUE,
    key = superpose,
    fun = mean,
    normalize.image = c("none", "linear"),
    contrast.enhance = c("none", "suppression", "histogram"),
    smooth.image = c("none", "gaussian", "adaptive"),
    \dots,
    xlab, xlim,
    ylab, ylim,
    zlab, zlim,
    layout,
    asp = 1,
    col = rainbow(nlevels(groups)),
    col.regions = intensity.colors(100),
    colorkey = !is3d,
    subset = TRUE,
    lattice = FALSE)

\S4method{image3D}{SImageSet}(x, formula = ~ x * y * z, \dots)

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

\S4method{image}{SpatialFastmap}(x, formula = substitute(mode ~ x * y),
    mode = "scores",
    \dots)

\S4method{image}{SpatialShrunkenCentroids}(x, formula = substitute(mode ~ x * y),
    mode = c("probabilities", "classes", "scores"),
    \dots)

\S4method{image}{SpatialKMeans}(x, formula = substitute(mode ~ x * y),
    mode = "cluster",
    \dots)
}

\arguments{
    
    \item{x}{An imaging dataset.}
    
    \item{formula}{A formula of the form 'z ~ x * y | g1 * g2 * ...' (or equivalently, 'z ~ x + y | g1 + g2 + ...'), indicating a LHS 'y' (on the y-axis) versus a RHS 'x' (on the x-axis) and conditioning variables 'g1, g2, ...'.

    Usually, the LHS is not supplied, and the formula is of the form '~ x * y | g1 * g2 * ...', and the y-axis is implicityl assumed to be the feature vectors corresponding to each pixel in the imaging dataset specified by the object 'x'. However, a variable evaluating to a vector of pixel values, or a sequence of such variables, can also be supplied.

    The RHS is evaluated in \code{pData(x)} and should provide values for the xy-axes. These must be spatial coordinates.

    The conditioning variables are evaluated in \code{fData(x)}. These can be specified in the formula as 'g1 * g2 * ...'. The argument 'feature.groups' allows an alternate way to specify a single conditioning variable. Conditioning variables specified using the formula interface will always appear on separate plots. This can be combined with 'superpose = TRUE' to both overlay plots based on a conditioning variable and use conditioning variables to create separate plots.}

    \item{data}{A \code{list} or \code{data.frame}-like object from which variables in \code{formula} should be taken.}

    \item{mz}{The m/z value(s) for which to plot the ion image(s).}

    \item{plusminus}{If specified, a window of m/z values surrounding the one given by \code{coord} will be included in the plot with \code{fun} applied over them, and this indicates the range of the window on either side.}
    
    \item{feature}{The feature or vector of features for which to plot the image. This is an expression that evaluates to a logical or integer indexing vector.}

    \item{feature.groups}{An alternative way to express a single conditioning variable. This is a variable or expression to be evaluated in \code{fData(x)}, expected to act as a grouping variable for the features specified by 'feature', typically used to distinguish different groups or ranges of features. Pixel vectors of images from features in the same feature group will have 'fun' applied over them; 'fun' will be applied to each feature group separately, usually for averaging. If 'superpose = FALSE' then these appear on separate plots.}

    \item{groups}{A variable or expression to be evaluated in \code{pData(x)}, expected to act as a grouping variable for the pixel regions in the image(s) to be plotted, typically used to distinguish different image regions by varying graphical parameters like color and line type. By default, if 'superpose = FALSE', these appear overlaid on the same plot.}

    \item{superpose}{Should feature vectors from different feature groups specified by 'feature.groups' be superposed on the same plot? If 'TRUE' then the 'groups' argument is ignored.}

    \item{strip}{Should strip labels indicating the plotting group be plotting along with the each panel? Passed to 'strip' in \code{\link{levelplot}} is 'lattice = TRUE'.}

    \item{key}{A logical, or \code{list} containing components to be used as a key for the plot. This is passed to 'key' in \code{\link{levelplot}} if 'lattice = TRUE'.}

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

    \item{layout}{The layout of the plots, given by a length 2 numeric as \code{c(ncol, nrow)}. This is passed to \code{\link{levelplot}} if 'lattice = TRUE'. For base graphics, this defaults to one plot per page.}

    \item{asp}{The aspect ratio of the plot.}

    \item{col}{A specification for the default plotting color(s) for groups.}

    \item{colorscale}{The color scale to use for the z-axis of image intensities. This may be either a vector of colors or a function which takes a single numeric argument \code{n} and generates a vector of colors of length \code{n}.}

    \item{col.regions}{The default plotting color(s) for the z-axis of image intensities. Thus must be a vector of colors.}

    \item{colorkey}{Should a coloykey describing the z-axis be drawn with the plot?}

    \item{alpha.power}{Opacity scaling factor (1 is linear).}

    \item{subset}{An expression that evaluates to a logical or integer indexing vector to be evaluated in \code{pData(x)}.}
    
    \item{\dots}{additional arguments passed to the underlying \code{\link{plot}} functions.}

    \item{fold}{What folds of the cross-validation should be plotted.}

    \item{model}{A vector or \code{list} specifying which fitted model to plot. If this is a vector, it should give a subset of the rows of \code{modelData(x)} to use for plotting. Otherwise, it should be a list giving the values of parameters in \code{modelData(x)}.}

    \item{mode}{What kind of results should be plotted. This is the name of the object to plot in the \code{ResultSet} object.}

    \item{values}{What kind of results should be plotted. This is the name of the object to plot in the \code{ResultImagingExperiment} object. Renamed from \code{mode} to avoid ambiguity.}

    \item{column}{What columns of the results should be plotted. If the results are a matrix, this corresponds to the columns to be plotted, which can be indicated either by numeric index or by name.}

    \item{lattice}{Should lattice graphics be used to create the plot?}

    \item{add}{Should the method call \code{plot.new()} or be added to the current plot?}

}

\note{
    For objects derived from class \linkS4class{SImageSet}, calling \code{image3D(x)} is equivalent to \code{image(x, ~ x * y * z)}.
}

\author{
    Kylie A. Bemis
}

\seealso{
    \code{\link{plot}},
    \code{\link{selectROI}}
}

\examples{
register(SerialParam())

set.seed(1)
x <- simulateImage(preset=2, npeaks=10, dim=c(10,10))
m <- mz(metadata(x)$design$featureData)

image(x, mz=m[1], plusminus=0.5)
image(x, mz=m[1], smooth.image="gaussian", contrast.enhance="histogram")
image(x, mz=m[1], colorscale=col.map("grayscale"))
image(x, mz=m[4:7], colorscale=col.map("cividis"))
image(x, mz=m[c(1,8)], normalize.image="linear", superpose=TRUE)

pixelData(x)$tic <- summarize(x, .by="pixel", .stat=c(tic="sum"))$tic

image(x, tic ~ x * y, colorscale=magma)
}

\keyword{hplot}

