\name{plot-methods}
\docType{methods}

\alias{plot}
\alias{plot-methods}
\alias{plot,SImageSet,missing-method}
\alias{plot,SImageSet,formula-method}
\alias{plot,MSImageSet,missing-method}
\alias{plot,MSImageSet,formula-method}
\alias{plot,ResultSet,missing-method}
\alias{plot,ResultSet,formula-method}
\alias{plot,CrossValidated,missing-method}
\alias{plot,PCA,missing-method}
\alias{plot,PLS,missing-method}
\alias{plot,OPLS,missing-method}
\alias{plot,SpatialKMeans,missing-method}
\alias{plot,SpatialShrunkenCentroids,missing-method}

\title{Plot the Feature-Space of an Imaging Dataset}

\description{
	Create and display plots in the feature space of an imaging dataset. This uses a formula interface inspired by the \code{\link[lattice]{lattice}} graphics package.
}

\usage{
\S4method{plot}{SImageSet,missing}(x, formula = ~ Feature,
    pixel,
    pixel.groups,
    groups = NULL,
    superpose = FALSE,
    strip = TRUE,
    key = FALSE,
    fun = mean,
    \dots,
    xlab,
    xlim,
    ylab,
    ylim,
    layout,
    type = 'l',
    col = "black",
    subset = TRUE,
    lattice = FALSE)

\S4method{plot}{MSImageSet,missing}(x, formula = ~ mz,
    pixel = pixels(x, coord=coord),
    pixel.groups,
    coord,
    plusminus,
    \dots,
    type = if (centroided(x)) 'h' else 'l')

\S4method{plot}{ResultSet,missing}(x, formula,
    model = pData(modelData(x)),
    pixel,
    pixel.groups,
    superpose = TRUE,
    strip = TRUE,
    key = superpose,
    \dots,
    xlab,
    ylab,
    column,
    col = if (superpose) rainbow(nlevels(pixel.groups)) else "black",
    lattice = FALSE)

\S4method{plot}{CrossValidated,missing}(x, fold = 1:length(x), layout, \dots)

\S4method{plot}{PCA,missing}(x, formula = substitute(mode ~ mz),
    mode = "loadings",
    type = 'h',
    \dots)

\S4method{plot}{PLS,missing}(x, formula = substitute(mode ~ mz),
    mode = c("coefficients", "loadings",
        "weights", "projection"),
    type = 'h',
    \dots)

\S4method{plot}{OPLS,missing}(x, formula = substitute(mode ~ mz),
    mode = c("coefficients", "loadings", "Oloadings",
        "weights", "Oweights", "projection"),
    type = 'h',
    \dots)

\S4method{plot}{SpatialShrunkenCentroids,missing}(x, formula = substitute(mode ~ mz),
    mode = c("centers", "tstatistics"),
    type = 'h',
    \dots)

\S4method{plot}{SpatialKMeans,missing}(x, formula = substitute(mode ~ mz),
    mode = c("centers", "betweenss", "withinss"),
    type = 'h',
    \dots)

}

\arguments{
    
    \item{x}{An imaging dataset.}
    
    \item{formula}{A formula of the form 'y ~ x | g1 * g2 * ...' (or equivalently, 'y ~ x | g1 + g2 + ...'), indicating a LHS 'y' (on the y-axis) versus a RHS 'x' (on the x-axis) and conditioning variables 'g1, g2, ...'.

    Usually, the LHS is not supplied, and the formula is of the form '~ x | g1 * g2 * ...', and the y-axis is implicityl assumed to be the feature vectors corresponding to each pixel in the imaging dataset specified by the object 'x'. However, a variable evaluating to a feature vector, or a sequence of such variables, can also be supplied.

    The RHS is evaluated in \code{fData(x)} and should provide values for the x-axis.

    The conditioning variables are evaluated in \code{pData(x)}. These can be specified in the formula as 'g1 * g2 * ...'. The argument 'pixel.groups' allows an alternate way to specify a single conditioning variable. Conditioning variables specified using the formula interface will always appear on separate plots. This can be combined with 'superpose = TRUE' to both overlay plots based on a conditioning variable and use conditioning variables to create separate plots.}

    \item{model}{A vector or \code{list} specifying which fitted model to plot. If this is a vector, it should give a subset of the rows of \code{modelData(x)} to use for plotting. Otherwise, it should be a list giving the values of parameters in \code{modelData(x)}.}
    
    \item{pixel}{The pixel or vector of pixels for which to plot the feature vectors. This is an expression that evaluates to a logical or integer indexing vector.}

    \item{pixel.groups}{An alternative way to express a single conditioning variable. This is a variable or expression to be evaluated in \code{pData(x)}, expected to act as a grouping variable for the pixels specified by 'pixel', typically used to distinguish different regions of the imaging data for comparison. Feature vectors from pixels in the same pixel group will have 'fun' applied over them; 'fun' will be applied to each pixel group separately, usually for averaging. If 'superpose = FALSE' then these appear on separate plots.}

    \item{groups}{A variable or expression to be evaluated in \code{fData(x)}, expected to act as a grouping variable for the features in the feature vector(s) to be plotted, typically used to distinguish different groups of features by varying graphical parameters like color and line type. By default, if 'superpose = FALSE', these appear overlaid on the same plot.}

    \item{superpose}{Should feature vectors from different pixel groups specified by 'pixel.groups' be superposed on the same plot?}

    \item{strip}{Should strip labels indicating the plotting group be plotting along with the each panel? Passed to 'strip' in \code{\link[lattice]{xyplot}}.}

    \item{key}{A logical, or \code{list} containing components to be used as a key for the plot. This is passed to 'key' in \code{\link[lattice]{levelplot}} if 'lattice = TRUE'.}

    \item{fun}{A function to apply over feature vectors grouped together by 'pixel.groups'. By default, this is used for averaging over pixels.}

    \item{xlab}{Character or expression giving the label for the x-axis.}

    \item{ylab}{Character or expression giving the label for the x-axis.}

    \item{xlim}{A numeric vector of length 2 giving the left and right limits for the x-axis.}

    \item{ylim}{A numeric vector of length 2 giving the lower and upper limits for the y-axis.}

    \item{layout}{The layout of the plots, given by a length 2 numeric as \code{c(ncol, nrow)}. This is passed to \code{\link[lattice]{levelplot}} if 'lattice = TRUE'. For base graphics, this defaults to one plot per page.}

    \item{col}{A specification for the default plotting color(s).}

    \item{type}{A character indicating the type of plotting.}

    \item{subset}{An expression that evaluates to a logical or integer indexing vector to be evaluated in \code{fData(x)}.}

    \item{lattice}{Should lattice graphics be used to create the plot?}

    \item{\dots}{Additional arguments passed to the underlying \code{\link[graphics]{plot}} or \code{\link[lattice]{xyplot}} functions.}

    \item{coord}{A named vector or list giving the coordinate of the pixel to plot.}

    \item{plusminus}{If specified, a window of pixels surrounding the one given by \code{coord} will be included in the plot with \code{fun} applied over them, and this indicates the number of pixels to include on either side.}

    \item{fold}{What folds of the cross-validation should be plotted.}

    \item{mode}{What kind of results should be plotted. This is the name of the object to plot in the \code{ResultSet} object.}

    \item{column}{What columns of the results should be plotted. If the results are a matrix, this corresponds to the columns to be plotted, which can be indicated either by numeric index or by name.}

}

\author{
	Kyle D. Bemis
}

\seealso{
    \code{\link[Cardinal]{image-methods}}
}

\examples{
data <- matrix(c(NA, NA, 1, 1, NA, NA, NA, NA, NA, NA, 1, 1, NA, NA, 
 NA, NA, NA, NA, NA, 0, 1, 1, NA, NA, NA, NA, NA, 1, 0, 0, 1, 
 1, NA, NA, NA, NA, NA, 0, 1, 1, 1, 1, NA, NA, NA, NA, 0, 1, 1, 
 1, 1, 1, NA, NA, NA, NA, 1, 1, 1, 1, 1, 1, 1, NA, NA, NA, 1, 
 1, NA, NA, NA, NA, NA, NA, 1, 1, NA, NA, NA, NA, NA), nrow=9, ncol=9)

set.seed(1)
sset <- generateImage(data, range=c(1000,5000), centers=c(3000,4000), resolution=100)

pData(sset)$pg <- factor(data[is.finite(data)], labels=c("black", "red"))
fData(sset)$fg <- factor(rep("bg", nrow(fData(sset))), levels=c("bg", "black", "red"))
fData(sset)$fg[2950 < fData(sset)$t & fData(sset)$t < 3050] <- "black"
fData(sset)$fg[3950 < fData(sset)$t & fData(sset)$t < 4050] <- "red"

plot(sset, pixel=1)

plot(sset, ~ t, pixel=1:ncol(sset))

plot(sset, ~ t | pg, pixel=1:ncol(sset), lattice=TRUE)

plot(sset, ~ t, pixel.groups=pg, pixel=1:ncol(sset), lattice=TRUE, superpose=TRUE)

plot(sset, ~ t | pg, groups=fg, pixel=1:ncol(sset), lattice=TRUE)

set.seed(1)
msset <- generateImage(data, as="MSImageSet", resolution=50)

plot(msset, pixel=1)

plot(msset, coord=list(x=3, y=1))

plot(msset, coord=list(x=3, y=1), plusminus=1)

plot(msset, coord=list(x=5, y=5), plusminus=c(2, 1))
}

\keyword{hplot}

