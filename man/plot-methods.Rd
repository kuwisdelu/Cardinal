\name{plot-methods}
\docType{methods}

\alias{plot}
\alias{plot-methods}
\alias{plot,SImageSet,missing-method}
\alias{plot,SImageSet,formula-method}

\title{Plot the Feature-Space of an Imaging Dataset}

\description{
	Create and display plots in the feature space of an imaging dataset. This uses a formula interface inspired by the \code{\link[lattice]{lattice}} graphics package.
}

\usage{
\S4method{plot}{SImageSet,missing}(x, formula = ~ featureNames(x),
    pixel,
    pixel.groups,
    groups = NULL,
    superpose = FALSE,
    fun = mean,
    subset = TRUE,
    lattice = FALSE,
    \dots)

\S4method{plot}{MSImageSet,missing}(x, \dots)
}

\arguments{
    
    \item{x}{An imaging dataset or results.}
    
    \item{formula}{A formula of the form 'y ~ x | g1 * g2 * ...' (or equivalently, 'y ~ x | g1 + g2 + ...'), indicating a LHS 'y' (on the y-axis) versus a RHS 'x' (on the x-axis) and conditioning variables 'g1, g2, ...'.

    Usually, the LHS is not supplied, and the formula is of the form '~ x | g1 * g2 * ...', and the y-axis is implicityl assumed to be the feature vectors corresponding to each pixel in the imaging dataset specified by the object 'x'. However, a variable evaluating to a feature vector, or a sequence of such variables, can also be supplied.

    The RHS is evaluated in \code{fData(x)} and should provide values for the x-axis.

    The conditioning variables are evaluated in \code{pData(x)}. These can be specified in the formula as 'g1 * g2 * ...'. The argument 'pixel.groups' allows an alternate way to specify a single conditioning variable. Conditioning variables specified using the formula interface will always appear on separate plots. This can be combined with 'superpose=TRUE' to both overlay plots based on a conditioning variable and use conditioning variables to create separate plots.}
    
    \item{pixel}{The pixel or vector of pixels for which to plot the feature vectors. This is an expression that evaluates to a logical or integer indexing vector.}

    \item{pixel.groups}{An alternative way to express a single conditioning variable. This is a variable or expression to be evaluated in \code{pData(x)}, expected to act as a grouping variable for the pixels specified by 'pixel', typically used to distinguish different regions of the imaging data for comparison. Feature vectors from pixels in the same pixel group will have 'fun' applied over them; 'fun' will be applied to each pixel group separately, usually for averaging. If 'superpose = FALSE' then these appear on separate plots.}

    \item{groups}{A variable or expression to be evaluated in \code{fData(x)}, expected to act as a grouping variable for the features in the feature vector(s) to be plotted, typically used to distinguish different groups of features by varying graphical parameters like color and line type. By default, if 'superpose = FALSE', these appear overlaid on the same plot.}

    \item{superpose}{Should feature vectors from different pixel groups specified by 'pixel.groups' be superposed on the same plot? If 'TRUE' then the 'groups' argument is ignored.}

    \item{fun}{A function to apply over feature vectors grouped together by 'pixel.groups'. By default, this is used for averaging over pixels.}

    \item{subset}{An expression that evaluates to a logical or integer indexing vector to be evaluated in \code{fData(x)}.}

    \item{lattice}{Should lattice graphics be used to create the plot?}
    
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
data <- structure(c(NA, NA, 1, 1, NA, NA, NA, NA, NA, NA, 1, 1, NA, NA, 
NA, NA, NA, NA, NA, 0, 1, 1, NA, NA, NA, NA, NA, 1, 0, 0, 1, 
1, NA, NA, NA, NA, NA, 0, 1, 1, 1, 1, NA, NA, NA, NA, 0, 1, 1, 
1, 1, 1, NA, NA, NA, NA, 1, 1, 1, 1, 1, 1, 1, NA, NA, NA, 1, 
1, NA, NA, NA, NA, NA, NA, 1, 1, NA, NA, NA, NA, NA), .Dim = c(9L, 9L))

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

msset <- generateImage(data, as="MSImageSet", resolution=50)

plot(msset, pixel=1)
}

\keyword{methods}

