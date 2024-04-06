\name{image-methods}

\alias{image}
\alias{image-methods}

\alias{image,MSImagingExperiment-method}
\alias{image,SpectralImagingExperiment-method}

\title{Plot an image from a spectral imaging dataset}

\description{
    Create and display images from the spectra or pixel data of a spectral imaging dataset using a formula interface.
}

\usage{
\S4method{image}{MSImagingExperiment}(x,
        formula = intensity ~ x * y,
        i = features(x, mz=mz),
        mz = NULL,
        tolerance = NA,
        units = c("ppm", "mz"),
        \dots,
        xlab, ylab,
        subset = TRUE)

\S4method{image}{SpectralImagingExperiment}(x,
        formula,
        i = 1L,
        groups = NULL,
        superpose = FALSE,
        key = TRUE,
        \dots,
        enhance = NULL,
        smooth = NULL,
        scale = NULL,
        subset = TRUE)
}

\arguments{
    
    \item{x}{A spectral imaging dataset.}
    
    \item{formula}{A formula of the form \code{vals ~ x * y} giving the image values and the pixel coordinates. The LHS is taken to be the name of an array in \code{spectraData()} and the RHS is taken to be columns of \code{pixelData()}. Alternatively, if \code{formula} is a string or if \code{i} is NULL, then the LHS is interpreted as the name of a column of \code{pixelData()} as well.}

    \item{i}{The index of the feature(s) to plot for the image(s).}

    \item{mz}{The m/z value(s) to plot for the image(s).}

    \item{tolerance}{If specified, the tolerance to consider a feature as being equal to the given \code{mz} value.}
    
    \item{units}{The units for the above tolerance.}

    \item{\dots}{Additional arguments passed to \code{\link{plot_image}}.}

    \item{xlab, ylab}{Plotting labels.}

    \item{subset}{A logical vector indicating which pixels to include in the image.}

    \item{groups}{A vector coercible to a factor indicating which of the specified features should be plotted with the same color.}

    \item{superpose}{If multiple images are plotted, should they be superposed on top of each other, or plotted seperately?}

    \item{key}{Should a legend or colorkey be plotted?}

    \item{enhance}{The name of a contrast enhancement method, such as \code{"hist"} or \code{"adapt"} for \code{enhance_hist()} and \code{enhance_adapt()}, etc. See \code{\link{enhance}} for details.}

    \item{smooth}{The name of a smoothing method, such as \code{"gauss"} or \code{"bi"} for \code{filt2_gauss()} and \code{filt2_bi()}, etc. See \code{\link{filt2}} for details.}

    \item{scale}{If multiple images are plotted, should they be scaled to the same intensity scale?}
}

\author{
    Kylie A. Bemis
}

\seealso{
    \code{\link{plot}},
    \code{\link{plot_image}},
    \code{\link{selectROI}}
}

\examples{
set.seed(1)
x <- simulateImage(preset=2, npeaks=10, dim=c(16,16))
peaks <- mz(metadata(x)$design$featureData)

image(x, mz=peaks[1L], tolerance=0.5, units="mz")
image(x, mz=peaks[1L], smooth="gaussian")
image(x, mz=peaks[1:9], smooth="adaptive")

x <- summarizePixels(x, stat=c(tic="mean"))
image(x, "tic")
}

\keyword{hplot}

