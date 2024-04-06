\name{plot-methods}

\alias{plot}
\alias{plot-methods}

\alias{plot,MSImagingExperiment,numeric-method}
\alias{plot,MSImagingExperiment,character-method}
\alias{plot,MSImagingExperiment,formula-method}
\alias{plot,MSImagingExperiment,missing-method}

\alias{plot,MSImagingArrays,numeric-method}
\alias{plot,MSImagingArrays,formula-method}
\alias{plot,MSImagingArrays,missing-method}

\alias{plot,SpectralImagingExperiment,numeric-method}
\alias{plot,SpectralImagingExperiment,character-method}
\alias{plot,SpectralImagingExperiment,formula-method}
\alias{plot,SpectralImagingExperiment,missing-method}

\alias{plot,SpectralImagingArrays,numeric-method}
\alias{plot,SpectralImagingArrays,formula-method}
\alias{plot,SpectralImagingArrays,missing-method}

\title{Plot spectra from a spectral imaging dataset}

\description{
	Create and display sliced images from a spectral imaging dataset using a formula interface.
}

\usage{
\S4method{plot}{MSImagingExperiment,missing}(x,
        formula = intensity ~ mz,
        i = pixels(x, coord=coord, run=run),
        coord = NULL,
        run = NULL,
        \dots,
        xlab, ylab,
        isPeaks = isCentroided(x))

\S4method{plot}{MSImagingArrays,missing}(x,
        formula = intensity ~ mz,
        i = pixels(x, coord=coord, run=run),
        coord = NULL,
        run = NULL,
        \dots,
        xlab, ylab,
        isPeaks = isCentroided(x))

\S4method{plot}{SpectralImagingExperiment,missing}(x,
        formula,
        i = 1L,
        groups = NULL,
        superpose = FALSE,
        key = TRUE,
        \dots,
        n = 2000L,
        downsampler = "lttb",
        isPeaks = FALSE,
        annPeaks = 0)

\S4method{plot}{SpectralImagingArrays,missing}(x,
        formula,
        i = 1L,
        groups = NULL,
        superpose = FALSE,
        key = TRUE,
        \dots,
        n = 2000L,
        downsampler = "lttb",
        isPeaks = FALSE,
        annPeaks = 0)
}

\arguments{
    \item{x}{A spectral imaging dataset.}
    
    \item{formula}{A formula of the form \code{vals ~ t} giving the spectra values and their domain locations. The LHS is taken to be the name of an array in \code{spectraData()} and the RHS is either an array in \code{spectraData()} for \code{SpectralImagingArrays}-derived classes or a column of \code{featureData()} for \code{SpectralImagingExperiment}-derived classes. Alternatively, if \code{formula} is a string or if \code{i} is NULL, then the LHS is interpreted as the name of a column of \code{featureData()} for \code{SpectralImagingExperiment} as well.}

    \item{i}{The index of the spectrum to plot.}

    \item{coord}{The coordinates of the spectrum to plot.}

    \item{run}{The run of the spectrum to plot.}

    \item{\dots}{Additional arguments passed to \code{\link[matter]{plot_signal}}.}

    \item{xlab, ylab}{Plotting labels.}

    \item{isPeaks}{Should the spectrum be plotted as peaks or as a continuous signal?}

    \item{annPeaks}{If \code{isPeaks} is \code{TRUE}, either an integer giving the number of peaks to annotate (i.e., label with their location), or a plotting symbol (e.g., "circle", "cross", etc.) to indicate the peak locations.}

    \item{groups}{A vector coercible to a factor indicating which of the specified spectra should be plotted with the same color.}

    \item{superpose}{If multiple spectra are plotted, should they be superposed on top of each other, or plotted seperately?}

    \item{key}{Should a legend or colorkey be plotted?}

    \item{n, downsampler}{A spectrum can contain far more data points than are needed to visualize it, potentially making the plotting unnecessarily slow. Downsampling is performed to improve plotting speed while maintaining the visual integrity of the spectrum. See \code{\link{downsample}} for details.}
}

\author{
	Kylie A. Bemis
}

\seealso{
    \code{\link{plot}},
    \code{\link{plot_signal}}
}

\examples{
set.seed(1)
x <- simulateImage(preset=1, npeaks=10, dim=c(3,3))

plot(x, i=4)
plot(x, coord=c(x=1, y=2))
plot(x, log2(intensity + 1) ~ mz, i=4,
    xlab=expression(italic("m/z")),
    ylab=expression(italic("Log Intensity")))
}

\keyword{hplot}

