\name{simulateSpectrum}

\alias{simulateSpectrum}
\alias{simulateImage}

\alias{addShape}
\alias{presetImageDef}

\title{Simulate a mass spectrum or MS imaging experiment}

\description{
	Simulate mass spectra or complete MS imaging experiments, including a possible baseline, spatial and spectral noise, mass drift, mass resolution, and multiplicative variation, etc.

    A number of preset imaging designs are available for quick-and-dirty simulation of images.

    These functions are designed for small proof-of-concept examples and testing, and may not scale well to simulating larger datasets.
}

\usage{
simulateSpectrum(n = 1L, peaks = 50L,
    mz = rlnorm(peaks, 7, 0.3), intensity = rlnorm(peaks, 1, 0.9),
    from = 0.9 * min(mz), to = 1.1 * max(mz), by = 400,
    sdpeaks = sdpeakmult * log1p(intensity), sdpeakmult = 0.2,
    sdnoise = 0.1, sdmz = 10, resolution = 1000, fmax = 0.5,
    baseline = 0, decay = 10, units=c("ppm", "mz"),
    representation = c("profile", "centroid"), \dots)

simulateImage(pixelData, featureData, preset,
    from = 0.9 * min(mz), to = 1.1 * max(mz), by = 400,
    sdrun = 1, sdpixel = 1, spcorr = 0.3, sptype = "SAR",
    representation = c("profile", "centroid"), units=c("ppm", "mz"),
    as = c("MSImagingExperiment", "SparseImagingExperiment"),
    BPPARAM = bpparam(), \dots)

addShape(pixelData, center, size, shape=c("circle", "square"), name=shape)

presetImageDef(preset = 1L, nruns = 1, npeaks = 30L,
    dim = c(20L, 20L), peakheight = 1, peakdiff = 1,
    jitter = TRUE, \dots)
}

\arguments{
    \item{n}{The number of spectra to simulate.}

    \item{peaks, npeaks}{The number of peaks to simulate. Not used if \code{mz} and \code{intensity} are provided.}

    \item{mz}{The theoretical m/z values of the simulated peaks.}

    \item{intensity}{The mean intensities of the simulated peaks.}

    \item{from}{The minimum m/z value used for the mass range.}

    \item{to}{The maximum m/z value used for the mass range.}

    \item{by}{The step-size used for the observed m/z-values of the profile spectrum.}

    \item{sdpeaks}{The standard deviation(s) for the distributions of observed peak intensities on the log scale.}

    \item{sdpeakmult}{A multiplier used to calculate \code{sdpeaks} based on the mean intensities of peaks; used to simulate multiplicative variance. Not used if \code{sdpeaks} is provided.}

    \item{sdnoise}{The standard deviation of the random noise in the spectrum on the log scale.}

    \item{sdmz}{The standard deviation of the mass error in the observed m/z values of peaks, in units indicated by \code{units}.}

    \item{resolution}{The mass resolution as defined by \code{m / dm}, where \code{m} is the observed mass and \code{dm} is the width of the peak at a proportion of its maximum height defined by \code{fmax} (defaults to full-width-at-half-maximum -- FWHM -- definition). Note that this is NOT the same as the definition of \code{resolution} in the \code{\link{readImzML}} function.}

    \item{fmax}{The proportion of the maximum peak height to use when defining the mass resolution.}

    \item{baseline}{The maximum intensity of the baseline. Note that \code{baseline=0} means there is no baseline.}

    \item{decay}{A constant used to calculate the exponential decay of the baseline. Larger values mean the baseline decays more sharply at the lower mass range of the spectrum.}

    \item{units}{The units for \code{by} and \code{sdmz}. Either parts-per-million or absolute m/z units.}

    \item{representation}{Should a profile spectrum be returned or only the centroided peaks?}

    \item{BPPARAM}{An optional instance of \code{BiocParallelParam}. See documentation for \code{\link{bplapply}}.}

    \item{pixelData}{A \code{\linkS4class{PositionDataFrame}} giving the pixel design of the experiment. The names of the columns should match the names of columns in \code{featureData}. Each column should be a logical vector corresponding to a morphological substructure, indicate which pixels belong to that substructure.}

    \item{featureData}{A \code{\linkS4class{MassDataFrame}} giving the feature design of the experiment. Each row should correspond to an expected peak. The names of the columns should match the names of columns in \code{pixelData}. Each column should be a numeric vector corresponding to a morphological substructure, giving the mean intensity of that peak for that substructure.}

    \item{preset}{A number indicating a preset image definition to use.}

    \item{nruns}{The number of runs to simulate for each condition.}

    \item{sdrun}{A standard deviation giving the run-to-run variance.}

    \item{sdpixel}{A standard deviation giving the pixel-to-pixel variance.}

    \item{spcorr}{The spatial autocorrelation. Must be between 0 and 1, where \code{spcorr=0} indicates no spatial autocorrelation.}

    \item{sptype}{The type of spatial autocorrelation.}

    \item{as}{The class of object to be returned.}

    \item{\dots}{Additional arguments to pass to \code{\link{simulateSpectrum}} or \code{\link{presetImageDef}}.}

    \item{dim}{The dimensions of the preset image.}

    \item{peakheight}{Reference intensities used for peak heights by the preset.}

    \item{peakdiff}{A reference intensity difference used for the mean peak height difference between conditions, for presets that simulate multiple conditions.}

    \item{jitter}{Should random noise be added to the location and size of the shapes?}

    \item{center}{The center of the shape.}

    \item{size}{The size of the shape (from the center).}

    \item{shape}{What type of shape to add.}

    \item{name}{The name of the added column.}
}

\details{
    The \code{simulateSpectrum()} and \code{simulateImage()} functions are used to simulate mass spectra and MS imaging experiments. They provide a great deal of control over the parameters of the simulation, including all sources of variation.

    For \code{simulateImage()}, the user should provide the design of the simulated experiment via matching columns in \code{pixelData} and \code{featureData}, where each column corresponds to different morphological substructures or differing conditions. These design data frames are returned in the \code{metadata()} of the returned object for later reference.

    A number of presets are defined by \code{presetImageDef()}, which returns only the \code{pixelData} and \code{featureData} necessary to define the experiment for \code{simulateImage()}. These can be referenced for help in understanding how to define experiments for \code{simulateImage()}.

    The \code{addShape()} function is provided for convenience when generating the \code{pixelData} for \code{simulateImage()}, as a way of simply adding morphological substructures using basic shapes such as squares and circles.
}

\value{
	For \code{simulateSpectrum}, a \code{list} with elements:

	\itemize{
        \item{\code{mz}: a numeric vector of the observed m/z values}

        \item{\code{intensity}: a numeric vector or matrix of the intensities}
    }

    For \code{simulateImage}, a \code{\linkS4class{MSImagingExperiment}} or a \code{\linkS4class{SparseImagingExperiment}}.

    For \code{addShape}, a new \code{\linkS4class{PositionDataFrame}} with a logical column added for the corresponding shape.

    For \code{presetImageDef}, a list with two elements: the \code{pixelData} and \code{featureData} to be used as input to \code{simulateImage()}.
}

\author{
	Kylie A. Bemis
}

\seealso{
	\code{\link{simulateSpectrum}},
    \code{\link{simulateImage}}
}

\examples{
# generate a spectrum
s <- simulateSpectrum(1)
plot(intensity ~ mz, data=s, type="l")

# generate a noisy low-resolution spectrum with a baseline
s <- simulateSpectrum(1, baseline=2, sdnoise=0.3, resolution=100)
plot(intensity ~ mz, data=s, type="l")

# generate a high-resolution spectrum
s <- simulateSpectrum(1, peaks=100, resolution=10000)
plot(intensity ~ mz, data=s, type="l")

# generate an image
x <- simulateImage(preset=1, npeaks=9, dim=c(9,9))
m <- mz(metadata(x)$design$featureData)

image(x, mz=m[5])

plot(x, coord=c(x=2, y=2))
}

\keyword{datagen}
