\name{generateSpectrum}

\alias{generateSpectrum}
\alias{generateImage}

\title{Generate a simulated spectrum or image}

\description{
	Generates a simulated spectral signal, or multiple such signals, with peaks of specified intensities.

    These functions are provided for backward-compatibility; new code should use \code{\link{simulateSpectrum}} and \code{\link{simulateImage}} instead.
}

\usage{
generateSpectrum(n, peaks = 100,
    range = c(1001, 20000),
    centers = seq(
        from = range[1] + diff(range) / (peaks + 1),
        to = range[2] - diff(range) / (peaks + 1),
        length.out = peaks),
    intensities = runif(peaks, min=0.1, max=1),
    step = diff(range)/1e3,
    resolution = 500,
    noise = 0.05,
    sd = 0.1,
    baseline = 2000,
    auc = TRUE)

generateImage(data = factor(1),
    coord = expand.grid(
        x = 1:max(1, nrow(data)),
        y = 1:max(1, ncol(data))),
    peaks = length(levels(as.factor(data))),
    delta = 10,
    as = c("SImageSet", "MSImageSet"),
    \dots)
}

\arguments{
    \item{n}{The number of signals to simulate.}

    \item{peaks}{The number of peaks in the signal.}
    
    \item{range}{A pair of numbers specifying the range of continues feature values at which the signal is measured.}
    
    \item{centers}{The values of the singal feature at which peaks occur.}

    \item{intensities}{The values of the intensities of the peaks, which could either be heights of the peaks or their area under the curve.}

    \item{step}{The step size between measurements in the feature space.}

    \item{resolution}{The instrument resolution. This affects the width of the peaks. Higher resolutions produce sharper peaks.}

    \item{noise}{A value without scale that indicates the amount of noise in the signal.}

    \item{sd}{Standard deviation of the intensities of the peaks.}

    \item{baseline}{A value without scale that indicates the shape and size of the baseline.}

    \item{auc}{Should the peak heights be influenced by the area under the curve? This reflects fragmentation and limited accuracy at higher mass ranges. If 'FALSE' then the peak heights correspond directly to the provided intensities.}

    \item{data}{Either a \code{factor} or an integer matrix. If a \code{factor} is used, the \code{coord} argument should be specified with \code{data} to indicate the arrangement of regions in the image. If a matrix is given, \code{coord} should not be specified. The image will automatically be generated with different regions corresponding to unique integers in the matrix.}
    
    \item{coord}{A \code{data.frame} with columns representing the spatial dimensions. Each row provides a spatial coordinate for the location of an element of \code{data} if \code{data} is a factor.}

    \item{delta}{The effect size of the difference between peaks differentiating different regions in the image (as specified by \code{data}).}

    \item{as}{Should the output object be an \code{\linkS4class{SImageSet}} or \code{\linkS4class{MSImageSet}}?}

    \item{\dots}{Additional arguments to pass to \code{\link{generateSpectrum}}.}
}

\value{
	For \code{generateSpectrum}, a \code{list} with elements:

	\itemize{
        \item{\code{x}: \code{numeric}, a numeric vector of signal intensities
        }
        \item{\code{t}: \code{numeric}, a numeric vector of signal features
        }
    }

    For \code{generateImage()}, a \code{\linkS4class{SImageSet}} or a \code{\linkS4class{MSImageSet}}.
}

\author{
	Kylie A. Bemis
}

\seealso{
	\code{\link{simulateSpectrum}},
    \code{\link{simulateImage}}
}

\examples{
# Generate spectra
s <- generateSpectrum(1)
plot(x ~ t, type="l", data=s)

s <- generateSpectrum(1, centers=c(2000,3000), resolution=10, baseline=3000)
plot(x ~ t, type="l", data=s)

s <- generateSpectrum(1, peaks=2, auc=FALSE, baseline=0)
plot(x ~ t, type="l", data=s)

# Generate images
data <- matrix(c(NA, NA, 1, 1, NA, NA, NA, NA, NA, NA, 1, 1, NA, NA, 
 NA, NA, NA, NA, NA, 0, 1, 1, NA, NA, NA, NA, NA, 1, 0, 0, 1, 
 1, NA, NA, NA, NA, NA, 0, 1, 1, 1, 1, NA, NA, NA, NA, 0, 1, 1, 
 1, 1, 1, NA, NA, NA, NA, 1, 1, 1, 1, 1, 1, 1, NA, NA, NA, 1, 
 1, NA, NA, NA, NA, NA, NA, 1, 1, NA, NA, NA, NA, NA), nrow=9, ncol=9)

set.seed(1)
x <- generateImage(data)

plot(x, pixel=1)
image(x, feature=1)

coord <- expand.grid(x=1:nrow(data), y=1:ncol(data))

data2 <- as.factor(data[is.finite(data)])
coord2 <- coord[is.finite(data),]

set.seed(1)
x2 <- generateImage(data=data, coord=coord, as="MSImageSet")

plot(x, pixel=1)
image(x2, feature=1)
}

\keyword{datagen}
