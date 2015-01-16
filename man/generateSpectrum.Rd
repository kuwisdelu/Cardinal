\name{generateSpectrum}
\alias{generateSpectrum}

\title{Generate a Simulated Spectrum}

\description{
	Generates a simulated spectral signal, or multiple such signals, with peaks of specified intensities.
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
}

\value{
	A \code{list} with elements:

	\itemize{
        \item{\code{x}: \code{numeric}, a numeric vector of signal intensities
        }
        \item{\code{t}: \code{numeric}, a numeric vector of signal features
        }
    }
}

\author{
	Kyle D. Bemis
}

\seealso{
	\code{\link[Cardinal]{generateImage}}
}

\examples{
s <- generateSpectrum(1)
plot(x ~ t, type="l", data=s)

s <- generateSpectrum(1, centers=c(2000,3000), resolution=10, baseline=3000)
plot(x ~ t, type="l", data=s)

s <- generateSpectrum(1, peaks=2, auc=FALSE, baseline=0)
plot(x ~ t, type="l", data=s)
}

\keyword{datagen}
