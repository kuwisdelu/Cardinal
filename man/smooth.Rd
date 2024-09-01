\name{smooth}

\alias{smooth}
\alias{smooth,SpectralImagingData-method}

\title{Smooth spectra}

\description{
    Apply deferred smoothing to spectra.
}

\usage{
\S4method{smooth}{SpectralImagingData}(x,
    method = c("gaussian", "bilateral", "adaptive",
        "diff", "guide", "pag", "sgolay", "ma"), \dots)
}

\arguments{
    \item{x}{A spectral imaging dataset.}

    \item{method}{The smoothing method to use. See \code{\link[matter]{filt1}} for details.}

    \item{\dots}{Additional arguments passed to the smoothing function.}
}

\details{
    The supported smoothing methods are:

    \itemize{

        \item{"gaussian": Gaussian smoothing using \code{\link{filt1_gauss}}.}

        \item{"bilateral": Bilateral filter using \code{\link{filt1_bi}}.}

        \item{"adaptive": Adaptive bilateral filter using \code{\link{filt1_adapt}}.}

        \item{"diff": Nonlinear diffusion smoothing using \code{\link{filt1_diff}}.}

        \item{"guide": Guided filter using \code{\link{filt1_guide}}.}

        \item{"pag": Peak-aware guided filter using \code{\link{filt1_pag}}.}

        \item{"sgolay": Savitzky-Golar filter using \code{\link{filt1_sg}}.}

        \item{"ma": Moving average filter using \code{\link{filt1_ma}}.}
    }
}

\note{
    The smoothing is deferred until \code{process()} is called.
}

\value{
    An object of the same class with the processing step queued.
}

\author{
    Kylie A. Bemis
}

\seealso{
    \code{\link{normalize}},
    \code{\link{recalibrate}},
    \code{\link{reduceBaseline}},
    \code{\link{peakPick}},
    \code{\link{process}}
}

\examples{
set.seed(1, kind="L'Ecuyer-CMRG")
mse <- simulateImage(preset=1, npeaks=10, dim=c(3,3))

# queue smoothing
mse2 <- smooth(mse, method="gaussian", width=11)
plot(mse2, i=4)

# apply smoothing
mse2 <- process(mse2)
}

\keyword{ts}
\keyword{smooth}
