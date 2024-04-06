\name{normalize}

\alias{normalize}
\alias{normalize,SpectralImagingData-method}

\title{Normalize spectra}

\description{
	Apply deferred normalization to spectra.
}

\usage{
\S4method{normalize}{SpectralImagingData}(object,
    method = c("tic", "rms", "reference"), \dots)
}

\arguments{
    \item{object}{A spectral imaging dataset.}

    \item{method}{The normalization method to use. See \code{\link[matter]{rescale}} for details.}

    \item{\dots}{Additional arguments passed to the normalization function.}
}

\details{
    The supported normalization methods are:

    \itemize{

        \item{"tic":}{Total ion current normalization using \code{\link{rescale_sum}}.}

        \item{"rms":}{Root-mean-squared normalization using \code{\link{rescale_rms}}.}

        \item{"reference":}{Normalization according to a reference feature using \code{\link{rescale_ref}}.}
    }
}

\note{
    The normalization is deferred until \code{process()} is called.
}

\value{
    An object of the same class with the processing step queued.
}

\author{
	Kylie A. Bemis
}

\seealso{
    \code{\link{smooth}},
    \code{\link{recalibrate}},
    \code{\link{reduceBaseline}},
    \code{\link{peakPick}},
    \code{\link{process}}
}

\examples{
set.seed(1)
mse <- simulateImage(preset=1, npeaks=10, dim=c(3,3))

# queue normalization
mse2 <- normalize(mse, method="tic")

# apply normalization
mse2 <- process(mse2)
}

\keyword{ts}
