\name{selectROI}

\alias{selectROI}
\alias{selectROI,SpectralImagingExperiment-method}
\alias{makeFactor}

\title{Select regions-of-interest in an image}

\description{
	Manually select regions-of-interest or pixels on an imaging dataset. The \code{selectROI} method uses the built-in \code{\link{locator}} function. It can be used with an existing image plot, or a new image will be plotted if \code{image} arguments are passed via \code{...}.

    The regions of interest are returned as logical vectors indicating which pixels have been selected. These logical vectors can be combined into factors using the \code{makeFactor} function.
}

\usage{
\S4method{selectROI}{SpectralImagingExperiment}(object, \dots, mode = c("region", "pixels"))

makeFactor(\dots, ordered = FALSE)
}
\arguments{
    \item{object}{A spectral imaging dataset.}

    \item{mode}{The mode of selection: "region" to select a region-of-interest as a polygon, or "pixels" to select individual pixels.}

    \item{\dots}{Additional arguments to be passed to \code{\link{image}} for \code{selectROI}, or name-value pairs of logical vectors to be combined by \code{makeFactor}.}
    
    \item{ordered}{Should the resulting factor be ordered or not?}
}

\value{
    A \code{logical} vector of length equal to the number of pixels for \code{selectROI}.

    A factor of the same length as the logical vectors for \code{makeFactor}.
}

\author{
	Kylie A. Bemis
}

\seealso{
    \code{\link[Cardinal]{image}}
}

\keyword{iplot}
