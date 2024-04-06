\name{selectROI-methods}
\docType{methods}

\alias{selectROI}
\alias{selectROI-methods}
\alias{selectROI,SparseImagingExperiment-method}
\alias{makeFactor}

\title{Select regions-of-interest of an imaging dataset}
\description{
	Manually select regions-of-interest or pixels on an imaging dataset. The \code{selectROI} method uses the built-in \code{\link{locator}} function. The method has the same form as the \code{\link{image}} method for plotting imaging datasets.

    The results are returned as logical vectors indicating which pixels have been selected. These logical vectors can be combined into factors using the \code{makeFactor} function.
}
\usage{
\S4method{selectROI}{SparseImagingExperiment}(object, \dots, mode = c("region", "pixels"))

makeFactor(\dots, ordered = FALSE)
}
\arguments{
    \item{object}{An imaging dataset.}
    \item{mode}{What kind of selection to perform: 'region' to select a region-of-interest, or 'pixels' to select individual pixels.}
    \item{\dots}{Addtional arguments to be passed to \code{\link{image}} for \code{selectROI}, or name-value pairs of logical vectors to be combined by \code{makeFactor}.}
    \item{ordered}{Should the resulting factor be ordered or not?}
}
\value{
    A \code{logical} vector of length equal to the number of pixels for \code{selectROI}.

    A \code{factor} of the same length as the passed \code{logical} vectors for \code{makeFactor}.
}
\author{
	Kylie A. Bemis
}
\seealso{
    \code{\link{image}}
}
\keyword{iplot}
