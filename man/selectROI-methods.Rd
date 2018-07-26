\name{selectROI-methods}
\docType{methods}

\alias{selectROI}
\alias{selectROI-methods}
\alias{selectROI,SparseImagingExperiment-method}
\alias{selectROI,SImageSet-method}

\title{Select regions-of-interest of an imaging dataset}
\description{
	Manually select regions-of-interest or pixels on an imaging dataset. This uses the built-in \code{\link{locator}} function. The method has the same form as the \code{\link{image}} method for plotting imaging datasets.
}
\usage{
\S4method{selectROI}{SparseImagingExperiment}(object, \dots, mode = c("region", "pixels"))

\S4method{selectROI}{SImageSet}(object, formula = ~ x * y,
    mode = c("region", "pixels"),
    \dots,
    main,
    subset = TRUE,
    lattice = FALSE)
}
\arguments{
    \item{object}{An imaging dataset.}
    \item{formula}{Passed to \code{\link{image}}.}
    \item{mode}{What kind of selectROIion to perform: 'region' to select a region-of-interest, or 'pixels' to select individual pixels.}
    \item{\dots}{Addtional arguments to be passed to \code{\link{image}}.}
    \item{main}{Passed to \code{\link{image}}.}
    \item{subset}{Passed to \code{\link{image}}.}
    \item{lattice}{Must be false.}
}
\value{
    A \code{logical} vector of length equal to the number of pixels.
}
\author{
	Kylie A. Bemis
}
\seealso{
    \code{\link{image}}
}
\keyword{iplot}
