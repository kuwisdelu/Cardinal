\name{select-methods}
\docType{methods}

\alias{select}
\alias{select-methods}
\alias{select,SImageSet-method}

\title{Select Regions of an Imaging Dataset}
\description{
	Manually select regions-of-interest or pixels on an imaging dataset. This uses the built-in \code{\link[graphics]{locator}} function. The method has the same form as the \code{\link[Cardinal]{image}} method for plotting imaging datasets.
}
\usage{
\S4method{select}{SImageSet}(x, formula = ~ x * y,
    mode = c("region", "pixel"),
    \dots,
    main,
    subset = TRUE,
    lattice = FALSE)
}
\arguments{
    \item{x}{An imaging dataset.}
    \item{formula}{Passed to \code{\link[Cardinal]{image}}.}
    \item{mode}{What kind of selection to perform: 'region' to select a region-of-interest, or 'pixel' to select individual pixels.}
    \item{\dots}{Addtional arguments to be passed to \code{\link[Cardinal]{image}}.}
    \item{main}{Passed to \code{\link[Cardinal]{image}}.}
    \item{subset}{Passed to \code{\link[Cardinal]{image}}.}
    \item{lattice}{Must be false.}
}
\value{
    A \code{logical} vector of length equal to the number of pixels.
}
\author{
	Kyle D. Bemis
}
\seealso{
    \code{\link[Cardinal]{image}}
}
\keyword{iplot}
