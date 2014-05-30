\name{Cardinal-package}
\alias{Cardinal-package}
\alias{Cardinal}
\docType{package}

\title{Mass spectrometry imaging tools}

\description{
Implements statistical & computational tools for analyzing mass spectrometry imaging datasets, including methods for efficient pre-processing, spatial segmentation, and classification.
}

\details{
The main class provided by Cardinal is \code{\linkS4class{MSImageSet}}, which offers an abstracted interface to manipulating mass spectrometry imaging datasets, simplifying most of the basic programmatic tasks encountered during the statistical analysis of imaging data. These include flipping and rotating the image, as well as cropping of unwanted pixels, and an apply-like method for mass spectra.

While pre-processing steps including normalization, baseline correction, and peak-picking are provided, the core functionality of the package is statistical analysis. The package includes classification and clustering methods based on nearest shrunken centroids, as well as traditional tools like PCA and PLS.

Type \code{vignette("Cardinal-demo")} for a walkthrough of common workflows.

To view other vignettes, type:

vignette(package="Cardinal")
}

\author{
Kyle D. Bemis

Maintainer: Kyle D. Bemis <kbemis@purdue.edu>
}

\references{
Coming soon
}

\keyword{package}
