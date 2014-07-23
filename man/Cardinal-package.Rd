\name{Cardinal-package}
\alias{Cardinal-package}
\alias{Cardinal}
\docType{package}

\title{Mass spectrometry imaging tools}

\description{
Implements statistical & computational tools for analyzing mass spectrometry imaging datasets, including methods for efficient pre-processing, spatial segmentation, and classification.
}

\details{
Cardinal provides an abstracted interface to manipulating mass spectrometry imaging datasets, simplifying most of the basic programmatic tasks encountered during the statistical analysis of imaging data. These include image manipulation and processing of both images and mass spectra, and dynamic plotting of both.

While pre-processing steps including normalization, baseline correction, and peak-picking are provided, the core functionality of the package is statistical analysis. The package includes classification and clustering methods based on nearest shrunken centroids, as well as traditional tools like PCA and PLS.

Type \code{vignette("Cardinal-demo")} for a brief walkthrough of common workflows.

To view other vignettes, type \code{browseVignettes("Cardinal")}.
}

\author{
Kyle D. Bemis

Maintainer: Kyle D. Bemis <kbemis@purdue.edu>
}

\keyword{package}
