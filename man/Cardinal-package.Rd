\name{Cardinal-package}
\alias{Cardinal-package}
\docType{package}

\alias{Cardinal}

\alias{CardinalVersion}
\alias{CardinalLog}

\alias{getCardinalBPPARAM}
\alias{setCardinalBPPARAM}

\alias{getCardinalVerbose}
\alias{setCardinalVerbose}

\alias{getCardinalNumBlocks}
\alias{setCardinalNumBlocks}

\alias{getCardinalDelayProc}
\alias{setCardinalDelayProc}

\title{Mass spectrometry imaging tools}

\description{
Implements statistical & computational tools for analyzing mass spectrometry imaging datasets, including methods for efficient pre-processing, spatial segmentation, and classification.
}

\details{
Cardinal provides an abstracted interface to manipulating mass spectrometry imaging datasets, simplifying most of the basic programmatic tasks encountered during the statistical analysis of imaging data. These include image manipulation and processing of both images and mass spectra, and dynamic plotting of both.

While pre-processing steps including normalization, baseline correction, and peak-picking are provided, the core functionality of the package is statistical analysis. The package includes classification and clustering methods based on nearest shrunken centroids, as well as traditional tools like PCA and PLS.

Type \code{browseVignettes("Cardinal")} to view a user's guide and vignettes of common workflows.
}


\section{Options}{

	The following options can be set via \code{options()}.

	\itemize{
		\item{\code{getCardinalgetCardinalBPPARAM(), setCardinalBPPARAM(BPPARAM=SerialParam())}: The default backend to use for parallel processing. By default, this is initially set to a serial backend (no parallelization).}

		\item{\code{getCardinalVerbose(), setCardinalVerbose(verbose=interactive())}: Should detailed messages be printed?}

		\item{\code{getCardinalNumBlocks(), setCardinalNumBlocks(n=20L)}: The default number of data chunks used by \code{pixelApply()}, \code{featureApply()}, and \code{spatialApply()} when \code{.blocks=TRUE}. Used by many methods internally.}

		\item{\code{getCardinalDelayProc(), setCardinalDelayProc(delay=TRUE)}: Should pre-processing functions like \code{normalize()} and \code{peakPick()} be delayed (until \code{process()} is called)?}
	}
}

\section{Logging}{

	For support or debugging help, please provide the output of a call to \code{CardinalLog()}. By default, this saves a log to the file "Cardinal.log" in the current working directory.

}

\author{
Kylie A. Bemis

Maintainer: Kylie A. Bemis <k.bemis@northeastern.edu>
}

\keyword{package}
