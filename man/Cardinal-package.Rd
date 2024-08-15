\name{Cardinal-package}
\alias{Cardinal-package}
\docType{package}

\alias{Cardinal}

\alias{getCardinalBPPARAM}
\alias{setCardinalBPPARAM}
\alias{getCardinalVerbose}
\alias{setCardinalVerbose}

\alias{getCardinalNChunks}
\alias{setCardinalNChunks}
\alias{getCardinalChunksize}
\alias{setCardinalChunksize}
\alias{getCardinalSerialize}
\alias{setCardinalSerialize}

\alias{getCardinalLog}
\alias{setCardinalLog}
\alias{saveCardinalLog}

\alias{vizi_style}
\alias{vizi_engine}
\alias{vizi_par}

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

	The following Cardinal-specific options are available:

	\itemize{
		\item{\code{getCardinalBPPARAM(), setCardinalBPPARAM(BPPARAM=NULL)}: The default backend to use for parallel processing. By default, this is initially set to \code{NULL} (no parallelization). Otherwise, it must be a \code{BiocParallelParam} instance. See documentation for \code{\link{bplapply}}.}

		\item{\code{getCardinalVerbose(), setCardinalVerbose(verbose=interactive())}: Should progress messages be printed?}
	}

	The following Cardinal-controlled \code{matter} chunk options are available:

	\itemize{
		\item{\code{getCardinalNChunks(), setCardinalNChunks(nchunks=20L)}: The default number of data chunks used when iterating over large datasets. Used by many methods internally.}

		\item{\code{getCardinalChunksize(), setCardinalChunksize(chunksize=NA, units="MB")}: The approximate size of data chunks used when iterating over large datasets. Can be used as an alternative to setting the number of chunks. The default (\code{NA}) means to ignore this parameter and use the \code{getCardinalNChunks()}.}

		\item{\code{getCardinalSerialize(), setCardinalSerialize(serialize=NA)}: Whether data chunks should be loaded on the manager and serialized to the workers (\code{TRUE}), or loaded on the workers (\code{FALSE}). The default (\code{NA}) means to choose automatically based on the type of data and the type of cluster.}
	}

	The following Cardinal-controlled \code{matter} logging options are available:

	\itemize{
		\item{\code{getCardinalLog(), setCardinalLog(file=NULL)}: The log file or its history. Setting the log file to \code{NULL} means the log is stored in memory. Providing a file path moves the log to the specified file. Accessing the log returns the complete history if the log is in memory, or the file path if it is stored in a log file.}

		\item{\code{saveCardinalLog(file = "Cardinal.log"))}: This is the same as \code{setCardinalLog} but defaults to saving the log file in the current working directory.}
	}

	Additionally, visualization parameters are available:

	\itemize{
		\item{\code{vizi_style()}: Set the default plotting style and color palettes.}

		\item{\code{vizi_engine()}: Set the default plotting engine.}

		\item{\code{vizi_par()}: Set default graphical parameters.}
	}
}

\author{
Kylie A. Bemis

Maintainer: Kylie A. Bemis <k.bemis@northeastern.edu>
}

\keyword{package}
