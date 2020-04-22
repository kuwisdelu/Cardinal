\name{deprecated}
\alias{Deprecated}

\alias{arrange}
\alias{filter}
\alias{select}
\alias{mutate}
\alias{summarise}
\alias{summarize}

\alias{filter.SparseImagingExperiment}
\alias{select.SparseImagingExperiment}
\alias{mutate.SparseImagingExperiment}
\alias{summarise.SparseImagingExperiment}
\alias{summarise.MSImagingExperiment}
\alias{slice.SparseImagingExperiment}

\alias{collect}
\alias{collect.SparseImagingExperiment}
\alias{collect.MSImagingExperiment}
\alias{collect.MSProcessedImagingExperiment}

\alias{groups}
\alias{group_by}
\alias{ungroup}

\title{Deprecated functions and methods in Cardinal}

\description{
	These functions are provided for compatibility with older versions of Cardinal, and will be defunct at the next release.

	\describe{
		\item{filter}{Use \code{subset} or \code{subsetFeatures} instead.}
		\item{select}{Use \code{subset} or \code{subsetPixels} instead.}
		\item{summarize}{Use \code{aggregate}, \code{summarizeFeatures} or \code{summarizePixels} instead.}
		\item{mutate}{No drop-in replacement provided. Use standard assignment instead.}
	}
}
