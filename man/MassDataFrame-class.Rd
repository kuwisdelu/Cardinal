\name{MassDataFrame-class}
\docType{class}

\alias{class:MassDataFrame}
\alias{MassDataFrame}
\alias{MassDataFrame-class}

\alias{mz,MassDataFrame-method}
\alias{mz<-,MassDataFrame-method}
\alias{coerce,DFrame,MassDataFrame-method}

\title{MassDataFrame: Extended data frame with key columns}

\description{
	A data frame for mass spectrometry feature metadata with a required column for m/z values.
}

\usage{
MassDataFrame(mz, \dots, row.names = FALSE)
}

\arguments{
	\item{mz}{A sorted vector of m/z values.}

	\item{\dots}{Arguments passed to the \code{DataFrame()}.}

	\item{row.names}{Either a vector of row names or a logical value indicating whether row names should be generated automatically (from the m/z values).}
}

\section{Methods}{
	\describe{
		\item{\code{mz(object, ...)}, \code{mz(object, ...) <- value}:}{Get or set the m/z values.}
	}
}

\author{Kylie A. Bemis}

\seealso{
	\code{\link{XDataFrame}},
	\code{\link{PositionDataFrame}}
}

\examples{
## Create an MassDataFrame object
MassDataFrame(mz=sort(500 * runif(10)), label=LETTERS[1:10])
}

\keyword{classes}
