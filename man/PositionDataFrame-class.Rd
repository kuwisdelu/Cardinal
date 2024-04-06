\name{PositionDataFrame-class}
\docType{class}

\alias{class:PositionDataFrame}
\alias{PositionDataFrame}
\alias{PositionDataFrame-class}

\alias{coord,PositionDataFrame-method}
\alias{coord<-,PositionDataFrame-method}
\alias{coordNames,PositionDataFrame-method}
\alias{coordNames<-,PositionDataFrame-method}
\alias{run,PositionDataFrame-method}
\alias{run<-,PositionDataFrame-method}
\alias{runNames,PositionDataFrame-method}
\alias{runNames<-,PositionDataFrame-method}
\alias{nrun,PositionDataFrame-method}

\title{PositionDataFrame: Extended data frame with key columns}

\description{
	A data frame for metadata with spatial coordinates and multiple experimental runs.
}

\usage{
PositionDataFrame(coord, run, \dots, row.names = FALSE)
}

\arguments{
	\item{coord}{A data frame or matrix of coordinates.}

	\item{run}{A factor giving the experimental runs.}

	\item{\dots}{Arguments passed to the \code{DataFrame()}.}

	\item{row.names}{Either a vector of row names or a logical value indicating whether row names should be generated automatically (from the m/z values).}
}

\section{Methods}{
	\describe{
		\item{\code{coord(object)}, \code{coord(object) <- value}:}{Get or set the coordinate columns.}

        \item{\code{coordNames(object)}, \code{coordNames(object) <- value}:}{Get or set the names of the coordinate columns.}

        \item{\code{run(object)}, \code{run(object) <- value}:}{Get or set the experimental run column.}

        \item{\code{runNames(object)}, \code{runNames(object) <- value}:}{Get or set the experimental run levels.}

        \item{\code{nrun(object)}:}{Get the number of experimental runs.}
	}
}

\author{Kylie A. Bemis}

\seealso{
	\code{\link{XDataFrame}},
	\code{\link{MassDataFrame}}
}

\examples{
## Create an PositionDataFrame object
coord <- expand.grid(x=1:3, y=1:3)
PositionDataFrame(coord=coord, label=LETTERS[1:9])
}

\keyword{classes}
