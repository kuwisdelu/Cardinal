\name{PositionDataFrame-class}
\docType{class}

\alias{class:PositionDataFrame}
\alias{PositionDataFrame}
\alias{PositionDataFrame-class}

\alias{coord}
\alias{coord<-}
\alias{coord-methods}
\alias{coordinates}
\alias{coordinates<-}
\alias{coordinates-methods}
\alias{coordLabels}
\alias{coordLabels<-}
\alias{coordLabels-methods}
\alias{coordnames}
\alias{coordnames<-}
\alias{coordnames-methods}
\alias{gridded}
\alias{gridded<-}

\alias{initialize,PositionDataFrame-method}
\alias{run}
\alias{run<-}
\alias{run,PositionDataFrame-method}
\alias{run<-,PositionDataFrame-method}
\alias{runNames}
\alias{runNames<-}
\alias{runNames,PositionDataFrame-method}
\alias{runNames<-,PositionDataFrame-method}
\alias{coord,PositionDataFrame-method}
\alias{coord<-,PositionDataFrame-method}
\alias{coordinates,PositionDataFrame-method}
\alias{coordinates<-,PositionDataFrame-method}
\alias{coordLabels,PositionDataFrame-method}
\alias{coordLabels<-,PositionDataFrame-method}
\alias{coordnames,PositionDataFrame-method}
\alias{coordnames<-,PositionDataFrame-method}
\alias{gridded,PositionDataFrame-method}
\alias{gridded<-,PositionDataFrame-method}
\alias{resolution}
\alias{resolution<-}
\alias{resolution,PositionDataFrame-method}
\alias{resolution<-,PositionDataFrame-method}
\alias{dims,PositionDataFrame-method}
\alias{as.list,PositionDataFrame-method}
\alias{showNames,PositionDataFrame-method}
\alias{[,PositionDataFrame,ANY,ANY,ANY-method}
\alias{cbind,PositionDataFrame-method}
\alias{rbind,PositionDataFrame-method}
\alias{is3D,PositionDataFrame-method}

\title{PositionDataFrame: data frame with spatial position metadata}

\description{
	An \code{PositionDataFrame} is an extension of the \code{\linkS4class{XDataFrame}} class with special slot-columns for spatial coordinates. It is designed specifically with biological imaging experiments in mind, so it also has an additional slot-column for tracking the experimental run.
}

\usage{
PositionDataFrame(coord, run, \dots, row.names = NULL, check.names = TRUE)
}

\arguments{
	\item{coord}{A \code{data.frame}-like object containing columns which are spatial coordinates. This will be coerced to a \code{\linkS4class{DataFrame}}.}
	
	\item{run}{A \code{factor} with levels for each experimental run.}
	
	\item{\dots}{Named arguments that will become columns of the object.}
	
	\item{row.names}{Row names to be assigned to the object; no row names are assigned if this is \code{NULL}.}
	
	\item{check.names}{Should the column names be checked for syntactic validity?}
}

\section{Methods}{
	\describe{
		\item{\code{run(object)}, \code{run(object) <- value}:}{Get or set the experimental run slot-column.}

		\item{\code{runNames(object)}, \code{runNames(object) <- value}:}{Get or set the experimental run levels.}

		\item{\code{coord(object)}, \code{coord(object) <- value}:}{Get or set the spatial position slot-columns.}

		\item{\code{coordLabels(object)}, \code{coordLabels(object) <- value}:}{Get or set the names of the spatial position slot-columns.}

		\item{\code{gridded(object)}, \code{gridded(object) <- value}:}{Get or set whether the spatial positions are gridded or not. Typically, this should not be set manually.}

		\item{\code{resolution(object)}, \code{resolution(object) <- value}:}{Get or set the spatial resolution of the spatial positions. Typically, this should not be set manually.}

		\item{\code{dims(object)}:}{Get the gridded dimensions of the spatial positions (i.e., as if projected to an image raster).}

		\item{\code{is3D(object)}:}{Check if the data is 3D or not.}

		\item{\code{as.list(x, ..., slots = TRUE)}:}{Coerce the object to a \code{list}, where the slot-columns are included by default. Use \code{slots=FALSE} to exclude the slot-columns.}
	}
}

\details{
	\code{PositionDataFrame} is designed for spatial data, specifically for biological imaging data. It includes a slot-column for the experimental run. In most 2D imaging experiments, each distinct image is considered a distinct run. No additional assumptions are made about the spatial structure of the data, and non-gridded spatial coordinates are allowed.

	This class is intended to eventually replace the \code{\linkS4class{IAnnotatedDataFrame}} class, and implements similar concepts but with a more robust and modern infrastructure.
}

\author{Kylie A. Bemis}

\seealso{
	\code{\link{XDataFrame}}
}

\examples{
## Create an PositionDataFrame object
coord <- expand.grid(x=1:3, y=1:3)
values <- seq_len(nrow(coord))
pdata <- PositionDataFrame(coord=coord, values=values)

## Check the spatial properties
gridded(pdata)
resolution(pdata)
dims(pdata)
}

\keyword{classes}
