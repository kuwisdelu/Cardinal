\name{MassDataFrame-class}
\docType{class}

\alias{class:MassDataFrame}
\alias{MassDataFrame}
\alias{MassDataFrame-class}

\alias{initialize,MassDataFrame-method}
\alias{mz,MassDataFrame-method}
\alias{mz<-,MassDataFrame-method}
\alias{resolution,MassDataFrame-method}
\alias{resolution<-,MassDataFrame-method}
\alias{as.list,MassDataFrame-method}
\alias{as.env,MassDataFrame-method}
\alias{[,MassDataFrame,ANY,ANY,ANY-method}
\alias{cbind,MassDataFrame-method}
\alias{rbind,MassDataFrame-method}

\title{MassDataFrame: data frame with mass-to-charge ratio metadata}

\description{
	An \code{MassDataFrame} is an extension of the \code{\linkS4class{XDataFrame}} class with a special slot-column for observed mass-to-charge ratios.
}

\usage{
MassDataFrame(mz, \dots, row.names = NULL, check.names = TRUE)
}

\arguments{
	\item{mz}{A \code{numeric} vector of mass-to-charge ratios.}
	
	\item{\dots}{Named arguments that will become columns of the object.}
	
	\item{row.names}{Row names to be assigned to the object; no row names are assigned if this is \code{NULL}.}
	
	\item{check.names}{Should the column names be checked for syntactic validity?}
}

\section{Methods}{
	\describe{
		\item{\code{mz(object)}, \code{mz(object) <- value}:}{Get or set the mass-to-charge ratio slot-column.}

		\item{\code{resolution(object)}, \code{resolution(object) <- value}:}{Get or set the estimated mass resolution of the mass-to-charge ratios. Typically, this should not be set manually.}

		\item{\code{as.list(x, ..., slots = TRUE)}:}{Coerce the object to a \code{list}, where the slot-columns are included by default. Use \code{slots=FALSE} to exclude the slot-columns.}
	}
}

\details{
	\code{MassDataFrame} is designed for mass spectrometry data. It includes a slot-column for the mass-to-charge ratio. It is intended to annotate either a single mass spectrum or an experiment where each mass spectrum share the same mass-to-charge ratios. The m/z values can be get and set by the \code{mz(object)} accessor, and are assumed to be unique and sorted in increasing order.
}

\author{Kylie A. Bemis}

\seealso{
	\code{\link{XDataFrame}}
}

\examples{
## Create an MassDataFrame object
mz <- seq(from=100, to=150, by=1.5)
values <- seq_len(length(mz))
fdata <- MassDataFrame(mz=mz, values=values)

## Check the mass-to-charge ratio properties
head(mz(fdata))
resolution(fdata)
}

\keyword{classes}
