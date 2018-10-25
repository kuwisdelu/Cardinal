\name{XDataFrame-class}
\docType{class}

\alias{class:XDataFrame}
\alias{XDataFrame}
\alias{XDataFrame-class}

\alias{names,XDataFrame-method}
\alias{length,XDataFrame-method}
\alias{lapply,XDataFrame-method}
\alias{as.env,XDataFrame-method}
\alias{as.env,list-method}
\alias{show,XDataFrame-method}
\alias{cbind,XDataFrame-method}
\alias{rbind,XDataFrame-method}

\alias{[[<-,XDataFrame,ANY,ANY-method}
\alias{$<-,XDataFrame-method}

\title{XDataFrame: DataFrame with eXtra metadata columns}

\description{
	An \code{XDataFrame} is an extension of the \code{\linkS4class{DataFrame}} class as defined in the 'S4Vectors' package, modified to support eXtra "slot-columns" that behave differently from other columns. It is intended to facilitate \code{data.frame}-like classes that require specialized column access and behavior. The specialized slot-columns are stored as distinct slots, unlike regular columns.
}

\usage{
XDataFrame(\dots)
}

\arguments{
	\item{\dots}{Arguments passed to the \code{DataFrame()}.}
}

\section{Methods}{
	\describe{
		\item{\code{names(object)}:}{Return the column names, not including any slot-columns.}

		\item{\code{length(object)}:}{Return the number of columns, not including any slot-columns.}

		\item{\code{lapply(X, FUN, ..., slots = FALSE)}:}{Returns a list of the same length as \code{X}, where each element is the result of applying \code{FUN} to the corresponding element of \code{X}. This version includes an additional argument for whether the slot-columns should be included or not. This method should be overwritten by subclasses to ensure correct behavior.}

		\item{\code{as.env(x, ...)}:}{Create an environment from \code{x} with a symbol for each column, including the slot-columns. This method should be overwritten by subclasses to ensure correct behavior.}
	}
}

\details{
	For the most part, \code{XDataFrame} behaves identically to \code{DataFrame}, with the exception of certain methods being overwritten to account for the additional eXtra "slot-columns" not counted among those returned by \code{ncol(x)}. These additional columns should typically have their own getter and setter methods.
}

\author{Kylie A. Bemis}

\seealso{
	\code{\link{DataFrame}},
	\code{\link{MassDataFrame}},
	\code{\link{PositionDataFrame}}
}

\examples{
## Create an XDataFrame object
XDataFrame(x=1:10, y=letters[1:10])
}

\keyword{classes}
