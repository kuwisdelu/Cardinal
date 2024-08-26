\name{XDataFrame-class}
\docType{class}

\alias{class:XDataFrame}
\alias{XDataFrame}
\alias{XDataFrame-class}

\alias{class:XDFrame}
\alias{XDFrame}
\alias{XDFrame-class}

\alias{classNameForDisplay,XDFrame-method}

\alias{show,XDataFrame-method}
\alias{keys}
\alias{keys<-}
\alias{keys,XDataFrame-method}
\alias{keys<-,XDataFrame-method}
\alias{names<-,XDataFrame-method}
\alias{cbind,XDataFrame-method}
\alias{rbind,XDataFrame-method}
\alias{coerce,DataFrame,XDataFrame-method}

\alias{[,XDataFrame,ANY,ANY,ANY-method}
\alias{[<-,XDataFrame,ANY,ANY,ANY-method}

\alias{[[<-,XDataFrame-method}
\alias{$<-,XDataFrame-method}

\title{XDataFrame: Extended data frame with key columns}

\description{
	The \code{XDataFrame} extends the \code{\linkS4class{DataFrame}} class from the \code{S4Vectors} package with support for columns (or sets of columns) designated as keys.
}

\usage{
XDataFrame(\dots, keys = list())
}

\arguments{
	\item{\dots}{Arguments passed to the \code{DataFrame()}.}

	\item{keys}{A named list of character vectors giving the names of key columns. The names of the list become the names of the keys (which may be different from the columns). The character vectors specify the names of columns that compose that key.}
}

\details{
	For the most part, \code{XDataFrame} behaves identically to \code{DataFrame}, and key columns can be get or set as usual.

	The \code{XDataFrame} class is primarily intended as a way to enforce additional requirements or constraints on specific sets of columns in a structured way. It provides an abstracted way of manipulating sets of columns that are expected to follow certain rules. The keys remain consistent and accessible even if the columns of the data frame are renamed.

	The base class currently has only minimal requirements for keys (i.e., that they are valid columns in the data frame). Additionally, keys are checked for compatibility when combining data frames. Uniqueness is \emph{not} checked.

	Subclasses can enforce additional constraints on key columns. For example, the \code{\linkS4class{PositionDataFrame}} and \code{\linkS4class{MassDataFrame}} classes.
}

\section{Methods}{
	\describe{
		\item{\code{keys(object, i = NULL, ..., drop = TRUE)}, \code{keys(object, i = NULL, ...) <- value}:}{Get or set the key columns. By default, this gets or sets the \code{keys} slot. Provide \code{i} to get or set specific keys.}

		\item{\code{dropkeys(object, ...)}:}{Return a \code{\linkS4class{DataFrame}} copy of the object without the key columns.}
	}
}

\author{Kylie A. Bemis}

\seealso{
	\code{\link{DataFrame}},
	\code{\link{MassDataFrame}},
	\code{\link{PositionDataFrame}}
}

\examples{
## Create an XDataFrame object
XDataFrame(id=1:10, letter=LETTERS[1:10], keys=list(index="id"))
}

\keyword{classes}
