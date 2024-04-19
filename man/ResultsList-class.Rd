\name{ResultsList-class}
\docType{class}

\alias{class:ResultsList}
\alias{ResultsList}
\alias{ResultsList-class}

\alias{show,ResultsList-method}
\alias{fitted,ResultsList-method}
\alias{predict,ResultsList-method}
\alias{topFeatures,ResultsList-method}
\alias{plot,ResultsList,ANY-method}
\alias{plot,ResultsList,missing-method}
\alias{image,ResultsList-method}

\title{ResultsList: List of modeling results}

\description{
   The \code{ResultsList} class provides a container for modeling results with spatial metadata.
}

\usage{
## Instance creation
ResultsList(\dots, mcols = NULL)

## Additional methods documented below
}

\arguments{
    \item{\dots}{The modeling results.}

    \item{mcols}{The metadata columns.}
}

\section{Methods}{
    All methods for \code{\linkS4class{SimpleList}} also work on \code{ResultsList} objects. Additional methods are documented below:

    \describe{
        \item{\code{fitted(object, ...)}:}{Extract fitted values from each modeling results object in the list.}

        \item{\code{predict(object, ...)}:}{Predict on each modeling results object in the list.}

        \item{\code{topFeatures(object, ...)}:}{Rank top features for each modeling results object in the list.}

        \item{\code{plot(x, i = 1L, ...)}:}{Plot the \code{i}th modeling results.}

        \item{\code{image(x, i = 1L, ...)}:}{Display images for the \code{i}th modeling results.}
    }
}

\author{Kylie A. Bemis}

\seealso{
    \code{\linkS4class{SpatialResults}}
}

\keyword{classes}
