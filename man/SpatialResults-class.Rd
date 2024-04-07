\name{SpatialResults-class}
\docType{class}

\alias{class:SpatialResults}
\alias{SpatialResults}
\alias{SpatialResults-class}

\alias{show,SpatialResults-method}
\alias{modelData}
\alias{modelData<-}
\alias{modelData,SpatialResults-method}
\alias{modelData<-,SpatialResults-method}
\alias{fData,SpatialResults-method}
\alias{fData<-,SpatialResults,ANY-method}
\alias{featureData,SpatialResults-method}
\alias{featureData<-,SpatialResults,ANY-method}
\alias{featureNames,SpatialResults-method}
\alias{featureNames<-,SpatialResults-method}
\alias{pData,SpatialResults-method}
\alias{pData<-,SpatialResults,ANY-method}
\alias{pixelData,SpatialResults-method}
\alias{pixelData<-,SpatialResults-method}
\alias{pixelNames,SpatialResults-method}
\alias{pixelNames<-,SpatialResults-method}
\alias{coord,SpatialResults-method}
\alias{coord<-,SpatialResults-method}
\alias{coordNames,SpatialResults-method}
\alias{coordNames<-,SpatialResults-method}
\alias{run,SpatialResults-method}
\alias{run<-,SpatialResults-method}
\alias{runNames,SpatialResults-method}
\alias{runNames<-,SpatialResults-method}
\alias{nrun,SpatialResults-method}

\alias{names,SpatialResults-method}
\alias{length,SpatialResults-method}
\alias{$,SpatialResults-method}
\alias{[[,SpatialResults-method}

\alias{plot,SpatialResults,ANY-method}
\alias{image,SpatialResults-method}

\title{SpatialResults: Modeling results with spatial metadata}

\description{
   The \code{SpatialResults} class provides a container for modeling results with spatial metadata. Most modeling functions applied to a \code{\link{SpectralImagingExperiment}} will return a \code{SpatialResults}-derived model object.
}

\usage{
## Instance creation
SpatialResults(model, data,
    featureData = if (!missing(data)) fData(data) else NULL,
    pixelData = if (!missing(data)) pData(data) else NULL)

\S4method{plot}{SpatialResults,ANY}(x, y, \dots,
    select = NULL, groups = NULL,
    superpose = TRUE, reducedDims = FALSE)

\S4method{image}{SpatialResults}(x, y, \dots,
    select = NULL, subset = TRUE,
    superpose = TRUE)

## Additional methods documented below
}

\arguments{
    \item{model}{The model object.}

    \item{data}{An object (typically the original dataset) with \code{featureData} and \code{pixelData} components.}

    \item{featureData}{A \code{\linkS4class{DataFrame}} with feature metadata, with a row for each feature.}

    \item{pixelData}{A \code{\linkS4class{PositionDataFrame}} with pixel metadata, with a row for each spectrum.}

    \item{x, y}{The model object and results to plot. (Not typically called directly.)}

    \item{\dots}{Additional options passed to plotting methods.}

    \item{select}{Select elements of the results to plot. For example, this selects a subset of matrix columns or a subset of factor levels to plot.}

    \item{subset}{A logical vector indicating which pixels to include in the image.}

    \item{groups}{A vector coercible to a factor indicating which of the specified spectra should be plotted with the same color.}

    \item{superpose}{If multiple results are plotted, should they be superposed on top of each other, or plotted seperately?}

    \item{reducedDims}{Does this results component represent reduced dimensions (e.g., from PCA)?}
}

\section{Slots}{
    \describe{
        \item{\code{model}:}{The model.}

        \item{\code{featureData}:}{A \code{\linkS4class{DataFrame}} containing feature-level metadata (e.g., a color channel, a molecular analyte, or a mass-to-charge ratio).}

        \item{\code{pixelData}:}{A \code{\linkS4class{PositionDataFrame}} containing spatial metadata, including each observations's pixel coordinates and experimental run information.}
    }
}

\section{Methods}{
    \describe{
        \item{\code{modelData(object)}, \code{modelData(object) <- value}:}{Get or set the \code{model} slot.}

        \item{\code{featureData(object)}, \code{featureData(object) <- value}:}{Get or set the \code{featureData} slot.}

        \item{\code{fData(object)}, \code{fData(object) <- value}:}{Get or set the \code{featureData} slot.}

        \item{\code{featureNames(object)}, \code{featureNames(object) <- value}:}{Get or set the feature names (i.e., the row names of the \code{featureData} slot).}

        \item{\code{pixelData(object)}, \code{pixelData(object) <- value}:}{Get or set the \code{elementMetadata} slot.}

        \item{\code{pData(object)}, \code{pData(object) <- value}:}{Get or set the \code{elementMetadata} slot.}

        \item{\code{pixelNames(object)}, \code{pixelNames(object) <- value}:}{Get or set the pixel names (i.e., the row names of the \code{elementMetadata} slot).}

        \item{\code{coord(object)}, \code{coord(object) <- value}:}{Get or set the pixel coordinate columns in \code{pixelData}.}

        \item{\code{coordNames(object)}, \code{coordNames(object) <- value}:}{Get or set the names of the pixel coordinate columns in \code{pixelData}.}

        \item{\code{run(object)}, \code{run(object) <- value}:}{Get or set the experimental run column from \code{pixelData}.}

        \item{\code{runNames(object)}, \code{runNames(object) <- value}:}{Get or set the experimental run levels from \code{pixelData}.}

        \item{\code{nrun(object)}:}{Get the number of experimental runs.}
    }
}

\author{Kylie A. Bemis}

\seealso{
    \code{\linkS4class{ResultsList}}
}

\keyword{classes}
