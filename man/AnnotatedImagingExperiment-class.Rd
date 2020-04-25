\name{AnnotatedImagingExperiment-class}
\docType{class}

\alias{class:AnnotatedImagingExperiment}
\alias{AnnotatedImagingExperiment}
\alias{AnnotatedImagingExperiment-class}

\alias{class:AnnotatedImageList}
\alias{AnnotatedImageList}
\alias{AnnotatedImageList-class}

\alias{[,AnnotatedImageList,ANY,ANY,ANY-method}
\alias{cbind,AnnotatedImageList-method}
\alias{rbind,AnnotatedImageList-method}
\alias{combine,AnnotatedImageList,ANY-method}

\alias{resolution,AnnotatedImagingExperiment-method}
\alias{coord,AnnotatedImagingExperiment-method}
\alias{coordinates,AnnotatedImagingExperiment-method}
\alias{width,AnnotatedImagingExperiment-method}
\alias{height,AnnotatedImagingExperiment-method}

\alias{dim,AnnotatedImageList-method}
\alias{dims,AnnotatedImageList-method}
\alias{dims,AnnotatedImagingExperiment-method}

\alias{show,AnnotatedImageList-method}
\alias{show,AnnotatedImagingExperiment-method}

\title{AnnotatedImagingExperiment: Mass spectrometry imaging experiments}

\description{
   The \code{AnnotatedImagingExperiment} class is designed for mass spectrometry imaging experimental data and metadata. It is designed to contain full MSI experiments, including multiple runs and replicates, potentially across multiple files. Both 2D and 3D imaging experiments are supported, as well as any type of experimental metadata such as diagnosis, subject, time point, etc.
}

\usage{
## AnnotatedImage list
AnnotatedImageList(\dots)

## Instance creation
AnnotatedImagingExperiment(
    imageData = AnnotatedImageList(),
    featureData = DataFrame(),
    phenoData = DataFrame(),
    metadata = list())

## Additional methods documented below
}

\arguments{
    \item{\dots}{Either \code{Image} or \code{AnnotatedImage} objects used to create the \code{AnnotatedImageList}.}

    \item{imageData}{An \code{Image}, an \code{AnnotatedImage}, or an \code{\linkS4class{AnnotatedImageList}}.}

    \item{featureData}{A \code{\linkS4class{DataFrame}} with feature metadata, with a row for each channel/frame.}

    \item{phenoData}{A \code{\linkS4class{DataFrame}} with phenotype metadata, with a row for each sample.}

    \item{metadata}{A \code{list} with experimental-level metadata.}
}

\details{
    The \code{AnnotatedImagingExperiment} class is designed as a replacement for the \code{\linkS4class{MSImageSet}} class, using a simplified, robust implementation that should be more future-proof and enable better support for large, high-resolution experiments, multimodal experiments, and experiments with specialized needs such as non-gridded pixel coordinates.

   Subclasses \code{\linkS4class{MSContinuousImagingExperiment}} and \code{\linkS4class{MSProcessedImagingExperiment}} exist to allow downstream methods to make assumptions about the underlying data storage (dense matrices for 'continous' format and sparse matrices for 'processed' format), which can sometimes allow more efficient computations.
}

\section{Slots}{
    \describe{
        \item{\code{imageData}:}{An object inheriting from \code{\linkS4class{AnnotatedImageList}}, storing one or more \code{AnnotatedImage} elements.}

        \item{\code{featureData}:}{Contains feature information in a \code{\linkS4class{DataFrame}}. Each row includes the metadata associated with frame/channel of the images.}

        \item{\code{elementMetadata}:}{Contains phenotype information in a \code{\linkS4class{DataFrame}}. Each row includes the metadata for a single observation (e.g., a sample).}

        \item{\code{metadata}:}{A \code{list} containing experiment-level metadata.}
    }
}

\section{Methods}{
    All methods for \code{\linkS4class{ImagingExperiment}} also work on \code{AnnotatedImagingExperiment} objects. Additional methods are documented below:

    \describe{
        \item{\code{coord(object)}:}{Get the absolute offsets of the top-left corner of the images.}

        \item{\code{resolution(object)}:}{Get the pixel resolutions of the images. This corresponds to the number of pixels per unit step on the x/y axes when plotted.}

        \item{\code{height(x)}:}{Get the heights of the images (as plotted).}

        \item{\code{width(x)}:}{Get the widths of the images (as plotted).}
    }
}

\author{Kylie A. Bemis}

\seealso{
    \code{\linkS4class{ImagingExperiment}},
    \code{\linkS4class{AnnotatedImage}}
}

\examples{
\dontrun{
x <- readImage(system.file('images', 'nuclei.tif', package='EBImage'))

y <- AnnotatedImagingExperiment(x)

print(y)
}
}

\keyword{classes}
