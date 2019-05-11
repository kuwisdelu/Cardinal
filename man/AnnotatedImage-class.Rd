\name{AnnotatedImage-class}
\docType{class}

\alias{class:AnnotatedImage}
\alias{AnnotatedImage}
\alias{AnnotatedImage-class}

\alias{length,AnnotatedImage-method}
\alias{coord,AnnotatedImage-method}
\alias{coord<-,AnnotatedImage-method}
\alias{coordinates,AnnotatedImage-method}
\alias{coordinates<-,AnnotatedImage-method}
\alias{resolution,AnnotatedImage-method}
\alias{resolution<-,AnnotatedImage-method}
\alias{height}
\alias{height<-}
\alias{height,AnnotatedImage-method}
\alias{height<-,AnnotatedImage-method}
\alias{width,AnnotatedImage-method}
\alias{width<-,AnnotatedImage-method}

\title{AnnotatedImage: Optical images with annotations}

\description{
	\code{AnnotatedImage} extends the \code{Image} class from the \code{EBImage} package with metadata columns and plotting in arbitrary coordinate systems. This facilitates annotations such as region-of-interest labeling, and plotting with axes reflective of real-world coordinates.
}

\usage{
AnnotatedImage(data = array(0, dim=c(1,1)), dim, colormode, \dots)
}

\arguments{
	\item{data}{A vector or array with pixel intensities of an image.}
	
	\item{dim}{The final dimensions of the image.}

	\item{colormode}{Either 'Grayscale' or 'Color'.}
	
	\item{\dots}{Additional arguments passed to the constructor. May be used to set the \code{offset} or \code{resolution} slots.}
}

\section{Methods}{
	\describe{
		\item{\code{mcols(x)}, \code{mcols(x) <- value}:}{Get or set the metadata columns.}

		\item{\code{coord(object)}, \code{coord(object) <- value}:}{Get or set the absolute offset of the top-left corner of the image.}

		\item{\code{resolution(object)}, \code{resolution(object) <- value}:}{Get or set the pixel resolution of the image. This corresponds to the number of pixels per unit step on the x/y axes when plotted.}

		\item{\code{height(x)}, \code{height(x) <- value}:}{Get or set the height of the image (as plotted).}

		\item{\code{width(x)}, \code{width(x) <- value}:}{Get or set the width of the image (as plotted).}
	}
}

\details{
	\code{AnnotatedImage} is extends \code{\link[EBImage]{Image}}, so all methods defined on that class also work here.

	Metadata columns with annotations can be set via \code{mcols()}. The object's \code{length} is defined as the product of the first two dimensions. It is assumed any additional frames represent different measurement channels on the same spatial locations.

	\code{AnnotatedImage} also facilitates integration with other imaging data such as a \code{SparseImagingExperiment} or \code{MSImagingExperiment} by allowing plotting of the image on arbitrary coordinate systems. This is controlled via an \code{offset} slot designating the absolute position of the top-left corner of the image in the current plotting coordinates. The height and width of the image (as plotted) are then controlled by its \code{resolution}. This is most easily changed by setting the \code{height()} and \code{width()} of the object.
}

\author{Kylie A. Bemis}

\seealso{
	\code{\link[EBImage]{Image}}
}

\keyword{classes}
