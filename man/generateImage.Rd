\name{generateImage}
\alias{generateImage}

\title{Generate a Simulated Image}

\description{
	Generates a simulated image of spectral signals.
}

\usage{
generateImage(data = factor(1),
    coord = expand.grid(
        x = 1:max(1, nrow(data)),
        y = 1:max(1, ncol(data))),
    peaks = length(levels(as.factor(data))),
    delta = 10,
    as = c("SImageSet", "MSImageSet"),
    \dots)
}

\arguments{
    \item{data}{Either a \code{factor} or an integer matrix. If a \code{factor} is used, the \code{coord} argument should be specified with \code{data} to indicate the arrangement of regions in the image. If a matrix is given, \code{coord} should not be specified. The image will automatically be generated with different regions corresponding to unique integers in the matrix.}
    
    \item{coord}{A \code{data.frame} with columns representing the spatial dimensions. Each row provides a spatial coordinate for the location of an element of \code{data} if \code{data} is a factor.}
    
    \item{peaks}{The number of peaks in the signal.}

    \item{delta}{The effect size of the difference between peaks differentiating different regions in the image (as specified by \code{data}).}

    \item{as}{Should the output object be an \code{\linkS4class{SImageSet}} or \code{\linkS4class{MSImageSet}}?}

    \item{\dots}{Additional arguments to pass to \code{\link[Cardinal]{generateSpectrum}}.}
}

\value{
	An \code{\linkS4class{SImageSet}} or an \code{\linkS4class{MSImageSet}}.
}

\author{
	Kyle D. Bemis
}

\seealso{
	\code{\link[Cardinal]{generateSpectrum}}
}

\examples{
data <- matrix(c(NA, NA, 1, 1, NA, NA, NA, NA, NA, NA, 1, 1, NA, NA, 
 NA, NA, NA, NA, NA, 0, 1, 1, NA, NA, NA, NA, NA, 1, 0, 0, 1, 
 1, NA, NA, NA, NA, NA, 0, 1, 1, 1, 1, NA, NA, NA, NA, 0, 1, 1, 
 1, 1, 1, NA, NA, NA, NA, 1, 1, 1, 1, 1, 1, 1, NA, NA, NA, 1, 
 1, NA, NA, NA, NA, NA, NA, 1, 1, NA, NA, NA, NA, NA), nrow=9, ncol=9)

set.seed(1)
x <- generateImage(data)

plot(x, pixel=1)
image(x, feature=1)

coord <- expand.grid(x=1:nrow(data), y=1:ncol(data))

data2 <- as.factor(data[is.finite(data)])
coord2 <- coord[is.finite(data),]

set.seed(1)
x2 <- generateImage(data=data, coord=coord, as="MSImageSet")

plot(x, pixel=1)
image(x2, feature=1)
}

\keyword{datagen}
