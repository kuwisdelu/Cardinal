\name{createMSImageSet}
\alias{createMSImageSet}
\title{Create an mass spectrometry imaging dataset}
\description{
	Creates an \code{\linkS4class{MSImageSet}} object.
}
\usage{
	createMSImageSet(spectra, mz, coord)
}
\arguments{
    \item{spectra}{\code{matrix}, where each column contains the mass spectrum for a pixel.}
    \item{mz}{\code{numeric}, giving the measured mass-to-charge ratios.}
    \item{coord}{\code{data.frame}, where each row gives the coordinates of a pixel.}
}
\value{
	A \code{\linkS4class{MSImageSet}} object.
}
\author{
	Kyle D. Bemis
}
\seealso{
	\code{\linkS4class{MSImageSet}}
}
\examples{
	## write examples to add here
}
\keyword{methods}

