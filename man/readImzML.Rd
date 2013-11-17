\name{readImzML}
\alias{readImzML}
\title{Read imzML Data}
\description{
  Read data files stored in the imzML format.
}
\usage{
  readImzML(file, imzMLConverter=NULL)
}
\arguments{
    \item{file}{the file path for the '.imzML' file.}
    \item{imzMLConverter}{the file path to 'imzMLConverter.jar'.}
}
\details{
	\code{readImlML} requires the use of the free and open-source Java application 'imzMLConverter' which can be downloaded at \code{http://www.imzmlconverter.co.uk}.
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

