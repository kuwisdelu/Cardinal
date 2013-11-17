\name{readAnalyze7.5}
\alias{readAnalyze7.5}
\title{Read Analyze 7.5 Data}
\description{
  Read data files stored in the Analyze 7.5 format.
}
\usage{
  readAnalyze7.5(name, folder=".")
}
\arguments{
    \item{name}{the common file name for the '.hdr', '.t2m', and '.img' files.}
    \item{folder}{the file path to the folder containing the image data files.}
}
\value{
  A \code{\linkS4class{MSImageSet}} object.
}
\author{
  Dario Amodei, Kyle D. Bemis
}
\seealso{
  \code{\linkS4class{MSImageSet}}
}
\examples{
  ## write examples to add here
}
\keyword{methods}

