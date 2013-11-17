\name{logLik-methods}
\docType{methods}
\alias{logLik,MSImageSegmentation-method}
\title{Extract Log-Likelihood}
\description{
	Log-likelihood for mass spectrometry imaging models.
}
\usage{
	\S4method{logLik}{MSImageSegmentation}(object, \dots)
}
\arguments{
	\item{object}{an \code{MSImageSegmentation} output from \code{\link[Cardinal]{spatialClassify}} or \code{\link[Cardinal]{spatialCluster}} or their sparse versions.}
	\item{\dots}{ignored.}
}
\value{
	A \code{logLik} object giving the log-likelihood(s) of the segmentation(s).
}
\seealso{
    \code{\linkS4class{MSImageSet}},
    \code{\link[Cardinal]{spatialClassify-methods}},
    \code{\link[Cardinal]{spatialCluster-methods}}
}
\keyword{methods}

