\name{summary-methods}
\docType{methods}
\alias{summary}
\alias{summary-methods}
\alias{summary,MSImageSegmentation-method}
\alias{summary,CrossValidation-method}
\title{Summary}
\description{
	Summarizes the results of an analysis on an \code{\linkS4class{MSImageSet}}.
}
\usage{
	\S4method{summary}{MSImageSegmentation}(object,
	which=names(object[["classes"]]), n=20, reshape=TRUE,
	sort.by=c("tstatistic", "intensity", "p.value", "mz"),
	class.mask=NULL, \dots)

	\S4method{summary}{CrossValidation}(object, \dots)
}
\arguments{
    \item{object}{\code{MSImageSegmentation} to be summarized.}
    \item{which}{which segmentation(s) to use in an \code{MSImageSegmentation} if multiple segmentations were created from multiple parameters; this can either be an \code{integer}, a \code{character} giving the names of the \code{plotnames} attribute, or a list specifying the names of parameters and their values.}
    \item{n}{\code{integer}, giving how many ions to list; use \code{Inf} to automatically retrieve full mass lists for all results.}
    \item{reshape}{\code{logical}, giving whether the output \code{data.frame} for each result should be reshaped into wide format.}
    \item{sort.by}{\code{character}, giving whether to sort by masses, p-values, t-statistics, or their intensities.}
    \item{class.mask}{a vector of numbers indicating the classes to use in the sorting.}
    \item{\dots}{ignored.}
}
\value{
	For an \code{MSImageSegmentation}, a \code{list} of \code{data.frame} objects ranking ion masses based on their p-values, t-statistics, or intensities, from a \code{MSImageSegmentation}.

	For a \code{CrossValidation} object, a \code{data.frame} summarizing the error rates for each fold and the cross-validated error rates for each parameter set.
}
\author{
	Kyle D. Bemis
}
\seealso{
	\code{\linkS4class{MSImageSet}},
	\code{\link[Cardinal]{spatialClassify}},
	\code{\link[Cardinal]{spatialCluster}}
}
\examples{
	## write examples to add here
}
\keyword{methods}
