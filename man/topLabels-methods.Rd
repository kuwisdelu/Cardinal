\name{topLabels-methods}
\docType{methods}
\alias{topLabels}
\alias{topLabels-methods}
\alias{topLabels,ResultSet-method}
\alias{topLabels,PCA-method}
\alias{topLabels,PLS-method}
\alias{topLabels,OPLS-method}
\alias{topLabels,SpatialKMeans-method}
\alias{topLabels,SpatialShrunkenCentroids-method}
\alias{topLabels,CrossValidated-method}

\title{Retrieve Top-Ranked Labels from Analysis Results}
\description{
    The generic function is a convenience method for retrieving top-ranked labels from the results of imaging experiment analyses. For mass spectrometry-based imaging experiments, this can be used for identifying important masses from an analysis.
}
\usage{
\S4method{topLabels}{ResultSet}(object, n = 6,
    model = pData(modelData(object)),
    type = c('+', '-', 'b'),
    sort.by = fvarLabels(object),
    filter = list(),
    \dots)

\S4method{topLabels}{PCA}(object, n = 6,
    sort.by = "loadings",
    \dots)

\S4method{topLabels}{PLS}(object, n = 6,
    sort.by = c("coefficients", "loadings", "weights"),
    \dots)

\S4method{topLabels}{OPLS}(object, n = 6,
    sort.by = c("coefficients",
            "loadings", "Oloadings",
            "weights", "Oweights"),
    \dots)

\S4method{topLabels}{SpatialKMeans}(object, n = 6,
    sort.by = c("betweenss", "withinss"),
    \dots)

\S4method{topLabels}{SpatialShrunkenCentroids}(object, n = 6,
    sort.by = c("tstatistics", "p.values"),
    \dots)

\S4method{topLabels}{CrossValidated}(object, \dots)
}
\arguments{
    \item{object}{A \code{\linkS4class{ResultSet}} derived object.}
    \item{n}{The number of top-ranked records to return.}
    \item{model}{If more than one model was fitted, results from which should be shown? Defaults to all models in the \code{ResultSet}. This can name the models explicitly or specify a list of parameter values.}
    \item{type}{How should the records be ranked? '+' shows greatest values first (decreasing order), '-' shows least values first (increasing order), and 'b' uses decreasing order based on absolute values.}
    \item{sort.by}{What variable should be used for sorting?}
    \item{filter}{A list of named variables with values to use to filter the results. For example, for testing or classification, this can be used to only show rankings for a particular condition.}
    \item{\dots}{Passed to the 'head' function when sorting the final list of results.}
}
\value{
    A \code{data.frame} with the top-ranked labels from the analysis.
}
\author{
    Kyle D. Bemis
}
\seealso{
    \code{\linkS4class{ResultSet}},
    \code{\link[Cardinal]{PCA}},
    \code{\link[Cardinal]{PLS}},
    \code{\link[Cardinal]{OPLS}},
    \code{\link[Cardinal]{spatialKMeans}},
    \code{\link[Cardinal]{spatialShrunkenCentroids}}
}
\examples{
set.seed(1)
data <- matrix(c(NA, NA, 1, 1, NA, NA, NA, NA, NA, NA, 1, 1, NA, NA, 
    NA, NA, NA, NA, NA, 0, 1, 1, NA, NA, NA, NA, NA, 1, 0, 0, 1, 
    1, NA, NA, NA, NA, NA, 0, 1, 1, 1, 1, NA, NA, NA, NA, 0, 1, 1, 
    1, 1, 1, NA, NA, NA, NA, 1, 1, 1, 1, 1, 1, 1, NA, NA, NA, 1, 
    1, NA, NA, NA, NA, NA, NA, 1, 1, NA, NA, NA, NA, NA), nrow=9, ncol=9)

msset <- generateImage(data, range=c(200, 300), step=1, as="MSImageSet")

clust1 <- spatialShrunkenCentroids(msset, r=c(1,2), k=c(2,3), s=c(0,1), method="gaussian")

topLabels(clust1)

topLabels(clust1, filter=list(classes=1))

topLabels(clust1, filter=list(r=1, k=2, s=1))
}

\keyword{methods}
