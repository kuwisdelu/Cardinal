\name{topFeatures-methods}
\docType{methods}
\alias{topFeatures}
\alias{topFeatures-methods}

\alias{topFeatures,MeansTest-method}
\alias{topFeatures,SegmentationTest-method}
\alias{topFeatures,SpatialShrunkenCentroids2-method}

\alias{topFeatures,ResultSet-method}
\alias{topFeatures,PCA-method}
\alias{topFeatures,PLS-method}
\alias{topFeatures,OPLS-method}
\alias{topFeatures,SpatialKMeans-method}
\alias{topFeatures,SpatialShrunkenCentroids-method}
\alias{topFeatures,CrossValidated-method}

\title{Top-ranked features from imaging analysis results}
\description{
    Extract the top-ranked features from the results of imaging analysis, based on test statistics, p-values, or adjusted p-values. The result is sorted data frame that can be further manipulated for downstream postprocessing.
}
\usage{

#### Methods for Cardinal >= 2.x classes ####

\S4method{topFeatures}{SpatialShrunkenCentroids2}(object, n = 10, \dots, model = modelData(object))

\S4method{topFeatures}{MeansTest}(object, n = 10, \dots, p.adjust = "BH")

\S4method{topFeatures}{SegmentationTest}(object, n = 10, \dots, model = modelData(object), p.adjust = "BH")

#### Methods for Cardinal 1.x classes ####

\S4method{topFeatures}{ResultSet}(object, n = 6,
    model = pData(modelData(object)),
    type = c('+', '-', 'b'),
    sort.by = fvarLabels(object),
    filter = list(),
    \dots)

\S4method{topFeatures}{PCA}(object, n = 6,
    sort.by = "loadings",
    \dots)

\S4method{topFeatures}{PLS}(object, n = 6,
    sort.by = c("coefficients", "loadings", "weights"),
    \dots)

\S4method{topFeatures}{OPLS}(object, n = 6,
    sort.by = c("coefficients",
            "loadings", "Oloadings",
            "weights", "Oweights"),
    \dots)

\S4method{topFeatures}{SpatialKMeans}(object, n = 6,
    sort.by = c("betweenss", "withinss"),
    \dots)

\S4method{topFeatures}{SpatialShrunkenCentroids}(object, n = 6,
    sort.by = c("tstatistics", "p.values"),
    \dots)

\S4method{topFeatures}{CrossValidated}(object, \dots)
}
\arguments{
    \item{object}{The results of an imaging experiment analysis.}
    \item{n}{The number of top-ranked records to return.}
    \item{model}{If more than one model was fitted, results from which should be shown? Defaults to all models in the object This can name the models explicitly or specify a list of parameter values.}
    \item{p.adjust}{The \code{p.adjust} method used adjust p-values to account for multiple testing. Defaults to Benjamini \& Hochberg ("BH") to control the false discovery rate (FDR).}
    \item{\dots}{For newer classes, additional arguments to be passed to \code{filter}, to further filter the results.}
    \item{type}{How should the records be ranked? '+' shows greatest values first (decreasing order), '-' shows least values first (increasing order), and 'b' uses decreasing order based on absolute values.}
    \item{sort.by}{What variable should be used for sorting?}
    \item{filter}{A list of named variables with values to use to filter the results. For example, for testing or classification, this can be used to only show rankings for a particular condition.}
}
\value{
    A data frame with the top-ranked features.
}
\author{
    Kylie A. Bemis
}
\seealso{ 
    \code{\link{meansTest}},
    \code{\link{segmentationTest}},
    \code{\link{spatialShrunkenCentroids}}
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

topFeatures(clust1)

topFeatures(clust1, filter=list(classes=1))

topFeatures(clust1, filter=list(r=1, k=2, s=1))
}

\keyword{methods}
