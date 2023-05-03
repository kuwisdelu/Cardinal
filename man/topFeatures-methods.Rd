\name{topFeatures-methods}
\docType{methods}
\alias{topFeatures}
\alias{topFeatures-methods}

\alias{topFeatures,MeansTest-method}
\alias{topFeatures,SegmentationTest-method}

\alias{topFeatures,SpatialKMeans2-method}
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
    Extract the top-ranked features from the results of imaging analysis, based on post-hoc correlation, test statistics, p-values, or adjusted p-values. The result is sorted data frame that can be further manipulated for downstream postprocessing.
}
\usage{

#### Methods for Cardinal >= 2.x classes ####

\S4method{topFeatures}{SpatialShrunkenCentroids2}(object, \dots, n = 10, model = modelData(object))

\S4method{topFeatures}{SpatialKMeans2}(object, \dots, n = 10, model = modelData(object))

\S4method{topFeatures}{MeansTest}(object, \dots, n = 10, p.adjust = "BH")

\S4method{topFeatures}{SegmentationTest}(object, \dots, n = 10, model = modelData(object), p.adjust = "BH")

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
    \item{p.adjust}{The \code{p.adjust} method used adjust p-values to account for multiple testing. Defaults to Benjamini and Hochberg ("BH") to control the false discovery rate (FDR).}
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
setCardinalBPPARAM(SerialParam())

set.seed(1)
x <- simulateImage(preset=2, npeaks=10, dim=c(10,10),
    snoise=1, sdpeaks=1, representation="centroid")

y <- makeFactor(circle=pData(x)$circle, square=pData(x)$square)

res <- spatialShrunkenCentroids(x, y, r=1, s=c(0,3,6))

topFeatures(res, model=list(s=6))
}

\keyword{methods}
