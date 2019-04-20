\name{PLS-methods}
\docType{methods}

\alias{PLS}
\alias{class:PLS}
\alias{PLS-class}
\alias{PLS-methods}

\alias{OPLS}
\alias{class:OPLS}
\alias{OPLS-class}
\alias{OPLS-methods}

\alias{PLS,SparseImagingExperiment,ANY-method}
\alias{OPLS,SparseImagingExperiment,ANY-method}
\alias{predict,PLS2-method}
\alias{fitted,PLS2-method}

\alias{PLS,SImageSet,matrix-method}
\alias{PLS,SImageSet,ANY-method}
\alias{predict,PLS-method}
\alias{summary,PLS-method}
\alias{print.summary.PLS}
\alias{plot.summary.PLS}

\alias{OPLS,SImageSet,matrix-method}
\alias{OPLS,SImageSet,ANY-method}
\alias{predict,OPLS-method}
\alias{summary,OPLS-method}
\alias{print.summary.OPLS}
\alias{plot.summary.OPLS}

\title{Partial least squares}
\description{
	Performs partial least squares (also called projection to latent structures or PLS) on an imaging dataset. This will also perform discriminant analysis (PLS-DA) if the response is a \code{factor}. Orthogonal partial least squares options (O-PLS and O-PLS-DA) are also available.
}
\usage{
\S4method{PLS}{SparseImagingExperiment,ANY}(x, y, ncomp = 3, method = c("pls", "opls"),
        center = TRUE, scale = FALSE,
        iter.max = 100, \dots)

\S4method{OPLS}{SparseImagingExperiment,ANY}(x, y, ncomp = 3, \dots)

\S4method{predict}{PLS2}(object, newx, newy, ncomp, \dots)

\S4method{fitted}{PLS2}(object, \dots)

\S4method{PLS}{SImageSet,matrix}(x, y, ncomp = 3,
    method = "nipals",
    center = TRUE,
    scale = FALSE,
    iter.max = 100, \dots)

\S4method{PLS}{SImageSet,ANY}(x, y, \dots)

\S4method{OPLS}{SImageSet,matrix}(x, y, ncomp = 3,
    method = "nipals",
    center = TRUE,
    scale = FALSE,
    keep.Xnew = TRUE,
    iter.max = 100, \dots)

\S4method{OPLS}{SImageSet,ANY}(x, y, \dots)

\S4method{predict}{PLS}(object, newx, newy, \dots)

\S4method{predict}{OPLS}(object, newx, newy, keep.Xnew = TRUE, \dots)
}
\arguments{
    \item{x}{The imaging dataset on which to perform partial least squares.}
    \item{y}{The response variable, which can be a \code{matrix} or a \code{vector} for ordinary PLS, or a \code{factor} or a \code{character} for PLS-DA.}
    \item{ncomp}{The number of PLS components to calculate.}
    \item{method}{The function used to calculate the projection.}
    \item{center}{Should the data be centered first? This is passed to \code{scale}.}
    \item{scale}{Shoud the data be scaled first? This is passed to \code{scale}.}
    \item{iter.max}{The number of iterations to perform for the NIPALS algorithm.}
    \item{\dots}{Passed to the next PLS method.}
    \item{object}{The result of a previous call to \code{\link{PLS}}.}
    \item{newx}{An imaging dataset for which to calculate their PLS projection and predict a response from an already-calculated \code{\link{PLS}} object.}
    \item{newy}{Optionally, a new response from which residuals should be calcualted.}
    \item{keep.Xnew}{Should the new data matrix be kept after filtering out the orthogonal variation?}
}
\value{
    An object of class \code{PLS}, which is a \code{ResultSet}, where each component of the \code{resultData} slot contains at least the following components:

    \describe{
        \item{\code{scores}:}{A matrix with the component scores for the explanatary variable.}
        \item{\code{loadings}:}{A matrix with the explanatory variable loadings.}
        \item{\code{weights}:}{A matrix with the explanatory variable weights.}
        \item{\code{Yscores}:}{A matrix objects with the component scores for the response variable.}
        \item{\code{Yweights}:}{A matrix objects with the response variable weights.}
        \item{\code{projection}:}{The projection matrix.}
        \item{\code{coefficients}:}{The matrix of the regression coefficients.}
        \item{\code{ncomp}:}{The number of PLS components.}
        \item{\code{method}:}{The method used to calculate the projection.}
        \item{\code{center}:}{The center of the dataset. Used for calculating PLS scores on new data.}
        \item{\code{scale}:}{The scaling factors for the dataset. Used for PLS scores on new data.}
        \item{\code{Ycenter}:}{The centers of the response variables. Used for predicting new observations.}
        \item{\code{Yscale}:}{The scaling factors for the response variables. Used for predicting new observation.}
        \item{\code{fitted}:}{The fitted response.}
    }

    The following components may also be available for \code{OPLS}.

    \describe{
        \item{\code{Xnew}:}{A new data matrix that has been filtered of the orthogonal variation.}
        \item{\code{Xortho}:}{A new data matrix that consists of \emph{only} the orthogonal variation.}
        \item{\code{Oscores}:}{A matrix with the orthogonal component scores for the explanatary variable.}
        \item{\code{Oloadings}:}{A matrix objects with the orthogonal explanatory variable loadings.}
        \item{\code{Oweights}:}{A matrix with the orthgonal explanatory variable weights.}
    }
}
\author{
	Kylie A. Bemis
}
\references{
    Trygg, J., & Wold, S. (2002). Orthogonal projections to latent structures (O-PLS). Journal of Chemometrics, 16(3), 119-128. doi:10.1002/cem.695
}
\seealso{
    \code{\link{PCA}},
    \code{\link{spatialShrunkenCentroids}},
}
\examples{
sset <- generateImage(diag(4), range=c(200, 300), step=1)

y <- factor(diag(4))

pls <- PLS(sset, y, ncomp=1:2)

opls <- OPLS(sset, y, ncomp=1:2)
}
\keyword{multivariate}
\keyword{classif}
