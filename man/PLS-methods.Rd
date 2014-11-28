\name{PLS-methods}
\docType{methods}
\alias{PLS}
\alias{class:PLS}
\alias{PLS-class}
\alias{PLS-methods}
\alias{PLS,SImageSet,matrix-method}
\alias{PLS,SImageSet,numeric-method}
\alias{PLS,SImageSet,factor-method}
\alias{PLS,SImageSet,character-method}
\alias{predict,PLS-method}
\alias{summary,PLS-method}
\alias{print.summary.PLS}
\alias{plot.summary.PLS}

\title{Partial Least Squares}
\description{
	Performs partial least squares (also called projection to latent structures or PLS) on an imaging dataset. This will also perform discriminant analysis (PLS-DA) if the response is a \code{factor}.
}
\usage{
    \S4method{PLS}{SImageSet,matrix}(x, y, ncomp = 20,
        method = "nipals",
        scale = FALSE,
        iter.max = 100, \dots)
    
    \S4method{PLS}{SImageSet,numeric}(x, y, \dots)

    \S4method{PLS}{SImageSet,factor}(x, y, \dots)

    \S4method{PLS}{SImageSet,character}(x, y, \dots)

    \S4method{predict}{PLS}(object, newx, newy, \dots)
}
\arguments{
    \item{x}{The imaging dataset on which to perform partial least squares.}
    \item{y}{The response variable, which can be a \code{matrix} or a \code{vector} for ordinary PLS, or a \code{factor} or a \code{character} for PLS-DA.}
    \item{ncomp}{The number of PLS components to calculate.}
    \item{method}{The function used to calculate the projection.}
    \item{scale}{Shoud the data be scaled first? This is passed to \code{scale}.}
    \item{iter.max}{The number of iterations to perform for the NIPALS algorithm.}
    \item{\dots}{Passed to the next PLS method.}
    \item{object}{The result of a previous call to \code{\link[Cardinal]{PLS}}.}
    \item{newx}{An imaging dataset for which to calculate their PLS projection and predict a response from an already-calculated \code{\link[Cardinal]{PLS}} object.}
    \item{newy}{Optionally, a new response from which residuals should be calcualted.}
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
}
\author{
	Kyle D. Bemis
}
\references{
    Trygg, J., & Wold, S. (2002). Orthogonal projections to latent structures (O-PLS). Journal of Chemometrics, 16(3), 119-128. doi:10.1002/cem.695
}
\seealso{
    \code{\link[Cardinal]{OPLS}},
    \code{\link[Cardinal]{PCA}},
    \code{\link[Cardinal]{spatialShrunkenCentroids}},
}
\examples{
sset <- generateImage(diag(4), range=c(200, 300), step=1)

y <- factor(diag(4))

pls <- PLS(sset, y, ncomp=1:2)
}
\keyword{multivariate}
\keyword{classif}
