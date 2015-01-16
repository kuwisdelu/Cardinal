\name{OPLS-methods}
\docType{methods}
\alias{OPLS}
\alias{class:OPLS}
\alias{OPLS-class}
\alias{OPLS-methods}
\alias{OPLS,SImageSet,matrix-method}
\alias{OPLS,SImageSet,numeric-method}
\alias{OPLS,SImageSet,factor-method}
\alias{OPLS,SImageSet,character-method}
\alias{predict,OPLS-method}
\alias{summary,OPLS-method}
\alias{print.summary.OPLS}
\alias{plot.summary.OPLS}

\title{Orthogonal Partial Least Squares}
\description{
    Performs orthogonal partial least squares (also called orthogonal projection to latent structures or O-PLS) on an imaging dataset. This will also perform discriminant analysis (O-PLS-DA) if the response is a \code{factor}.
}
\usage{
\S4method{OPLS}{SImageSet,matrix}(x, y, ncomp = 20,
    method = "nipals",
    scale = FALSE,
    keep.Xnew = TRUE,
    iter.max = 100, \dots)

\S4method{OPLS}{SImageSet,numeric}(x, y, \dots)

\S4method{OPLS}{SImageSet,factor}(x, y, \dots)

\S4method{OPLS}{SImageSet,character}(x, y, \dots)

\S4method{predict}{OPLS}(object, newx, newy, keep.Xnew = TRUE, \dots)
}
\arguments{
    \item{x}{The imaging dataset on which to perform partial least squares.}
    \item{y}{The response variable, which can be a \code{matrix} or a \code{vector} for ordinary O-PLS, or a \code{factor} or a \code{character} for O-PLS-DA.}
    \item{ncomp}{The number of O-PLS components to calculate.}
    \item{method}{The function used to calculate the projection.}
    \item{scale}{Shoud the data be scaled first? This is passed to \code{scale}.}
    \item{keep.Xnew}{Should the new data matrix be kept after filtering out the orthogonal variation?}
    \item{iter.max}{The number of iterations to perform for the NIPALS algorithm.}
    \item{\dots}{Passed to the next OPLS method.}
    \item{object}{The result of a previous call to \code{\link[Cardinal]{OPLS}}.}
    \item{newx}{An imaging dataset for which to calculate their OPLS projection and predict a response from an already-calculated \code{\link[Cardinal]{OPLS}} object.}
    \item{newy}{Optionally, a new response from which residuals should be calculated.}
}
\value{
    An object of class \code{OPLS}, which is a \code{ResultSet}, where each component of the \code{resultData} slot contains at least the following components:

    \describe{
        \item{\code{Xnew}:}{A new data matrix that has been filtered of the orthogonal variation.}
        \item{\code{Xortho}:}{A new data matrix that consists of \emph{only} the orthogonal variation.}
        \item{\code{Oscores}:}{A matrix with the orthogonal component scores for the explanatary variable.}
        \item{\code{Oloadings}:}{A matrix objects with the orthogonal explanatory variable loadings.}
        \item{\code{Oweights}:}{A matrix with the orthgonal explanatory variable weights.}
        \item{\code{scores}:}{A matrix with the component scores for the explanatary variable.}
        \item{\code{loadings}:}{A matrix with the explanatory variable loadings.}
        \item{\code{weights}:}{A matrix with the explanatory variable weights.}
        \item{\code{Yscores}:}{A matrix objects with the component scores for the response variable.}
        \item{\code{Yweights}:}{A matrix objects with the response variable weights.}
        \item{\code{projection}:}{The projection matrix.}
        \item{\code{coefficients}:}{The matrix of the regression coefficients.}
        \item{\code{ncomp}:}{The number of O-PLS components.}
        \item{\code{method}:}{The method used to calculate the projection.}
        \item{\code{center}:}{The center of the dataset. Used for calculating O-PLS scores on new data.}
        \item{\code{scale}:}{The scaling factors for the dataset. Used for O-PLS scores on new data.}
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
    \code{\link[Cardinal]{PLS}},
    \code{\link[Cardinal]{PCA}},
    \code{\link[Cardinal]{spatialShrunkenCentroids}},
}
\examples{
sset <- generateImage(diag(4), range=c(200, 300), step=1)

y <- factor(diag(4))

opls <- OPLS(sset, y, ncomp=1:2)
}
\keyword{multivariate}
\keyword{classif}
