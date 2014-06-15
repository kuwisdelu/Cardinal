\name{PLS-methods}
\docType{methods}
\alias{PLS}

\alias{PLS-methods}
\alias{PLS,SImageSet,matrix-method}
\alias{PLS,SImageSet,numeric-method}
\alias{PLS,SImageSet,factor-method}
\alias{PLS,SImageSet,character-method}

\title{Partial Least Squares}
\description{
    Performs partial least squares (also called projection to latent structures or PLS) on an imaging dataset. This will also perform a variant of discriminant analysis (PLS-DA) if the response is a \code{factor}.
}
\usage{
    \S4method{PLS}{SImageSet,matrix}(x, y, ncomp = 20,
        method = "nipals", scale = FALSE,
        iter.max = 100, \dots)
    
    \S4method{PLS}{SImageSet,numeric}(x, y, \dots)

    \S4method{PLS}{SImageSet,factor}(x, y, \dots)

    \S4method{PLS}{SImageSet,character}(x, y, \dots)

    \S4method{predict}{PLS}(object, newx, newy, \dots)
}
\arguments{
    \item{x}{The imaging dataset on which to perform partial least squares.}
    \item{y}{The response variable, which can be a \code{matrix} or a \code{numeric} for ordinary PLS, or a \code{factor} or a \code{character} for PLS-DA.}
    \item{ncomp}{The number of OPLS components to calculate.}
    \item{method}{The function used to calculate the projection.}
    \item{scale}{Shoud the data be scaled first? This is passed to \code{scale}.}
    \item{iter.max}{The number of iterations to perform for the NIPALS algorithm.}
    \item{\dots}{Ignored.}
    \item{object}{The result of a previous call to \code{\link[Cardinal]{PLS}}.}
    \item{newx}{An imaging dataset for which to calculate their PLS projection and predict a response from an already-calculated \code{\link[Cardinal]{PLS}} object.}
    \item{newy}{Optionally, a new response from which residuals should be calcualted.}
}
\value{
    An object of class \code{PCS}, which is an \code{ResultSet}, where each component of the \code{resultData} slot contains at least the following components:

    \itemize{
        \item{Xnew}{A new data \code{matrix} that has been filtered of the orthogonal variation.}
        \item{Xortho}{A new data \code{matrix} that consists of \emph{only} the orthogonal variation.}
        \item{Oscores}{A \code{matrix} with the orthogonal component scores for the explanatary variable.}
        \item{Oloadings}{A \code{matrix} objects with the orthogonal explanatory variable loadings.}
        \item{Oweights}{A \code{matrix} with the orthgonal explanatory variable weights.}
        \item{scores}{A \code{matrix} with the component scores for the explanatary variable.}
        \item{loadings}{A \code{matrix} with the explanatory variable loadings.}
        \item{weights}{A \code{matrix} with the explanatory variable weights.}
        \item{Yscores}{A \code{matrix} objects with the component scores for the response variable.}
        \item{Yweights}{A \code{matrix} objects with the response variable weights.}
        \item{projection}{The projection \code{matrix}.}
        \item{coefficients}{The \code{matrix} of the regression coefficients.}
        \item{ncomp}{The number of PLS components.}
        \item{method}{The method used to calculate the projection.}
        \item{center}{The center of the dataset. Used for calculating PLS scores on new data.}
        \item{scale}{The scaling factors for the dataset. Used for PLS scores on new data.}
        \item{Ycenter}{The centers of the response variables. Used for predicting new observations.}
        \item{Yscale}{The scaling factors for the response variables. Used for predicting new observation.}
        \item{coefficients}{a \code{list} of the vectors of variable coefficients for the regression.}
        \item{fitted}{The fitted response.}
    }
}
\author{
    Kyle D. Bemis
}
\seealso{
    \code{\link[Cardinal]{OPLS}},
    \code{\link[Cardinal]{PCA}},
    \code{\link[Cardinal]{spatialShrunkenCentroids}},
}
\examples{
sset <- generateImage(diag(4), range=c(200, 300), step=1)

y <- factor(diag(4))

opls <- OPLS(sset, y, ncomp=1:2)
}
\keyword{methods}
