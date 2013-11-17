\name{OPLS-methods}
\docType{methods}
\alias{OPLS}
\alias{OPLS-methods}
\alias{OPLS,matrix-method}
\alias{OPLS,MSImageSet-method}
\title{Orthogonal partial least squares}
\description{
	Performs orthogonal partial least squares (also called orthogonal projection to latent structures).
}
\usage{
    \S4method{OPLS}{MSImageSet}(x, y, ncomp=1, acomp=1, standardize=FALSE, method="nipals", \dots)
    
    \S4method{OPLS}{matrix}(x, y, ncomp=1, acomp=1, isNxP=TRUE, standardize=FALSE, method="nipals", \dots)
}
\arguments{
    \item{x}{the data on which to perform orthogonal partial least squares.}
    \item{y}{the response variable, which can be a \code{numeric} vector, a \code{matrix}, or a \code{factor}.}
    \item{ncomp}{the number of orthogonal components to calculate.}
    \item{acomp}{the number of additional components to calculate.}
    \item{isNxP}{whether the data matrix is n x p, i.e., whether the each row corresponds to an observation.}
    \item{standardize}{whether the data should be standardized.}
    \item{method}{the method to use to calculate the components}
    \item{\dots}{additional arguments to pass to specified method.}
}
\value{
    An object of class \code{OPLS} with componenets:
    \item{Xnew}{a new data \code{matrix} that has been filtered of the orthogonal variation.}
    \item{Xortho}{a new data \code{matrix} that consists of only the orthogonal variation.}
    \item{Oscores}{a \code{matrix} with the orthogonal component scores for the explanatary variable.}
    \item{Oloadings}{a \code{matrix} objects with the orthogonal explanatory variable loadings.}
    \item{Oweights}{a \code{matrix} with the orthgonal explanatory variable weights.}
    \item{coefficients}{a \code{list} of the vectors of variable coefficients for the regression.}
    \item{scores}{a \code{matrix} with the component scores for the explanatary variable.}
    \item{loadings}{a \code{list} of \code{matrix} objects with the explanatory variable loadings.}
    \item{weights}{a \code{list} of \code{matrix} objects with the explanatory variable weights.}
    \item{ncomp}{the number of orthogonal components.}
    \item{acomp}{the number of additional components.}
    \item{isNxP}{whether the original data matrix was n x p or not.}
    \item{standardize}{whether the data matrix was standardized beforehand.}
    \item{method}{the method used.}
    \item{fitted}{a \code{list} of fitted values.}
    \item{classes}{a \code{list} of predicted classes if 'y' was a \code{factor}.}
    \item{residuals}{a \code{list} of residuals.}
    \item{misclassified}{a \code{list} of which observations were misclassified if 'y' was a \code{factor}.}
    \item{MSEP}{the mean squared error of prediction.}
    \item{metadata}{a \code{list} of metadata used for prediction}
}
\author{
	Kyle D. Bemis
}
\seealso{
    \code{\linkS4class{MSImageSet}},
    \code{\link[Cardinal]{PLS-methods}}
}
\examples{
    ## write examples to put here
}
\keyword{methods}

