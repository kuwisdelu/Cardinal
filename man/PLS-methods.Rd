\name{PLS-methods}
\docType{methods}
\alias{PLS}
\alias{PLS-methods}
\alias{PLS,matrix-method}
\alias{PLS,MSImageSet-method}
\title{Partial least squares}
\description{
	Performs partial least squares (also called projection to latent structures).
}
\usage{
    \S4method{PLS}{MSImageSet}(x, y, ncomp=2, standardize=FALSE, method="nipals", \dots)
    
    \S4method{PLS}{matrix}(x, y, ncomp=2, isNxP=TRUE, standardize=FALSE, method="nipals", \dots)
}
\arguments{
    \item{x}{the data on which to perform partial least squares.}
    \item{y}{the response variable, which can be a \code{numeric} vector, a \code{matrix}, or a \code{factor}.}
    \item{ncomp}{the number of components to calculate.}
    \item{isNxP}{whether the data matrix is n x p, i.e., whether the each row corresponds to an observation.}
    \item{standardize}{whether the data should be standardized.}
    \item{method}{the method to use to calculate the components}
    \item{\dots}{additional arguments to pass to specified method.}
}
\value{
    An object of class \code{PLS} with componenets:
    \item{scores}{a \code{matrix} with the component scores for the explanatary variable.}
    \item{loadings}{a \code{matrix} with the explanatory variable loadings.}
    \item{weights}{a \code{matrix} with the explanatory variable weights.}
    \item{Yscores}{a \code{matrix} objects with the component scores for the response variable.}
    \item{Yweights}{a \code{matrix} objects with the response variable weights.}
    \item{projection}{the projection \code{matrix}.}
    \item{coefficients}{a \code{list} of the vectors of variable coefficients for the regression.}
    \item{ncomp}{the number of components.}
    \item{coefficients}{a \code{list} of the vectors of variable coefficients for the regression.}
    \item{method}{the method.}
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
    \code{\link[Cardinal]{OPLS-methods}}
}
\examples{
    ## write examples to put here
}
\keyword{methods}

