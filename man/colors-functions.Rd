\name{intensity.colors}
\alias{intensity.colors}
\alias{risk.colors}
\alias{gradient.colors}
\alias{alpha.colors}
\title{Color Palettes for MS Imaging}
\description{
  Create a vector of \code{n} continuous colors.
}
\usage{
  intensity.colors(n, alpha=1)
  risk.colors(n, alpha=1)
  gradient.colors(n, start="white", end="black", alpha=1)
  alpha.colors(n, col="red", ramp=c("linear", "power"), range=c(0,1))
}
\arguments{
    \item{n}{the number of colors}
    \item{alpha}{a value between 0 and 1 to set the transparency level}
    \item{start}{the starting color value}
    \item{end}{the ending color value}
    \item{col}{the color(s) to expand with transparency}
    \item{ramp}{how opacity should be ramped}
    \item{range}{the range of the opacity}
}
\value{
  A pallete of colors.
}
\author{
  Kyle D. Bemis
}
\examples{
  ## write examples to add here
}
\keyword{methods}

