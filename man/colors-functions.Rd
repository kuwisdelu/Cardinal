\name{intensity.colors}
\alias{intensity.colors}
\alias{risk.colors}
\alias{gradient.colors}
\alias{alpha.colors}
\title{Color Palettes for Imaging}

\description{
	Create a vector of \code{n} continuous colors.
}

\usage{
intensity.colors(n, alpha=1)
risk.colors(n, alpha=1)
gradient.colors(n, start="white", end="black", alpha=1)
alpha.colors(n, col="red", alpha.power=2, alpha=(seq_len(n)/n)^alpha.power)
}

\arguments{
    \item{n}{the number of colors}
    \item{alpha}{a vector of alpha values between 0 and 1}
    \item{start}{the starting color value}
    \item{end}{the ending color value}
    \item{col}{the color(s) to expand with transparency}
    \item{alpha.power}{how the alpha should ramp as it increases}
}

\value{
	A pallete of colors.
}
\author{
	Kyle D. Bemis
}
\examples{
col <- intensity.colors(100^2)
if ( interactive() ) {
	image(matrix(1:(100^2), nrow=100), col=col)
}
}

\keyword{color}

