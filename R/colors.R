
## Colors for image intensities
intensity.colors <- function(n, alpha=1) {
	col2 <- rainbow(3*n, alpha=alpha)[(2*n):1]
	f <- colorRamp(c("black", rainbow(3*n)[2*n]))
	alpha <- col2rgb(col2, alpha=TRUE)[[4]]
	col1 <- sapply(seq(from=0, to=1, length.out=n), function(i) do.call(rgb,
		c(as.list(f(i)), maxColorValue=255, alpha=alpha)))
	cols <- c(col1, col2)
	cols[seq(from=1, to=3*n, by=3)]
}

## Colors for image intensities (alternative)
intensity.colors2 <- function(n, alpha=1) {
	cols <- rainbow(3*n, alpha=alpha)[(2*n):1]
	cols[round(seq(from=1, to=2*n, length.out=n))]
}

## Colors for risk ranging blue to red through white
risk.colors <- function(n, alpha=1) {
	alpha <- round(alpha * 255)
	f1 <- colorRamp(c("blue", "white"))
	f2 <- colorRamp(c("white", "red"))
	col1 <- sapply(seq(from=0, to=1, length.out=n), function(i) do.call(rgb,
			c(as.list(f1(i)), maxColorValue=255, alpha=alpha)))
	col2 <- sapply(seq(from=0, to=1, length.out=n), function(i) do.call(rgb,
			c(as.list(f2(i)), maxColorValue=255, alpha=alpha)))
	cols <- c(col1, col2)
	cols[seq(from=1, to=2*n, by=2)]
}

## Gradient of two colors
gradient.colors <- function(n, start="white", end="black", alpha=1) {
	alpha <- round(alpha * 255)
	f <- colorRamp(c(start, end))
	cols <- sapply(seq(from=0, to=1, length.out=n), function(i) do.call(rgb,
			c(as.list(f(i)), maxColorValue=255, alpha=alpha)))
	cols
}

## Convert a color or vector of colors to be translucent
alpha.colors <- function(n, col="red", ramp=c("linear","power"), range=c(0,1)) {
	ramp <- match.arg(ramp)
	if ( missing(n) ) n <- length(col)
	if ( length(col) < n ) col <- rep(col, n)
	cols <- col2rgb(col, alpha=TRUE)
	alpha.ramp <- seq(from=range[[1]], to=range[[2]], length.out=n)
	if ( ramp == "power" ) alpha.ramp <- alpha.ramp^2
	alphas <- 255 * alpha.ramp
	cols <- rgb(cols[1,], cols[2,], cols[3,], alpha=as.integer(alphas), max=255)
	cols
}
