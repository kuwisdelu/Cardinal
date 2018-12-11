
## Preset color maps
color.map <- function(map = c("redblack", "greenblack", "blueblack",
	"parula", "darkrainbow", "rainbow", "grayscale",
	"jet", "hot", "cool"), n = 100)
{
	map <- match.arg(map)
	switch(map,
		redblack = gradient.colors(n, end="#EE2200"),
		greenblack = gradient.colors(n, end="#00FF44"),
		blueblack = gradient.colors(n, end="#00AAFF"),
		parula = topo.colors(n),
		darkrainbow = intensity.colors(n),
		rainbow = intensity.colors2(n),
		grayscale = bw.colors(n),
		jet = jet.colors(n),
		hot = c(
			divergent.colors(ceiling(n/2), "black", "darkred", "red"),
			divergent.colors(floor(n/2), "red", "orange", "yellow")),
		cool = gradient.colors(n, "cyan", "magenta"))
}

col.map <- function(...) color.map(...)

## Colors for image intensities
intensity.colors <- function(n = 100, alpha = 1) {
	col2 <- rainbow(3*n, alpha=alpha)[(2*n):1]
	f <- colorRamp(c("black", rainbow(3*n)[2*n]))
	alpha <- col2rgb(col2, alpha=TRUE)[[4]]
	col1 <- sapply(seq(from=0, to=1, length.out=n), function(i) do.call("rgb",
		c(as.list(f(i)), maxColorValue=255, alpha=alpha)))
	cols <- c(col1, col2)
	cols[seq(from=1, to=3*n, by=3)]
}

## Colors for image intensities (alternative)
intensity.colors2 <- function(n = 100, alpha = 1) {
	cols <- rainbow(3*n, alpha=alpha)[(2*n):1]
	cols[round(seq(from=1, to=2*n, length.out=n))]
}

## Colors for the "jet" color scheme
jet.colors <- function(n = 100, alpha = 1) {
	col2 <- rainbow(9*n, alpha=alpha)[1:(6*n)]
	f <- colorRamp(c("darkred", rainbow(n)[1]))
	g <- colorRamp(c(col2[length(col2)], "darkblue"))
	alpha <- col2rgb(col2, alpha=TRUE)[[4]]
	col1 <- sapply(seq(from=0, to=1, length.out=n), function(i) do.call("rgb",
		c(as.list(f(i)), maxColorValue=255, alpha=alpha)))
	col3 <- sapply(seq(from=0, to=1, length.out=n), function(i) do.call("rgb",
		c(as.list(g(i)), maxColorValue=255, alpha=alpha)))
	cols <- rev(c(col1, col2, col3))
	cols[seq(from=1, to=8*n, by=8)]
}


## Colors for diverging conditions
divergent.colors <- function(n = 100, start = "#00AAEE",
	middle = "#FFFFFF", end = "#EE2200", alpha = 1)
{
	alpha <- round(alpha * 255)
	f1 <- colorRamp(c(start, middle))
	f2 <- colorRamp(c(middle, end))
	col1 <- sapply(seq(from=0, to=1, length.out=n), function(i) do.call("rgb",
			c(as.list(f1(i)), maxColorValue=255, alpha=alpha)))
	col2 <- sapply(seq(from=0, to=1, length.out=n), function(i) do.call("rgb",
			c(as.list(f2(i)), maxColorValue=255, alpha=alpha)))
	cols <- c(col1, col2)
	cols[seq(from=1, to=2*n, by=2)]
}

## Colors for risk ranging blue to red through white
risk.colors <- function(n = 100, alpha = 1)
{
	cols <- divergent.colors(n=n, start="#0000FF", middle="#FFFFFF", end="#FF0000", alpha=1)
	cols
}

## Gradient of two colors
gradient.colors <- function(n = 100, start = "#000000", end = "#00AAFF", alpha = 1) {
	alpha <- round(alpha * 255)
	f <- colorRamp(c(start, end))
	cols <- sapply(seq(from=0, to=1, length.out=n), function(i) do.call("rgb",
			c(as.list(f(i)), maxColorValue=255, alpha=alpha)))
	cols
}

## Gradient of black and white
bw.colors <- function(n = 100, alpha = 1) {
	cols <- gradient.colors(n=n, start="#000000", end="#FFFFFF", alpha=alpha)
	cols
}

## Discrete color scale
discrete.colors <- function(n = 2, chroma = 150, luminance = 65, alpha = 1) {
	if ( n == 1L )
		return(rgb(0, 4/9, 2/3, alpha))
	hue <- c(0, 360) + 15
	if ( diff(hue) %% 360 < 1 )
		hue[2] <- hue[2] - 360 / n
	rotate <- function(x) x %% 360
	hues <- rotate(seq(hue[1], hue[2], length.out = n))
	cols <- hcl(hues, c=chroma, l=luminance, alpha=alpha)
	cols
}

## Convert a color or vector of colors to be translucent
alpha.colors <- function(col, n = 100, alpha = (seq_len(n)/n)^alpha.power, alpha.power = 2) {
	if ( missing(n) )
		n <- length(col)
	if ( length(col) != n )
		col <- rep(col, length.out=n)
	if ( length(alpha) != n )
		alpha <- rep(alpha, length.out=n)
	cols <- col2rgb(col, alpha=TRUE)
	alphas <- 255 * alpha
	cols <- rgb(cols[1,], cols[2,], cols[3,],
		alpha=as.integer(alphas),
		maxColorValue=255)
	cols
}
