
#### Histogram plotting for optical images ####
## ------------------------------------------

setMethod("plot", c(x = "AnnotatedImage"),
	function(x, breaks = "Sturges",
		key = TRUE, col,
		add = FALSE, ...)
{
	obj <- new("DFrame", nrows=length(x))
	colors <- c("red", "green", "blue")
	if ( colorMode(x) == EBImage::Color ) {
		if ( length(dim(x)) > 2L ) {
			nc <- dim(x)[3L]
		} else {
			nc <- 1
		}
		layers <- lapply(colors[1:nc], function(ci)
			imageData(EBImage::channel(x, ci)))
		names(layers) <- colors
		if ( missing(col) )
			col <- colors
		args <- list(lhs=NULL, rhs=layers, g=NULL)
		facet.count(args, formula=NULL, obj=obj,
			facets=NULL, groups=NULL, superpose=TRUE,
			strip=FALSE, key=key, breaks=breaks, ...,
			col=col, subset=NULL, add=add)
	} else {
		if ( missing(col) )
			col <- discrete.colors(1)
		args <- list(lhs=NULL, rhs=list(imageData(x)), g=NULL)
		facet.count(args, formula=NULL, obj=obj,
			facets=NULL, groups=NULL, superpose=FALSE,
			strip=FALSE, key=key, breaks=breaks, ...,
			col=col, subset=NULL, add=add)
	}
})


