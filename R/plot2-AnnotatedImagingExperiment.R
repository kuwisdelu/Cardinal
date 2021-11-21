
#### Histogram plotting for optical imaging experiments ####
## ------------------------------------------------

setMethod("plot", c(x = "AnnotatedImagingExperiment"),
	function(x, i, ...)
{
	plot(imageData(x), i=i, ...)
})

setMethod("plot", c(x = "AnnotatedImageList"),
	function(x, i, breaks = "Sturges",
		strip = TRUE,
		key = TRUE, col,
		layout = !add,
		add = FALSE, ...)
{
	if ( !missing(i) )
		x <- x[i]
	if ( length(unique(sapply(x, colorMode))) != 1L )
		.stop("all images must have the same colorMode")
	obj <- new("DFrame", nrows=length(x[[1L]]))
	data <- as(x, "SimpleList", strict=FALSE)
	if ( colorMode(x[[1L]]) == EBImage::Color ) {
		colors <- c("red", "green", "blue")
		images <- lapply(unname(data), function(y) {
			if ( length(dim(y)) > 2L ) {
				nc <- dim(y)[3L]
			} else {
				nc <- 1
			}
			layers <- lapply(colors[1:nc], function(ci)
				imageData(EBImage::channel(y, ci)))
			names(layers) <- colors
			layers
		})
		if ( is.null(names(x)) ) {
			fids <- rep.int(seq_along(x), lengths(images))
			facets <- data.frame(.id=fids)
		} else {
			fids <- rep.int(names(x), lengths(images))
			facets <- data.frame(.name=fids)
		}
		images <- do.call("c", images)
		if ( missing(col) )
			col <- colors
		args <- list(lhs=NULL, rhs=images, g=NULL)
		facet.count(args, formula=NULL, obj=obj,
			facets=facets, groups=NULL, superpose=TRUE,
			strip=strip, key=key, breaks=breaks, ...,
			layout=layout, col=col, subset=NULL, add=add)
	} else {
		args <- list(lhs=NULL, rhs=as.list(data), g=NULL)
		if ( is.null(names(x)) ) {
			facets <- data.frame(.id=seq_along(x))
		} else {
			facets <- data.frame(.name=names(x))
		}
		if ( missing(col) )
			col <- discrete.colors(1)
		facet.count(args, formula=NULL, obj=obj,
			facets=facets, groups=NULL, superpose=FALSE,
			strip=strip, key=key, breaks=breaks, ...,
			layout=layout, col=col, subset=NULL, add=add)
	}
})


