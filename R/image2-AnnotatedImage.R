
#### Image plotting with optical images ####
## -------------------------------------------

setMethod("image", c(x = "AnnotatedImage"),
	function(x, frame = 1, offset = coord(x),
		height, width,
		layout = !add,
		native = TRUE,
		interpolate = TRUE,
		add = FALSE, ...)
{
	if ( missing(height) )
		height <- Cardinal::height(x)
	if ( missing(width) )
		width <- Cardinal::width(x)
	raster <- getFrame(x, frame, type="render")
	if ( native ) {
		raster <- EBImage:::as.nativeRaster(raster)
	} else {
		raster <- as.raster(raster)
	}
	facet.raster(raster, offsets=offset,
		heights=height, widths=width, ...,
		strip=FALSE, layout=layout,
		interpolate=interpolate,
		add=add)
})

