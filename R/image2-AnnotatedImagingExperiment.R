
#### Image plotting for optical imaging experiments ####
## ---------------------------------------------

setMethod("image", c(x = "AnnotatedImagingExperiment"),
	function(x, i, frame = 1,
		strip = TRUE,
		layout = !add,
		native = TRUE,
		interpolate = TRUE,
		add = FALSE, ...)
{
	if ( !missing(i) )
		x <- x[i]
	data <- as(imageData(x), "SimpleList", strict=FALSE)
	fun <- function(y) {
		y <- getFrame(y, frame, type="render")
		if ( native ) {
			EBImage:::as.nativeRaster(y)
		} else {
			as.raster(y)
		}
	}
	rasters <- lapply(data, fun)
	offsets <- coordinates(x, simplify=FALSE)
	heights <- height(x)
	widths <- width(x)
	facet.raster(rasters, offsets=offsets,
		heights=heights, widths=widths, ...,
		strip=strip, layout=layout,
		interpolate=interpolate,
		add=add)
})

