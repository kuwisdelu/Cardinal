
#### Deprecated and defunct ####
## -----------------------------

setMethod("fetch", "SpectralImagingData",
	function(object, ...)
	{
		.Deprecated()
		spectraData(object) <- fetch(spectraData(object), ...)
		if ( validObject(object) )
			object
	})

setMethod("flash", "SpectralImagingData",
	function(object, ...)
	{
		.Deprecated()
		spectraData(object) <- flash(spectraData(object), ...)
		if ( validObject(object) )
			object
	})

setMethod("fetch", "SpectraArrays",
	function(object, ...,
		verbose = getCardinalVerbose(),
		BPPARAM = bpparam())
	{
		.Deprecated()
		for ( i in seq_along(object) ) {
			object[[i]] <- fetch(object[[i]], ...,
				verbose=verbose, BPPARAM=BPPARAM)
		}
		object
	})

setMethod("flash", "SpectraArrays",
	function(object, ...,
		verbose = getCardinalVerbose(),
		BPPARAM = bpparam())
	{
		.Deprecated()
		for ( i in seq_along(object) ) {
			object[[i]] <- flash(object[[i]], ...,
				verbose=verbose, BPPARAM=BPPARAM)
		}
		object
	})

getCardinalNumBlocks <- function() {
	.Defunct("getCardinalNChunks")
	getCardinalNChunks()
}

setCardinalNumBlocks <- function(n = 20L) {
	.Defunct("setCardinalNChunks")
	setCardinalNChunks(n)
}

## Summarize the pixels or features of an imaging dataset

setMethod("aggregate", "SpectralImagingExperiment",
	function(x, by = c("feature", "pixel"), FUN,
		groups = NULL, tform = identity, as = "ImagingExperiment",
		BPPARAM = getCardinalBPPARAM(), ...)
	{
		.Defunct("summarizeFeatures")
	})

## image
image3d <- function(
	x = seq(0, 1, length.out=dim(values)[1]),
	y = seq(0, 1, length.out=dim(values)[2]),
	z = seq(0, 1, length.out=dim(values)[3]),
	values,
	xlim = range(x),
	ylim = range(y),
	zlim = range(z),
	xlab, ylab, zlab,
	col = heat.colors(12),
	alpha.power = 1,
	alpha = (seq_along(col) / length(col))^alpha.power,
	pch = 15, cex = 1,
	scale = FALSE,
	add = FALSE,
	...)
{
	.Defunct("matter::vizi")
}

points3d <- function(
	x, y, z,
	values,
	xlim = range(x),
	ylim = range(y),
	zlim = range(z),
	xlab, ylab, zlab,
	col = heat.colors(12),
	alpha.power = 1,
	alpha = (seq_along(col) / length(col))^alpha.power,
	pch = 15, cex = 1,
	scale = FALSE,
	add = FALSE,
	...)
{
	.Defunct("matter::vizi")
}


# Set to dark mode
darkmode <- function(default = TRUE) {
	.Defunct("matter::vizi_style")
	matter::vizi_style("dark")
}

# Set to dark mode
lightmode <- function(default = TRUE) {
	.Defunct("matter::vizi_style")
	matter::vizi_style("light")
}

