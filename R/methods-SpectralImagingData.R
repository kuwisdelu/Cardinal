
#### SpectralImagingData ####
## --------------------------

# VIRTUAL class for containing spectra arrays
# with imaging position/pixel information

.valid_SpectralImagingData <- function(object)
{
	errors <- NULL
	if ( length(object@processing) > 0L )
	{
		is_ps <- vapply(object@processing, is, logical(1L), "ProcessingStep")
		if ( !all(is_ps) )
			errors <- c(errors, paste0("all processing elements ",
				"must be ProcessingStep objects"))
	}
	if ( is.null(errors) ) TRUE else errors
}

setValidity("SpectralImagingData", .valid_SpectralImagingData)

# Make matter::mem() report virtual memory correctly
setMethod("vm_used", "SpectralImagingData",
	function(x) vm_used(spectraData(x)))

## Slot getters and setters

# spectraData

setMethod("spectraData", "SpectralImagingData",
	function(object, ...) object@spectraData)
setReplaceMethod("spectraData", "SpectralImagingData",
	function(object, ..., value) {
		object@spectraData <- SpectraArrays(value)
		if ( validObject(object) )
			object
	})

setMethod("spectraNames", "SpectralImagingData",
	function(object, ...) names(spectraData(object)))
setReplaceMethod("spectraNames", "SpectralImagingData",
	function(object, ..., value) {
		names(spectraData(object)) <- value
		object
	})

# Spectra array access

setMethod("spectra", "SpectralImagingData",
	function(object, i = 1L, ...) object@spectraData[[i]])
setReplaceMethod("spectra", "SpectralImagingData",
	function(object, i = 1L, ..., value) {
		object@spectraData[[i]] <- value
		object
	})

# pixelData

setMethod("pixelData", "SpectralImagingData",
	function(object) object@elementMetadata)
setReplaceMethod("pixelData", "SpectralImagingData",
	function(object, value) {
		object@elementMetadata <- value
		if ( validObject(object) )
			object
	})

setMethod("pData", "SpectralImagingData",
	function(object) pixelData(object))
setReplaceMethod("pData", "SpectralImagingData",
	function(object, value) {
		pixelData(object) <- value
		object
	})

setMethod("pixelNames", "SpectralImagingData",
	function(object) rownames(pixelData(object)))
setReplaceMethod("pixelNames", "SpectralImagingData",
	function(object, value) {
		rownames(pixelData(object)) <- value
		object
	})

setMethod("pixels", "SpectralImagingData",
	function(object, ..., env = NULL)
	{
		if ( is.null(env) )
			env <- parent.frame(2)
		env <- as.env(pixelData(object), enclos=env)
		conditions <- eval(substitute(alist(...)))
		i <- .find_conditions(conditions, env, nrow(pixelData(object)))
		setNames(i, pixelNames(object)[i])
	})

.find_conditions <- function(expr, env, n)
{
	if ( length(expr) == 0L )
		return(seq_len(n))
	FUN <- function(cond) {
		ci <- eval(cond, envir=env)
		if ( is.logical(ci) && length(ci) != n )
			.Error("length of condition [", length(ci),
				"] must match extent of object [", n, "]")
		if ( is.numeric(ci) && any(ci < 1L | ci > n) )
			.Error("subscript out of bounds")
		ci
	}
	ans <- lapply(expr, FUN)
	bools <- vapply(ans, is.logical, logical(1L))
	conditions <- do.call(cbind, ans[bools])
	i <- unique(unlist(ans[!bools]))
	if ( length(conditions) > 0L ) {
		trues <- apply(conditions, 1L, all)
		if ( length(i) > 0L ) {
			intersect(i, which(trues))
		} else {
			which(trues)
		}
	} else {
		i
	}
}

# Variable names

setMethod("spectraVariables", "SpectralImagingData",
	function(object, ...) names(pixelData(object)))

# Coord/Run access

setMethod("coord", "SpectralImagingData",
	function(object, ...) coord(pixelData(object)))
setReplaceMethod("coord", "SpectralImagingData",
	function(object, ..., value) {
		coord(pixelData(object)) <- value
		object
	})

setMethod("coordNames", "SpectralImagingData",
	function(object) coordNames(pixelData(object)))
setReplaceMethod("coordNames", "SpectralImagingData",
	function(object, value) {
		coordNames(pixelData(object)) <- value
		object
	})

setMethod("run", "SpectralImagingData",
	function(object, ...) run(pixelData(object)))
setReplaceMethod("run", "SpectralImagingData",
	function(object, ..., value) {
		run(pixelData(object)) <- value
		object
	})

setMethod("runNames", "SpectralImagingData",
	function(object) runNames(pixelData(object)))
setReplaceMethod("runNames", "SpectralImagingData",
	function(object, value) {
		runNames(pixelData(object)) <- value
		object
	})

setMethod("nrun", "SpectralImagingData",
	function(x) nrun(pixelData(x)))

setMethod("is3D", "SpectralImagingData",
	function(object) is3D(pixelData(object)))

# processingData

setMethod("processingData", "SpectralImagingData",
	function(object, ...) object@processing)
setReplaceMethod("processingData", "SpectralImagingData",
	function(object, ..., value) {
		object@processing <- value
		if ( validObject(object) )
			object
	})

## Basic getters and setters

# access spectra variables
setMethod("[[", "SpectralImagingData",
	function(x, i, j, ...) pixelData(x)[[i, ...]])

setReplaceMethod("[[", "SpectralImagingData",
	function(x, i, j, ..., value) {
		pixelData(x)[[i, ...]] <- value
		x
	})

setMethod("$", "SpectralImagingData",
	function(x, name) pixelData(x)[[name, exact=FALSE]])

setReplaceMethod("$", "SpectralImagingData",
	function(x, name, value) {
		pixelData(x)[[name]] <- value
		x
	})

# allow tab completion in console
.DollarNames.SpectralImagingData <- function(x, pattern = "") {
	grep(pattern, names(pixelData(x)), value=TRUE)
}
