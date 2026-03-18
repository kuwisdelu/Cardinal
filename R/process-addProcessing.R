
#### Spectral pre-processing queueing ####
## ----------------------------------------

setMethod("addProcessing", "SpectralImagingArrays",
	function(object, FUN, ..., pixelVariables = character(), label = NULL)
{
	ps <- ProcessingStep(FUN, ARGS=list(...))
	ps <- setNames(list(ps), label)
	psvars <- union(object@processingVariables, pixelVariables)
	object@processing <- c(object@processing, ps)
	object@processingVariables <- psvars
	if ( validObject(object) )
		object
})

dropProcessing <- function(object, ...)
{
	object@processing <- list()
	object@processingVariables <- character()
	if ( .hasSlot(object, "experimentData") )
		object@experimentData[["dataProcessing"]] <- NULL
	if ( validObject(object) )
		object
}
