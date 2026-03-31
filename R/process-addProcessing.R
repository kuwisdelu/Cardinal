
#### Spectral pre-processing queueing ####
## ----------------------------------------

setMethod("addProcessing", "SpectralImagingArrays",
	function(object, FUN, ..., pixelVariables = character(), label = NULL)
{
	ps <- ProcessingStep(FUN, ARGS=list(...))
	ps <- setNames(list(ps), label)
	psvars <- union(object@processingVariables, pixelVariables)
	object@processingQueue <- c(object@processingQueue, ps)
	object@processingVariables <- psvars
	if ( validObject(object) )
		object
})

dropProcessing <- function(object, ...)
{
	object@processingQueue <- list()
	object@processingVariables <- character()
	if ( .hasSlot(object, "experimentData") )
		object@experimentData[["dataProcessing"]] <- NULL
	if ( validObject(object) )
		object
}

updateProcessingStep <- function(object, ARGS, replace = FALSE)
{
	if ( !is(object, "ProcessingStep") )
		stop("'object' must be a 'ProcessingStep' object")
	if ( replace ) {
		object@ARGS <- ARGS
	} else {
		object@ARGS <- c(object@ARGS, ARGS)
	}
	if ( validObject(object) )
		object
}
