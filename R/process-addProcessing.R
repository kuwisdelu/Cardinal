
#### Spectral pre-processing queueing ####
## ----------------------------------------

setMethod("addProcessing", "SpectralImagingArrays",
	function(object, FUN, ..., pixelVariables = character(), id = NULL)
{
	if ( is.null(id) )
		id <- paste0("PS", length(object@processingQueue) + 1L)
	step <- ProcessingStep(FUN, ARGS=list(...))
	step <- setNames(list(step), id)
	mvars <- union(object@processingVariables, pixelVariables)
	object@processingQueue <- c(object@processingQueue, step)
	object@processingVariables <- mvars
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

reset <- function(object)
{
	.Deprecated("dropProcessing")
	dropProcessing(object)
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
