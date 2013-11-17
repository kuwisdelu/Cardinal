
#### implement unload methods ####

setMethod("unload", "MSImageSet", function(object, ...) {
	dots <- match.call(expand.dots=FALSE)$... # from base 'remove'
	objectname <- deparse(substitute(object))
	names <- c(objectname, sapply(dots, as.character))
	unloaded <- sapply(names, internalUnload, envir=parent.frame())
	invisible(all(unloaded))
} )

#### helper functions ####

internalUnload <- function(objectname, envir) {
	object <- try(get(objectname, envir=envir))
	if ( inherits(object, "try-error") || !isMSImageSet(object) ) return(FALSE)
	object@spectra$spectra <- NULL
	do.call(remove, list(objectname, envir=envir))
	TRUE
}
