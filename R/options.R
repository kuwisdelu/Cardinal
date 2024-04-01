
#### Options ####
## --------------

CardinalVersion <- function() {
	paste0(utils::packageVersion("Cardinal"), collapse=".")
}

# set up Cardinal defaults
.onLoad <- function(libname, pkgname) {
	setCardinalVerbose()
	setCardinalNChunks()
	setCardinalBPPARAM()
}

# should progress messages be printed?
getCardinalBPPARAM <- function() {
	getOption("Cardinal.bpparam")
}
setCardinalBPPARAM <- function(BPPARAM = NULL) {
	if ( !is.null(BPPARAM) && !is(BPPARAM, "BiocParallelParam") )
		stop("BPPARAM must be a BiocParallelParam instance or NULL")
	options("Cardinal.bpparam" = BPPARAM)
}

# should progress messages be printed?
getCardinalVerbose <- function() {
	getOption("Cardinal.verbose")
}
setCardinalVerbose <- function(verbose = interactive()) {
	verbose <- as.logical(verbose)
	if ( !isTRUE(verbose) && !isFALSE(verbose) )
		stop("verbose must be a single logical value")
	options("Cardinal.verbose" = verbose)
}

# number of chunks to use for processing
getCardinalNChunks <- function() {
	getOption("Cardinal.nchunks")
}
setCardinalNChunks <- function(nchunks = 20L) {
	nchunks <- as.integer(nchunks)
	if ( !isTRUE(nchunks > 0L) || length(nchunks) != 1L )
		stop("nchunks must be a single positive integer")
	options("Cardinal.nchunks" = nchunks)
}

