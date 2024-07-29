
#### Options ####
## --------------

CardinalVersion <- function() {
	.Defunct("packageVersion")
}

# set up Cardinal defaults
.onLoad <- function(libname, pkgname) {
	setCardinalBPPARAM()
	setCardinalVerbose()
	setCardinalNChunks()
	setCardinalChunksize()
}

# set up Cardinal defaults
.onAttach <- function(libname, pkgname) {
	version <- utils::packageVersion("Cardinal")
	if ( version >= "3.5.0" )
		packageStartupMessage("Cardinal v3.6 is a major update.\n",
			"Use vignette('Cardinal3-guide') to see what's new.")
}

# should progress messages be printed?
getCardinalBPPARAM <- function() {
	getOption("CardinalBPPARAM")
}
setCardinalBPPARAM <- function(BPPARAM = NULL) {
	if ( !is.null(BPPARAM) && !is(BPPARAM, "BiocParallelParam") )
		stop("BPPARAM must be a BiocParallelParam instance or NULL")
	options("CardinalBPPARAM" = BPPARAM)
}

# should progress messages be printed?
getCardinalVerbose <- function() {
	getOption("CardinalVerbose")
}
setCardinalVerbose <- function(verbose = interactive()) {
	verbose <- as.logical(verbose)
	if ( !isTRUE(verbose) && !isFALSE(verbose) )
		stop("verbose must be a single logical value")
	options("CardinalVerbose" = verbose)
}

# number of chunks to use for processing
getCardinalNChunks <- function() {
	getOption("matter.default.nchunks")
}
setCardinalNChunks <- function(nchunks = 20L) {
	nchunks <- as.integer(nchunks)
	if ( !isTRUE(nchunks > 0L) || length(nchunks) != 1L )
		stop("nchunks must be a single positive number")
	options("matter.default.nchunks" = nchunks)
}

# size of chunks to use for processing (in bytes)
getCardinalChunksize <- function() {
	getOption("matter.default.chunksize")
}
setCardinalChunksize <- function(chunksize = NA_real_) {
	chunksize <- as.numeric(chunksize)
	if ( isTRUE(chunksize <= 0L) || length(chunksize) != 1L )
		stop("nchunks must be a single positive number (or NA)")
	options("matter.default.chunksize" = chunksize)
}

