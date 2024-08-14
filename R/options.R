
#### Setup options ####
## --------------------

CardinalVersion <- function() {
	.Defunct("packageVersion")
}

# set up Cardinal defaults
.onLoad <- function(libname, pkgname) {
	setCardinalBPPARAM()
	setCardinalVerbose()
}

# set up Cardinal defaults
.onAttach <- function(libname, pkgname) {
	version <- utils::packageVersion("Cardinal")
	if ( version >= "3.5.0" )
		packageStartupMessage("Cardinal v3.6 is a major update.\n",
			"Use vignette('Cardinal3-guide') to see what's new.")
}

#### Cardinal-specific options ####
## --------------------------------

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
		stop("verbose must be TRUE or FALSE")
	options("CardinalVerbose" = verbose)
}

#### Cardinal-controlled matter options ####
## -----------------------------------------

# number of chunks to use for processing
getCardinalNChunks <- function() {
	matter_defaults()[["nchunks"]]
}
setCardinalNChunks <- function(nchunks = 20L) {
	matter_defaults(nchunks=nchunks)
}

# size of chunks to use for processing
getCardinalChunksize <- function() {
	matter_defaults()[["chunksize"]]
}
setCardinalChunksize <- function(chunksize = NA, units = "MB") {
	matter_defaults(chunksize=size_bytes(chunksize, units))
}

# whether to serialize external data
getCardinalSerialize <- function() {
	matter_defaults()[["serialize"]]
}
setCardinalSerialize <- function(serialize = NA) {
	matter_defaults(serialize=serialize)
}

# history and log file location
getCardinalLog <- function() {
	if ( length(matter_logger()$logfile) ) {
		matter_logger()$logfile
	} else {
		matter_logger()$history()
	}
}
setCardinalLog <- function(file = NULL) {
	matter_logger()$move(file)
}
saveCardinalLog <- function(file = "Cardinal.log") {
	setCardinalLog(file)
}

