
#### Setup options and resources ####
## ----------------------------------

CardinalResources <- list2env(list(logger=simple_logger()))

CardinalNamespace <- environment(NULL)

CardinalEnv <- function() CardinalNamespace

# set up Cardinal defaults
.onLoad <- function(libname, pkgname) {
	setCardinalBPPARAM()
	setCardinalVerbose()
	setCardinalLogger()
}

#### Cardinal-specific options ####
## --------------------------------

# parallel defaults
getCardinalParallel <- function() {
	BPPARAM <- getOption("CardinalBPPARAM")
	if ( is.null(BPPARAM) ) {
		FALSE
	} else {
		bpworkers()
	}
}
setCardinalParallel <- function(workers = snowWorkers()) {
	maxcores <- snowWorkers()
	if ( isTRUE(workers) )
		workers <- maxcores
	if ( isFALSE(workers) || is.null(workers) ) {
		workers <- 1L
	} else {
		if ( !"L'Ecuyer-CMRG" %in% RNGkind() ) {
			.Log("making RNG parallel-safe with \"L'Ecuyer-CMRG\"",
				message=getCardinalVerbose())
			RNGkind("L'Ecuyer-CMRG")
		}
	}
	if ( is.numeric(workers) ) {
		nworkers <- min(workers, maxcores)
	} else if ( is.character(workers) ) {
		nworkers <- length(workers)
	} else {
		.Error("'workers' must be nodenames or the number of workers")
	}
	if ( nworkers > 1L ) {
		nchunks <- 4L * max(nworkers, 1L)
		.Log("making Snowfast cluster with ", nworkers, " workers ",
			"and ", nchunks, " chunks",
			message=getCardinalVerbose())
		setCardinalNChunks(nchunks)
		setCardinalBPPARAM(SnowfastParam(workers))
	} else {
		setCardinalNChunks()
		setCardinalBPPARAM(NULL)
	}
}

# parallel backend
getCardinalBPPARAM <- function() {
	getOption("CardinalBPPARAM")
}
setCardinalBPPARAM <- function(BPPARAM = NULL) {
	if ( !is.null(BPPARAM) && !is(BPPARAM, "BiocParallelParam") )
		stop("BPPARAM must be a BiocParallelParam instance or NULL")
	options(CardinalBPPARAM=BPPARAM)
}

# should progress messages be printed?
getCardinalVerbose <- function() {
	getOption("CardinalVerbose")
}
setCardinalVerbose <- function(verbose = interactive()) {
	verbose <- as.logical(verbose)
	if ( !isTRUE(verbose) && !isFALSE(verbose) )
		stop("verbose must be TRUE or FALSE")
	options(CardinalVerbose=verbose)
}

# default logger
getCardinalLogger <- function() {
	CardinalResources[["logger"]]
}
setCardinalLogger <- function(logger = matter_logger()) {
	if ( !inherits(logger, "simple_logger") )
		stop("logger must inherit from class 'simple_logger'")
	CardinalResources[["logger"]] <- logger
}

# save log file
saveCardinalLog <- function(file = "Cardinal.log") {
	getCardinalLogger()$append_trace()
	getCardinalLogger()$move(file)
	if ( getCardinalVerbose() ) {
		.Log("saved log file to: ",
			sQuote(getCardinalLogger()$logfile), message=TRUE)
	}
	invisible(getCardinalLogger())
}

# logging functions
.Log <- function(..., message = FALSE) {
	getCardinalLogger()$log(..., signal=message)
}
.Message <- function(...) {
	getCardinalLogger()$message(...)
}
.Warn <- function(...) {
	call <- sys.call(-1L)
	getCardinalLogger()$warning(..., call=call)
}
.Error <- function(...) {
	call <- sys.call(-1L)
	getCardinalLogger()$stop(..., call=call)
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
setCardinalChunksize <- function(chunksize = NA, units = names(chunksize)) {
	matter_defaults(chunksize=setNames(chunksize, units))
}

# whether to serialize external data
getCardinalSerialize <- function() {
	matter_defaults()[["serialize"]]
}
setCardinalSerialize <- function(serialize = NA) {
	matter_defaults(serialize=serialize)
}

