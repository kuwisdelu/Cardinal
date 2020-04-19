
#### get and set Cardinal options ####

# should progress messages be printed?

getCardinalBPPARAM <- function() {
	getOption("Cardinal.bpparam")
}

setCardinalBPPARAM <- function(BPPARAM = SerialParam()) {
	options("Cardinal.bpparam" = BPPARAM)
}

# should progress messages be printed?

getCardinalVerbose <- function() {
	getOption("Cardinal.verbose")
}

setCardinalVerbose <- function(verbose = interactive()) {
	options("Cardinal.verbose" = verbose)
}

# number of blocks to use for block-processing

getCardinalNumBlocks <- function() {
	getOption("Cardinal.numblocks")
}

setCardinalNumBlocks <- function(n = 20L) {
	options("Cardinal.numblocks" = n)
}

# should processing be delayed when possible

getCardinalDelayProc <- function() {
	getOption("Cardinal.delay")
}

setCardinalDelayProc <- function(delay = TRUE) {
	options("Cardinal.delay" = delay)
}



