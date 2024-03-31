
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

## Mass utilities

seq_mz <- function(from, to, by, units = c("ppm", "mz"))
{
	units <- match.arg(units)
	by <- unname(by)
	mz <- switch(units,
		ppm=seq_rel(from=from, to=to, by=1e-6 * by),
		mz=seq.default(from=from, to=to, by=by))
	switch(units,
		ppm=structure(mz, resolution=c(ppm=by)),
		mz=structure(mz, resolution=c(mz=by)))
}

seq_mzr <- function(range, by, units = c("ppm", "mz"))
{
	seq_mz(min(range), max(range), by=by, units=units)
}


## Miscellanious utilities

makeFactor <- function(..., ordered = FALSE)
{
	inds <- list(...)
	labs <- vapply(substitute(...()), deparse, character(1L))
	if ( !is.null(names(inds)) ) {
		nz <- nzchar(names(inds))
		labs[nz] <- names(inds)[nz]
	}
	names(labs) <- NULL
	inds <- do.call("cbind", inds)
	inds <- apply(inds, 1, function(i) which(i)[1L])
	factor(labs[inds], levels=labs, ordered=ordered)
}


## Math utilities

# L1 norm of a vector
l1norm <- function(x) sum(abs(x))

# L2 norm of a vector
l2norm <- function(x) sqrt(sum(x^2))

# Soft thresholding
soft <- function(x, delta) sign(x) * pmax(0, abs(x) - delta)

# Regularize a matrix of probabilities
regpr <- function(x, lambda = 0.1) {
	x <- x + lambda
	x / rowSums(x)
}

# Return positive part
pos <- function(x) pmax(x, 0)

# Return negative part
neg <- function(x) pmin(x, 0)

# Combined (pooled) mean of multiple subsets
combinedMean <- function(xbar, n) {
	sum(xbar * n) / sum(n)
}

# Combined (pooled) variance of multiple subsets
combinedVar <- function(xbar, var, n) {
	xc <- combinedMean(xbar, n)
	(sum(n * var) + sum(n * (xbar - xc)^2)) / sum(n)
}

# Grouped mean, e.g., of a histogram
groupMean <- function(x, f) {
	if ( any(f < 0) ) f[f < 0] <- 0
	n <- sum(f)
	sum(f * x) / n
}

# Grouped variance, e.g., of a histogram
groupVar <- function(x, f) {
	if ( any(f < 0) ) f[f < 0] <- 0
	n <- sum(f)
	xbar <- sum(f * x) / n
	Sfx2 <- sum(f * x^2)
	(Sfx2 / n) - xbar^2
}

# Mode of a vector
Mode <- function(x, na.rm = FALSE) {
	if ( na.rm )
		x <- x[!is.na(x)]
	ux <- unique(x)
	ux[which.max(tabulate(match(x, ux)))]
}

# Kurtosis of a vector
kurtosis <- function(x, na.rm=FALSE) {
	if ( na.rm ) x <- x[!is.na(x)]
	n <- length(x)
	n * sum((x - mean(x))^4) / (sum((x - mean(x))^2)^2)
}

# Sensitivity (true positive rate)
sensitivity <- function(ref, pred, positive = levels(ref)[1]) {
	nas <- is.na(ref) | is.na(pred)
	if ( any(nas) ) {
		ref <- ref[!nas]
		pred <- pred[!nas]
	}
	real_pos <- ref %in% positive
	pred_pos <- pred %in% positive
	sum(real_pos & pred_pos) / sum(real_pos)
}

# Specificity (true negative rate)
specificity <- function(ref, pred, positive = levels(ref)[1]) {
	nas <- is.na(ref) | is.na(pred)
	if (any(nas)) {
		ref <- ref[!nas]
		pred <- pred[!nas]
	}
	real_neg <- !ref %in% positive
	pred_neg <- !pred %in% positive
	sum(real_neg & pred_neg) / sum(real_neg)
}

# Vectorized maximum
is.max <- function(x) {
	i <- seq_along(x)
	i == which.max(x)
}

# Vectorized maximum
is.min <- function(x) {
	i <- seq_along(x)
	i == which.min(x)
}

# Match score between two logical vectors
Mscore <- function(a, b, type=3) {
	switch(type,
		sum(a & b, na.rm=TRUE) / sum(a, na.rm=TRUE),
		sum(a & b, na.rm=TRUE) / sum(b, na.rm=TRUE),
		sum(a & b, na.rm=TRUE) / sum(a | b, na.rm=TRUE))
}


