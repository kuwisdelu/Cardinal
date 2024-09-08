
#### Summarize features ####
## --------------------------

summarizeFeatures <- function(x, stat = "mean", groups = NULL,
	verbose = getCardinalVerbose(), chunkopts = list(),
	BPPARAM = getCardinalBPPARAM(), ...)
{
	if ( "FUN" %in% ...names() ) {
		.Deprecated(old="FUN", new="stat")
		stat <- list(...)$FUN
	}
	if ( !is(x, "SpectralImagingExperiment") )
		.Error("object must inherit from SpectralImagingExperiment")
	if ( is.null(names(stat)) ) {
		labels <- stat
	} else {
		labels <- ifelse(nchar(names(stat)), names(stat), stat)
	}
	ans <- rowStats(x, stat=stat, group=groups, simplify=FALSE,
		verbose=verbose, chunkopts=chunkopts,
		BPPARAM=BPPARAM, ...)
	for ( i in seq_along(ans) ) {
		y <- as.vector(ans[[i]])
		if ( is.null(dim(ans[[i]])) ) {
			names(y) <- names(ans[[i]])
		} else {
			dim(y) <- dim(ans[[i]])
			dimnames(y) <- dimnames(ans[[i]])
		}
		if ( is.array(y) ) {
			nm <- paste0(colnames(y), ".", labels[i])
			featureData(x)[nm] <- y
		} else {
			featureData(x)[[labels[i]]] <- y
		}
	}
	x
}


#### Summarize pixels ####
## ------------------------

summarizePixels <- function(x, stat = c(tic="sum"), groups = NULL,
	verbose = getCardinalVerbose(), chunkopts = list(),
	BPPARAM = getCardinalBPPARAM(), ...)
{
	if ( "FUN" %in% ...names() ) {
		.Deprecated(old="FUN", new="stat")
		stat <- list(...)$FUN
	}
	if ( !is(x, "SpectralImagingData") )
		.Error("object must inherit from SpectralImagingData")
	if ( is.null(names(stat)) ) {
		labels <- stat
	} else {
		labels <- ifelse(nchar(names(stat)), names(stat), stat)
	}
	if ( is(x, "SpectralImagingExperiment") ) {
		ans <- colStats(x, stat=stat, group=groups, simplify=FALSE,
			verbose=verbose, chunkopts=chunkopts,
			BPPARAM=BPPARAM, ...)
	} else {
		if ( !is.null(groups) )
			.Error("'groups' not supported for class ", sQuote(class(x)[1L]))
		FUN <- isofun(function(xi, stat, labels, ...) {
			yi <- vector("list", length=length(stat))
			yi <- setNames(yi, labels)
			for ( i in seq_along(yi) )
				yi[[i]] <- vapply(xi, match.fun(stat[i]), numeric(1L), ...)
			list(yi)
		}, CardinalEnv())
		ans <- chunk_lapply(spectra(x), FUN, stat=stat, labels=labels,
			verbose=verbose, chunkopts=chunkopts,
			BPPARAM=BPPARAM, ...)
		ans <- Reduce(function(e1, e2) Map(c, e1, e2), ans)
	}
	for ( i in seq_along(ans) ) {
		y <- as.vector(ans[[i]])
		if ( is.null(dim(ans[[i]])) ) {
			names(y) <- names(ans[[i]])
		} else {
			dim(y) <- dim(ans[[i]])
			dimnames(y) <- dimnames(ans[[i]])
		}
		if ( is.array(y) ) {
			nm <- paste0(colnames(y), ".", labels[i])
			pixelData(x)[nm] <- y
		} else {
			pixelData(x)[[labels[i]]] <- y
		}
	}
	x
}


#### Row/column statistics ####
## ----------------------------

setMethod("rowStats", "SpectralImagingExperiment",
	function(x, stat, ...,
		verbose = getCardinalVerbose(), chunkopts = list(),
		BPPARAM = getCardinalBPPARAM())
	{
		rowStats(spectra(x), stat=stat,
			verbose=verbose, chunkopts=chunkopts,
			BPPARAM=BPPARAM, ...)
	})

setMethod("colStats", "SpectralImagingExperiment",
	function(x, stat, ...,
		verbose = getCardinalVerbose(), chunkopts = list(),
		BPPARAM = getCardinalBPPARAM())
	{
		colStats(spectra(x), stat=stat,
			verbose=verbose, chunkopts=chunkopts,
			BPPARAM=BPPARAM, ...)
	})

setMethod("rowSums", "SpectralImagingExperiment",
	function(x, na.rm = FALSE, dims = 1, ...)
	{
		rowStats(x, stat="sum", ..., na.rm=na.rm)
	})

setMethod("colSums", "SpectralImagingExperiment",
	function(x, na.rm = FALSE, dims = 1, ...)
	{
		colStats(x, stat="sum", ..., na.rm=na.rm)
	})

setMethod("rowMeans", "SpectralImagingExperiment",
	function(x, na.rm = FALSE, dims = 1, ...)
	{
		rowStats(x, stat="mean", ..., na.rm=na.rm)
	})

setMethod("colMeans", "SpectralImagingExperiment",
	function(x, na.rm = FALSE, dims = 1, ...)
	{
		colStats(x, stat="mean", ..., na.rm=na.rm)
	})


