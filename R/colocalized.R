
#### Find features colocalized with a reference ####
## ------------------------------------------------

setMethod("colocalized", "MSImagingExperiment",
	function(object, mz, ...)
{
	if ( missing(mz) || is.null(mz) ) {
		callNextMethod()
	} else {
		i <- features(object, mz=mz)
		if ( length(i) < length(mz) )
			stop("no matching features for some m/z-values")
		callNextMethod(object, i=i, ...)
	}
})

setMethod("colocalized", "SpectralImagingExperiment",
	function(object, i, ref,
		threshold = median, n = Inf,
		sort.by = c("cor", "MOC", "M1", "M2", "Dice", "none"),
		nchunks = getCardinalNChunks(),
		verbose = getCardinalVerbose(),
		BPPARAM = getCardinalBPPARAM(), ...)
{
	sort.by <- match.arg(sort.by)
	if ( !missing(i) && !is.null(i) ) {
		ref <- as.matrix(spectra(object)[i,,drop=FALSE])
		ref <- apply(ref, 1L, identity, simplify=FALSE)
	}
	if ( is.factor(ref) || is.character(ref) ) {
		ref <- as.factor(ref)
		lvl <- setNames(levels(ref), levels(ref))
		ref <- lapply(lvl, function(ci) ref %in% ci)
	}
	if ( !is.list(ref) && !is(ref, "List") )
		ref <- list(ref)
	if ( any(lengths(ref) != ncol(object)) )
		stop("length of reference [", length(ref[[1L]]), "] ",
			"does not match length of object [", length(object), "]")
	if ( verbose ) {
		lab <- if (length(ref) != 1L) "images" else "image"
		message("calculating colocalization with ", length(ref), " ", lab)
	}
	FUN <- function(x)
	{
		vapply(ref, function(y) {
			cor <- cor(y, x, use="pairwise.complete.obs")
			c(cor=cor, coscore(y, x, threshold=threshold))
		}, numeric(5L))
	}
	scores <- chunkApply(spectra(object), 1L, FUN,
		nchunks=nchunks, verbose=verbose,
		BPPARAM=BPPARAM, ...)
	scores <- simplify2array(lapply(scores, t))
	ans <- apply(scores, 1L, function(sc)
		{
			sc <- as.data.frame(t(sc))
			data <- .rank_featureData(object, sc, sort.by)
			head(data, n=n)
		})
	if ( length(ans) > 1L ) {
		ans
	} else {
		ans[[1L]]
	}
})

setMethod("colocalized", "SpatialDGMM",
	function(object, ref,
		threshold = median, n = Inf,
		sort.by = c("MOC", "M1", "M2", "Dice", "none"),
		nchunks = getCardinalNChunks(),
		verbose = getCardinalVerbose(),
		BPPARAM = getCardinalBPPARAM(), ...)
{
	sort.by <- match.arg(sort.by)
	if ( is.factor(ref) || is.character(ref) ) {
		ref <- as.factor(ref)
		lvl <- setNames(levels(ref), levels(ref))
		ref <- lapply(lvl, function(ci) ref %in% ci)
	}
	if ( !is.list(ref) && !is(ref, "List") )
		ref <- list(ref)
	if ( any(lengths(ref) != nrow(pixelData(object))) )
		stop("length of reference [", length(ref[[1L]]), "] ",
			"does not match length of object [", nrow(pixelData(object)), "]")
	if ( verbose ) {
		lab <- if (length(ref) != 1L) "images" else "image"
		message("calculating colocalization with ", length(ref), " ", lab)
	}
	FUN <- function(x)
	{
		vapply(ref, function(y) {
			scs <- lapply(levels(x),
				function(lvl) coscore(y, as.factor(x) == lvl))
			scs[[which.max(vapply(scs, max, numeric(1L), na.rm=TRUE))]]
		}, numeric(4L))
	}
	scores <- chunkLapply(object$class, FUN,
		nchunks=nchunks, verbose=verbose,
		BPPARAM=BPPARAM, ...)
	scores <- simplify2array(lapply(scores, t))
	ans <- apply(scores, 1L, function(sc)
		{
			sc <- as.data.frame(t(sc))
			data <- .rank_featureData(object, sc, sort.by)
			head(data, n=n)
		})
	if ( length(ans) > 1L ) {
		ans
	} else {
		ans[[1L]]
	}
})

.rank_featureData <- function(object, importance, sort.by)
{
	data <- featureData(object)
	data$i <- seq_len(nrow(data))
	if ( is(data, "XDataFrame") ) {
		keep <- c("i", unlist(keys(data)))
	} else {
		keep <- "i"
	}
	data <- data[keep]
	data <- cbind(data, importance)
	if ( sort.by != "none" ) {
		data <- as(data, "DFrame", strict=TRUE)
		i <- order(data[[sort.by]], decreasing=TRUE)
		data <- data[i,,drop=FALSE]
	}
	data
}

