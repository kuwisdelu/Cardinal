
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
		sort.by = c("cor", "MOC", "M1", "M2", "Dice"),
		nchunks = getCardinalNChunks(),
		verbose = getCardinalVerbose(),
		BPPARAM = getCardinalBPPARAM(), ...)
{
	sort.by <- match.arg(sort.by)
	if ( !missing(i) && !is.null(i) ) {
		ref <- as.matrix(spectra(object)[i,,drop=FALSE])
		ref <- apply(ref, 1L, identity, simplify=FALSE)
	}
	if ( !is(ref, "List") && !is.list(ref) )
		ref <- list(ref)
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
	ans <- apply(scores, 1L, function(s)
		{
			s <- as.data.frame(t(s))
			data <- featureData(object)
			data$i <- seq_len(nrow(data))
			data[names(s)] <- s
			if ( is(data, "XDataFrame") ) {
				nms <- c("i", union(unlist(keys(data)), names(s)))
			} else {
				nms <- c("i", names(s))
			}
			data <- data[nms]
			if ( sort.by != "none" ) {
				data <- as(data, "DFrame", strict=TRUE)
				j <- order(data[[sort.by]], decreasing=TRUE)
				data <- data[j,,drop=FALSE]
			}
			head(data, n=n)
		})
	if ( length(ans) > 1L ) {
		ans
	} else {
		ans[[1L]]
	}
})

