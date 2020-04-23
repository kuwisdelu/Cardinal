
setMethod("pixelApply", "SparseImagingExperiment",
	function(.object, .fun, ...,
			.simplify = TRUE,
			.outpath = NULL,
			.params = list(),
			.blocks = getCardinalNumBlocks(),
			.verbose = getCardinalVerbose(),
			.view = "element",
			BPREDO = list(),
			BPPARAM = getCardinalBPPARAM())
	{
		.checkForIncompleteProcessing(.object)
		alist <- list(idx=seq_len(ncol(.object)))
		alist <- c(alist, .params)
		chunk_apply(iData(.object),
			FUN=.fun,
			MARGIN=2L,
			...,
			simplify=.simplify,
			chunks=.blocks,
			view=.view,
			attr=list(mcols=fData(.object)),
			alist=alist,
			outfile=.outpath,
			verbose=.verbose,
			BPREDO=BPREDO,
			BPPARAM=BPPARAM)
	})

setMethod("pixelApply", "SImageSet",
	function(.object, .fun, ...,
			.pixel,
			.feature,
			.feature.groups,
			.simplify = TRUE,
			.use.names = TRUE)
	{
		.Deprecated_Cardinal1()
		# set up subset variables if not provided
		if ( !missing(.pixel) )
			.pixel <- tryCatch(eval(substitute(.pixel), envir=pData(.object),
				enclos=environment(.fun)), error=function(e) eval(.pixel))
		if ( missing(.pixel) || is.null(.pixel) )
			.pixel <- rep(TRUE, nrow(.object@pixelData))
		.pixel <- pixels(.object)[.pixel]
		if ( !missing(.feature) )
			.feature <- tryCatch(eval(substitute(.feature), envir=fData(.object),
				enclos=environment(.fun)), error=function(e) eval(.feature))
		if ( missing(.feature) || is.null(.feature) )
			.feature <- rep(TRUE, nrow(.object@featureData))
		.feature <- features(.object)[.feature]
		# set up grouping variables if not provided
		if ( !missing(.feature.groups) )
			.feature.groups <- tryCatch(eval(substitute(.feature.groups), envir=fData(.object),
				enclos=environment(.fun)), error=function(e) eval(.feature.groups))
		if ( missing(.feature.groups) || is.null(.feature.groups) )
			.feature.groups <- factor(integer(length(.feature)), labels="")
		.feature.groups <- as.factor(.feature.groups)
		if ( !length(.feature.groups) %in% c(length(.feature), nrow(.object)) )
			.stop("'.feature.groups' must have length equal to '.feature' or '.object' feature extent")
		if ( length(.feature) != length(.feature.groups) )
			.feature.groups <- .feature.groups[.feature]
		groups <- split(.feature, .feature.groups, drop=TRUE)
		# set up function environment
		parent <- environment(.fun)
		if ( is.null(parent) )
			parent <- emptyenv()
		env <- new.env(parent=parent)
		if ( length(fData(.object)) != 0 )
			multiassign(names(fData(.object)), fData(.object), envir=env)
		assign(".Object", .object, envir=env)
		environment(.fun) <- env
		# prepare and calculate result
		ans <- vector("list", length(.pixel))
		for ( i in seq_along(.pixel) ) {
			ans[[i]] <- sapply(groups, function(j) {
				assign(".Index", .pixel[i], envir=env)
				.fun(iData(.object)[j, .pixel[i]], ...)
			}, simplify=.simplify, USE.NAMES=.use.names)
			if ( !.simplify && length(groups) == 1 )
				ans[[i]] <- ans[[i]][[1]]
		}
		# simplify result
		if ( .use.names ) names(ans) <- names(.pixel)
		if ( .simplify ) ans <- simplify2array(ans)
		if ( .simplify && length(dim(ans)) > 2 && any(dim(ans) == 1) ) {
			dmn <- dimnames(ans)
			rmInd <- which(dim(ans) == 1)
			dim(ans) <- dim(ans)[-rmInd]
			dimnames(ans) <- dmn[-rmInd]
		}
		if ( .simplify && length(dim(ans)) == 1 )
			ans <- as.matrix(ans)
		ans
	})
