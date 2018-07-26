
setMethod("pixelApply", "SparseImagingExperiment",
	function(.object, .fun, ...,
			.blocks = FALSE,
			.simplify = TRUE,
			.use.names = TRUE,
			.outpath = NULL,
			BPREDO = list(),
			BPPARAM = bpparam())
	{
		.fun <- match.fun(.fun)
		serial <- is(BPPARAM, "SerialParam")
		remote <- !is.null(.outpath)
		if ( .blocks ) {
			if ( !is.logical(.simplify) )
				reduce_blocks <- match.fun(.simplify)
			.simplify <- !is.logical(.simplify)
			if ( is.numeric(.blocks) ) {
				idx <- blocks(seq_len(ncol(.object)), .blocks)
			} else {
				idx <- blocks(seq_len(ncol(.object)), 20L)
			}
		} else {
			idx <- seq_len(ncol(.object))
		}
		pid <- ipcid()
		if ( remote ) {
			.outpath <- .outpath[1L]
			.message("using outpath = ", .outpath)
			rwrite <- .remote_writer(pid, .outpath)
		}
		if ( serial )
			.message(progress="start", max=length(idx))
		ans <- bplapply(idx, function(i) {
			suppressPackageStartupMessages(require(Cardinal))
			x <- iData(.object)[,i,drop=!.blocks]
			attr(x, "idx") <- i
			attr(x, "mcols") <- fData(.object)
			res <- .fun(x, ...)
			if ( remote )
				res <- rwrite(res)
			if ( serial )
				.message(progress="increment")
			res
		}, BPREDO=BPREDO, BPPARAM=BPPARAM)
		if ( serial )
			.message(progress="stop")
		if ( remote )
			ans <- .remote_collect(ans, .outpath, .simplify)
		if ( .use.names && !.blocks )
			if ( remote && is(ans, "matter_mat") ) {
				colnames(ans) <- pixelNames(.object)
			} else {
				names(ans) <- pixelNames(.object)
			}
		if ( .simplify ) {
			if ( .blocks ) {
				ans <- reduce_blocks(ans)
			} else if ( !remote ) {
				ans <- drop(simplify2array(ans))
			}
		}
		ipcremove(pid)
		ans
	})

setMethod("pixelApply", "SImageSet",
	function(.object, .fun, ...,
			.pixel,
			.feature,
			.feature.groups,
			.pixel.dependencies,
			.simplify = TRUE,
			.use.names = TRUE,
			.verbose = FALSE)
	{
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
		.message(progress="start", max=length(.pixel))
		if ( isTRUE(getOption("Cardinal.debug.pixelApply")) ) browser()
		for ( i in seq_along(.pixel) ) {
			ans[[i]] <- sapply(groups, function(j) {
				assign(".Index", .pixel[i], envir=env)
				.fun(iData(.object)[j, .pixel[i]], ...)
			}, simplify=.simplify, USE.NAMES=.use.names)
			if ( !.simplify && length(groups) == 1 )
				ans[[i]] <- ans[[i]][[1]]
			.message(progress="increment")
		}
		.message(progress="stop")
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
