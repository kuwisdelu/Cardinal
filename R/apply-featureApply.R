
setMethod("featureApply", "SparseImagingExperiment",
	function(.object, .fun, ...,
			.simplify = TRUE,
			.use.names = TRUE,
			.chunks = 0L,
			.chunksize = nrow(.object) / .chunks,
			.cache.chunks = FALSE,
			BPPARAM = bpparam())
	{
		.fun <- match.fun(.fun)
		.args <- list(...)
		if ( is.null(environment(.fun)) ) {
			e <- new.env(parent=parent.frame(2))
		} else {
			e <- new.env(parent=environment(.fun))
		}
		environment(.fun) <- e
		if ( .chunksize > 0L && is.finite(.chunksize)  ) {
			.blockfun <- function(X, FUNC, ARGS, PRELOAD, ...) {
				suppressPackageStartupMessages(require(Cardinal))
				assign(".object", X, envir=environment(FUNC))
				if ( PRELOAD )
					iData(X) <- as.matrix(iData(X))
				lapply(seq_len(nrow(X)), function(i) {
					assign("..i..", i, envir=environment(FUNC))
					do.call(FUNC, c(list(iData(X)[i,]), ARGS))
				})
			}
			ans <- .featureBlockApply(.object, .blockfun,
				FUNC=.fun, ARGS=.args, PRELOAD=.cache.chunks,
				.chunks=.chunks, .chunksize=.chunksize,
				BPPARAM=BPPARAM)
		} else {
			assign(".object", .object, envir=environment(.fun))
			.indexfun <- function(i, OBJ, FUNC, ARGS, ...) {
				suppressPackageStartupMessages(require(Cardinal))
				assign("..i..", i, envir=environment(FUNC))
				do.call(FUNC, c(list(iData(OBJ)[i,]), ARGS))
			}
			ans <- bplapply(seq_len(nrow(.object)), .indexfun,
				OBJ=.object, FUNC=.fun, ARGS=.args, BPPARAM=BPPARAM)
		}
		if ( .use.names )
			names(ans) <- featureNames(.object)
		if ( .simplify )
			ans <- drop(simplify2array(ans))
		ans
	})

.featureIterator <- function(obj, n = 1000L) {
	i <- 1L
	n <- min(n, nrow(obj))
	function() {
		if ( i > nrow(obj) )
			return(NULL)
		i2 <- min(i + n - 1L, nrow(obj))
		idx <- i:i2
		i <<- i + n
		if ( length(i2) > 0L ) {
			obj[idx,]
		} else {
			NULL
		}
	}
}

.featureBlockApply <- function(.object, .fun, ...,
							.chunks, .chunksize,
							REDUCE = combine,
							init = NULL,
							reduce.in.order = TRUE,
							BPPARAM = bpparam())
{
	iter <- .featureIterator(.object, floor(.chunksize))
	if ( bpprogressbar(BPPARAM) && !missing(.chunks) )
		.message("expected iterations: ", .chunks + 1L)
	bpiterate(iter, .fun, ..., REDUCE=REDUCE, init=init,
		reduce.in.order=reduce.in.order, BPPARAM=BPPARAM)
}

setMethod("featureApply", "SImageSet",
	function(.object, .fun, ...,
			.feature,
			.pixel,
			.pixel.groups,
			.feature.dependencies,
			.simplify = TRUE,
			.use.names = TRUE,
			.verbose = FALSE) {
		# set up subset variables if not provided
		if ( !missing(.feature) )
			.feature <- tryCatch(eval(substitute(.feature), envir=fData(.object),
				enclos=environment(.fun)), error=function(e) eval(.feature))
		if ( missing(.feature) || is.null(.feature) )
			.feature <- rep(TRUE, nrow(.object@featureData))
		.feature <- features(.object)[.feature]
		if ( !missing(.pixel) )
			.pixel <- tryCatch(eval(substitute(.pixel), envir=pData(.object),
				enclos=environment(.fun)), error=function(e) eval(.pixel))
		if ( missing(.pixel) || is.null(.pixel) )
			.pixel <- rep(TRUE, nrow(.object@pixelData))
		.pixel <- pixels(.object)[.pixel]
		# set up grouping variables if not provided
		if ( !missing(.pixel.groups) )
			.pixel.groups <- tryCatch(eval(substitute(.pixel.groups), envir=pData(.object),
				enclos=environment(.fun)), error=function(e) eval(.pixel.groups))
		if ( missing(.pixel.groups) || is.null(.pixel.groups) )
			.pixel.groups <- factor(integer(length(.pixel)), labels="")
		.pixel.groups <- as.factor(.pixel.groups)
		if ( !length(.pixel.groups) %in% c(length(.pixel), ncol(.object)) )
			.stop("'.pixel.groups' must have length equal to '.pixel' or '.object' pixel extent")
		if ( length(.pixel) != length(.pixel.groups) )
			.pixel.groups <- .pixel.groups[.pixel]
		groups <- split(.pixel, .pixel.groups, drop=TRUE)
		# set up function environment
		parent <- environment(.fun)
		if ( is.null(parent) )
			parent <- emptyenv()
		env <- new.env(parent=parent)
		if ( length(pData(.object)) != 0 )
			multiassign(names(pData(.object)), pData(.object), envir=env)
		assign(".Object", .object, envir=env)
		environment(.fun) <- env
		# prepare and calculate result
		ans <- vector("list", length(.feature))
		.message(progress="start", max=length(.feature))
		if ( isTRUE(getOption("Cardinal.debug.featureApply")) ) browser()
		for ( i in seq_along(.feature) ) {
			ans[[i]] <- sapply(groups, function(j) {
				assign(".Index", .feature[i], env)
				.fun(iData(.object)[.feature[i], j], ...)
			}, simplify=.simplify, USE.NAMES=.use.names)
			if ( !.simplify && length(groups) == 1 )
				ans[[i]] <- ans[[i]][[1]]
			.message(progress="increment")
		}
		.message(progress="stop")
		# simplify result
		if ( .use.names ) names(ans) <- names(.feature)
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

