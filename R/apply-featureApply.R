
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

