
setMethod("pixelApply", "SImageSet",
	function(.object, .fun, ...,
			.pixel,
			.feature,
			.feature.groups,
			.pixel.dependencies,
			.simplify = TRUE,
			.use.names = TRUE,
			.verbose = FALSE) {
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
