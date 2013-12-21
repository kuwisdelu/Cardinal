
## Adapted from combine(matrix, matrix) from BiocGenerics
setMethod("combine",
	signature = c(x = "array", y = "array"),
	function(x, y, ...) {
		if ( length(y) == 0 )
			return(x)
		if ( length(x) == 0 )
			return(y)
		if (mode(x) != mode(y))
			stop("array modes ", mode(x), ", ", mode(y), " differ")
		if (typeof(x) != typeof(y))
			warning("array typeof ", typeof(x), ", ", typeof(y), " differ")
		xdim <- dimnames(x)
		ydim <- dimnames(y)
		if ( is.null(xdim) || is.null(ydim) ||
				any(sapply(xdim, is.null)) ||
				any(sapply(ydim, is.null)) )
			stop("arrays must have dimnames for 'combine'")
		sharedDims <- mapply(intersect, xdim, ydim, SIMPLIFY=FALSE)
		ok <- all.equal(do.call("[", c(list(x), sharedDims)),
			do.call("[", c(list(x), sharedDims)))
		if ( !isTRUE(ok) )
			stop("array shared row and column elements differ: ", ok)
		unionDims <- mapply(union, xdim, ydim, SIMPLIFY=FALSE)
		arr <- array(new(class(as.vector(x))),
			dim=sapply(unionDims, length),
			dimnames=unionDims)
		arr <- do.call("[<-", c(list(arr), dimnames(x), list(x)))
		arr <- do.call("[<-", c(list(arr), dimnames(y), list(y)))
		arr
	})

