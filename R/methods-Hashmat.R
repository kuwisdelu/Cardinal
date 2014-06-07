

setMethod("initialize", "Hashmat",
	function(.Object,
			data = list(),
			keys = character(),
			dim = c(0, 0),
			dimnames = list(NULL, NULL),
			nrow,
			ncol,
			...)
	{
		if ( missing(dim) && !missing(nrow) && !missing(ncol))
			dim <- c(nrow, ncol)
		if ( length(data) != dim[[2]] )
			data <- rep(data, length.out=dim[[2]])
		names(keys) <- dimnames[[1]]
		names(data) <- dimnames[[2]]
		callNextMethod(.Object,
			data=data,
			keys=keys,
			dim=dim,
			dimnames=dimnames,
			...)
	})

Hashmat <- function(data = NA, nrow = 1, ncol = 1, byrow=FALSE,
	dimnames = NULL, ...)
{
	if ( ncol == 0 ) {
		data <- list(numeric())
	} else if ( is.vector(data) && !is.list(data) ) {
		data <- matrix(data, nrow=nrow, ncol=ncol, byrow=byrow)
	}
	if ( is.matrix(data) ) {
		nrow <- nrow(data)
		ncol <- ncol(data)
		data <- lapply(seq_len(ncol(data)), function(i) {
			key <- which(data[,i] != 0)
			x <- data[key,i]
			names(x) <- key
			x
		})
	}
	if ( is.null(dimnames) ) dimnames <- list(NULL, NULL)
	keys <- as.character(seq_len(nrow))
	.Hashmat(data=data,
		keys=keys,
		dimnames=dimnames,
		nrow=nrow,
		ncol=ncol,
		...)
}

setMethod("dim", "Hashmat", function(x) x@dim)

setReplaceMethod("dim", "Hashmat", function(x, value) {
	x@dim <- value
	x
})

setMethod("dimnames", "Hashmat", function(x) x@dimnames)

setReplaceMethod("dimnames", "Hashmat", function(x, value) {
	x@dimnames <- value
	names(x@keys) <- x@dimnames[[1]]
	names(x@data) <- x@dimnames[[2]]
	x
})

setMethod("rownames", "Hashmat", function(x) x@dimnames[[1]])

setReplaceMethod("rownames", "Hashmat", function(x, value) {
	x@dimnames[[1]] <- value
	names(x@keys) <- x@dimnames[[1]]
	x
})

setMethod("colnames", "Hashmat", function(x) x@dimnames[[2]])

setReplaceMethod("colnames", "Hashmat", function(x, value) {
	x@dimnames[[2]] <- value
	names(x@data) <- x@dimnames[[2]]
	x
})

setMethod("nrow", "Hashmat", function(x) x@dim[[1]])

setMethod("ncol", "Hashmat", function(x) x@dim[[2]])

setMethod("[", "Hashmat", function(x, i, j, ..., drop) {
	if ( missing(i) ) i <- seq_len(dim(x)[[1]])
	if ( missing(j) ) j <- seq_len(dim(x)[[2]])
	if ( missing(drop) ) drop <- TRUE
	xsub <- sapply(x@data[j], function(xi) {
		xi <- xi[x@keys[i]]
		xi[is.na(xi)] <- 0
		xi
	}, simplify=TRUE, USE.NAMES=FALSE)
	dim(xsub) <- c(length(i), length(j))
	names(dim(xsub)) <- names(dim(x))
	dimnames(xsub) <- list(rownames(x)[i], colnames(x)[j])
	if ( drop ) {
		if ( length(i) == 1 || length(j) == 1 )
			xsub <- as.vector(xsub)
		if ( length(i) == 1 && length(j) != 1 )
			names(xsub) <- colnames(x)[j]
		if ( length(i) != 1 && length(j) == 1 )
			names(xsub) <- rownames(x)[i]
	}
	xsub
})

setReplaceMethod("[", "Hashmat", function(x, i, j, ..., value) {
	if ( missing(i) ) i <- seq_len(dim(x)[[1]])
	if ( missing(j) ) j <- seq_len(dim(x)[[2]])
	if ( length(i) == 1 && length(j) == 1 ) {
		value <- list(value)
	} else if ( length(i) == 1 && length(j) != 1 ) {
		value <- as.list(value)
	} else if ( length(i) != 1 && length(j) == 1 ) {
		value <- list(value)
	} else {
		value <- lapply(seq_along(j), function(jj) value[,jj])
	}
	x@data[j] <- mapply(function(xi, vi) {
		xi[x@keys[i]] <- vi
		xi <- xi[xi != 0]
		xi
	}, x@data[j], value, SIMPLIFY=FALSE, USE.NAMES=FALSE)
	x
})

## Adapted from combine(matrix, matrix) from BiocGenerics
setMethod("combine",
	signature = c(x = "Hashmat", y = "Hashmat"),
	function(x, y, ...) {
		if ( length(y) == 0 )
			return(x)
		if ( length(x) == 0 )
			return(y)
		xdim <- dimnames(x)
		ydim <- dimnames(y)
		if ( is.null(xdim) || is.null(ydim) ||
				any(sapply(xdim, is.null)) ||
				any(sapply(ydim, is.null)) )
			stop("Hashmat objects must have dimnames for 'combine'")
		sharedRows <- intersect(xdim[[1]], ydim[[1]])
		sharedCols <- intersect(xdim[[2]], ydim[[2]])
		ok <- all.equal(x[sharedRows, sharedCols], y[sharedRows, sharedCols])
		if (!isTRUE(ok))
			stop("Hashmat shared row and column elements differ: ", ok)
		unionRows <- union(xdim[[1]], ydim[[1]])
		unionCols <- union(xdim[[2]], ydim[[2]])
		m <- Hashmat(nrow=length(unionRows), ncol=length(unionCols),
			dimnames=list(unionRows, unionCols))
		m[rownames(x), colnames(x)] <- x
		m[rownames(y), colnames(y)] <- y
		m
	})

