

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
		if ( length(data) != dim[2] )
			data <- rep(data, length.out=dim[2])
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

setValidity("Hashmat", function(object) {
	msg <- validMsg(NULL, NULL)
	if ( any(duplicated(object@keys)) )
		msg <- validMsg(msg, "elements of keys must be unique")
	dm <- object@dim
	if ( dm[1] != length(object@keys) )
		msg <- validMsg(msg, paste("dims [", dm[1], "] does not match the length of keys [",
			length(object@data), "]", sep=""))
	if ( dm[2] != length(object@data) )
		msg <- validMsg(msg, paste("dims [", dm[2], "] does not match the length of data [",
			length(object@data), "]", sep=""))
	dmn <- object@dimnames
	if ( length(dmn) != 2 )
		msg <- validMsg(msg, paste("length of 'dimnames' [",
			length(dmn), "] must match that of 'dims' [2]", sep=""))
	if ( !is.null(dmn[[1]]) && length(dmn[[1]]) != dm[1] )
		msg <- validMsg(msg, paste("length of 'dimnames' [",
			length(dmn[[1]]), "] not equal to array extent", sep=""))
	if ( !is.null(dmn[[2]]) && length(dmn[[2]]) != dm[2] )
		msg <- validMsg(msg, paste("length of 'dimnames' [",
			length(dmn[[2]]), "] not equal to array extent", sep=""))
	if (is.null(msg)) TRUE else msg
})

setMethod("dim", "Hashmat", function(x) x@dim)

setReplaceMethod("dim", "Hashmat", function(x, value) {
	x@dim <- value
	x
})

setMethod("dimnames", "Hashmat", function(x) {
	if ( is.null(x@dimnames[[1]]) && is.null(x@dimnames[[2]]) ) {
		NULL
	} else {
		x@dimnames
	}
})

setReplaceMethod("dimnames", "Hashmat", function(x, value) {
	if ( is.null(value) ) {
		x@dimnames <- list(NULL, NULL)
	} else {
		x@dimnames <- value
	}
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

# setMethod("nrow", "Hashmat", function(x) x@dim[1])

# setMethod("ncol", "Hashmat", function(x) x@dim[2])

setMethod("pData", "Hashmat", function(object) object@data)

setReplaceMethod("pData", "Hashmat",
	function(object, value) {
		object@data <- value
		if ( validObject(object) )
			object
	})

setMethod("keys", "Hashmat", function(object) object@keys)

setReplaceMethod("keys", c("Hashmat", "character"), function(object, value) {
	object@dimnames <- list(names(value), object@dimnames[[2]])
	object@dim <- c(length(value), object@dim[2])
	object@keys <- value
	if ( validObject(object) )
		object
})

setReplaceMethod("keys", c("Hashmat", "list"), function(object, value) {
	object@data <- mapply(function(x, y) {
		names(x) <- object@keys[y]
		x
	}, object@data, value, SIMPLIFY=FALSE)
	if ( validObject(object) )
		object
})

# setMethod("lapply", "Hashmat",
# 	function(X, FUN, ...) {
# 		lapply(X@data, FUN=FUN, ...)
# 	})

# setMethod("sapply", "Hashmat",
# 	function(X, FUN, ..., simplify=TRUE, USE.NAMES=TRUE) {
# 		sapply(X@data, FUN=FUN, ..., simplify=TRUE, USE.NAMES=TRUE)
# 	})

setMethod("[", "Hashmat", function(x, i, j, ..., drop) {
	if ( missing(i) ) i <- seq_len(dim(x)[1])
	if ( missing(j) ) j <- seq_len(dim(x)[2])
	if ( !missing(drop) && is.na(drop) ) {
		# return a subset of class Hashmat
		new("Hashmat",
			data=x@data[j],
			keys=x@keys[i],
			dim=c(length(x@keys[i]), length(x@data[j])),
			dimnames=list(rownames(x)[i], colnames(x)[j]))
	} else {
		# reconstruct the dense matrix and return a subset of it
		if ( missing(drop) ) drop <- TRUE
		xsub <- sapply(x@data[j], function(xi) {
			if ( length(i) < length(xi) ) {
				xout <- xi[x@keys[i]]
				xout[is.na(xout)] <- 0
			} else {
				xout <- numeric(length(i))
				names(xout) <- x@keys[i]
				nm <- names(xi)[names(xi) %in% names(xout)]
				xout[nm] <- xi[nm]
			}
			xout
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
	}
})

setReplaceMethod("[", "Hashmat", function(x, i, j, ..., value) {
	if ( missing(i) ) i <- seq_len(dim(x)[1])
	if ( missing(j) ) j <- seq_len(dim(x)[2])
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
		if ( prod(dim(y)) == 0 )
			return(x)
		if ( prod(dim(x)) == 0 )
			return(y)
		xdim <- dimnames(x)
		ydim <- dimnames(y)
		if ( is.null(xdim) || is.null(ydim) ||
				any(sapply(xdim, is.null)) ||
				any(sapply(ydim, is.null)) )
			.stop("Hashmat objects must have dimnames for 'combine'")
		sharedRows <- intersect(xdim[[1]], ydim[[1]])
		sharedCols <- intersect(xdim[[2]], ydim[[2]])
		ok <- all.equal(x[sharedRows, sharedCols], y[sharedRows, sharedCols])
		if (!isTRUE(ok))
			.stop("Hashmat shared row and column elements differ: ", ok)
		unionRows <- union(xdim[[1]], ydim[[1]])
		unionCols <- union(xdim[[2]], ydim[[2]])
		m <- Hashmat(nrow=length(unionRows), ncol=length(unionCols),
			dimnames=list(unionRows, unionCols))
		m[rownames(x), colnames(x)] <- x
		m[rownames(y), colnames(y)] <- y
		m
	})

setMethod("cbind", "Hashmat", function(..., deparse.level=1) {
	obs <- list(...)
	ncol <- sapply(obs, ncol)
	nrow <- sapply(obs, nrow)
	if ( length(unique(nrow)) != 1 )
		.stop("number of rows must match")
	keys <- lapply(obs, keys)
	if ( length(setdiff(unlist(keys), keys[[1]])) != 0 )
		.stop("keys must match")
	new(class(obs[[1]]),
		data=unlist(lapply(obs, pData), recursive=FALSE),
		keys=keys[[1]],
		dim=c(length(keys[[1]]), sum(unlist(ncol))),
		dimnames=list(rownames(obs[[1]]),
			unlist(sapply(obs, colnames))))
})

setMethod("rbind", "Hashmat", function(..., deparse.level=1) {
	.stop("cannot 'rbind' Hashmat objects")
})

setMethod("show", "Hashmat", function(object) {
	cat("An object of class '", class(object), "'\n", sep="")
	cat("  ", object@dim[[1]], " x ", object@dim[[2]],
		" sparse matrix", "\n", sep="")
})

