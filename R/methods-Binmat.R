

setMethod("initialize", "Binmat",
	function(.Object,
			files = factor(),
			offsets = numeric(),
			extents = numeric(),
			datatype = factor(),
			dim = c(0, 0),
			dimnames = list(NULL, NULL),
			nrow,
			ncol,
			...)
	{
		if ( missing(dim) && !missing(nrow) && !missing(ncol))
			dim <- c(nrow, ncol)
		names(files) <- dimnames[[2]]
		names(offsets) <- dimnames[[2]]
		names(extents) <- dimnames[[2]]
		names(datatype) <- dimnames[[2]]
		callNextMethod(.Object,
			files=files,
			offsets=offsets,
			extents=extents,
			datatype=datatype,
			dim=dim,
			dimnames=dimnames,
			...)
	})

Binmat <- function(
	files,
	nrow, ncol,
	offsets = 0,
	extents = rep(nrow, ncol),
	datatype = c("16-bit integer",
		"32-bit integer",
		"64-bit integer",
		"32-bit float",
		"64-bit float"),
	dimnames = NULL,
	...)
{
	if ( !missing(nrow) && !missing(ncol)) {
		dim <- c(nrow, ncol)
	} else if ( !missing(extents) && !missing(offsets) ) {
		dim <- c(extents[1], length(offsets))
		nrow <- dim[1]
		ncol <- dim[2]
	} else {
		.stop("must specify either 'nrow' and 'ncol', or 'extents' and 'offsets'")
	}
	files <- as.factor(rep(normalizePath(files), dim[2]))
	datatype <- as.factor(rep(match.arg(datatype), dim[2]))
	if ( length(offsets) != dim[2] )
		offsets <- seq.int(offsets[1],
			by=sizeof(datatype) * dim[1],
			length.out=dim[2])
	if ( length(extents) != dim[2] )
		extents <- rep(extents, length.out=dim[2])
	if ( is.null(dimnames) ) dimnames <- list(NULL, NULL)
	.Binmat(files=files,
		offsets=offsets,
		extents=extents,
		datatype=datatype,
		dimnames=dimnames,
		nrow=nrow,
		ncol=ncol,
		...)
}

setValidity("Binmat", function(object) {
	msg <- validMsg(NULL, NULL)
	dm <- object@dim
	if ( dm[2] != length(object@files) )
		msg <- validMsg(msg, paste("dims [", dm[2], "] does not match the length of files [",
			length(object@files), "]", sep=""))
	if ( dm[2] != length(object@datatype) )
		msg <- validMsg(msg, paste("dims [", dm[2], "] does not match the length of datatype [",
			length(object@datatype), "]", sep=""))
	if ( any(dm[1] != object@extents) )
		msg <- validMsg(msg, paste("dims [", dm[1], "] does not match the extents [",
			object@extents[1], "]", sep=""))
	if ( dm[2] != length(object@extents) )
		msg <- validMsg(msg, paste("dims [", dm[2], "] does not match the length of extents [",
			length(object@extents), "]", sep=""))
	if ( dm[2] != length(object@offsets) )
		msg <- validMsg(msg, paste("dims [", dm[2], "] does not match the length of offsets [",
			length(object@offsets), "]", sep=""))
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

setMethod("dim", "Binmat", function(x) x@dim)

setReplaceMethod("dim", "Binmat", function(x, value) {
	x@dim <- value
	x
})

setMethod("dimnames", "Binmat", function(x) {
	if ( is.null(x@dimnames[[1]]) && is.null(x@dimnames[[2]]) ) {
		NULL
	} else {
		x@dimnames
	}
})

setReplaceMethod("dimnames", "Binmat", function(x, value) {
	if ( is.null(value) ) {
		x@dimnames <- list(NULL, NULL)
	} else {
		x@dimnames <- value
	}
	names(offsets) <- x@dimnames[[2]]
	names(extents) <- x@dimnames[[2]]
	x
})

setMethod("rownames", "Binmat", function(x) x@dimnames[[1]])

setReplaceMethod("rownames", "Binmat", function(x, value) {
	x@dimnames[[1]] <- value
	x
})

setMethod("colnames", "Binmat", function(x) x@dimnames[[2]])

setReplaceMethod("colnames", "Binmat", function(x, value) {
	x@dimnames[[2]] <- value
	names(offsets) <- x@dimnames[[2]]
	names(extents) <- x@dimnames[[2]]
	x
})

setMethod("[", "Binmat", function(x, i, j, ..., drop) {
	if ( missing(i) ) i <- seq_len(dim(x)[1])
	if ( missing(j) ) j <- seq_len(dim(x)[2])
	is <- seq_len(dim(x)[1])
	names(is) <- rownames(x)
	js <- seq_len(dim(x)[2])
	names(js) <- colnames(x)
	readBinarySubmatrix(x, is[i], js[j], ..., drop=drop)
})

readBinarySubmatrix <- function(x, i, j, ..., drop) {
	is <- sort(i)
	js <- sort(j)
	count <- as.integer(length(js))
	offsets <- as.double(x@offsets[js] + min(is - 1) * sizeof(x@datatype[js]))
	extents <- pmin(x@extents[js], rep(as.integer(diff(range(is)) + 1), count))
	if ( nlevels(x@files) == 1 ) {
		xsub <- .Call("readIbdIntensityArray", as.character(x@files[1]),
			"processed", as.character(x@datatype[1]), offsets, extents, count)
	} else {
		fun <- function(file, datatype, offset, extent) {
			.Call("readIbdIntensityArray", as.character(file), "processed",
				as.character(datatype), offset, extent, as.integer(1))[[1]]
		}
		xsub <- mapply(fun, x@files[js], x@datatype[js], offsets, extents)
	}
	xsub <- simplify2array(xsub)
	dim(xsub) <- c(diff(range(is)) + 1, length(js))
	names(dim(xsub)) <- names(dim(x))
	dimnames(xsub) <- list(rownames(x)[min(is):max(is)], colnames(x)[js])
	xsub[i - min(is) + 1, j - min(js) + 1, ..., drop=drop]
}

setMethod("cbind", "Binmat", function(..., deparse.level=1) {
	obs <- list(...)
	ncol <- sapply(obs, ncol)
	nrow <- sapply(obs, nrow)
	if ( length(unique(nrow)) != 1 )
		.stop("number of rows must match")
	new(class(obs[[1]]),
		files=as.factor(unlist(lapply(obs, function(x) x@files), recursive=FALSE)),
		offsets=as.integer(unlist(lapply(obs, function(x) x@offsets), recursive=FALSE)),
		extents=as.integer(unlist(lapply(obs, function(x) x@extents), recursive=FALSE)),
		datatype=as.factor(unlist(lapply(obs, function(x) x@datatype), recursive=FALSE)),
		dim=c(nrow[1], sum(unlist(ncol))),
		dimnames=list(rownames(obs[[1]]),
			unlist(sapply(obs, colnames))))
})

setMethod("rbind", "Binmat", function(..., deparse.level=1) {
	.stop("cannot 'rbind' Binmat objects")
})

setMethod("show", "Binmat", function(object) {
	cat("An object of class '", class(object), "'\n", sep="")
	cat("  ", object@dim[[1]], " x ", object@dim[[2]],
		" on-disk binary matrix", "\n", sep="")
})

