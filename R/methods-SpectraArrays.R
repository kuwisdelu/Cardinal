
#### SpectraArrays ####
## --------------------

# Similar to Assays class from SummarizedExperiment
# but allows 1-dimensional array and list elements
# and requires spectra to have unique array names

.valid_SpectraArrays <- function(object)
{
	errors <- NULL
	arrays <- try(as(object, "SimpleList"), silent=TRUE)
	if ( inherits(arrays, "try-error") )
		errors <- c(errors, "object must be coercible to SimpleList")
	if ( !is(arrays, "SimpleList") )
		errors <- c(errors, "coercion to SimpleList must return a SimpleList")
	all_nrows <- vapply(arrays, NROW, numeric(1L))
	all_ncols <- vapply(arrays, NCOL, numeric(1L))
	if ( !all(all_nrows == all_nrows[1L]) )
		errors <- c(errors, "all spectra arrays must have the same NROW")
	if ( !all(all_ncols == all_ncols[1L]) )
		errors <- c(errors, "all spectra arrays must have the same NCOL")
	if ( anyDuplicated(names(arrays)) )
		errors <- c(errors, "all spectra arrays must have unique names")
	if ( is.null(errors) ) TRUE else errors
}

setValidity("SpectraArrays", .valid_SpectraArrays)

setAs("list", "SpectraArrays",
    function(from) SpectraArrays(from))

setAs("SpectraArrays", "list",
    function(from) as.list(from@data))

setAs("SimpleList", "SpectraArrays",
    function(from) new("SpectraArrays", data=from))

setAs("SpectraArrays", "SimpleList", function(from) from@data)

setMethod("as.list", "SpectraArrays", function(x, ...) as.list(x@data))

SpectraArrays <- function(arrays = SimpleList())
{
	if ( is(arrays, "SpectraArrays") )
		return(arrays)
	if ( !is(arrays, "SimpleList") ) {
		if ( !is(arrays, "list_OR_List") )
			arrays <- SimpleList(arrays)
		arrays <- as(arrays, "SimpleList")
	}
	object <- as(arrays, "SpectraArrays")
	if ( is.null(names(object)) )
		names(object) <- make.unique(rep.int("intensity", length(object)))
	if ( validObject(object) )
		object
}

setMethod("show", "SpectraArrays",
	function(object) {
		cat(class(object), "of length", length(object), "\n")
		if ( length(object) == 0 )
			return()
		nms <- names(object)
		if ( is.null(nms) )
			nms <- character(length(object))
		arrays <- as(object, "SimpleList", strict=FALSE)
		# names()
		nms <- c(sprintf("names(%d):", length(names(object))), .select_head_tail(nms))
		cls <- vapply(arrays, function(a) class(a)[1L], character(1L))
		# class()
		cls <- c(sprintf("class(%d):", length(cls)), .select_head_tail(cls))
		# dim() or length()
		if ( is.null(dim(arrays[[1L]])) ) {
			dms_text <- "length(%d):"
			dms <- vapply(arrays, function(a) {
				paste0("<", length(a), ">")
			}, character(1L))
		} else {
			dms_text <- "dim(%d):"
			dms <- vapply(arrays, function(a) {
				d <- paste0(dim(a), collapse=" x ")
				paste0("<", d, ">")
			}, character(1L))
		}
		dms <- c(sprintf(dms_text, length(dms)), .select_head_tail(dms))
		# matter::mem()
		mem <- vapply(arrays, function(x) format(mem(x)), character(3L))
		real <- c(sprintf("real mem(%d):", length(mem[1L,])), .select_head_tail(mem[1L,]))
		shm <- c(sprintf("shared mem(%d):", length(mem[2L,])), .select_head_tail(mem[2L,]))
		vm <- c(sprintf("virtual mem(%d):", length(mem[3L,])), .select_head_tail(mem[3L,]))
		out <- rbind(cls, dms, real, shm, vm)
		colnames(out) <- nms
		rownames(out) <- character(nrow(out))
		print(out, quote = FALSE, right = TRUE)
	})

# Make matter::mem() report virtual memory correctly
setMethod("vm_used", "SpectraArrays",
	function(x) {
		arrays <- as(x, "SimpleList", strict=FALSE)
		vm <- vapply(arrays, function(y) vm_used(y), numeric(1L))
		if ( all(is.na(vm)) ) {
			matter::size_bytes(NA_real_)
		} else {
			matter::size_bytes(sum(vm, na.rm=TRUE))
		}
	})

.select_head_tail <- function(x, n = 6L)
{
	len <- length(x)
	n <- max(3L, n)
	if ( len > n ) {
		head <- ceiling(n / 2L)
		tail <- len - (n - head - 1L)
		nms <- x[c(1:head,tail:len)]
		c(as.character(nms[1:head]), "...", as.character(nms[-(1:head)]))
	} else {
		as.character(x)
	}
}

setMethod("fetch", "SpectraArrays",
	function(object, ...,
		verbose = getCardinalVerbose(),
		BPPARAM = bpparam())
	{
		for ( i in seq_along(object) ) {
			if ( is.matter(object[[i]]) ) {
				object[[i]] <- fetch(object[[i]], ...,
					verbose=verbose, BPPARAM=BPPARAM)
			} else {
				object[[i]] <- as.shared(object[[i]])
			}
		}
		object
	})

setMethod("flash", "SpectraArrays",
	function(object, ...,
		verbose = getCardinalVerbose(),
		BPPARAM = bpparam())
	{
		for ( i in seq_along(object) ) {
			if ( is.matter(object[[i]]) ) {
				object[[i]] <- flash(object[[i]], ...,
					verbose=verbose, BPPARAM=BPPARAM)
			} else {
				object[[i]] <- as.matter(object[[i]])
			}
		}
		object
	})

## List-like getters and setters

setMethod("length", "SpectraArrays",
	function(x) {
		x <- as(x, "SimpleList")
		callGeneric()
	})

setMethod("names", "SpectraArrays",
	function(x) {
		x <- as(x, "SimpleList")
		callGeneric()
	})
setReplaceMethod("names", "SpectraArrays",
	function(x, value) {
		cls <- class(x)
		x <- as(x, "SimpleList")
		as(callGeneric(), cls)
	})

setMethod("[[", "SpectraArrays",
	function(x, i, exact = TRUE) {
		x <- as(x, "SimpleList")
		callGeneric()
	})
setReplaceMethod("[[", "SpectraArrays",
	function(x, i, value) {
		cls <- class(x)
		x <- as(x, "SimpleList")
		ans <- as(callGeneric(), cls)
		if ( validObject(ans) )
			ans
	})

setMethod("$", "SpectraArrays",
	function(x, name) x[[name, exact=FALSE]])
setReplaceMethod("$", "SpectraArrays",
	function(x, name, value) {
		x[[name]] <- value
		x
	})

# allow tab completion in console
.DollarNames.SpectraArrays <- function(x, pattern = "") {
	grep(pattern, names(x), value=TRUE)
}

## Array-like getters and setters

setMethod("dim", "SpectraArrays",
	function(x) {
		len <- length(x)
		if ( len == 0L )
			return(0L)
		if ( length(dim(x[[1L]])) >= 2L ) {
			c(dim(x[[1L]])[1:2], len)
		} else {
			c(NROW(x[[1L]]), len)
		}
	})

.get_SpectraArrays_subset <- function(x, i, j)
{
	if ( length(dim(x)) > 2L ) {
		subscripts <- list(
			if (missing(i)) quote(expr=) else i,
			if (missing(j)) quote(expr=) else j)
	} else {
		subscripts <- list(if (missing(i)) quote(expr=) else i)
	}
	get_array_subset <- function(a)
	{
		drop <- if (is.matter(a)) NULL else FALSE
		ndim <- if (is.null(dim(a))) 1L else length(dim(a))        
		more_subscripts <- rep.int(list(quote(expr=)), ndim - length(subscripts))
		args <- c(list(a), subscripts, more_subscripts, list(drop=drop))
		do.call(`[`, args)
	}
	arrays <- as(x, "SimpleList")
	as(endoapply(arrays, get_array_subset), class(x))
}

setMethod("[", "SpectraArrays",
	function(x, i, j, ..., drop = TRUE) .get_SpectraArrays_subset(x, i, j))

.set_SpectraArrays_subset <- function(x, i, j, value)
{
	if ( length(dim(x)) > 2L ) {
		subscripts <- list(
			if (missing(i)) quote(expr=) else i,
			if (missing(j)) quote(expr=) else j)
	} else {
		subscripts <- list(if (missing(i)) quote(expr=) else i)
	}
	set_array_subset <- function(a, v)
	{
		drop <- if (is.matter(a)) NULL else FALSE
		ndim <- if (is.null(dim(a))) 1L else length(dim(a))        
		more_subscripts <- rep.int(list(quote(expr=)), ndim - length(subscripts))
		args <- c(list(a), subscripts, more_subscripts, list(value=v))
		do.call(`[<-`, args)
	}
	arrays <- as(x, "SimpleList")
	values <- as(value, "SimpleList")
	as(mendoapply(set_array_subset, arrays, values), class(x))
}

setReplaceMethod("[", "SpectraArrays",
	function(x, i, j, ..., value) .set_SpectraArrays_subset(x, i, j, value))


## cbind / rbind

.bind_SpectraArrays <- function(objects, margin)
{
	if ( length(objects) == 0L )
		return(SpectraArrays())
	lens <- vapply(objects, length, numeric(1L))
	if ( !all(lens == lens[1L]) )
		.Error("objects to bind must have the same number of spectra arrays")
	if ( lens[1L] == 0L )
		return(SpectraArrays())
	lnms <- lapply(objects, names)
	nms <- unique(unlist(lnms))
	if ( length(nms) > 0L ) {
		# match by name
		ok <- vapply(lnms, setequal, logical(1L), nms)
		if ( !all(ok) )
			.Error("all spectra arrays must have the same set of names")
		ans <- lapply(nms, function(nm) {
			arrays <- lapply(objects, `[[`, nm)
			switch(margin + 1L, do.call(c, arrays),
				do.call(rbind, arrays),
				do.call(cbind, arrays))
		})
		names(ans) <- nms
	} else {
		# match by position
		ans <- lapply(seq_len(lens[1L]), function(i) {
			arrays <- lapply(objects, `[[`, i)
			switch(margin + 1L, do.call(c, arrays),
				do.call(rbind, arrays),
				do.call(cbind, arrays))
		})
	}
	as(as(ans, "SimpleList"), class(objects[[1L]]))
}

setMethod("cbind", "SpectraArrays",
	function(..., deparse.level = 1) .bind_SpectraArrays(list(...), 2L))

setMethod("rbind", "SpectraArrays",
	function(..., deparse.level = 1) .bind_SpectraArrays(list(...), 1L))

setMethod("c", "SpectraArrays",
	function(x, ...) .bind_SpectraArrays(list(x, ...), 0L))

