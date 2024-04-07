
#### XDataFrame ####
## -----------------

.valid_XDataFrame <- function(object)
{
	errors <- NULL
	nms <- names(object)
	keys <- object@keys
	if ( length(keys) > 0L && is.null(nms) )
		errors <- c(errors , "'keys' must be a named list")
	strings <- vapply(keys, is.character, logical(1L))
	if ( !all(strings) )
		errors <- c(errors , "'keys' must all be character vectors")
	columns <- vapply(keys, function(k) all(k %in% nms), logical(1L))
	if ( !all(columns) )
		errors <- c(errors , "'keys' must all name columns in the data frame")
	unique_keys <- vapply(keys, function(k) setequal(unique(k), k), logical(1L))
	if ( !all(unique_keys) )
		errors <- c(errors , "'keys' can't specify the same column more than once")
	multi_keys <- vapply(keys, function(k) anyDuplicated(nms[nms %in% k]), integer(1L))
	if ( any(multi_keys) )
		errors <- c(errors , "'keys' can't match to more than one column")
	if ( is.null(errors) ) TRUE else errors
}

setValidity("XDataFrame", .valid_XDataFrame)

setAs("DataFrame", "XDataFrame",
	function(from) new("XDFrame", from, keys=list()))

XDataFrame <- function(..., keys = list())
{
	ans <- DataFrame(...)
	ans <- new("XDFrame", ans, keys=keys)
	if ( validObject(ans) )
		ans
}

setMethod("show", "XDataFrame", 
	function(object) {
		callNextMethod()
		for ( nm in names(object@keys) )
		{
			n <- length(object@keys[[nm]])
			ks <- paste0(object@keys[[nm]], collapse=", ")
			cat(nm, "(", n, "): ", ks, "\n", sep="")
		}
	})

setMethod("classNameForDisplay", "XDFrame",
	function(x) if (class(x) == "XDFrame") "XDataFrame" else class(x))

# get specific key cols or keys slot if i=NULL
setMethod("keys", "XDataFrame",
	function(object, i = NULL, ..., drop = TRUE) {
		if ( is.null(i) )
			return(object@keys)
		nms <- object@keys[[i]]
		if ( is.null(nms) )
			return(NULL)
		if ( length(nms) == 1L && drop ) {
			object[[nms]]
		} else {
			object[nms] # note: `[` drops class
		}
	})

# set specific key cols or keys slot if i=NULL
setReplaceMethod("keys", "XDataFrame",
	function(object, i = NULL, ..., value) {
		if ( is.null(i) ) {
			object@keys <- value
			if ( validObject(object) )
				return(object)
		}
		if ( is(value, "list_OR_List") && !is.null(names(value)) ) {
			if ( !setequal(names(value), object@keys[[i]]) )
				object@keys[[i]] <- names(value)
		}
		nms <- object@keys[[i]]
		if ( is.null(nms) )
			stop("key does not exist: ", sQuote(i))
		object[nms] <- value
		if ( validObject(object) )
			object
	})

# subsetting by cols will drop keys and class
setMethod("[", "XDataFrame",
	function(x, i, j, ..., drop = TRUE) {
		ans <- callNextMethod()
		if ( !missing(j) || (nargs() - !missing(drop)) < 3L )
			ans <- as(ans, "DFrame")
		if ( validObject(ans) )
			ans
	})

.update_renamed_keys <- function(before, after)
{
	nms <- setNames(names(after), names(before))
	keys <- lapply(before@keys, function(ks) unname(nms[ks]))
	new(class(before), after, keys=keys)
}

# make sure setting names will also rename keys
setReplaceMethod("names", "XDataFrame",
	function(x, value) {
		ans <- setNames(as(x, "DFrame"), value)
		ans <- .update_renamed_keys(x, ans)
		if ( validObject(ans) )
			ans
	})

.remove_nulled_keys <- function(before, after)
{
	nulled <- setdiff(names(before), names(after))
	keys <- lapply(before@keys, function(ks) setdiff(ks, nulled))
	new(class(before), after, keys=keys)
}

setReplaceMethod("[", "XDataFrame",
	function(x, i, j, ..., value) {
		ans <- callNextMethod()
		ans <- new(class(x), ans, keys=x@keys)
		ans <- .remove_nulled_keys(x, ans)
		if ( validObject(ans) )
			ans
	})

setReplaceMethod("[[", "XDataFrame",
	function(x, i, value) {
		ans <- as(x, "DFrame")
		ans[[i]] <- value
		ans <- new(class(x), ans, keys=x@keys)
		ans <- .remove_nulled_keys(x, ans)
		if ( validObject(ans) )
			ans
	})

.get_key_cols_as_df <- function(x) as(x[unlist(x@keys)], "DFrame")

.drop_key_cols <- function(x) as(x[!names(x) %in% unlist(x@keys)], "DFrame")

.compare_cols <- function(objects, cols, tol = 1e-5)
{
	len <- length(cols)
	cmp <- function(x, y) {
		if ( is(x, "DataFrame") && is(y, "DataFrame") ) {
			vapply(cols, function(col) {
				isTRUE(all.equal(x[col], y[col], tolerance=tol))
			}, logical(1L))
		} else {
			rep.int(TRUE, len)
		}
	}
	vapply(objects, cmp, logical(len), objects[[1L]])
}

.merge_lists <- function(x, y, ...)
{
	if ( ...length() > 0L )
		y <- do.call(.merge_lists, list(y, ...))
	if ( missing(y) )
		return(x)
	ans <- x
	for ( nm in names(y) ) {
		if ( nm %in% names(ans) ) {
			ans[[nm]] <- union(x[[nm]], y[[nm]])
		} else {
			ans[[nm]] <- y[[nm]]
		}
	}
	ans
}

.cbind_XDFrame <- function(objects)
{
	keys <- lapply(objects, keys)
	keys <- do.call(.merge_lists, keys)
	keys_ok <- .compare_cols(objects, cols=unlist(keys))
	if ( !all(keys_ok) ) {
		if ( !is.null(dim(keys_ok)) ) {
			badkeys <- !apply(keys_ok, 1L, all)
		} else {
			badkeys <- !all(keys_ok)
		}
		badkeys <- paste0(sQuote(unlist(keys)[badkeys]), collapse=" ")
		stop("can't cbind XDataFrames with non-matching key columns: ", badkeys)
	}
	nrows <- nrow(objects[[1L]])
	all_keycols <- new("DFrame", nrows=nrows)
	all_othercols <- new("DFrame", nrows=nrows)
	for ( i in seq_along(objects) ) {
		keycols <- .get_key_cols_as_df(objects[[i]])
		newkeys <- setdiff(names(keycols), names(all_keycols))
		if ( length(newkeys) > 0L )
			all_keycols <- cbind(all_keycols, keycols[newkeys])
		other <- .drop_key_cols(objects[[i]])
		newcols <- setdiff(names(other), unlist(keys))
		if ( length(newcols) > 0L )
			all_othercols <- cbind(all_othercols, other[newcols])
	}
	ans <- cbind(all_keycols, all_othercols)
	new(class(objects[[1L]]), ans, keys=keys)
}

# only used when all ... are XDFrame-derived
setMethod("cbind", "XDataFrame",
	function(..., deparse.level = 1) .cbind_XDFrame(list(...)))

.rbind_XDFrame <- function(objects)
{
	keys <- lapply(objects, keys)
	keys <- do.call(.merge_lists, keys)
	ans <- do.call(rbind, lapply(objects, as, "DFrame"))
	new(class(objects[[1L]]), ans, keys=keys)
}

# only used when all ... are XDFrame-derived
setMethod("rbind", "XDataFrame",
	function(..., deparse.level = 1) .rbind_XDFrame(list(...)))


#### PositionDataFrame ####
## ---------------------

.valid_PositionDataFrame <- function(object)
{
	errors <- NULL
	keys <- object@keys
	if ( length(keys) != 2L )
		errors <- c(errors , "PositionDataFrame must have exactly two keys")
	if ( !"coord" %in% names(keys) )
		errors <- c(errors , "'keys' must include a key named 'coord'")
	if ( !"run" %in% names(keys) )
		errors <- c(errors , "'keys' must include a key named 'run'")
	if ( length(keys[["coord"]]) < 2L )
		errors <- c(errors , "'coord' key must include at least 2 columns")
	coord <- keys(object, "coord")
	if ( !is.null(coord) && !all(vapply(coord, is.numeric, logical(1L))) )
		errors <- c(errors , "'coord' key columns must be numeric")
	run <- keys(object, "run")
	if ( !is.null(run) && !is.factor(run) )
		errors <- c(errors , "'run' key column must be a factor")
	if ( is.null(errors) ) TRUE else errors
}

setValidity("PositionDataFrame", .valid_PositionDataFrame)

PositionDataFrame <- function(coord, run, ..., row.names = FALSE)
{
	if ( missing(coord) || is.null(coord) )
	{
		if ( ...length() > 0L ) {
			n <- NROW(..1)
		} else if ( !missing(run) ) {
			n <- length(run)
		} else {
			n <- 0L
		}
		coord <- expand.grid(x=seq_len(n), y=rep.int(1L, n))
	}
	coord <- DataFrame(coord)
	if ( missing(run) || is.null(run) ) {
		run <- rep.int(factor("run0"), nrow(coord))
		if ( anyDuplicated(coord) )
			warning("'coord' does not uniquely identify rows")
	} else {
		run <- as.factor(run)
		if ( anyDuplicated(cbind(coord, DataFrame(run=run))) )
			warning("'coord' x 'run' do not uniquely identify rows")
	}
	keys <- list(coord=names(coord), run="run")
	ans <- cbind(coord, DataFrame(run=run, ...))
	ans <- new("PositionDataFrame", ans, keys=keys)
	if ( isTRUE(row.names) ) {
		row.names <- .make_pixelNames(ans)
	} else if ( isFALSE(row.names) ) {
		row.names <- NULL
	}
	row.names(ans) <- row.names
	if ( validObject(ans) )
		ans
}

.make_pixelNames <- function(object)
{
	FUN <- function(nm, vals) paste0(nm, " = ", vals)
	nms <- Map(FUN, coordNames(object), coord(object))
	nms <- as.data.frame(nms)
	nms <- apply(nms, 1L, function(x) paste0(x, collapse=", "))
	paste0(run(object), ": ", nms)
}

setMethod("coord", "PositionDataFrame",
	function(object, i = NULL, ...) {
		if ( is.null(i) ) {
			keys(object, "coord", drop=FALSE)
		} else {
			keys(object, "coord", drop=FALSE)[[i]]
		}
	})
setReplaceMethod("coord", "PositionDataFrame",
	function(object, i = NULL, ..., value) {
		if ( is.null(i) ) {
			keys(object, "coord") <- value
		} else {
			keys(object, "coord")[[i]] <- value
		}
		if ( validObject(object) )
			object
	})

setMethod("coordNames", "PositionDataFrame",
	function(object) keys(object)[["coord"]])
setReplaceMethod("coordNames", "PositionDataFrame",
	function(object, value) {
		nms <- keys(object)[["coord"]]
		i <- match(nms, names(object))
		names(object)[i] <- value
		if ( validObject(object) )
			object
	})

setMethod("run", "PositionDataFrame",
	function(object, ...) keys(object, "run", drop=TRUE))
setReplaceMethod("run", "PositionDataFrame",
	function(object, ..., value) {
		keys(object, "run") <- as.factor(value)
		if ( validObject(object) )
			object
	})

setMethod("runNames", "PositionDataFrame",
	function(object) levels(run(object)))
setReplaceMethod("runNames", "PositionDataFrame",
	function(object, value) {
		levels(run(object)) <- value
		if ( validObject(object) )
			object
	})

setMethod("nrun", "PositionDataFrame",
	function(x) nlevels(run(x)))

# drop unused run levels when subsetting
setMethod("[", "PositionDataFrame",
	function(x, i, j, ..., drop = TRUE) {
		ans <- callNextMethod()
		if ( is(ans, class(x)) )
			run(ans) <- droplevels(run(ans))
		if ( validObject(ans) )
			ans
	})

setAs("DFrame", "PositionDataFrame",
	function(from) {
		cnames <- intersect(c("x", "y", "z"), names(from))
		dnames <- setdiff(names(from), c(cnames, "run"))
		if ( length(dnames) > 0L ) {
			PositionDataFrame(coord=from[cnames], run=from[["run"]], from[dnames])
		} else {
			PositionDataFrame(coord=from[cnames], run=from[["run"]])
		}
	})

setMethod("updateObject", "PositionDataFrame",
	function(object, ..., verbose = FALSE)
	{
		if ( .hasSlot(object, "keys") ) {
			return(object)
		} else if ( .hasSlot(object, "run") && .hasSlot(object, "coord") ) {
			if ( length(object@listData) > 0L ) {
				PositionDataFrame(coord=object@coord, run=object@run, object@listData)
			} else {
				PositionDataFrame(coord=object@coord, run=object@run)
			}
		} else {
			stop("don't know how to update this PositionDataFrame instance")
		}
	})

#### MassDataFrame ####
## ---------------------

.valid_MassDataFrame <- function(object)
{
	errors <- NULL
	keys <- object@keys
	if ( length(keys) != 1L )
		errors <- c(errors , "MassDataFrame must have exactly one key")
	if ( !any(c("mz", "mass") %in% names(keys)) )
		errors <- c(errors , "'keys' must include a key named 'mz' or 'mass'")
	mz <- keys(object, "mz")
	if ( !is.null(mz) && !is.numeric(mz) )
		errors <- c(errors , "'mz' key column must be numeric")
	if ( !is.null(mz) && is.unsorted(mz) )
		errors <- c(errors , "'mz' key column must be sorted")
	mass <- keys(object, "mass")
	if ( !is.null(mass) && !is.numeric(mass) )
		errors <- c(errors , "'mass' key column must be numeric")
	if ( !is.null(mass) && is.unsorted(mass) )
		errors <- c(errors , "'mass' key column must be sorted")
	if ( is.null(errors) ) TRUE else errors
}

setValidity("MassDataFrame", .valid_MassDataFrame)

MassDataFrame <- function(mz, ..., row.names = FALSE)
{
	if ( missing(mz) && ...length() == 0L )
		mz <- numeric()
	ans <- DataFrame(mz=mz, ...)
	ans <- new("MassDataFrame", ans, keys=list(mz="mz"))
	if ( isTRUE(row.names) ) {
		row.names <- .make_featureNames(ans)
	} else if ( isFALSE(row.names) ) {
		row.names <- NULL
	}
	row.names(ans) <- row.names
	if ( validObject(ans) )
		ans
}

.make_featureNames <- function(object)
{
	paste0("m/z = ", round(mz(object), digits=4L))
}

setMethod("mz", "MassDataFrame",
	function(object, ...) keys(object, "mz", drop=TRUE))
setReplaceMethod("mz", "MassDataFrame",
	function(object, ..., value) {
		keys(object, "mz") <- value
		if ( validObject(object) )
			object
	})

setMethod("mass", "MassDataFrame",
	function(object) keys(object, "mass", drop=TRUE))

setAs("DFrame", "MassDataFrame",
	function(from) {
		dnames <- setdiff(names(from), "mz")
		if ( length(dnames) > 0L ) {
			MassDataFrame(mz=from[["mz"]], from[dnames])
		} else {
			MassDataFrame(mz=from[["mz"]])
		}
	})

setMethod("updateObject", "MassDataFrame",
	function(object, ..., verbose = FALSE)
	{
		if ( .hasSlot(object, "keys") ) {
			return(object)
		} else if ( .hasSlot(object, "mz") ) {
			if ( length(object@listData) > 0L ) {
				MassDataFrame(mz=object@mz, object@listData)
			} else {
				MassDataFrame(mz=object@mz)
			}
		} else {
			stop("don't know how to update this MassDataFrame instance")
		}
	})
