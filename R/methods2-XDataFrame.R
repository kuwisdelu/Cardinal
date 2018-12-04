
#### Methods for XDataFrame ####
## ------------------------------

XDataFrame <- function(...)  as(DataFrame(...), "XDataFrame")

# Need to overwrite 'names', 'length', and 'show' methods
# for XDataFrame which may have special eXtra columns that
# must be printed but not counted as part of 'ncol'.

setMethod("names", "XDataFrame", 
	function(x) {
		names(x@listData)
	})

setMethod("length", "XDataFrame", 
	function(x) {
		length(x@listData)
	})

setMethod("lapply", "XDataFrame", 
	function(X, FUN, ..., slots = FALSE) {
		if ( slots ) {
			lapply(as.list(X), FUN=FUN, ...)
		} else {
			lapply(X@listData, FUN=FUN, ...)
		}
	})

setMethod("as.env", "list",
	function(x, enclos = parent.frame(2), tform = identity) {
		env <- new.env(parent = enclos)
		lapply(names(x), function(name) {
			fun <- function() {
				val <- tform(x[[name]])
				rm(list = name, envir = env)
				assign(name, val, env)
				val
			}
			makeActiveBinding(name, fun, env)
		})
		env$.. <- x
		env	
	})

setMethod("as.env", "XDataFrame",
	function(x, enclos = parent.frame(2), ..., slots = FALSE) {
		enclos <- force(enclos)
		if ( slots ) {
			as.env(as.list(x), enclos=enclos, ...)
		} else {
			as.env(x@listData, enclos=enclos, ...)
		}
	})

setMethod("cbind", "XDataFrame",
	function(..., deparse.level = 1) {
		as(callNextMethod(...), class(..1))
	})

setMethod("rbind", "XDataFrame",
	function(..., deparse.level = 1) {
		as(callNextMethod(...), class(..1))
	})

# Need to overwrite '[[<-' and '$<-' now due
# to changes in S4Vectors in BioC 3.8

setReplaceMethod("[[", "XDataFrame",
	function(x, i, j,..., value) {
		nrx <- nrow(x)
		lv <- NROW(value)
		if (!missing(j) || length(list(...)) > 0)
			warning("arguments beyond 'i' ignored")
		if (missing(i))
			stop("subscript is missing")
		if (!is.character(i) && !is.numeric(i))
			stop("invalid subscript type")
		if (length(i) < 1L)
			stop("attempt to select less than one element")
		if (length(i) > 1L)
			stop("attempt to select more than one element")
		if (is.numeric(i) && (i < 1L || i > ncol(x) + 1L))
			stop("subscript out of bounds")
		if (!is.null(value) && (nrx != lv)) {
			if ((nrx == 0) || (lv == 0) || (nrx %% lv != 0)) {
				stop(paste(lv, "elements in value to replace",
					nrx, "elements"))
			} else {
				value <- rep(value, length.out = nrx)
			}
		}
		if ( is.null(value) ) {
			x <- callNextMethod(x, i, ..., value=value)
		} else {
			x[,i] <- value
		}
		x
	})

# Subclasses should define an 'as.list' method to include
# the additional slot-columns by default, or instead redefine
# an 'lapply' method to include them when 'slots = TRUE'

setMethod("show", "XDataFrame",
	function(object)
{
	nhead <- get_showHeadLines()
	ntail <- get_showTailLines()
	nr <- nrow(object)
	nc <- ncol(object)
	true_nc <- length(lapply(object, function(x) NULL, slots=TRUE))
	cat(class(object), " with ",
		nr, ifelse(nr == 1, " row and ", " rows and "),
		nc, ifelse(nc == 1, " column\n", " columns\n"),
		sep = "")
	if (nr > 0 && true_nc > 0) {
		nms <- rownames(object)
		if (nr <= (nhead + ntail + 1L)) {
			out <-
				as.matrix(format(as.data.frame(
					lapply(object, showAsCell, slots=TRUE),
						optional = TRUE)))
			if (!is.null(nms))
				rownames(out) <- nms
		} else {
			out <-
				rbind(as.matrix(format(as.data.frame(
						lapply(object, function(x)
							showAsCell(head(x, nhead)), slots=TRUE),
						optional = TRUE))),
					rbind(rep.int("...", true_nc)),
					as.matrix(format(as.data.frame(
						lapply(object, function(x) 
							showAsCell(tail(x, ntail)), slots=TRUE),
						optional = TRUE))))
				rownames(out) <- .make.rownames(nms, nr, nhead, ntail) 
		}
		classinfo <-
			matrix(unlist(lapply(object, function(x)
						paste0("<", classNameForDisplay(x)[1], ">"), slots=TRUE),
					use.names = FALSE), nrow = 1,
				dimnames = list("", colnames(out)))
		out <- rbind(classinfo, out)
		print(out, quote = FALSE, right = TRUE)
	}
})

# copied from S4Vectors::show,DataTable
.make.rownames <- function(nms, nrow, nhead, ntail)
{
	p1 <- ifelse (nhead == 0, 0L, 1L)
	p2 <- ifelse (ntail == 0, 0L, ntail-1L)
	s1 <- s2 <- character(0)

	if ( is.null(nms) ) {
		if ( nhead > 0 )
			s1 <- paste0(as.character(p1:nhead))
		if ( ntail > 0 )
			s2 <- paste0(as.character((nrow-p2):nrow))
	} else { 
		if ( nhead > 0 )
			s1 <- paste0(head(nms, nhead))
		if ( ntail > 0 )
			s2 <- paste0(tail(nms, ntail))
	}
	c(s1, "...", s2)
}
