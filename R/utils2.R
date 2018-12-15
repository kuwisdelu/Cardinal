
# Apply that returns a 'list' instead of array
.apply <- function(x, margin, fun, ...) {
	fun <- match.fun(fun)
	if ( margin == 1L ) {
		lapply(seq_len(nrow(x)), function(i) fun(x[i,,drop=TRUE]))
	} else if ( margin == 2L ) {
		lapply(seq_len(ncol(x)), function(j) fun(x[,j,drop=TRUE]))
	} else {
		stop("internal 'apply' error")
	}
}

# Tabulate-apply that returns a 'list' instead of array
.tapply <- function(x, index, fun, ...) {
	index <- as.factor(index)
	ans <- lapply(levels(index), function(i) {
		i2 <- which(index == i)
		if ( length(i2) > 0L ) {
			fun(x[i2], ...)
		} else {
			NA
		}
	})
	setNames(ans, levels(index))
}

# Apply an expression or function to an object '.'
.do_dot_expr <- function(x, what, env) {
	assign(".", x, env)
	y <- eval(what, envir=env)
	if ( is.function(y) ) {
		y(x)
	} else {
		y
	}
}

# Unlist (once) and re-order output of spatialApply
.unlist_and_reorder <- function(ans) {
	unlist(ans, recursive=FALSE)[order(unlist(attr(ans, "idx")))]
}

# Try eval in specified envir, otherwise try in current envir
.try_eval <- function(expr, envir) {
	p <- parent.frame()
	tryCatch(eval(expr, envir),
		error=function(e) eval(expr, envir=p))
}

# Transform a factor into a matrix of indicator variables
.factor_matrix <- function(fc) {
	fc <- as.factor(fc)
	ret <- vapply(levels(fc), function(l) fc == l,
		integer(length(fc)))
	colnames(ret) <- levels(fc)
	ret
}

# Create a virtual copy of the object with no data
.virtual.image <- function(x) {
	imageData(x) <- new(class(imageData(x)))
	if ( validObject(x) )
		x
}

# Generate a function for (parallel) writing output to a file
.remote_writer <- function(pid, path) {
	fun <- function(x) {
		ipclock(pid)
		eof <- file.size(path)
		eof <- ifelse(is.na(eof), 0, eof)
		if ( !is.numeric(x) )
			stop("non-numeric output not allowed for remote writing")
		res <- matter::matter_vec(x, datamode=typeof(x),
			filemode="rw", offset=eof, paths=path)
		ipcunlock(pid)
		c(mode=datamode(res), offset=eof, length=length(res))
	}
	fun
}

# Collect the metadata from (parallel) file output
.remote_collect <- function(ans, path, simplify) {
	ans <- do.call(rbind, ans)
	mode <- matter::make_datamode(ans[,1], type="R")
	mode <- as.character(mode)
	offset <- ans[,2]
	extent <- ans[,3]
	if ( simplify && all(extent == 1L) ) {
		if ( !is.unsorted(offset) ) {
			offset <- 0
			extent <- nrow(ans)
			mode <- mode[1L]
		}
		x <- matter::matter_vec(datamode=mode, filemode="rw",
				offset=offset, extent=extent, paths=path)
	} else if ( simplify && length(unique(extent)) == 1L ) {
		x <- matter::matter_mat(datamode=mode, filemode="rw",
			offset=offset, extent=extent, paths=path)
	} else {
		x <- matter::matter_list(datamode=mode, filemode="rw",
			offset=offset, extent=extent, paths=path)
	}
	x
}

# If processing flattens key-value pairs, expand and collect them
.collect_keyval_pairs <- function(ans) {
	if ( is(ans, "matter_list") ) {
		len <- floor(lengths(ans) / 2)
		adata <- as.list(atomdata(ans))
		mode <- as.character(adata$datamode)
		keyData <- matter_list(datamode=mode,
			offset=adata$offset, extent=len,
			paths=paths(ans))
		valueData <- matter_list(datamode=mode,
			offset=adata$offset + sizeof(mode) * len,
			extent=len, paths=paths(ans))
	} else {
		keyData <- lapply(ans, function(x)
			x[1:floor(length(x) / 2)])
		valueData <- lapply(ans, function(x)
			x[floor(1 + (length(x) / 2)):length(x)])
	}
	list(keys=keyData, values=valueData)
}

# Combine many lists/dataframes into a single result
.bind_as_df <- function(data, guess = 100L) {
	dhead <- lapply(head(data, n=guess), as.data.frame,
		check.names=FALSE, fix.empty.names=FALSE,
		check.rows=FALSE, stringsAsFactors=FALSE)
	coltypes <- sapply(do.call(rbind, dhead), typeof)
	ans <-  lapply(coltypes, function(col_t)
		vector(length(data), mode=col_t))
	for ( i in seq_len(length(data)) )
		for ( j in seq_along(ans) )
			ans[[j]][i] <- data[[i]][[j]]
	ans
}

# Match a function or NULL
.matchFunOrNULL <- function(f) {
	if ( is.null(f) ) {
		f
	} else {
		match.fun(f)
	}
}

# Select and concatenate metadata for show() method
.scat <- function(x, vals=character(), collapse=" ", exdent=4, prefix="", ...)
{
	if ( is.null(vals) ) vals <- character()
	vals <- ifelse(nzchar(vals), vals, "''")
	labels <- paste(selectSome(vals), collapse=collapse)
	txt <- sprintf(x, length(vals), labels)
	cat(strwrap(txt, exdent=exdent, prefix=prefix, ...), sep="\n")
}

# Setup plotting layout
.setup.layout <- function(layout, byrow = TRUE) {
	if ( length(layout) < 1L )
		layout <- rep_len(c(layout, 1L), 2)
	if ( length(layout) > 2L )
		byrow <- layout[3L] <= 1L
	par(mar=c(3,3,2,1), mgp=c(1.5,0.5,0),
		cex.axis=0.8, cex.lab=0.8)
	layout(matrix(seq_len(prod(layout)),
		nrow=layout[1L], ncol=layout[2L], byrow=byrow))
}

# Auto plotting layout
.auto.layout <- function(x) {
	n <- .num.panels(x)
	nc <- ceiling(sqrt(n))
	nr <- ceiling(n / nc)
	.setup.layout(c(nr, nc))
}

# Number of panels in a facet plot
.num.panels <- function(x) {
	x <- lapply(x$layers,
		function(l1) lapply(l1,
			function(l2) l2$add))
	sum(!unlist(x))
}

# Draw strip labels
.draw.strip.labels <- function(strip, text) {
	args <- switch(class(strip),
		"logical"=list(),
		"character"=list(legend=strip),
		"list"=strip,
		stop("invalid 'strip' argument"))
	if ( is.character(strip) || is.list(strip) )
		strip <- TRUE
	if ( !"x" %in% names(args) )
		args$x <- "top"
	if ( !"legend" %in% names(args) )
		args$legend <- text
	if ( !"x.intersp" %in% names(args) )
		args$x.intersp <- 0
	if ( !"bg" %in% names(args) )
		args$bg <- rgb(1, 1, 1, 0.75)
	if ( !"cex" %in% names(args) )
		args$cex <- 0.8
	if ( isTRUE(strip) && length(args$legend) != 0 )
		do.call("legend", args)
}

# Draw keys
.draw.key <- function(key, text, fill) {
	args <- switch(class(key),
		"logical"=list(),
		"character"=list(legend=key),
		"list"=key,
		stop("invalid 'key' argument"))
	if ( is.character(key) || is.list(key) )
		key <- TRUE
	if ( !"x" %in% names(args) )
		args$x <- "topright"
	if ( !"legend" %in% names(args) )
		args$legend <- text	
	if ( !"fill" %in% names(args) )
		args$fill <- fill
	if ( !"bg" %in% names(args) )
		args$bg <- rgb(1, 1, 1, 0.75)
	if ( isTRUE(key) && length(args$legend) != 0 )
		do.call("legend", args)
}

# Draw colorkeys
.draw.colorkey <- function(colorkey, text, col) {
	args <- switch(class(colorkey),
		"logical"=list(),
		"character"=list(x=colorkey),
		"list"=colorkey,
		stop("invalid 'colorkey' argument"))
	if ( is.character(colorkey) || is.list(colorkey) )
		colorkey <- TRUE
	if ( !"x" %in% names(args) )
		args$x <- "topright"
	if ( !"legend" %in% names(args) )
		args$legend <- c(text[2], rep(NA, length(col) - 2L), text[1])
	if ( !"col" %in% names(args) )
		args$col <- rev(col)
	if ( !"y.intersp" %in% names(args) )
		args$y.intersp <- 0.1
	if ( !"bg" %in% names(args) )
		args$bg <- rgb(1, 1, 1, 0.75)
	if ( !"cex" %in% names(args) )
		args$cex <- 0.6
	if ( !"lwd" %in% names(args) )
		args$lwd <- 2
	if ( isTRUE(colorkey) )
		do.call("legend", args)
}

## Format numbered labels
.format.numbered <- function(label, n, sep="") {
	if ( n == 1L )
		return(label)
	nums <- as.character(seq_len(n))
	nums <- paste0(sep, nums)
	nums[1L] <- ""
	paste0(label, nums)
}

## Format a data.frame of labels
.format.data.labels <- function(data, sym = " = ", append = "") {
	data <- as.data.frame(data)
	apply(data, 1, function(a) {
		a <- paste0(names(a), sym, a)
		a <- paste0(a, append)
		paste0(a, collapse=", ")
	})
}

## Format plotting labels
.format.plot.label <- function(label, character.only=FALSE) {
	if ( character.only ) {
		if ( label == "mz" ) {
			"m/z"
		} else {
			label
		}
	} else {
		if ( label == "mz" ) {
			expression(italic(m/z))
		} else {
			parse(text=paste0("italic(", label, ")"))
		}
	}
}

## Format m/z values
.format.mz <- function(mz, digits=2) {
	diffmz <- diff(mz)
	if ( length(diffmz) > 0 ) {
		mindiff <- signif(min(diffmz), 1)
		dplaces <- strsplit(paste(mindiff), "[.]")[[1]]
		if ( length(dplaces) > 1 ) {
			dplaces <- nchar(strsplit(paste(mindiff), "[.]")[[1]][2])	
		} else {
			dplaces <- 0
		}
	} else {
		dplaces <- 0
	}
	digits <- max(dplaces, digits)
	if ( length(mz) > 0 ) {
		paste0("m/z", " = ", round(mz, digits=digits))
	} else {
		character()
	}
}
