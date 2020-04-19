
# Make sure nested parallel calls don't overload sockets
.protectNestedBPPARAM <- function(BPPARAM) {
	if ( !is.list(BPPARAM) ) {
		BPPARAM <- list(BPPARAM, SerialParam())
	} else {
		BPPARAM
	}
}

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

# Unlist (once)
.unlist_once <- function(ans) {
	unlist(ans, recursive=FALSE)
}

# Unlist (once) and re-order output of spatialApply
.unlist_and_reorder <- function(ans) {
	unlist(ans, recursive=FALSE)[order(unlist(attr(ans, "idx")))]
}

# Combine by column and re-order output of spatialApply
.cbind_and_reorder <- function(ans) {
	ans2 <- do.call("cbind", ans)
	ans2[,order(unlist(attr(ans, "idx"))),drop=FALSE]
}

# Combine by row and re-order output of spatialApply
.rbind_and_reorder <- function(ans) {
	ans2 <- do.call("rbind", ans)
	ans2[order(unlist(attr(ans, "idx"))),,drop=FALSE]
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
		res <- matter_vec(x, datamode=typeof(x),
			filemode="rw", offset=eof, paths=path)
		ipcunlock(pid)
		c(mode=datamode(res), offset=eof, length=length(res))
	}
	fun
}

# Collect the metadata from (parallel) file output
.remote_collect <- function(ans, path, simplify) {
	ans <- do.call(rbind, ans)
	mode <- make_datamode(ans[,1], type="R")
	mode <- as.character(mode)
	offset <- ans[,2]
	extent <- ans[,3]
	if ( simplify && all(extent == 1L) ) {
		if ( !is.unsorted(offset) ) {
			offset <- 0
			extent <- nrow(ans)
			mode <- mode[1L]
		}
		x <- matter_vec(datamode=mode, filemode="rw",
				offset=offset, extent=extent, paths=path)
	} else if ( simplify && length(unique(extent)) == 1L ) {
		x <- matter_mat(datamode=mode, filemode="rw",
			offset=offset, extent=extent, paths=path)
	} else {
		x <- matter_list(datamode=mode, filemode="rw",
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
.bind_dfs_as_list <- function(data, guess = 100L) {
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
	cat(.spaste(x, vals=vals, collapse=collapse,
		exdent=exdent, prefix=prefix, ...), sep="\n")
}

# Select values for show() method (without printing)
.spaste <- function(x, vals=character(), collapse=" ", exdent=4, prefix="", ...)
{
	if ( is.null(vals) ) vals <- character()
	vals <- ifelse(nzchar(vals), vals, "''")
	labels <- paste(selectSome(vals), collapse=collapse)
	txt <- sprintf(x, length(vals), labels)
	strwrap(txt, exdent=exdent, prefix=prefix, ...)
}

# Setup plotting layout
.setup.layout <- function(layout, byrow = TRUE, ...,
	bottom = 0, left = 0, top = 0, right = 0, par = list())
{
	layout <- as.integer(layout)
	if ( length(layout) <= 1L ) {
		layout <- rep_len(c(layout, 1L), 2)
	} else if ( length(layout) > 2L ) {
		byrow <- as.logical(layout[3L])
	}
	layout <- layout[c(1,2)]
	par <- par[c("mar", "mgp", "cex.axis", "cex.lab")]
	par <- par[!sapply(par, is.null)]
	par <- c(par, list(...))
	if ( is.null(par$mar) )
		par$mar <- c(2.5 + bottom, 2.5 + left, 2 + top, 1 + right) + 0.1
	if ( is.null(par$mgp) )
		par$mgp <- c(1.4, 0.6, 0)
	if ( is.null(par$cex.axis) )
		par$cex.axis <- 0.8
	if ( is.null(par$cex.lab) )
		par$cex.lab <- 0.8
	do.call("par", par)
	if ( byrow ) {
		par(mfrow=layout)
	} else {
		par(mfcol=layout)
	}
	list(layout=layout, byrow=byrow)
}

# Auto plotting layout
.auto.layout <- function(x, byrow = TRUE, ...) {
	if ( is.atomic(x) || is.null(x$dpages) ) {
		nd <- 0
	} else {
		nd <- length(x$dpages) > 1L
	}
	if ( is.atomic(x) || is.null(x$facets) ) {
		nf <- 0
	} else {
		nf <- all(sapply(x$fids, nlevels) > 1L) + (length(x$fids) > 1L)
	}
	if ( !is.atomic(x) && (nd + nf) > 1L ) {
		if ( length(x$dpages) > 1L ) {
			n1 <- nlevels(interaction(x$fids))
			n2 <- length(x$dpages)
		} else {
			n1 <- length(unique(x$fids[[1]]))
			n2 <- length(unique(x$fids[[2]]))
		}
		if ( byrow ) {
			.setup.layout(c(n2, n1), byrow=byrow, ...)
		} else {
			.setup.layout(c(n1, n2), byrow=byrow, ...)
		}
	} else {
		if ( is.numeric(x) ) {
			n <- x
		} else {
			n <- .num.panels(x)
		}
		nc <- ceiling(sqrt(n))
		nr <- ceiling(n / nc)
		.setup.layout(c(nr, nc), byrow=byrow, ...)
	}
}

# Number of panels in a facet plot
.num.panels <- function(x) {
	x <- lapply(x$facets,
		function(l1) lapply(l1,
			function(l2) l2$add))
	sum(!unlist(x))
}

# Update a plotting object's par
.update.par <- function(obj, ...) {
	dots <- list(...)
	if ( length(dots) > 0L ) {
		rm <- sapply(dots, is.null)
		if ( any(rm) )
			dots[rm] <- NULL
		if ( !is.null(dots$add) ) {
			obj$add <- dots$add
			dots$add <- NULL
		}
		if ( !is.null(dots$layout) || !is.null(dots$byrow) ) {
			layout <- obj$layout$layout
			byrow <- obj$layout$byrow
			layout <- list(layout=layout, byrow=byrow)
			if ( !is.null(dots$layout) ) {
				layout$layout <- dots$layout
				dots$layout <- NULL
			}
			if ( !is.null(dots$byrow) ) {
				layout$byrow <- dots$byrow
				dots$byrow <- NULL
			}
			obj$layout <- layout
		}
		nms <- names(dots)
		update <- nms %in% names(obj$par)
		if ( any(update) ) {
			obj$par[nms[update]] <- dots[nms[update]]
			dots[nms[update]] <- NULL
		}
		obj$par <- c(obj$par, dots)
	}
	obj
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
	if ( "oldstyle" %in% names(args) && args$oldstyle ) {
		args$oldstyle <- NULL
		if ( !"x" %in% names(args) )
			args$x <- "top"
		if ( !"legend" %in% names(args) )
			args$legend <- text
		if ( !"x.intersp" %in% names(args) )
			args$x.intersp <- 0
		if ( !"bg" %in% names(args) ) {
			col <- as.numeric(col2rgb(par()$bg) / 255)
			args$bg <- rgb(col[1], col[2], col[3], 0.75)
		}
		if ( !"cex" %in% names(args) )
			args$cex <- 0.8
		if ( isTRUE(strip) && length(args$legend) != 0 )
			do.call("legend", args)
	} else {
		if ( !"text" %in% names(args) )
			args$text <- paste0(text, collapse="\n")
		if ( !"cex" %in% names(args) )
			args$cex <- par()$cex
		if ( "legend" %in% names(args) )
			args$legend <- NULL
		if ( isTRUE(strip) )
			do.call("mtext", args)
	}
}

# Draw keys
.draw.key <- function(key, text, fill) {
	par <- par()$xpd
	par(xpd=TRUE)
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
	if ( !"bg" %in% names(args) ) {
		col <- as.numeric(col2rgb(par()$bg) / 255)
		args$bg <- rgb(col[1], col[2], col[3], 0.75)
	}
	if ( isTRUE(key) && length(args$legend) != 0 ) {
		do.call("legend", args)
		par(xpd=par)
	}
}

# Draw colorkeys
.draw.colorkey <- function(colorkey, text, col, layout = NULL) {
	args <- switch(class(colorkey),
		"logical"=list(),
		"character"=list(x=colorkey),
		"list"=colorkey,
		stop("invalid 'colorkey' argument"))
	if ( is.character(colorkey) || is.list(colorkey) )
		colorkey <- TRUE
	if ( "oldstyle" %in% names(args) && args$oldstyle ) {
		args$oldstyle <- NULL
		if ( !"x" %in% names(args) )
			args$x <- "topright"
		if ( !"legend" %in% names(args) )
			args$legend <- c(text[2], rep(NA, length(col) - 2L), text[1])
		if ( !"col" %in% names(args) )
			args$col <- rev(col)
		if ( !"y.intersp" %in% names(args) )
			args$y.intersp <- 0.1
		if ( !"bg" %in% names(args) ) {
			col <- as.numeric(col2rgb(par()$bg) / 255)
			args$bg <- rgb(col[1], col[2], col[3], 0.75)
		}
		if ( !"cex" %in% names(args) )
			args$cex <- 0.6
		if ( !"lwd" %in% names(args) )
			args$lwd <- 2
		if ( isTRUE(colorkey) )
			do.call("legend", args)
	} else {
		old.par <- par(no.readonly = TRUE)
		plt <- par()$plt
		o <- 1 - plt[2]
		plt <- c(plt[2] + 0.1 * o, plt[2] + 0.3 * o, 0.2, 0.8)
		range <- as.numeric(text)
		vals <- seq(from=range[1], to=range[2], length.out=100)
		if ( !"new" %in% names(args) )
			args$new <- TRUE
		if ( !"pty" %in% names(args) )
			args$pty <- "m"
		if ( !"plt" %in% names(args) )
			args$plt <- plt
		if ( "x" %in% names(args) )
			args$x <- NULL
		if ( "cex.axis" %in% names(args)	) {
			cex.axis <- args$cex.axis
		} else {
			cex.axis <- par()$cex.axis
		}
		if ( isTRUE(colorkey) ) {
			par(args)
			image(1, vals, t(as.matrix(vals)), col=col,
				xaxt='n', yaxt='n', xlab="", ylab="")
			axis(side=4, las=2, cex.axis=0.75 * cex.axis)
			mfg <- par()$mfg
			par(old.par)
			if ( !is.numeric(layout$layout) ) {
				par(mfg=mfg, new=FALSE)
			} else {
				if ( isFALSE(layout$byrow) ) {
					par(mfcol=layout$layout,
						mfg=mfg, new=FALSE)
				} else {
					par(mfrow=layout$layout,
						mfg=mfg, new=FALSE)
				}
			}
			invisible()
		}
	}
}

.next.figure <- function(layout, byrow = TRUE, last = FALSE) {
	if ( missing(layout) || !is.numeric(layout$layout) ) {
		if ( !missing(layout) && is.logical(layout$byrow) )
			byrow <- layout$byrow
		if ( byrow ) {
			layout <- par()$mfrow
		} else {
			layout <- par()$mfcol
		}
	} else {
		byrow <- layout$byrow
		layout <- layout$layout
	}
	mfg <- par()$mfg
	if ( !last ) {
		mat <- matrix(1:prod(layout), byrow=byrow,
			nrow=layout[1], ncol=layout[2])
		cur <- mfg[c(1,2)]
		cur <- mat[cur[1], cur[2]]
		nxt <- (cur %% prod(layout)) + 1
		nxt <- which(mat == nxt, arr.ind=TRUE)
		mfg[c(1,2)] <- nxt
	} else {
		mfg[c(1,2)] <- c(layout[1], layout[2])
	}
	par(mfg=mfg)
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
.format.mz <- function(mz, digits=4) {
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
