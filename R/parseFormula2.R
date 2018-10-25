
.parseFormula2 <- function(formula, lhs.e, rhs.e, g.e) {
	e <- environment(formula)
	if ( length(formula) == 2L ) {
		rhs <- formula[[2L]]
		lhs <- NULL
	} else if ( length(formula) == 3L ) {
		rhs <- formula[[3L]]
		lhs <- formula[[2L]]
	}
	if ( length(rhs) == 1L ) {
		# single-term rhs that doesn't include |
		g <- NULL
		rhs <- rhs
	} else if ( length(rhs) == 3L && deparse(rhs[[1L]]) != "|" ) {
		# rhs includes multiple terms but not |
		g <- NULL
		rhs <- rhs
	} else if ( length(rhs) == 3L && deparse(rhs[[1L]]) == "|" ) {
		# rhs includes | so add condition
		g <- rhs[[3]]
		rhs <- rhs[[2]]
	} else {
		# failsafe
		g <- NULL
	}
	# parse lhs
	if ( !is.null(lhs) )
		lhs <- .parseSide2(lhs)
	if ( !missing(lhs.e) )
		for ( i in seq_along(lhs) )
			lhs[[i]] <- eval(lhs[[i]], envir=lhs.e, enclos=e)
	# parse rhs
	if ( !is.null(rhs) )
		rhs <- .parseSide2(rhs)
	if ( !missing(rhs.e) )
		for ( i in seq_along(rhs) )
			rhs[[i]] <- eval(rhs[[i]], envir=rhs.e, enclos=e)
	# parse condition
	if ( !is.null(g) )
		g <- .parseSide2(g)
	if ( !missing(g.e) )
		for ( i in seq_along(g) )
			g[[i]] <- eval(g[[i]], envir=g.e, enclos=e)
	list(lhs=lhs, rhs=rhs, g=g)
}

.parseSide2 <- function(formula, e) {
	enclos <- environment(formula)
	if ( length(formula) != 1L ) {
		if ( deparse(formula[[1L]]) %in% c("~", "*", "+") ) {
			side <- lapply(as.list(formula)[-1L], .parseSide2)
		} else {
			side <- list(formula)
		}
	} else {
		side <- list(formula)
	}
	if ( is.list(side) ) {
		side <- unlist(side, recursive=TRUE)
		names(side) <- sapply(side, deparse)
	}
	if ( !missing(e) ) {
		for ( i in seq_along(side) )
			side[[i]] <- eval(side[[i]], envir=e, enclos=enclos)
	}
	side
}

