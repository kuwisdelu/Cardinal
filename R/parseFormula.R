
.parsePlotFormula <- function(formula, object, enclos) {
	out <- .parseFormula(formula)
	out <- lapply(out, as.list)
	for ( i in seq_along(out$left) )
		out$left[[i]] <- eval(parse(text=out$left[[i]]),
			envir=fData(object), enclos=enclos)
	for ( i in seq_along(out$right) )
		out$right[[i]] <- eval(parse(text=out$right[[i]]),
			envir=fData(object), enclos=enclos)
	for ( i in seq_along(out$condition) )
		out$condition[[i]] <- eval(parse(text=out$condition[[i]]),
			envir=pData(object), enclos=enclos)
	out <- lapply(out, function(o) if ( length(o) == 0) NULL else o)
	out
}

.parseImageFormula <- function(formula, object, enclos) {
	out <- .parseFormula(formula)
	out <- lapply(out, as.list)
	for ( i in seq_along(out$left) )
		out$left[[i]] <- eval(parse(text=out$left[[i]]),
			envir=pData(object), enclos=enclos)
	for ( i in seq_along(out$right) )
		out$right[[i]] <- eval(parse(text=out$right[[i]]),
			envir=pData(object), enclos=enclos)
	for ( i in seq_along(out$condition) )
		out$condition[[i]] <- eval(parse(text=out$condition[[i]]),
			envir=fData(object), enclos=enclos)
	out <- lapply(out, function(o) if ( length(o) == 0) NULL else o)
	out
}

.parseSide <- function(formula) {
	if ( length(formula) == 1 ) {
		paste(formula)
	} else if ( paste(formula[[1]]) %in% c("[", "[[", "$") ) {
		deparse(formula)
	} else if ( length(formula) == 2  && paste(formula[[1]]) == "I" ) {
		paste(formula)[[2]]
	} else if ( length(formula[[2]]) == 3 ) {
		c(.parseSide(formula[[2]]), paste(formula[[3]]))
	} else if ( length(formula) == 2 ) {
		paste(paste(formula, collapse="("), ")", sep="")
	} else {
		paste(formula[-1])
	}
}

.parseFormula <- function(formula) {
	if ( length(formula) == 2 ) {
		right <- formula[[2]]
		left <- NULL
	} else if ( length(formula) == 3 ) {
		right <- formula[[3]]
		left <- formula[[2]]
	}
	if ( length(right) == 1 ) {
		condition <- NULL
		right <- right
	} else if ( length(right) == 3 && paste(right[[1]]) != "|" ) {
		condition <- NULL
		right <- right
	} else if ( length(right) == 3 && paste(right[[1]]) == "|" ) {
		condition <- right[[3]]
		right <- right[[2]]
	} else {
		condition <- NULL
	}
	if ( !is.null(left) )
		left <- .parseSide(left)
	names(left) <- left
	if ( !is.null(right) )
		right <- .parseSide(right)
	names(right) <- right
	if ( !is.null(condition) )
		condition <- .parseSide(condition)
	names(condition) <- condition
	list(left=left, right=right, condition=condition)
}

