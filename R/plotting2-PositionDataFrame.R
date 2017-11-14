
#### Plotting for PositionDataFrame ####
## -------------------------------------

setMethod("plot",
	signature = c(x = "PositionDataFrame", y = "formula"),
	function(x, y, ...) {
		plot(x, formula = y, ...)
	})

setMethod("plot", c(x = "PositionDataFrame", y = "missing"),
	function(x, formula, ...,
		xlim,
		ylim,
		asp = 1,
		pch = 20,
		col = intensity.colors)
{
	obj <- x
	args <- .parseFormula2(formula,
		lhs.e=obj@listData,
		rhs.e=obj@coord,
		g.e=obj@listData)
	if ( length(args$rhs) != 2 )
		stop("rhs of formula must include 2 variables")
	x <- args$rhs[[1]]
	y <- args$rhs[[2]]
	vals <- args$lhs[[1]]
	if ( gridded(obj) ) {
		rx <- resolution(obj)[names(args$rhs[[1]])]
		ry <- resolution(obj)[names(args$rhs[[2]])]
	} else {
		rx <- min(diff(sort(unique(x))))
		ry <- min(diff(sort(unique(y))))
	}
	if ( missing(xlim) )
		xlim <- range(x) + rx * c(-1, 1)
	if ( missing(ylim) )
		ylim <- range(y) + ry * c(-1, 1)
	if ( is.function(col) ) {
		colors <- col(100)
	} else {
		colors <- col
	}
	bins <- cut(vals, breaks=seq(
		from=min(vals, na.rm=TRUE),
		to=max(vals, na.rm=TRUE),
		length.out=length(colors)+1),
		include.lowest=TRUE)
	colors <- colors[bins]
	plot(x, y, xlim=xlim, ylim=rev(ylim),
		pch=pch, asp=asp, col=colors, ...)
})

setMethod("image", c(x = "PositionDataFrame"),
	function(x, formula, ...,
		xlim,
		ylim,
		asp = 1,
		col = intensity.colors)
{
	obj <- x
	args <- .parseFormula2(formula,
		lhs.e=obj@listData,
		rhs.e=obj@coord,
		g.e=obj@listData)
	if ( length(args$rhs) != 2 )
		stop("rhs of formula must include 2 variables")
	x <- args$rhs[[1]]
	y <- args$rhs[[2]]
	vals <- args$lhs[[1]]
	if ( gridded(obj) ) {
		rx <- resolution(obj)[names(args$rhs[[1]])]
		ry <- resolution(obj)[names(args$rhs[[2]])]
	} else {
		rx <- min(diff(sort(unique(x))))
		ry <- min(diff(sort(unique(y))))
	}
	xrange <- range(x)
	yrange <- range(y)
	if ( missing(xlim) )
		xlim <- xrange + rx * c(-1, 1)
	if ( missing(ylim) )
		ylim <- yrange + ry * c(-1, 1)
	if ( is.function(col) ) {
		colors <- col(100)
	} else {
		colors <- col
	}
	if ( gridded(obj) ) {
		z <- projectToRaster(x, y, vals, dim=dims(obj))
	} else {
		z <- projectToRaster(x, y, vals, dim=NULL)
	}
	x <- seq(xrange[1], xrange[2], length.out=dim(z)[1])
	y <- seq(yrange[1], yrange[2], length.out=dim(z)[2])
	image(x, y, z, xlim=xlim, ylim=rev(ylim),
		asp=asp, col=colors, ...)
})

projectToRaster <- function(x, y, values, dim = NULL) {
	if ( is.double(x) || is.double(y) ) {
		x <- as.double(x)
		y <- as.double(y)
	}
	if ( is.null(dim) ) {
		xrange <- range(x)
		yrange <- range(y)
		asp <- diff(yrange) / diff(xrange)
		nrow <- as.integer(sqrt(length(values) / asp))
		ncol <- as.integer(asp * nrow)
		rx <- (xrange[2] - xrange[1]) / (nrow - 1)
		ry <- (yrange[2] - yrange[1]) / (ncol - 1)
		rows <- as.integer(round((x - xrange[1]) / rx))
		cols <- as.integer(round((y - yrange[1]) / ry))
	} else {
		rows <- dim[1]
		cols <- dim[2]
	}
	init <- as.vector(NA, mode=typeof(values))
	rs <- matrix(init, nrow=nrow, ncol=ncol)
	idx <- rows + cols * nrow + 1L
	idx[idx > length(rs)] <- length(rs)
	rs[idx] <- values
	rs
}

.parseFormula2 <- function(formula, lhs.e, rhs.e, g.e) {
	args <- .parseFormula(formula)
	args <- lapply(args, as.list)
	names(args) <- c("lhs", "rhs", "g")
	for ( i in seq_along(args$lhs) )
		args$lhs[[i]] <- eval(parse(text=args$lhs[[i]]),
			envir=lhs.e, enclos=environment(formula))
	for ( i in seq_along(args$rhs) )
		args$rhs[[i]] <- eval(parse(text=args$rhs[[i]]),
			envir=rhs.e, enclos=environment(formula))
	for ( i in seq_along(args$g) )
		args$g[[i]] <- eval(parse(text=args$g[[i]]),
			envir=g.e, enclos=environment(formula))
	lapply(args, function(a) if ( length(a) == 0) NULL else a)
}
