
setMethod("select",
	signature = c(x = "SImageSet"),
	function(x, formula = ~ x * y,
		mode = c("region", "pixel"),
		...,
		main,
		subset = TRUE,
		lattice = FALSE)
	{
		mode <- match.arg(mode)
		if ( missing(main) )
			main <- paste("Select", mode)
		if ( lattice )
			.stop("select: Selection not currently supported for lattice graphics.")
		image(x, formula=formula, ..., main=main, subset=subset, lattice=lattice)
		model <- .parseImageFormula(formula, object=x, enclos=environment(formula))
		subset <- tryCatch(eval(substitute(subset), envir=pData(x),
			enclos=environment(formula)), error = function(e) eval(subset))
		if ( length(subset) < ncol(x) )
			subset <- rep(subset, length.out=ncol(x))
		.message("Select pixels and press ESC or second mouse button when done")
		if ( mode == "region" ) {
			loc <- locator(type="o", pch=20, col="white", lwd=1.5)
			if ( is.null(loc) ) return(NULL)
			coord <- coord(x)[subset, names(model$right)]
			selected <- point.in.polygon(coord[,1], coord[,2], loc$x, loc$y)
			selected <- selected > 0
			names(selected) <- pixelNames(x)
		} else {
			loc <- locator(type="p", pch=4, col="white")
			if ( is.null(loc) ) return(NULL)
			coord <- data.frame(round(loc$x), round(loc$y))
			names(coord) <- names(model$right)
			ok <- logical(ncol(x))
			ok[subset] <- TRUE
			selected <- logical(ncol(x))
			selected[pixels(x, coord=coord)] <- TRUE
			selected <- selected & ok
			names(selected) <- pixelNames(x)
		}
		selected
	})
