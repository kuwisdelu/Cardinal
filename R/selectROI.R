
#### Select a Region-of-Interest ####

setMethod("selectROI", "SparseImagingExperiment",
	function(object, ..., mode = c("region", "pixels"))
	{
		mode <- match.arg(mode)
		dots <- match.call(expand.dots=FALSE)$...
		if ( length(dots) > 0L ) {
			p <- image(object, ...)
			print(p)
		} else {
			p <- .Cardinal$lastplot
			if ( is.null(dev.list()) )
				print(p)
		}
		.selectROI(object, p, mode)
	})

# selector functions

.selectROI <- function(object, plot, mode) {
	if ( is.null(dev.list()) )
		.stop("no open plot to use")
	title(sub="select pixels", col.sub="red")
	nruns <- nlevels(droplevels(run(object)[plot$subset]))
	if ( nruns > 1 )
		.warning("multiple runs plotted; results may be unexpected")
	.message("select pixels; press ESC or 2nd mouse button to stop")
	loc <- .locator(mode == "region")
	if ( mode == "region" ) {
		.selectRegion(loc, pixelData(object),
			subset=plot$subset, axs=plot$coordnames)
	} else {
		.selectPixels(loc, pixelData(object),
			subset=plot$subset, axs=plot$coordnames)
	}
}

.selectRegion <- function(loc, pdata, subset, axs = c("x", "y")) {
	roi <- rep(FALSE, nrow(pdata))
	coord <- coord(pdata)[subset,axs,drop=FALSE]
	selected <- point.in.polygon(coord[,1], coord[,2], loc$x, loc$y) > 0
	roi[subset] <- selected
	roi
}

.selectPixels <- function(loc, pdata, subset, axs = c("x", "y")) {
	pixels <- rep(FALSE, nrow(pdata))
	coord <- coord(pdata)[subset,axs,drop=FALSE]
	if ( !gridded(pdata) )
		.warning("pixel coordinates are not gridded")
	selected <- data.frame(round(loc$x), round(loc$y))
	names(selected) <- names(coord)
	coord$..i.. <- seq_len(nrow(coord))
	idx <- merge(coord, selected, by=axs)$..i..
	p <- rep(FALSE, nrow(coord))
	p[idx] <- TRUE
	pixels[subset] <- p
	pixels
}

# hack to force plot to update on RStudio device

.locator <- function(area = TRUE, fill = TRUE) {
	xs <- numeric()
	ys <- numeric()
	while ( TRUE ) {
		loc <- locator(1)
		if ( !is.null(loc) ) {
			if ( area ) {
				xi <- c(xs[length(xs)], loc$x)
				yi <- c(ys[length(ys)], loc$y)
				lines(xi, yi, type='b', pch=20, col="white")
			} else {
				points(loc$x, loc$y, pch=4, col="white")
			}
			xs <- c(xs, loc$x)
			ys <- c(ys, loc$y)
		} else {
			break
		}
	}
	if ( area && fill )
		polygon(xs, ys, col=rgb(1,1,1,0.5))
	list(x=xs, y=ys)
}

# make a factor from logicals

makeFactor <- function(..., ordered = FALSE) {
	inds <- list(...)
	labs <- sapply(substitute(...()), deparse)
	if ( !is.null(names(inds)) ) {
		nz <- nzchar(names(inds))
		labs[nz] <- names(inds)[nz]
	}
	names(labs) <- NULL
	inds <- do.call("cbind", inds)
	inds <- apply(inds, 1, function(i) which(i)[1L])
	factor(labs[inds], levels=labs, ordered=ordered)
}
