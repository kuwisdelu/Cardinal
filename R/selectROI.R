
#### Select ROIs ####
## -------------------

setMethod("selectROI", "SpectralImagingExperiment",
	function(object, ..., mode = c("region", "pixels"))
{
	mode <- match.arg(mode)
	if ( ...length() > 0L ) {
		plot <- plot(image(object, ...))
	} else {
		plot <- plot(.last$image)
	}
	.select_ROI(object, plot, mode)
})

# combine logical ROIs into a factor
makeFactor <- function(..., ordered = FALSE)
{
	inds <- list(...)
	labs <- vapply(substitute(...()), deparse, character(1L))
	if ( !is.null(names(inds)) ) {
		nz <- nzchar(names(inds))
		labs[nz] <- names(inds)[nz]
	}
	names(labs) <- NULL
	inds <- do.call("cbind", inds)
	inds <- apply(inds, 1, function(i) which(i)[1L])
	factor(labs[inds], levels=labs, ordered=ordered)
}

.select_ROI <- function(object, plot, mode)
{
	if ( length(dev.list()) == 0L )
		.Error("no plot available to use")
	if ( nrun(object) > 1L )
		.Warn("multiple runs plotted; results may be unexpected")
	box(bty="o", col="red", lty="solid", lwd=2)
	.Message("select ", mode, ": press ESC or 2nd mouse button to stop")
	loc <- .select_locator(plot, mode == "region")
	roi <- logical(ncol(object))
	sub <- rep_len(.last$subset, ncol(object))
	pos <- coord(object)[sub,,drop=FALSE]
	if ( mode == "region" ) {
		selected <- inpoly(pos, cbind(loc$x, loc$y))
	} else {
		selected <- logical(nrow(pos))
		ind <- kdsearch(cbind(loc$x, loc$y), pos, tol=0.5)
		selected[unlist(ind)] <- TRUE
	}
	roi[sub] <- selected
	roi
}

.select_locator <- function(plot, region = TRUE)
{
	xs <- numeric()
	ys <- numeric()
	shape <- if (region) 1L else 4L
	while ( TRUE ) {
		loc <- locator(1)
		if ( !is.null(loc) ) {
			xs <- c(xs, loc$x)
			ys <- c(ys, loc$y)
			if ( region ) {
				plot <- add_mark(plot, "lines", x=xs, y=ys,
					params=list(col="white"),
					trans=list(sort=FALSE))
			}
			plot <- add_mark(plot, "points", x=xs, y=ys,
				params=list(col="white", pch=shape))
			print(plot)
			box(bty="o", col="red", lty="solid", lwd=2)
		} else {
			break
		}
	}
	if ( region ) {
		xsp <- c(xs, xs[1L])
		ysp <- c(ys, ys[1L])
		plot <- add_mark(plot, "lines", x=xsp, y=ysp,
			params=list(col="white"),
			trans=list(sort=FALSE))
		print(plot)
	}
	list(x=xs, y=ys)
}

.last <- list2env(list(
	plot = NULL,
	image = NULL,
	subset = TRUE
))

