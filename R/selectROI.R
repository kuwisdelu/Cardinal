
#### Select ROIs ####
## -------------------

setMethod("selectROI", "SpectralImagingExperiment",
	function(object, ..., mode = c("region", "pixels"))
{
	mode <- match.arg(mode)
	if ( ...length() > 0L ) {
		plot(image(object, ...))
	} else {
		plot(.lastplot$image)
	}
	.select_ROI(object, mode)
})

.select_ROI <- function(object, mode)
{
	if ( length(dev.list()) == 0L )
		stop("no plot available to use")
	if ( nrun(object) > 1L )
		warning("multiple runs plotted; results may be unexpected")
	box(bty="o", col="red", lty="solid", lwd=2)
	message("select pixels; press ESC or 2nd mouse button to stop")
	loc <- .select_locator(mode == "region")
	roi <- logical(ncol(object))
	sub <- rep_len(.lastplot$subset, ncol(object))
	pos <- coord(object)[sub,,drop=FALSE]
	if ( mode == "region" ) {
		selected <- inpoly(pos, cbind(loc$x, loc$y))
	} else {
		selected <- logical(nrow(pos))
		ind <- kdsearch(pos, cbind(loc$x, loc$y), tol=0.5)
		selected[unlist(ind)] <- TRUE
	}
	roi[sub] <- selected
	roi
}

.select_locator <- function(area = TRUE, fill = TRUE)
{
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

