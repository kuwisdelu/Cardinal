
#### Align spectra to reference spectrum ####
## -----------------------------------------

setMethod("mzAlign", c("MSImagingExperiment", "numeric"),
	function(object, ref, tolerance = NA, units = c("ppm", "mz"),
		span = 0.75, control = loess.control(), ...)
	{
		units <- match.arg(units)
		if ( is.na(tolerance) )
			tolerance <- 2 * .estimateMassResolution(mz(object), units)
		tol <- switch(units,
			ppm = c("relative" = unname(tolerance) * 1e-6),
			mz = c("absolute" = unname(tolerance)))
		metadata(featureData(object))[["reference peaks"]] <- ref
		object <- process(object, fun=mzAlign_fun,
			label="mzAlign", kind="pixel",
			moreargs=list(tol=tol, span=span, control=control),
			plotfun=mzAlign_plotfun,
			delay=getCardinalDelayProc())
		object
	})

setMethod("mzAlign", c("MSImagingExperiment", "missing"),
	function(object, tolerance = NA, units = c("ppm", "mz"),
		span = 0.75, control = loess.control(), quantile = 0.2, ...)
	{
		units <- match.arg(units)
		if ( is.na(tolerance) )
			tolerance <- 2 * .estimateMassResolution(mz(object), units)
		tol <- switch(units,
			ppm = c("relative" = unname(tolerance) * 1e-6),
			mz = c("absolute" = unname(tolerance)))
		object <- process(object, fun=mzAlign_fun,
			label="mzAlign", kind="pixel",
			moreargs=list(tol=tol, span=span, control=control),
			prefun=mzAlign_prefun,
			preargs=list(quantile=quantile),
			plotfun=mzAlign_plotfun,
			delay=TRUE)
		object
	})

mzAlign_fun <- function(x, tol, span, control, ...) {
	mz <- mz(attr(x, "mcols"))
	mz.ref <- metadata(attr(x, "mcols"))[["reference peaks"]]
	if ( is.null(mz.ref) )
		.stop("couldn't find reference peaks")
	tol.ref <- switch(names(tol),
		relative = "key",
		absolute = "none")
	max.test <- locmax(x)
	mz.test <- mz[max.test]
	i <- bsearch(mz.ref, mz.test, tol=tol, tol.ref=tol.ref)
	found <- !is.na(i)
	if ( sum(found) < 1 ) {
		.warning("no matching peaks found; try a larger tolerance")
		return(x)
	}
	mz.ref <- mz.ref[found]
	i <- i[found]
	mz.test <- mz.test[i]
	diff <- mz.ref - mz.test
	mz.test <- c(mz[1], mz.test, mz[length(mz)])
	diff <- c(diff[1], diff, diff[length(diff)])
	shift <- suppressWarnings(loess(diff ~ mz.test, span=span, control=control))
	dmz <- predict(shift, mz)
	warp <- splinefun(mz + dmz, x)
	pmax(warp(mz), 0)
}

mzAlign_prefun <- function(object, quantile, ..., BPPARAM) {
	s <- rowStats(spectra(object), stat="mean",
		chunks=getCardinalNumBlocks(),
		verbose=getCardinalVerbose(),
		BPPARAM=BPPARAM)
	maxs <- locmax(s)
	cutoff <- quantile(s[maxs], 1 - quantile)
	maxs <- maxs[s[maxs] >= cutoff]
	mz.ref <- mz(object)[maxs]
	metadata(featureData(object))[["reference peaks"]] <- mz.ref
	object
}

mzAlign_plotfun <- function(s2, s1, ...,
	main="Spectral alignment", xlab="m/z", ylab="")
{
	mz <- mz(attr(s1, "mcols"))
	ref <- metadata(attr(s1, "mcols"))[["reference peaks"]]
	if ( is.null(ref) )
		.stop("couldn't find reference peaks")
	plot(mz, s1, main=main, xlab=xlab, ylab=ylab,
		col="gray", type='l', ...)
	abline(v=ref, col="green", lty=2)
}
