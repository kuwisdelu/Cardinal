
#### Align spectra to reference spectrum ####
## -----------------------------------------

setMethod("mzAlign", c("MSImagingExperiment", "numeric"),
	function(object, ref, tolerance = NA, units = c("ppm", "mz"),
		quantile = 0.2, span = 0.75, ...)
	{
		units <- match.arg(units)
		if ( is.na(tolerance) )
			tolerance <- 2 * .estimateMassResolution(mz(object), units)
		tol <- switch(units,
			ppm = c("relative" = unname(tolerance) * 1e-6),
			mz = c("absolute" = unname(tolerance)))
		if ( length(ref) != nrow(object) ) {
			.stop("length of reference [", length(ref),
				"] does not match object [", nrow(object), "]")
		}
		metadata(featureData(object))[["reference spectrum"]] <- ref
		object <- process(object, fun=mzAlign_fun,
			label="mzAlign", kind="pixel",
			moreargs=list(tol=tol, span=span, quantile=quantile),
			plotfun=mzAlign_plotfun,
			delay=getCardinalDelayProc())
		object
	})

setMethod("mzAlign", c("MSImagingExperiment", "missing"),
	function(object, tolerance = NA, units = c("ppm", "mz"),
		quantile = 0.2, span = 0.75, ...)
	{
		units <- match.arg(units)
		if ( is.na(tolerance) )
			tolerance <- 2 * .estimateMassResolution(mz(object), units)
		tol <- switch(units,
			ppm = c("relative" = unname(tolerance) * 1e-6),
			mz = c("absolute" = unname(tolerance)))
		object <- process(object, fun=mzAlign_fun,
			label="mzAlign", kind="pixel",
			moreargs=list(tol=tol, span=span, quantile=quantile),
			prefun=mzAlign_prefun,
			plotfun=mzAlign_plotfun,
			delay=TRUE)
		object
	})

mzAlign_fun <- function(x, tol, span, quantile, ...) {
	mz <- mz(attr(x, "mcols"))
	ref <- metadata(attr(x, "mcols"))[["reference spectrum"]]
	if ( is.null(ref) )
		.stop("couldn't find reference spectrum")
	if ( length(ref) != length(x) ) {
		.stop("length of reference [", length(ref),
			"] does not match object [", length(x), "]")
	}
	tol.ref <- switch(names(tol),
		relative = "key",
		absolute = "none")
	max.test <- locmax(x)
	max.ref <- locmax(ref)
	cutoff <- quantile(ref[max.ref], 1 - quantile)
	max.ref <- max.ref[ref[max.ref] >= cutoff]
	mz.test <- mz[max.test]
	mz.ref <- mz[max.ref]
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
	shift <- suppressWarnings(loess(diff ~ mz.test, span=span))
	dmz <- predict(shift, mz)
	warp <- splinefun(mz + dmz, x)
	s <- pmax(warp(mz), 0)
	attr(s, "ref") <- ref
	s
}

mzAlign_prefun <- function(object, ..., BPPARAM) {
	s <- rowStats(spectra(object), stat="mean",
		chunks=getCardinalNumBlocks(),
		verbose=getCardinalVerbose(),
		BPPARAM=BPPARAM)
	metadata(featureData(object))[["reference spectrum"]] <- s
	object
}

mzAlign_plotfun <- function(s2, s1, ...,
	main="Spectral alignment", xlab="m/z", ylab="")
{
	mz <- mz(attr(s1, "mcols"))
	ref <- metadata(attr(s1, "mcols"))[["reference spectrum"]]
	if ( is.null(ref) )
		.stop("couldn't find reference spectrum")
	plot(mz, s1, main=main, xlab=xlab, ylab=ylab,
		col="gray", type='l', ...)
	ref <- attr(s2, "ref")
	if ( !is.null(ref) )
		lines(mz, ref, col="green")
	lines(mz, s2, col="black")
}
