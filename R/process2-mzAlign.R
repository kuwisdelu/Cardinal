
#### Align spectra to reference spectrum ####
## -----------------------------------------

setMethod("mzAlign", c("MSImagingExperiment", "numeric"),
	function(object, ref, tolerance = 1000, units = c("ppm", "mz"),
		quantile = 0.2, span = 0.75, ...)
	{
		tol <- switch(match.arg(units),
			ppm = c("relative" = tolerance * 1e-6),
			mz = c("absolute" = tolerance))
		fun <- mzAlign_fun(tol, span, quantile)
		if ( length(ref) != nrow(object) ) {
			.stop("length of reference [", length(ref),
				"] does not match object [", nrow(object), "]")
		}
		metadata(featureData(object))[["reference spectrum"]] <- ref
		object <- process(object, fun=fun,
			label="mzAlign", kind="pixel",
			moreargs=list(...),
			plotfun=mzAlign_plotfun,
			delay=getOption("Cardinal.delay"))
		object
	})

setMethod("mzAlign", c("MSImagingExperiment", "missing"),
	function(object, tolerance = 1000, units = c("ppm", "mz"),
		quantile = 0.2, span = 0.75, ...)
	{
		tol <- switch(match.arg(units),
			ppm = c("relative" = tolerance * 1e-6),
			mz = c("absolute" = tolerance))
		fun <- mzAlign_fun(tol, span, quantile)
		object <- process(object, fun=fun, ...,
			label="mzAlign", kind="pixel",
			prefun=mzAlign_prefun,
			plotfun=mzAlign_plotfun,
			delay=TRUE)
		object
	})

mzAlign_fun <- function(tol, span, quantile) {
	fun <- function(x, ...) {
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
		max.test <- localMaxima(x)
		max.ref <- localMaxima(ref)
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
	fun
}

mzAlign_prefun <- function(object, ..., BPPARAM) {
	s <- summarize(object, .stat="mean",
		.by="feature", BPPARAM=BPPARAM)
	metadata(featureData(object))[["reference spectrum"]] <- s$mean
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
