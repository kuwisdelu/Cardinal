
#### Peak picking methods ####
## ---------------------------

setMethod("peakPick", "MSImagingExperiment",
	function(object, method = c("simple", "adaptive"), ...)
	{
		fun <- peakPick.method2(method)
		object <- process(object, fun=fun, ...,
			label="peakPick", kind="pixel",
			postfun=peakPick_postfun,
			plotfun=peakPick_plotfun,
			delay=TRUE)
		object
	})

peakPick_plotfun <- function(s2, s1, ...,
	main="Peak picking", xlab="m/z", ylab="")
{
	mz <- mz(attr(s1, "mcols"))
	plot(mz, s1, main=main, xlab=xlab, ylab=ylab,
		col="gray", type='l', ...)
	lines(mz[s2], s1[s2], col="red", type='h')
}

peakPick_postfun <- function(object, peak_ids, ...) {
	numPeaks <- lengths(peak_ids)
	.message("detected ~", round(mean(numPeaks)), " peaks per spectrum ",
		"(min = ", min(numPeaks), ", max = ", max(numPeaks), ")")
	if ( is(peak_ids, "matter_list") ) {
		mzData <- matter_list(datamode=typeof(mz(object)),
			lengths=lengths(peak_ids), paths=tempfile())
		peakData <- matter_list(datamode=typeof(spectra(object)[,1L]),
			lengths=lengths(peak_ids), paths=tempfile())
		.message("copying to tempfile = ", paths(mzData))
		.message("copying to tempfile = ", paths(peakData))
		for ( i in seq_len(length(peak_ids)) ) {
			idx <- peak_ids[[i]]
			mzData[[i]] <- mz(object)[idx]
			peakData[[i]] <- spectra(object)[idx,i]
		}
	} else {
		mzData <- lapply(peak_ids, function(idx) mz(object)[idx])
		peakData <- lapply(seq_along(peak_ids), function(i) {
			idx <- peak_ids[[i]]
			s <- spectra(object)[idx,i]
			as.numeric(s)
		})
	}
	tol <- c(absolute=min(abs(diff(mz(object)))) / 2)
	data <- list(keys=mzData, values=peakData)
	data <- sparse_mat(data, keys=mz(object),
		nrow=nrow(object), ncol=ncol(object),
		tolerance=tol, combiner="sum")
	imageData(object) <- MSProcessedImagingSpectraList(data)
	object <- as(object, "MSProcessedImagingExperiment")
	object
}

peakPick.method2 <- function(method) {
	if ( is.character(method) ) {
		method <- match.method(method, c("simple", "adaptive"))
		switch(method,
			simple = peakPick.simple2,
			adaptive = peakPick.adaptive2,
			match.fun(method))
	} else {
		match.fun(method)
	}
}

peakPick.simple2 <- function(x, ...) {
	which(peakPick.simple(x, ...)$peaks)
}

peakPick.adaptive2 <- function(x, ...) {
	which(peakPick.adaptive(x, ...)$peaks)
}

