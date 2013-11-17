
#### functions for peak alignment ####

alignPeaksFunction <- function(method) {
	if ( is.function(method) ) {
		return(method)
	} else if ( is.character(method) ) {
		if ( length(method) > 1 ) method <- method[[1]]
		switch(method,
			"diff" = alignPeaksDiff,
			"ztest" = alignPeaksZTest,
			"dp" = alignPeaksDP,
			match.fun(method)
		)
	} else {
		stop("could not find matching function for ", substitute(method))
	}
}

alignPeaksDiff <- function(peaks, reference, diff.max=200, units=c("ppm", "mz")) {
	units <- match.arg(units)
	if ( units == "ppm" ) {
		diff.max <- 1e-6 * diff.max * reference[["mz"]]
	} else if ( length(diff.max) != length(reference) ) {
		diff.max <- rep(diff.max, length(reference))
	}
	mz.peaks <- matrix(peaks[["mz"]], nrow=length(reference[["mz"]]),
		ncol=length(peaks[["mz"]]), byrow=TRUE)
	mz.reference <- matrix(reference[["mz"]], nrow=length(reference[["mz"]]),
		ncol=length(peaks[["mz"]]))
	mz.diff <- abs(mz.reference - mz.peaks)
	mz.min <- apply(mz.diff, 1, min)
	mz.which <- apply(mz.diff, 1, which.min)
	matched <- data.frame(peaks=mz.which, reference=1:length(reference[["mz"]]))
	matched <- matched[mz.min < diff.max,]
	rownames(matched) <- NULL
	matched
}

alignPeaksZTest <- function(peaks, reference, alpha=0.05, ...) {
	p.values <- ztests(peaks[,c("mu", "sigma")], reference[,c("mu", "sigma")])
	matched.ref <- apply(p.values, 1, function(p) {
		if ( max(p) > alpha ) which.max(p) else NULL
	} )
	matched.ref <- data.frame(peaks=unlist(matched.ref),
		reference=(1:nrow(p.values))[!sapply(matched.ref, is.null)])
	matched.peaks <- apply(p.values, 2, function(p) {
		if ( max(p) > alpha ) which.max(p) else NULL
	} )
	matched.peaks <- data.frame(peaks=(1:ncol(p.values))[!sapply(matched.peaks, is.null)],
		reference=unlist(matched.peaks))
	if ( nrow(matched.peaks) == 0 || nrow(matched.ref) == 0 ) return(data.frame())
	matched <- merge(matched.peaks, matched.ref)
	matched <- matched[order(matched$peaks),]
	rownames(matched) <- NULL
	matched
}

alignPeaksDP <- function(peaks, reference, gap=0, centroid=FALSE, ...) {
	if ( centroid ) {
		peaks1 <- peaks[["mu"]]
		peaks2 <- reference[["mu"]]
	} else {
		peaks1 <- peaks[["mz"]]
		peaks2 <- reference[["mz"]]
	}
	matched <- dynamicAlign(as.numeric(peaks1), as.numeric(peaks2), gap=gap, ...)
	matched <- as.data.frame(matched)
	names(matched) <- c("peaks", "reference")
	rownames(matched) <- NULL
	matched
}

#### helper functions ####

listPeaks <- function(peaks) {
	l <- lapply(1:nrow(peaks@peaks), function(i) peaks[i,])
	new("MSPeakList", peaks=l, metaData=generateMetaData("MSPeakList"))
}

combinePeaks <- function(peaklist, reflist, matched) {
	if ( nrow(matched) < 1 ) return(poolPeaks(peaklist, reflist))
	alignlist <- mapply(poolPeaks, peaklist@peaks[matched[["peaks"]]],
		reflist@peaks[matched[["reference"]]], SIMPLIFY=FALSE)
	alignlist <- new("MSPeakAlignment", peaks=alignlist,
		metaData=generateMetaData("MSPeakAlignment"))
	alignlist
}

combinePeaks2 <- function(peaklist1, peaklist2, matched) {
	if ( nrow(matched) < 1 ) return(poolPeaks(peaklist1, peaklist2))
	peaklist <- mapply(poolPeaks, peaklist1@peaks[matched[,1]],
		peaklist2@peaks[matched[,2]], SIMPLIFY=FALSE)
	peaklist <- new("MSPeakAlignment", peaks=peaklist,
		metaData=generateMetaData("MSPeakAlignment"))
	poolPeaks(peaklist, peaklist1[-matched[,1]], peaklist2[-matched[,2]])
}


