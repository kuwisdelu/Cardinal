
#### Baseline reduction methods ####
## ---------------------------------

setMethod("reduceBaseline", "MSImageSet",
	function(object, method = "interp",
		...,
		pixel=pixels(object),
		plot=FALSE)
	{
		fun <- reduceBaseline.method(method)
		data <- pixelApply(object, function(s) {
			sout <- fun(s, ...)
			if ( plot ) {
				wrap(plot(mz(object), s, type="l", xlab="m/z", ylab="Intensity", col="gray", ...),
					..., signature=fun)
				wrap(lines(mz(object), s - sout, col="red", ...),
					..., signature=fun)
				wrap(lines(mz(object), sout, lwd=0.5, ...),
					..., signature=fun)
			}
			sout
		}, .pixel=pixel, ..., .use.names=FALSE)
		object@imageData <- SImageData(data=data,
			coord=coord(object)[pixel,],
			storageMode=storageMode(object@imageData),
			dimnames=list(featureNames(object), pixelNames(object)[pixel]))
		object@pixelData <- object@pixelData[pixel,]
		baselineReduction(processingData(object)) <- match.method(method)
		prochistory(processingData(object)) <- .history()
		object
	})

reduceBaseline.method <- function(method) {
	if ( is.character(method) ) {
		method <- switch(method[[1]],
			interp = reduceBaseline.interp,
			match.fun(method))
	}
	match.fun(method)
}

reduceBaseline.interp <- function(x, blocks=500, choose=c("min", "median"), spar=1, ...) {
	xint <- intervals(x, blocks=blocks)
	choose <- match.fun(match.arg(choose))
	baseval <- sapply(xint, choose)
	baseidx <- sapply(xint, function(xi) which.min(abs(choose(xi) - xi)))
	baseidx <- baseidx + c(0, cumsum(sapply(xint, length))[-length(xint)])
	if ( diff(range(baseval))==0 )
		return(rep(0, length(x)))
	if ( !is.na(spar) ) {
		cutoff <- smooth.spline(x=baseidx, y=baseval, spar=spar)$y
		keep <- which(baseval <= cutoff)
		baseidx <- baseidx[keep]
		baseval <- baseval[keep]
	}
	baseval[c(1,length(baseval))] <- c(choose(xint[[1]]), choose(xint[[length(xint)]]))
	baseidx[c(1,length(baseidx))] <- c(1, length(x))
	baseline <- interp1(x=baseidx, y=baseval, xi=seq_along(x), method="linear")
	pmax(x - baseline, 0)
}
