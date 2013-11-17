
#### functions for baseline estimate ####

estimateBaselineFunction <- function(method) {
	if ( is.function(method) ) {
		return(method)
	} else if ( is.character(method) ) {
		if ( length(method) > 1 ) method <- method[[1]]
		switch(method,
			"interp" = estimateBaselineInterp1,
			match.fun(method)
		)
	} else {
		stop("could not find matching function for ", substitute(method))
	}
}

estimateBaselineInterp1 <- function(x, t, blocks=500, choose=min, preserve.peaks=TRUE,
	preserve.level=1, interp1=c("linear", "nearest", "pchip", "cubic", "spline"), ...)
{
	if ( missing(t) ) t <- 1:length(x)
	method = match.arg(interp1)
	xint <- intervals(x, blocks=blocks)
	choose <- match.fun(choose)
	choose.val <- sapply(xint, choose)
	choose.idx <- sapply(xint, function(xi) which.min(abs(choose(xi) - xi)))
	choose.idx <- choose.idx + c(0, cumsum(sapply(xint, length))[-length(xint)])
	if ( diff(range(choose.val))==0 ) return(rep(0, length(x)))
	if ( preserve.peaks ) {
		cutoff <- smooth.spline(x=t[choose.idx], y=choose.val, spar=preserve.level)$y
		keep <- which(choose.val <= cutoff)
		choose.idx <- choose.idx[keep]
		choose.val <- choose.val[keep]
	}
	choose.val[c(1,length(choose.val))] <- c(choose(xint[[1]]),
		choose(xint[[length(xint)]]))
	choose.idx[c(1,length(choose.idx))] <- c(1, length(x))
	interp1(x=t[choose.idx], y=choose.val, xi=t, method=method)
}

