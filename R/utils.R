
## Convert pixels to pData row indices
.match.pixel <- function(object, pixel) {
	ps <- seq_len(ncol(object))
	names(ps) <- pixelNames(object)
	ps[pixel]
}

## Convert features to fData row indices
.match.feature <- function(object, feature) {
	fs <- seq_len(nrow(object))
	names(fs) <- featureNames(object)
	fs[feature]
}

## Match methods to functions
.match.method <- function(method, which=-2) {
	tryCatch({
		base <- deparse(sys.call(which)[[1]])
		match.fun(paste(base, method, sep="."))
	}, error=function(e) match.fun(method))
}
