
## Utility function for converting pixels to pData row indices
.match.pixel <- function(object, pixel) {
	ps <- seq_len(ncol(object))
	names(ps) <- pixelNames(object)
	ps[pixel]
}

## Utility function for converting features to fData row indices
.match.feature <- function(object, feature) {
	fs <- seq_len(nrow(object))
	names(fs) <- featureNames(object)
	fs[feature]
}
