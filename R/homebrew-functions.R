
#### assorted helper functions ####

# match parameters in an MSImageSegmentation
match.which <- function(x, which) {
	if ( is.list(which) ) {
		lwhich <- lapply(names(which), function(n) {
			 metaData(x)$parameters[[n]] %in% which[[n]]
		} )
		if ( length(lwhich) > 1 ) {
			lwhich <- do.call(`&`, lwhich)
		} else {
			lwhich <- lwhich[[1]]
		}
		which <- which(lwhich)
	} else if ( is.character(which) ) {
		which <- which(metaData(x)[["parnames"]] %in% which)
	}
	which
}
