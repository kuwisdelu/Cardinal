
#### functions for formatting for printing and plotting ####

formatMZ <- function(mz) {
	if ( is.null(mz) || length(mz) < 1 ) {
		""
	} else {
		if ( length(mz) > 1 ) mz <- c(mz[[1]], mz[[length(mz)]])
		paste("m/z =", paste(round(mz, digits=2), collapse=" - "))
	}
}

formatCoord <- function(coord, n=10) {
	paste(paste(names(coord), collapse=", "), " = ",
		paste(coord, collapse=", "), sep="")
}
