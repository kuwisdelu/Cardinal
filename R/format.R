
## Format m/z values
formatMz <- function(mz) {
	diffmz <- diff(mz)
	if ( length(diffmz) > 0 ) {
		mindiff <- signif(min(diffmz), 1)
		digits <- nchar(strsplit(paste(mindiff), "[.]")[[1]][2])
	} else {
		digits <- 0
	}
	digits <- max(digits, 2)
	if ( length(mz) > 0 ) {
		paste("m/z", "=", round(mz, digits=digits))
	} else {
		character()
	}
}

## Format coordinates values
formatCoord <- function(coord) {
	apply(coord, 1, function(xyz) {
		paste(paste(names(xyz), "=", xyz), collapse=", ")
	})
}
