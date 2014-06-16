
## Format a list of arguments
.format.list <- function(args) {
	apply(args, 1, function(a) {
		paste(paste(names(a), "=", a), collapse=", ")
	})
}

## Format plotting labels
.format.label <- function(label) {
	if ( label == "mz" ) {
		expression(italic(m/z))
	} else {
		parse(text=paste0("italic(", label, ")"))
	}
}

## Format m/z values
.format.mz <- function(mz) {
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
