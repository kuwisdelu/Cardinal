
## Format a data.frame of labels
.format.data.frame <- function(data) {
	apply(data, 1, function(a) {
		paste0(paste0(names(a), " = ", a), collapse=", ")
	})
}

## Format plotting labels
.format.label <- function(label, character.only=FALSE) {
	if ( character.only ) {
		if ( label == "mz" ) {
			"m/z"
		} else {
			label
		}
	} else {
		if ( label == "mz" ) {
			expression(italic(m/z))
		} else {
			parse(text=paste0("italic(", label, ")"))
		}
	}
}

## Format m/z values
.format.mz <- function(mz) {
	diffmz <- diff(mz)
	if ( length(diffmz) > 0 ) {
		mindiff <- signif(min(diffmz), 1)
		digits <- strsplit(paste(mindiff), "[.]")[[1]]
		if ( length(digits) > 1 ) {
			digits <- nchar(strsplit(paste(mindiff), "[.]")[[1]][2])	
		} else {
			digits <- 0
		}
	} else {
		digits <- 0
	}
	digits <- max(digits, 2)
	if ( length(mz) > 0 ) {
		paste0("m/z", " = ", round(mz, digits=digits))
	} else {
		character()
	}
}
