
#### Methods for SummaryDataFrame ####
## ---------------------------------

SummaryDataFrame <- function(..., .rownumbers = FALSE, .summary = list()) {
	ans <- as(DataFrame(..., check.names=FALSE), "SummaryDataFrame")
	if ( .rownumbers )
		rownames(ans) <- seq_len(nrow(ans))
	ans@summary <- as.list(.summary)
	ans
}

# Extract summary information (for printing)

setMethod("summary", "SummaryDataFrame",
	function(object) object@summary)

# Need to overwrite 'show' methods for SummaryDataFrame 
# to print out any elements of "summary" before the data

setMethod("show", "SummaryDataFrame",
	function(object)
{
	for ( s in summary(object) ) {
		if ( is.null(s) )
			next
		if ( is.character(s) ) {
			cat(s, "\n")
		} else {
			print(s)
		}
	}
	vars <- names(object)
	nms <- rownames(object)
	out <- as.data.frame(object)
	out <- as.matrix(format(out))
	colnames(out) <- vars
	if ( is.null(nms) ) {
		rownames(out) <- rep_len("", nrow(out))
	} else {
		rownames(out) <- nms
	}
	print(out, quote = FALSE, right = TRUE)
})

