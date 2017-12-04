#### Methods for XDataFrame ####
## ------------------------------

# Need to overwrite 'names', 'length', and 'show' methods
# for XDataFrame which may have special eXtra columns that
# must be printed but not counted as part of 'ncol'.
#
# Subclasses need to define 'lapply' method to include
# the additional columns by default for printing, and
# optionally redefine 'as.list' method for consistency

setMethod("names", "XDataFrame", 
	function(x) {
		names(x@listData)
	})

setMethod("length", "XDataFrame", 
	function(x) {
		length(x@listData)
	})

setMethod("show", "XDataFrame",
	function(object)
{
	nhead <- get_showHeadLines()
	ntail <- get_showTailLines()
	nr <- nrow(object)
	nc <- ncol(object)
	show_nc <- length(lapply(object, function(x) NULL))
	cat(class(object), " with ",
		nr, ifelse(nr == 1, " row and ", " rows and "),
		nc, ifelse(nc == 1, " column\n", " columns\n"),
		sep = "")
	if (nr > 0 && show_nc > 0) {
		nms <- rownames(object)
		if (nr <= (nhead + ntail + 1L)) {
			out <-
				as.matrix(format(as.data.frame(
					lapply(object, showAsCell),
						optional = TRUE)))
			if (!is.null(nms))
				rownames(out) <- nms
		} else {
			out <-
				rbind(as.matrix(format(as.data.frame(
						lapply(object, function(x)
							showAsCell(head(x, nhead))),
						optional = TRUE))),
					rbind(rep.int("...", show_nc)),
					as.matrix(format(as.data.frame(
						lapply(object, function(x) 
							showAsCell(tail(x, ntail))),
						optional = TRUE))))
				rownames(out) <- .rownames(nms, nr, nhead, ntail) 
		}
		classinfo <-
			matrix(unlist(lapply(object, function(x)
						paste0("<", classNameForDisplay(x)[1], ">")),
					use.names = FALSE), nrow = 1,
				dimnames = list("", colnames(out)))
		out <- rbind(classinfo, out)
		print(out, quote = FALSE, right = TRUE)
	}
})

# copied from S4Vectors::show,DataTable
.rownames <- function(nms, nrow, nhead, ntail)
{
	p1 <- ifelse (nhead == 0, 0L, 1L)
	p2 <- ifelse (ntail == 0, 0L, ntail-1L)
	s1 <- s2 <- character(0)

	if ( is.null(nms) ) {
		if ( nhead > 0 )
			s1 <- paste0(as.character(p1:nhead))
		if ( ntail > 0 )
			s2 <- paste0(as.character((nrow-p2):nrow))
	} else { 
		if ( nhead > 0 )
			s1 <- paste0(head(nms, nhead))
		if ( ntail > 0 )
			s2 <- paste0(tail(nms, ntail))
	}
	c(s1, "...", s2)
}
