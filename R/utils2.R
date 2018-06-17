
# Apply that returns a 'list' instead of array
.apply <- function(x, margin, fun, ...) {
	fun <- match.fun(fun)
	if ( margin == 1L ) {
		lapply(seq_len(nrow(x)), function(i) fun(x[i,,drop=TRUE]))
	} else if ( margin == 2L ) {
		lapply(seq_len(ncol(x)), function(j) fun(x[,j,drop=TRUE]))
	} else {
		stop("internal 'apply' error")
	}
}

# Create a virtual copy of the object with no data
.virtual.image <- function(x) {
	imageData(x) <- new(class(imageData(x)))
	if ( validObject(x) )
		x
}

# Setup plotting layout
.setup.layout <- function(layout) {
	par(mar=c(3,3,2,1), mgp=c(1.5,0.5,0),
		cex.axis=0.8, cex.lab=0.8)
	layout(matrix(seq_len(prod(layout)),
		ncol=layout[1], nrow=layout[2],
		byrow=TRUE))
}

# Draw strip labels
.draw.strip.labels <- function(strip, text) {
	args <- switch(class(strip),
		"logical"=list(),
		"character"=list(legend=strip),
		"list"=strip,
		stop("invalid 'strip' argument"))
	if ( is.character(strip) || is.list(strip) )
		strip <- TRUE
	if ( !"x" %in% names(args) )
		args$x <- "top"
	if ( !"legend" %in% names(args) )
		args$legend <- text
	if ( !"x.intersp" %in% names(args) )
		args$x.intersp <- 0
	if ( !"bg" %in% names(args) )
		args$bg <- rgb(1, 1, 1, 0.75)
	if ( !"cex" %in% names(args) )
		args$cex <- 0.8
	if ( isTRUE(strip) && length(args$legend) != 0 )
		do.call("legend", args)
}

# Draw keys
.draw.key <- function(key, text, fill) {
	args <- switch(class(key),
		"logical"=list(),
		"character"=list(legend=key),
		"list"=key,
		stop("invalid 'key' argument"))
	if ( is.character(key) || is.list(key) )
		key <- TRUE
	if ( !"x" %in% names(args) )
		args$x <- "topright"
	if ( !"legend" %in% names(args) )
		args$legend <- text	
	if ( !"fill" %in% names(args) )
		args$fill <- fill
	if ( !"bg" %in% names(args) )
		args$bg <- rgb(1, 1, 1, 0.75)
	if ( isTRUE(key) && length(args$legend) != 0 )
		do.call("legend", args)
}

# Draw colorkeys
.draw.colorkey <- function(colorkey, text, col) {
	args <- switch(class(colorkey),
		"logical"=list(),
		"character"=list(x=colorkey),
		"list"=colorkey,
		stop("invalid 'colorkey' argument"))
	if ( is.character(colorkey) || is.list(colorkey) )
		colorkey <- TRUE
	if ( !"x" %in% names(args) )
		args$x <- "topright"
	if ( !"legend" %in% names(args) )
		args$legend <- c(text[2], rep(NA, length(col) - 2L), text[1])
	if ( !"col" %in% names(args) )
		args$col <- rev(col)
	if ( !"y.intersp" %in% names(args) )
		args$y.intersp <- 0.1
	if ( !"bg" %in% names(args) )
		args$bg <- rgb(1, 1, 1, 0.75)
	if ( !"cex" %in% names(args) )
		args$cex <- 0.6
	if ( !"lwd" %in% names(args) )
		args$lwd <- 2
	if ( isTRUE(colorkey) )
		do.call("legend", args)
}

## Format numbered labels
.format.numbered <- function(label, n, sep="") {
	if ( n == 1L )
		return(label)
	nums <- as.character(seq_len(n))
	nums <- paste0(sep, nums)
	nums[1L] <- ""
	paste0(label, nums)
}

## Format a data.frame of labels
.format.data.labels <- function(data, append = "") {
	data <- as.data.frame(data)
	apply(data, 1, function(a) {
		a <- paste0(names(a), " = ", a)
		a <- paste0(a, append)
		paste0(a, collapse=", ")
	})
}

## Format plotting labels
.format.plot.label <- function(label, character.only=FALSE) {
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
.format.mz <- function(mz, digits=2) {
	diffmz <- diff(mz)
	if ( length(diffmz) > 0 ) {
		mindiff <- signif(min(diffmz), 1)
		dplaces <- strsplit(paste(mindiff), "[.]")[[1]]
		if ( length(dplaces) > 1 ) {
			dplaces <- nchar(strsplit(paste(mindiff), "[.]")[[1]][2])	
		} else {
			dplaces <- 0
		}
	} else {
		dplaces <- 0
	}
	digits <- max(dplaces, digits)
	if ( length(mz) > 0 ) {
		paste0("m/z", " = ", round(mz, digits=digits))
	} else {
		character()
	}
}
