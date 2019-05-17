
#### Save log and session history ####
## -----------------------------------

Cardinal.version <- function() {
	paste0(packageVersion("Cardinal"), collapse=".")
}

Cardinal.history <- function(file = "Cardinal.log", history = TRUE) {
	if ( !is.null(file) ) {
		path <- normalizePath(file, mustWork=FALSE)
		.message("saving history to ", path)
		if ( isTRUE(history) )
			.log(.history())
		con <- file(path, open="at")
		for ( m in .Cardinal$log )
			writeLines(c(m, "\n"), con=con)
		close(con)
		invisible(.Cardinal$log)
	} else {
		.Cardinal$log
	}
}

#### Logging and flushing log to output files ####
## -----------------------------------------------

.log <- function(...) {
	msg <- paste0(c(date(), ": ", ...), collapse="")
	.Cardinal$log <- append(.Cardinal$log, list(msg))
	flushtime <- getOption("Cardinal.flush")
	if ( !is.null(flushtime) ) {
		elapsed <- proc.time()[3] - .Cardinal$time$flush
		if ( elapsed > flushtime )
			.log.flush()
	}
}

.log.collapse <- function(...) {
	.log(paste0(c(...), collapse="\n"))
}

.log.flush <- function(e=.Cardinal) {
	if ( isTRUE(getOption("Cardinal.log")) && length(e$log) != 0 ) {
		res <- tryCatch({
			e$time$flush <- proc.time()[3]
			filepath <- file.path(tempdir(), "Cardinal.log")
			con <- file(filepath, open="at")
			for ( m in e$log )
				writeLines(c(m, "\n"), con=con)
			close(con)
			e$log <- list()
			sink()
			TRUE
		}, error=function(e) FALSE)
	} else {
		res <- FALSE
	}
	invisible(res)
}

#### User messages ####
## --------------------

.message <- function(..., progress=c("none", "start", "stop", "increment"), min=0, max=1) {
	progress <- match.arg(progress)
	if ( progress == "none" ) {
		.log(...)
		for ( f in .Cardinal$message ) {
			f(...)
		}
	} else if ( progress == "start" ) {
		if ( length(list(...)) > 1 )
			.log(...)
		.Cardinal$progress$i <- min
		.Cardinal$progress$min <- min
		.Cardinal$progress$max <- max
		for ( f in .Cardinal$progress$start ) {
			f(..., min=min, max=max)
		}
	} else if ( progress == "increment" ) {
		.Cardinal$progress$i <- .Cardinal$progress$i + 1
		for ( f in .Cardinal$progress$increment ) {
			f()
		}
	} else if ( progress == "stop" ) {
		if ( length(list(...)) > 1 )
			.log(...)
		for ( f in .Cardinal$progress$stop ) {
			f(...)
		}
	}
}

#### Console messages and progress bars ####
## -----------------------------------------

.console <- function(...) {
	if ( getOption("Cardinal.verbose") ) {
		message(...)
		flush.console()
	}
}

.time.start <- function() {
	.Cardinal$time$start <- proc.time()
}

.time.stop <- function() {
	.Cardinal$time$stop <- proc.time()
	time <- .Cardinal$time$stop - .Cardinal$time$start
	if ( isTRUE(getOption("Cardinal.time")) )
		.console("Cardinal: Operation took ", round(time[[3]], digits=1), " seconds.")
	.log("Cardinal: Operation took ", round(time[[3]], digits=2), " seconds.")
}

.progress.start <- function(..., min=0, max=1) {
	if ( length(list(...)) > 1 )
		.message(...)
	.Cardinal$progress$bar <- NULL
	if ( getOption("Cardinal.progress") && max - min > 1 ) {
		.Cardinal$progress$bar <- txtProgressBar(min=min, max=max, style=3)
	}
}

.progress.increment <- function() {
	if ( getOption("Cardinal.progress") && !is.null(.Cardinal$progress$bar) ) {
		setTxtProgressBar(.Cardinal$progress$bar, value=.Cardinal$progress$i)
	}
}

.progress.stop <- function(...) {
	if ( getOption("Cardinal.progress") && !is.null(.Cardinal$progress$bar) ) {
		close(.Cardinal$progress$bar)
	}
	.Cardinal$progress$bar <- NULL
	if ( length(list(...)) > 1 )
		.message(...)
}

#### Generate R console history ####
## ---------------------------------

.history <- function() {
	file <- tempfile(fileext=".Rhistory")
	savehistory(file)
	history <- readLines(file)
	history <- paste0(history, collapse="\n")
	c("Session history:\n", history)
}

#### Capture session info ####
## ---------------------------

.session <- function() {
	info <- capture.output(sessionInfo())
	paste0(info, collapse="\n")
}

#### Errors and warnings ####
## --------------------------

.stop <- function(...) {
	.log("Error:", ...)
	stop(..., call.=FALSE)
}

.warning <- function(...) {
	.log("Warning:", ...)
	warning(..., call.=FALSE)
}

