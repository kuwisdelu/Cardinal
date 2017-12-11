
#### Logging and flushing log to output files ####
## -----------------------------------------------

.log <- function(...) {
	msg <- paste(date(), paste0(..., collapse="\n  "))
	.Cardinal$log <- append(.Cardinal$log, msg)
	elapsed <- proc.time()[3] - .Cardinal$time$flush
	if ( elapsed > getOption("Cardinal.flush") )
		.log.flush()
}

.log.flush <- function(e=.Cardinal) {
	if ( getOption("Cardinal.log") ) {
		tryCatch({
			if ( length(e$log) != 0 ) {
				e$time$flush <- proc.time()[3]
				filepath <- file.path(system.file(package="Cardinal"), "Cardinal.log")
				sink(filepath, append=TRUE)
				for ( m in e$log ) {
					cat(m, "\n\n")
				}
				e$log <- list()
				sink()
			}
			TRUE
		}, error=function(e) FALSE)
	} else {
		FALSE
	}
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
	if ( getOption("Cardinal.time") )
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

#### Generate entry for history log ####
## -------------------------------------

.history <- function(which=-2) {
	error <- paste("Error: in", deparse(match.call()))
	history <- tryCatch({
		call <- sys.call(which)
		def <- match.fun(as.character(call)[[1]])
		call <- match.call(def, call=call)
		history <- paste("Call:", deparse(call))
		if ( length(call) != 1 ) {
			args <- sapply(as.list(call)[-1], function(x) {
				tryCatch({
					class(eval(x, envir=sys.frame(which)))
				}, error=function(e) "unknown")
			})
			history <- append(history, paste(names(args), args, sep=" = "))
		}
		paste(history, collapse="\n")
	}, error=function(e) error)
	.log(history)
	history
}

#### Capture session info ####
## ---------------------------

.session <- function() {
	info <- sessionInfo()
	paste(names(info[[1]]), info[[1]], sep=" : ", collapse="\n")
}

#### Errors and warnings ####
## --------------------------

.stop <- function(...) {
	.log(...)
	stop(..., call.=FALSE)
}

.warning <- function(...) {
	.log(...)
	warning(..., call.=FALSE)
}

