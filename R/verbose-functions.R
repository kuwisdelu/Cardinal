
#### verbose output functions ####

tryVerboseProgress <- function(start=FALSE, stop=FALSE, increment=FALSE, total=NULL, ...) {
	if ( getOption("Cardinal.verbose.output") || getOption("Cardinal.track.progress") ) {
		if ( start ) {
			.verboseState$counter <- 0
			.verboseState$total <- total
			if ( getOption("Cardinal.verbose.output") ) {
				.verboseState$progress <- txtProgressBar(max=total, style=3)
			}
			if ( !is.null(.verboseState$guiStart) ) .verboseState$guiStart()
		} else if ( stop ) {
			if ( getOption("Cardinal.verbose.output") ) {
				close(.verboseState$progress)
			}
			if ( !is.null(.verboseState$guiStop) ) .verboseState$guiStop()
		} else if ( increment ) {
			.verboseState$counter <- .verboseState$counter + 1
			if ( getOption("Cardinal.verbose.output") ) {
				setTxtProgressBar(.verboseState$progress, value=.verboseState$counter)
			}
			if ( !is.null(.verboseState$guiIncrement) ) .verboseState$guiIncrement()
		}
	}
}

tryVerboseMessage <- function(..., console.only=FALSE, precedes.progress.output=TRUE) {
	if ( getOption("Cardinal.verbose.output") || getOption("Cardinal.track.progress") ) {
		if ( getOption("Cardinal.verbose.output") ) {
			message(...)
			flush.console()
		}
		if ( !is.null(.verboseState$guiMessage) && !console.only ) {
			.verboseState$guiMessage(...,
				precedes.progress.output=precedes.progress.output)
		}
	}
}
