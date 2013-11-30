
#### set up the Cardinal environment ####

.CardinalState <- new.env()
.verboseState <- new.env()

.onLoad <- function(libname, pkgname) {
	options(Cardinal.verbose.output=FALSE)
	options(Cardinal.track.progress=FALSE)
}

#### end setup ####
