
#### set up the environment ####

.cardinalState <- new.env()
.verboseState <- new.env()

.onLoad <- function(libname, pkgname) {
	options(Cardinal.verbose.output=FALSE)
	options(Cardinal.track.progress=FALSE)
	options(Cardinal.debug.level=0)
}

#### end setup ####
