
#### set up the Cardinal environment ####

.Cardinal <- list2env(list(
	log=list(),
	message=list(.console),
	progress=list(
		bar=NULL,
		min=numeric(),
		max=numeric(),
		i=numeric(),
		start=list(.progress.start),
		increment=list(.progress.increment),
		stop=list(.progress.stop)),
	time=list(
		start=NULL,
		stop=NULL,
		flush=0),
	processing=FALSE,
	trans3d=NULL,
	lastplot=NULL))

globalVariables(".Index")

.onLoad <- function(libname, pkgname) {
	options(Cardinal.verbose=interactive()) # public
	options(Cardinal.progress=interactive()) # public
	options(Cardinal.numblocks=20) # public
	options(Cardinal.delay=TRUE) # public
	options(Cardinal.dark=FALSE) # public
	reg.finalizer(.Cardinal, .log.flush, onexit=TRUE)
	.log("Cardinal loaded.", "\n", .session())
}

.onAttach <- function(libname, pkgname) {
	.log("Cardinal attached.")
}
