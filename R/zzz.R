
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
	options(Cardinal.verbose=interactive())
	options(Cardinal.progress=interactive())
	options(Cardinal.numblocks=20)
	options(Cardinal.delay=TRUE)
	options(Cardinal.dark=FALSE)
	reg.finalizer(.Cardinal, .log.flush, onexit=TRUE)
	.log(.session())
	.log("Cardinal ", Cardinal.version(), " loaded.", "\n")
}

.onAttach <- function(libname, pkgname) {
	.log("Cardinal ", Cardinal.version(), " attached.", "\n")
}

