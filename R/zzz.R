
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

.init_SerialParam <- function() {
	# this is a hack -- fix me into something better later
	if ( requireNamespace("BiocParallel", quietly=TRUE) ) {
		bp <- try(length(BiocParallel:::.registry$bpparams) == 0L, silent=TRUE)
		if ( isTRUE(bp) )
			BiocParallel::register(BiocParallel::SerialParam())
	}
}

.onLoad <- function(libname, pkgname) {
	.init_SerialParam()
	options(Cardinal.verbose=interactive())
	options(Cardinal.progress=interactive())
	options(Cardinal.numblocks=20L)
	options(Cardinal.delay=TRUE)
	options(Cardinal.dark=FALSE)
	.log(.session())
	.log("Cardinal ", packageVersion("Cardinal"), " loaded.", "\n")
	reg.finalizer(.Cardinal, .log.flush, onexit=TRUE)
}

.onAttach <- function(libname, pkgname) {
	.log("Cardinal ", packageVersion("Cardinal"), " attached.", "\n")
}

