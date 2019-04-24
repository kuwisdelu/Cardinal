
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
	reg.finalizer(.Cardinal, .log.flush, onexit=TRUE)
	.log("Cardinal loaded.", "\n", .session())
}

.onAttach <- function(libname, pkgname) {
	msg0 <- paste("\nWelcome to Cardinal (version ",
		packageVersion("Cardinal"), ")\n\n", sep="")
	msg1 <- "This is a big update with lots of changes!
			See what's new by viewing 'vignette(\"Cardinal-2-guide\")'."
	msg1 <- strwrap(paste(msg1, collapse=""), exdent=4, indent=4)
	packageStartupMessage(msg0, paste(msg1, collapse="\n"), "\n")
	addVigs2WinMenu("Cardinal")
	.log("Cardinal attached.")
}
