
#### set up the Cardinal environment ####

.Cardinal <- list2env(list(
	log=list(),
	message=list(.message),
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
		flush=0)))

.onLoad <- function(libname, pkgname) {
	options(Cardinal.verbose=FALSE)
	options(Cardinal.progress=FALSE)
	options(Cardinal.autoflush=60)
	reg.finalizer(.Cardinal, .log.flush, onexit=TRUE)
	.log("Cardinal loaded.", "\n", .session())
}

.onAttach <- function(libname, pkgname) {
	msg0 <- paste("Welcome to Cardinal (version ",
		packageVersion("Cardinal"), ")\n\n", sep="")
	msg1 <- "To get started, view the introductory
	 		vignettes with 'browseVignettes(\"Cardinal\")'."
	msg1 <- strwrap(paste(msg1, collapse=""), exdent=4, indent=4)
	packageStartupMessage(msg0, paste(msg1, collapse="\n"), "\n")
	addVigs2WinMenu("Cardinal")
	.log("Cardinal attached.")
}
