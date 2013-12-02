
#### set up the Cardinal environment ####

.onLoad <- function(libname, pkgname) {
	.CardinalState <- new.env()
	.verboseState <- new.env()
	options(Cardinal.verbose.output=FALSE)
	options(Cardinal.track.progress=FALSE)
}

.onAttach <- function(libname, pkgname) {
	msg0 <- paste("Welcome to Cardinal (version ", packageVersion("Cardinal"), ")\n", sep="")
	# msg1 <- "To get started, view the introductory
	# 		vignettes with 'browseVignettes(\"Cardinal\")'."
	# msg1 <- strwrap(paste(msg1, collapse=""), exdent=4, indent=4)
	# packageStartupMessage(msg0, paste(msg1, collapse="\n"), "\n")
	packageStartupMessage(paste(msg0, "\n"))
	# addVigs2WinMenu("Cardinal-demo") 
}

