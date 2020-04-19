
#### set up the Cardinal environment ####

.Cardinal <- list2env(list(
	log=list(),
	message=list(.console),
	time=list(
		start=NULL,
		stop=NULL,
		flush=0),
	processing=FALSE,
	trans3d=NULL,
	lastplot=NULL))

globalVariables(".Index")

.onLoad <- function(libname, pkgname) {
	setCardinalBPPARAM()
	setCardinalVerbose()
	setCardinalNumBlocks()
	setCardinalDelayProc()
	.log(.session())
	.log("Cardinal ", CardinalVersion(), " loaded.", "\n")
	reg.finalizer(.Cardinal, .log.flush, onexit=TRUE)
}

.onAttach <- function(libname, pkgname) {
	.log("Cardinal ", CardinalVersion(), " attached.", "\n")
}

