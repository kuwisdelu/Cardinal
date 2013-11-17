
#### functions for checking whether an object is a defined S4 class ####

isMSImageSet <- function(object) {
	is(object, "MSImageSet")
}

isMSPeakFrame <- function(object) {
	is(object, "MSPeakFrame")
}

isMSPeakList <- function(object) {
	is(object, "MSPeakList")
}

isMSPeakAlignment <- function(object) {
	is(object, "MSPeakAlignment")
}

isMSImageSegmentation <- function(object) {
	is(object, "MSImageSegmentation")
}
