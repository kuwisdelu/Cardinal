
#### implement segmentation calibration methods ####

setMethod("calibrateSegmentation", c("MSImageSegmentation", "missing"), function(object,
	correlation=c("spatial", "spectral"), ...)
{
	dups <- duplicated(object@metaData$parameters$k)
	refs <- which(!dups)
	for ( i in seq_along(refs) ) {
		if ( i == 1 ) next
		object <- registerClasses(object, object, refs[i], refs[i - 1], correlation=correlation)
	}
	for ( i in which(dups) ) {
		object <- registerClasses(object, object, i, i - 1, correlation=correlation)
	}
	return(object)
} )

setMethod("calibrateSegmentation", c("MSImageSegmentation", "MSImageSegmentation"), function(object,
	reference, which.object=NULL, which.reference=NULL, correlation=c("spatial", "spectral"), ...)
{
	if ( !is.null(which.object) && !is.null(which.reference) ) {
		which.object <- match.which(object, which.object)
		which.reference <- match.which(reference, which.reference)
		object <- registerClasses(object, reference,
			which.object, which.reference, correlation)
	} else {
		for ( k in unique(object@metaData$parameters$k) ) {
			tar <- which(k == object@metaData$parameters$k)[[1]]
			ref <- which(k == reference@metaData$parameters$k)
			if ( length(ref) < 1 ) {
				next
			} else {
				ref <- ref[[1]]
			}
			object <- registerClasses(object, reference, tar, ref, correlation)
		}
		for ( i in which(duplicated(object@metaData$parameters$k)) ) {
			object <- registerClasses(object, object, i, i - 1, correlation)
		}
	}
	return(object)
} )

registerClasses <- function(target.object, reference.object,
	target.which, reference.which, correlation=c("spatial", "spectral"))
{
	correlation <- match.arg(correlation)
	if ( correlation == "spatial" ) {
		reg <- apply(target.object$probabilities[[target.which]], 2, cor,
			y=reference.object$probabilities[[reference.which]])
	} else if ( correlation == "spectral" ) {
		reg <- apply(target.object$centroids[[target.which]], 2, cor,
			y=reference.object$centroids[[reference.which]])
	}
	match <- apply(reg, 1, which.max)
	if ( ncol(reg) < length(match) ) match <- match[1:ncol(reg)]
	dup <- duplicated(match)
	if ( any(dup) ) {
		replace <- which(!1:ncol(reg) %in% unique(match))
		match[dup] <- replace[1:sum(dup)]
	}
	permuteClasses(target.object, target.which, update=match)
}

permuteClasses <- function(object, which, update) {
	object$probabilities[[which]] <- cbind(object$probabilities[[which]][,update],
		object$probabilities[[which]][,-update])
	object$scores[[which]] <- cbind(object$scores[[which]][,update],
		object$scores[[which]][,-update])
	object$centroids[[which]] <- cbind(object$centroids[[which]][,update],
		object$centroids[[which]][,-update])
	object$tstatistics[[which]] <- cbind(object$tstatistics[[which]][,update],
		object$tstatistics[[which]][,-update])
	object$classes[[which]] <- apply(object$probabilities[[which]], 1, which.max)
	if ( !is.null(object$labels) ) {
		updatemap <- sapply(seq_along(update), function(i) which(i == update))
		object$labels <- updatemap[object$labels]
	}
	object
}

