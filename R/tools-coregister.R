
setMethod("coregister",
	signature = c("SpatialShrunkenCentroids", "missing"),
	function(object, ...) {
		.Deprecated_Cardinal1()
		ks <- unlist(object$k)
		nclasses <- sapply(object$classes, function(cs) length(unique(cs)))
		regorder <- order(ks, nclasses)
		result <- resultData(object)
		for ( i in which(!duplicated(ks[regorder])) ) {
			j <- regorder[i]
			classes <- result[[j]]$classes
			order <- rep(NA, nlevels(classes))
			order[unique(classes)] <- seq_along(unique(classes))
			order[is.na(order)] <- setdiff(seq_len(nlevels(classes)),
				seq_along(unique(classes)))
			result[[j]] <- .spatialShrunkenCentroids.reclass(result[[j]], order)
		}
		for ( i in which(duplicated(ks[regorder])) ) {
			j <- regorder[i]
			k <- regorder[i-1]
			ref <- .coregisterCenters(result[[j]], result[[k]])$ref
			result[[j]] <- .spatialShrunkenCentroids.reclass(result[[j]], ref)
		}
		resultData(object) <- result
		object
	})

setMethod("coregister",
	signature = c("SpatialKMeans", "missing"),
	function(object, ...) {
		.Deprecated_Cardinal1()
		ks <- unlist(object$k)
		regorder <- order(ks)
		result <- resultData(object)
		for ( i in seq_along(regorder[-1]) ) {
			j <- regorder[i+1]
			k <- regorder[i]
			ref <- .coregisterCenters(result[[j]], result[[k]])$ref
			result[[j]] <- .spatialKMeans.reclass(result[[j]], ref)
		}
		resultData(object) <- result
		object
	})

.coregisterCenters <- function(object, ref) {
	extant <- apply(object$centers, 2, function(o) all(is.finite(o)))
	dists <- apply(object$centers, 2, function(o) colSums((o - ref$centers)^2))
	matched <- sapply(seq_len(ncol(dists)), function(j) {
		for ( i in order(dists[,j]) ) {
			if ( all(is.na(dists[i,]))) {
				match <- NA
				next
			} else if ( j == which.min(dists[i,]) ) {
				match <- i
				break
			} else {
				match <- NA
			}
		}
		match
	})
	matched <- data.frame(object=seq_len(ncol(object$centers)), ref=matched)
	unmatched <- setdiff(which(extant), matched$object[!is.na(matched$ref)])
	matched$ref[unmatched] <- setdiff(seq_len(ncol(object$centers)),
		matched$ref)[seq_along(unmatched)]
	matched$ref[is.na(matched$ref)] <- setdiff(seq_len(ncol(object$centers)),
		matched$ref)
	matched
}

