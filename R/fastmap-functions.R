
#### functions for fastmap projection ####

fastmap <- function(x, ncomp=5, isNxP=TRUE, standardize=TRUE) {
	x <- as.matrix(x)
	if ( isNxP ) {
		x <- apply(x, 2, scale, scale=standardize)
	} else {
		x <- apply(x, 1, scale, scale=standardize)
	}
	x.new <- matrix(0, nrow=nrow(x), ncol=ncomp)
	pivot.array <- matrix(0, nrow=ncomp, ncol=2)
	out.fastmap <- .C("fastmap_euclidean", as.double(x), as.double(x.new),
		as.integer(pivot.array), as.integer(nrow(x)), as.integer(ncol(x)),
		as.integer(ncomp))
	x.new <- matrix(out.fastmap[[2]], nrow=nrow(x), ncol=ncomp)
	pivot.array <- matrix(out.fastmap[[3]] + 1, nrow=ncomp, ncol=2)
	return(list(scores=x.new, pivot.array=pivot.array))	
}

fastmap.spatial <- function(x, coord, r=2, ncomp=5, standardize=FALSE,
	neighbors, w, alpha, beta, ...)
{
	x <- as.matrix(x)
	if ( standardize ) x <- t(apply(x, 1, scale))
	if ( missing(neighbors) ) {
		neighbors <- NULL # needed to make neighbors() work
		neighbors <- neighbors(coord, r=r, na.replace=TRUE, ...)
	}
	if ( missing(w) ) w <- rep(1, nrow(x))
	ncomp <- ifelse(sum(w > 0) > ncomp, ncomp, sum(w > 0))
	if ( missing(alpha) ) alpha <- spatial.alpha(r, p=ncol(coord))
	if ( missing(beta) ) beta <- spatial.beta(x, neighbors=neighbors)
	x.new <- matrix(0, nrow=ncol(x), ncol=ncomp)
	pivot.array <- matrix(0, nrow=ncomp, ncol=2)
	out.fastmap <- .C("fastmap_spatial", as.double(x), as.integer(nrow(x)), as.integer(ncol(x)),
		as.integer(neighbors - 1), as.integer(ncol(neighbors)), as.double(w),
		as.double(alpha), as.double(beta), as.double(x.new), as.integer(pivot.array),
		as.integer(ncomp))
	x.new <- matrix(out.fastmap[[9]], nrow=ncol(x), ncol=ncomp)
	x.new <- as.data.frame(x.new)
	names(x.new) <- paste("FC", 1:ncomp, sep="")
	pivot.array <- matrix(out.fastmap[[10]] + 1, nrow=ncomp, ncol=2)
	pivot.array <- as.data.frame(pivot.array)
	names(pivot.array) <- c("Oa", "Ob")
	return(list(scores=x.new, pivot.array=pivot.array))
}
