

fastmap <- function(x, distfun, ncomp=2, ...) {
	if ( is.matrix(x) || is.data.frame(x) ) {
		n <- nrow(x)
	} else {
		n <- length(x)
	}
	x.proj <- matrix(0, nrow=n, ncol=ncomp)
	x.pivot <- matrix(NA_integer_, nrow=ncomp, ncol=2)
	for ( j in seq_len(ncomp) ) {
		x.pivot[j,] <- .findDistantObjects(x, x.proj, distfun, ...)
		if ( any(is.na(x.pivot[j,])) )
			break
		fun <- .distanceFun(x, x.proj, distfun)
		d_ab <- fun(x.pivot[j,1], x.pivot[j,2])
		x.proj[,j] <- vapply(seq_len(n), function(i) {
			d_ai <- fun(x.pivot[j,1], i)
			d_bi <- fun(x.pivot[j,2], i)
			(d_ai^2 + d_ab^2 - d_bi^2) / (2 * d_ab)
		}, numeric(1))
	}
	list(scores=x.proj, pivot.array=x.pivot)
}

.findDistantObjects <- function(x, x.proj, distfun, iter.max = 3, ...) {
	if ( is.matrix(x) ) {
		n <- nrow(x)
	} else {
		n <- length(x)
	}
	fun <- .distanceFun(x, x.proj, distfun)
	iter <- 1
	oa <- 1
	ob <- NULL
	while ( iter <= iter.max ) {
		dists <- vapply(seq_len(n), fun, numeric(1), oa)
		cand <- which.max(dists)
		if ( dists[cand] == 0 )
			return(c(NA, NA))		
		if ( isTRUE(ob == cand) )
			return(c(oa, ob))
		ob <- cand
		dists <- vapply(seq_len(n), fun, numeric(1), ob)
		oa <- which.max(dists)
		iter <- iter + 1
	}
	c(oa, ob)
}

.distanceFun <- function(x, x.proj, distfun) {
	function(i, j) {
		d2 <- distfun(x, i, j)^2 - sum((x.proj[i,] - x.proj[j,])^2)
		if ( d2 > 0 ) {
			sqrt(d2)
		} else {
			0
		}
	}
}
