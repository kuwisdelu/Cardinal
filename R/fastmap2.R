

fastmap2 <- function(x, distfun, ncomp=2, ...) {
	if ( is.matrix(x) ) {
		n <- nrow(x)
	} else {
		n <- length(x)
	}
	x.proj <- matrix(0, nrow=n, ncol=ncomp)
	x.pivot <- matrix(0, nrow=ncomp, ncol=2)
	for ( j in seq_len(ncomp) ) {
		x.pivot[j,] <- .findDistantObjects(x, x.proj, distfun, ...)
		fun <- .distanceFun(x, x.proj, distfun)
		d_ab <- fun(x.pivot[j,1], x.pivot[j,2])
		for ( i in seq_len(n) ) {
			d_ai <- fun(x.pivot[j,1], i)
			d_bi <- fun(x.pivot[j,2], i)
			x.proj[i,j] <- (d_ai^2 + d_ab^2 - d_bi^2) / (2 * d_ab)
		}
	}
	list(scores=x.proj, pivot.array=x.pivot)
}

.findDistantObjects <- function(x, x.proj, distfun, iter.max = 5, ...) {
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
		distmax <- 0
		for ( i in seq_len(n) ) {
			dist_i <- fun(oa, i)
			if ( dist_i > distmax ) {
				distmax <- dist_i
				omax <- i
			}
		}
		if ( isTRUE(ob == omax) )
			break
		ob <- omax
		distmax <- 0
		for ( j in seq_len(n) ) {
			dist_j <- fun(ob, j)
			if ( dist_j > distmax ) {
				distmax <- dist_j
				omax <- j
			}
		}
		oa <- omax
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
