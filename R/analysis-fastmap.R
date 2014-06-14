
#### Fastmap algorithm for dimension reduction ####

## Fastmap for euclidean distances
## 'x' is a N x P matrix
## 'ncomp' is the number of fastmap components to calculate
## 'scale' indicates whether the x should be standardized
## -----------------------------------------------------
fastmap <- function(x, ncomp=1, scale=FALSE, ...) {
	x <- as.matrix(x)
	if ( scale ) x <- scale(x, scale=scale)
	x.new <- matrix(0, nrow=nrow(x), ncol=ncomp)
	pivot.array <- matrix(0, nrow=ncomp, ncol=2)
	out.fastmap <- .C("fastmap_euclidean", as.double(x), as.double(x.new),
		as.integer(pivot.array), as.integer(nrow(x)), as.integer(ncol(x)),
		as.integer(ncomp))
	x.new <- matrix(out.fastmap[[2]], nrow=nrow(x), ncol=ncomp)
	pivot.array <- matrix(out.fastmap[[3]] + 1, nrow=ncomp, ncol=2)
	return(list(scores=x.new, pivot.array=pivot.array))	
}

## Fastmap for spatially-aware clustering
## 'x' is a P x N matrix
## 'coord' is a data.frame of coordinates
## 'r' is the neighborhood radius
## 'ncomp' is the number of fastmap components to calculate
## 'scale' indicates whether the x should be standardized
## 'spatial' is spatial info (see spatial.info())
## 'w' is the feature weights
## ------------------------------------
fastmap.spatial <- function(x, r=1, ncomp=1, scale=FALSE, spatial, w, ...) {
	x <- as.matrix(x)
	if ( scale ) x <- t(apply(x, 1, scale))
	alpha <- spatial$alpha
	beta <- t(simplify2array(spatial$beta, higher=FALSE))
	neighbors <- t(simplify2array(spatial$neighbors, higher=FALSE))
	ncomp <- ifelse(sum(w > 0) > ncomp, ncomp, sum(w > 0))
	x.new <- matrix(0, nrow=ncol(x), ncol=ncomp)
	pivot.array <- matrix(0, nrow=ncomp, ncol=2)
	start.time <- proc.time()
	out.fastmap <- .C("fastmap_spatial", as.double(x),
		as.integer(nrow(x)), as.integer(ncol(x)),
		as.integer(neighbors - 1), as.integer(ncol(neighbors)),
		as.double(w), as.double(alpha), as.double(beta),
		as.double(x.new), as.integer(pivot.array), as.integer(ncomp))
	x.new <- matrix(out.fastmap[[9]], nrow=ncol(x), ncol=ncomp)
	x.new <- as.data.frame(x.new)
	names(x.new) <- paste("FC", 1:ncomp, sep="")
	pivot.array <- matrix(out.fastmap[[10]] + 1, nrow=ncomp, ncol=2)
	pivot.array <- as.data.frame(pivot.array)
	names(pivot.array) <- c("Oa", "Ob")
	list(scores=x.new, pivot.array=pivot.array,
		time=proc.time() - start.time)
}

