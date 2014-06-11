
# #### functions for log-likelihood ####

# kmeansLogLik <- function(x, clusters, standardize, tol=1e-9, ...) {
# 	if ( !is.matrix(x) ) stop("'x' must be a p x n matrix")
# 	if ( standardize ) x <- t(apply(x, 1, scale))
# 	n <- ncol(x)
# 	z <- tabulate(clusters)
# 	phi <- z / n
# 	logfk <- sapply(sort(unique(clusters)), function(k) {
# 		is.cluster <- clusters == k
# 		mu <- rowMeans(x[,is.cluster,drop=FALSE])
# 		S <- var(t(x[,is.cluster]))
# 		D <- svd(S)$d
# 		p <- ifelse(any(D > tol), sum(D > tol), 1)
# 		Slogdet <- ifelse(any(D > tol), sum(log(D[D > tol])), 1)
# 		Sinv <- ginv(S)
# 		logfi <- sapply(which(is.cluster), function(i) {
# 			d <- x[,i] - mu
# 			-(p / 2) * log(2 * pi) - (1 / 2) * Slogdet - (1 / 2) * t(d) %*% Sinv %*% d
# 		} )
# 		z[k] * log(phi[k]) + sum(logfi)
# 	} )
# 	return(sum(logfk))
# }

# spatialKMeansLogLik <- function(x, clusters, standardize, neighbors, alpha, beta,
# 	tol=1e-9, ...)
# {
# 	if ( !is.matrix(x) ) stop("'x' must be a p x n matrix")
# 	if ( standardize ) x <- t(apply(x, 1, scale))
# 	if ( missing(alpha) ) alpha <- rep(1, nrow(neighbors))
# 	if ( missing(beta) ) beta <- matrix(1, nrow=nrow(neighbors), ncol=ncol(neighbors))
# 	n <- ncol(x)
# 	z <- tabulate(clusters)
# 	phi <- z / n
# 	logfk <- sapply(sort(unique(clusters)), function(k) {
# 		is.cluster <- clusters == k
# 		mu <- rowMeans(x[,is.cluster,drop=FALSE])
# 		S <- cov(t(x[,is.cluster]))
# 		D <- svd(S)$d
# 		p <- ifelse(any(D > tol), sum(D > tol), 1)
# 		Slogdet <- ifelse(any(D > tol), sum(log(D[D > tol])), 1)
# 		Sinv <- ginv(S)
# 		logfi <- sapply(which(is.cluster), function(i) {
# 			gamma <- sqrt(alpha) * sqrt(beta[,i])
# 			gamma <- gamma / sum(gamma)
# 			xi <- x[,neighbors[,i]]
# 			d <- xi - mu
# 			y <- sapply(seq_along(gamma), function(j) t(d[,j]) %*% Sinv %*% d[,j])
# 			-(p / 2) * log(2 * pi) - (1 / 2) * Slogdet - (1 / 2) * sum(gamma * y)
# 		} )
# 		z[k] * log(phi[k]) + sum(logfi)
# 	} )
# 	return(sum(logfk))
# }

