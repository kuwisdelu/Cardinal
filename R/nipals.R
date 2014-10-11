
#### NIPALS algorithm ####

## Principal components calculated using NIPALS
## 'X' is a _centered_ N x P matrix
## 'ncomp' is the number of components to calculate
## 'tol' is the tolerance for the convergence criterion
## 'iter.max' is the number of iterations
nipals.PCA <- function(X, ncomp=2, tol=1e-6, iter.max=100) {
	if ( ncomp > ncol(X) ) ncomp <- ncol(X)
	loadings <- matrix(nrow=ncol(X), ncol=ncomp)
	scores <- matrix(nrow=nrow(X), ncol=ncomp)
	for ( i in 1:ncomp ) {
		u <- X[,which.max(colSums(X)),drop=FALSE]
		udiff <- 1
		iter <- 1
		while ( iter <= iter.max && udiff > tol ) {
			b <- crossprod(X, u) / as.numeric(crossprod(u, u))
			b <- b / l2norm(b)
			unew <- X %*% b
			udiff <- unew - u
			udiff <- as.numeric(crossprod(udiff, udiff))
			u <- unew
			iter <- iter + 1
		}
		if ( iter > iter.max && udiff > tol ) {
			warning("NIPALS did not converge in ", iter - 1, " iterations for component ", i)
		}
		scores[,i] <- u
		loadings[,i] <- b
		X <- X - tcrossprod(u, b)
	}
	colnames(loadings) <- paste("PC", 1:ncomp, sep="")
	colnames(scores) <- paste("PC", 1:ncomp, sep="")
	list(scores=scores, loadings=loadings, sdev=apply(scores, 2, sd))
}

## Partial least squares using NIPALS
## 'X' is a _centered_ N x P matrix
## 'Y' is a _centered_ N x D matrix
## 'ncomp' is the number of components to calculate
## 'tol' is the tolerance for the convergence criterion
## 'iter.max' is the number of iterations
nipals.PLS <- function(X, Y, ncomp=2, tol=1e-6, iter.max=100) {
	if ( ncomp > ncol(X) ) ncomp <- ncol(X)
	if ( nrow(X) != nrow(Y) ) stop("'X' and 'Y' must have the same number of rows")
	loadings <- matrix(nrow=ncol(X), ncol=ncomp)
	weights <- matrix(nrow=ncol(X), ncol=ncomp)
	scores <- matrix(nrow=nrow(X), ncol=ncomp)
	Yweights <- matrix(nrow=ncol(Y), ncol=ncomp)
	Yscores <- matrix(nrow=nrow(Y), ncol=ncomp)
	Xnew <- X
	Ynew <- Y
	for ( i in 1:ncomp ) {
		u <- Ynew[,which.max(colSums(Y)),drop=FALSE]
		udiff <- 1
		iter <- 1
		while ( iter <= iter.max && udiff > tol ) {
			w <- crossprod(Xnew, u) / as.numeric(crossprod(u, u))
			w <- w / l2norm(w)
			t <- Xnew %*% w
			c <- crossprod(Ynew, t) / as.numeric(crossprod(t, t))
			unew <- Ynew %*% c
			udiff <- unew - u
			udiff <- as.numeric(crossprod(udiff, udiff))
			u <- unew
			iter <- iter + 1
		}
		if ( iter > iter.max && udiff > tol ) {
			warning("NIPALS did not converge in ", iter - 1, " iterations for component ", i)
		}
		p <- crossprod(Xnew, t) / as.numeric(crossprod(t, t))
		d <- crossprod(u, t) / as.numeric(crossprod(t, t))
		Xnew <- Xnew - tcrossprod(t, p)
		Ynew <- Ynew - as.numeric(d) * tcrossprod(t, c)
		loadings[,i] <- p
		weights[,i] <- w
		scores[,i] <- t
		Yweights[,i] <- c
		Yscores[,i] <- u
	}
	H <- weights %*% solve(crossprod(loadings, weights))
	Bhat <- tcrossprod(H, Yweights)
	Yhat <- X %*% Bhat
	colnames(loadings) <- paste("C", 1:ncomp, sep="")
	colnames(weights) <- paste("C", 1:ncomp, sep="")
	colnames(scores) <- paste("C", 1:ncomp, sep="")
	colnames(Yweights) <- paste("C", 1:ncomp, sep="")
	colnames(Yscores) <- paste("C", 1:ncomp, sep="")
	list(scores=scores, loadings=loadings, weights=weights, Yweights=Yweights,
		Yscores=Yscores, projection=H, coefficients=Bhat, fitted=Yhat)
}

## Orthogonal partial least squares using NIPALS
## 'X' is a _centered_ N x P matrix
## 'Y' is a _centered_ N x D matrix
## 'ncomp' is the number of components to calculate
## 'tol' is the tolerance for the convergence criterion
## 'iter.max' is the number of iterations
nipals.OPLS <- function(X, Y, ncomp=2, tol=1e-6, iter.max=100) {
	if ( ncomp > ncol(X) ) ncomp <- ncol(X)
	if ( nrow(X) != nrow(Y) ) stop("'X' and 'Y' must have the same number of rows")
	loadings <- matrix(nrow=ncol(X), ncol=ncomp)
	weights <- matrix(nrow=ncol(X), ncol=ncomp)
	scores <- matrix(nrow=nrow(X), ncol=ncomp)
	Xnew <- X
	Ynew <- Y
	W <- matrix(nrow=ncol(X), ncol=ncol(Y))
	for ( l in 1:ncol(Y) ) {
		W[,l] <- crossprod(X, Y[,l]) / as.numeric(crossprod(Y[,l], Y[,l]))
	}
	Yprincomp <- nipals.PCA(W, ncomp=ncol(W))
	Tw <- Yprincomp$scores[,Yprincomp$sdev > tol,drop=FALSE]
	Tw <- Tw / rep(apply(Tw, 2, l2norm), each=nrow(Tw))
	for ( i in 1:ncomp ) {
		u <- Ynew[,which.max(colSums(Y)),drop=FALSE]
		udiff <- 1
		iter <- 1
		while ( iter <= iter.max && udiff > tol ) {
			w <- crossprod(Xnew, u) / as.numeric(crossprod(u, u))
			w <- w / l2norm(w)
			t <- Xnew %*% w
			c <- crossprod(Ynew, t) / as.numeric(crossprod(t, t))
			unew <- Ynew %*% c
			udiff <- unew - u
			udiff <- as.numeric(crossprod(udiff, udiff))
			u <- unew
			iter <- iter + 1
		}
		if ( iter > iter.max && udiff > tol ) {
			warning("NIPALS did not converge in ", iter - 1, " iterations for component ", i)
		}
		p <- crossprod(Xnew, t) / as.numeric(crossprod(t, t))
		p.ortho <- p
		for ( k in 1:ncol(Tw) ) {
			p.ortho <- p.ortho - ( as.numeric(crossprod(Tw[,k], p.ortho)) / 
				as.numeric(crossprod(Tw[,k], Tw[,k])) ) * Tw[,k]
		}
		w.ortho <- p.ortho / l2norm(p.ortho)
		t.ortho <- Xnew %*% w.ortho / as.numeric(crossprod(w.ortho, w.ortho))
		p.ortho <- crossprod(Xnew, t.ortho) / as.numeric(crossprod(t.ortho, t.ortho))
		Xnew <- Xnew - tcrossprod(t.ortho, p.ortho)
		loadings[,i] <- p.ortho
		weights[,i] <- w.ortho
		scores[,i] <- t.ortho
	}
	Xortho <- tcrossprod(scores, loadings)
	colnames(loadings) <- paste("C", 1:ncomp, sep="")
	colnames(weights) <- paste("C", 1:ncomp, sep="")
	colnames(scores) <- paste("C", 1:ncomp, sep="")
	list(Xnew=Xnew, Xortho=Xortho, Oscores=scores, Oloadings=loadings, Oweights=weights)
}
