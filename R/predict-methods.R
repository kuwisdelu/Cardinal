
# setMethod("predict", "PCA", function(object, newx, ncomp=object$ncomp, ...) {
# 	if ( isMSImageSet(newx) ) newx <- as.matrix(spectra(newx))
# 	tryVerboseMessage("Predicting...", precedes.progress.output=FALSE)
# 	if ( ncomp > object$ncomp ) stop("too many components")
# 	if ( object$standardize ) {
# 		standardize <- attr(object, "scaled:scale")
# 	} else {
# 		standardize <- FALSE
# 	}
# 	tryVerboseMessage("Centering data...", precedes.progress.output=FALSE)
# 	if ( object$isNxP ) {
# 		x <- scale(newx, center=attr(object, "scaled:center"), scale=standardize)
# 	} else {
# 		x <- scale(t(newx), center=attr(object, "scaled:center"), scale=standardize)
# 	}
# 	tryVerboseMessage("Calculating principal component scores...", precedes.progress.output=FALSE)
# 	scores <- x %*% object$loadings[,1:ncomp,drop=FALSE]
# 	colnames(scores) <- paste("PC", 1:ncomp, sep="")
# 	outlist <- object
# 	outlist$scores <- scores
# 	outlist$ncomp <- ncomp
# 	outlist$sdev <- apply(scores, 2, sd)
# 	return(outlist)
# } )

# setMethod("predict", "PLS", function(object, newx, newy, ncomp=object$ncomp, ...) {
# 	if ( isMSImageSet(newx) ) newx <- as.matrix(spectra(newx))
# 	tryVerboseMessage("Predicting...", precedes.progress.output=FALSE)
# 	if ( ncomp > object$ncomp ) stop("too many components")
# 	if ( object$standardize ) {
# 		standardize <- attr(object, "scaled:scale")
# 	} else {
# 		standardize <- FALSE
# 	}
# 	tryVerboseMessage("Centering data...", precedes.progress.output=FALSE)
# 	if ( object$isNxP ) {
# 		x <- scale(newx, center=attr(object, "scaled:center"), scale=standardize)
# 	} else {
# 		x <- scale(t(newx), center=attr(object, "scaled:center"), scale=standardize)
# 	}
# 	tryVerboseMessage("Calculating new scores and fitted values...", precedes.progress.output=FALSE)
# 	scores <- x %*% object$projection[,1:ncomp,drop=FALSE]
# 	colnames(scores) <- paste("C", 1:ncomp, sep="")
# 	coefficients <- list()
# 	fitted <- list()
# 	for ( i in 1:ncomp ) {
# 		H <- object$weights[,1:i,drop=FALSE] %*% solve(crossprod(object$loadings[,1:i,drop=FALSE],
# 			object$weights[,1:i,drop=FALSE]))
# 		coefficients[[i]] <- tcrossprod(H, object$Yweights[,1:i,drop=FALSE])
# 		fitted[[i]] <- x %*% coefficients[[i]]
# 		if ( object$standardize ) {
# 			Yscale <- attr(object, "Yscaled:scale")
# 		} else {
# 			Yscale <- 1
# 		}
# 		fitted[[i]] <- Yscale * fitted[[i]] + attr(object, "Yscaled:center")
# 	}
# 	names(coefficients) <- paste(1:ncomp, "components")
# 	names(fitted) <- paste(1:ncomp, "components")
# 	outlist <- object
# 	outlist$scores <- scores
# 	outlist$coefficients <- coefficients
# 	outlist$fitted <- fitted
# 	if ( outlist$metadata$isfactor ) {
# 		outlist$classes <- list()
# 		for ( i in seq_along(outlist$fitted) ) {
# 			temp <- apply(outlist$fitted[[i]], 1, which.max)
# 			outlist$classes[[i]] <- factor(temp,
# 				levels=outlist$metadata$levels,
# 				labels=outlist$metadata$labels)
# 		}
# 	}
# 	if ( !missing(newy) ) {
# 		tryVerboseMessage("Calculating residuals...", precedes.progress.output=FALSE)
# 		if ( outlist$metadata$isfactor ) {
# 			outlist$misclassified <- lapply(outlist$classes, function(yhat) newy != yhat)
# 			error <- outlist$misclassified
# 			names(outlist$misclassified) <- paste(1:ncomp, "components")
# 		} else {
# 			outlist$residuals <- lapply(outlist$fitted, function(yhat) newy - yhat)
# 			error <- outlist$residuals
# 			names(outlist$residuals) <- paste(1:ncomp, "components")
# 		}
# 		outlist$MSEP <- sapply(error, function(e) sum(e^2,
# 			na.rm=TRUE) / sum(is.finite(as.numeric(e))))
# 		names(outlist$MSEP) <- paste(1:ncomp, "components")
# 	}
# 	return(outlist)
# } )

# setMethod("predict", "OPLS", function(object, newx, newy, ncomp=object$ncomp, ...) {
# 	if ( isMSImageSet(newx) ) newx <- as.matrix(spectra(newx))
# 	tryVerboseMessage("Predicting...", precedes.progress.output=FALSE)
# 	if ( ncomp > object$ncomp ) stop("too many components")
# 	if ( object$standardize ) {
# 		standardize <- attr(object, "scaled:scale")
# 	} else {
# 		standardize <- FALSE
# 	}
# 	tryVerboseMessage("Centering data...", precedes.progress.output=FALSE)
# 	if ( object$isNxP ) {
# 		x <- scale(newx, center=attr(object, "scaled:center"), scale=standardize)
# 	} else {
# 		x <- scale(t(newx), center=attr(object, "scaled:center"), scale=standardize)
# 	}
# 	tryVerboseMessage("Calculating new scores and fitted values...", precedes.progress.output=FALSE)
# 	scores <- matrix(nrow=nrow(x), ncol=ncomp)
# 	fitted <- list()
# 	for ( i in 1:ncomp) {
# 		scores[,i] <- x %*% object$Oweights[,i]
# 		x <- x - tcrossprod(scores[,i], object$Oloadings[,i])
# 		fitted[[i]] <- x %*% object$coefficients[[i]]
# 		if ( object$standardize ) {
# 			Yscale <- attr(object, "Yscaled:scale")
# 		} else {
# 			Yscale <- 1
# 		}
# 		fitted[[i]] <- Yscale * fitted[[i]] + attr(object, "Yscaled:center")
# 	}
# 	names(fitted) <- paste(1:ncomp, "+", object$acomp, "components")
# 	outlist <- object
# 	outlist$Xnew <- x
# 	outlist$Oscores <- scores
# 	outlist$fitted <- fitted
# 	if ( outlist$metadata$isfactor ) {
# 		outlist$classes <- list()
# 		for ( i in seq_along(outlist$fitted) ) {
# 			temp <- apply(outlist$fitted[[i]], 1, which.max)
# 			outlist$classes[[i]] <- factor(temp,
# 				levels=outlist$metadata$levels,
# 				labels=outlist$metadata$labels)
# 		}
# 	}
# 	if ( !missing(newy) ) {
# 		tryVerboseMessage("Calculating residuals...", precedes.progress.output=FALSE)
# 		if ( outlist$metadata$isfactor ) {
# 			outlist$misclassified <- lapply(outlist$classes, function(yhat) newy != yhat)
# 			error <- outlist$misclassified
# 			names(outlist$misclassified) <- paste(1:ncomp, "components")
# 		} else {
# 			outlist$residuals <- lapply(outlist$fitted, function(yhat) newy - yhat)
# 			error <- outlist$residuals
# 			names(outlist$residuals) <- paste(1:ncomp, "components")
# 		}
# 		outlist$MSEP <- sapply(error, function(e) sum(e^2,
# 			na.rm=TRUE) / sum(is.finite(as.numeric(e))))
# 		names(outlist$MSEP) <- paste(1:ncomp, "components")
# 	}
# 	return(outlist)
# } )

