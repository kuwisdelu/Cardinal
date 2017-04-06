
setMethod("summary", "iSet", function(object, ...) {
	outlist <- 	list()
	outlist[["Class"]] <- class(object)
	outlist[["Features"]] <- paste0(paste(selectSome(featureNames(object), maxToShow=2), collapse=" "),
		" (", nrow(fData(object)), " total)")
	outlist[["Pixels"]] <- paste0(paste(selectSome(pixelNames(object), maxToShow=2), collapse=" "),
		" (", nrow(pData(object)), " total)")
	for ( co in coordLabels(object) ) {
		outlist[[co]] <- paste(range(pData(object)[[co]]), collapse=" ... ")
	}
	size <- sapply(names(imageData(object)),
		function(nm) object.size(imageData(object)[[nm]]))
	size <- sum(size) + object.size(object)
	outlist[["Size in memory"]] <- format(size, units="Mb")
	class(outlist) <- "summary.iSet"
	outlist
})

print.summary.iSet <- function(x, ...) {
	for ( nm in names(x) ) {
		cat(nm, ": ", x[[nm]], "\n", sep="")
	}
}

setMethod("summary", "PCA",
	function(object, ...) {
		topLabels <- do.call("rbind", lapply(resultData(object), function(x) {
			ncomp <- x$ncomp
			data.frame(ncomp=ncomp,
				loadings=as.vector(x$loadings[,ncomp,drop=FALSE]))
		}))
		row.names(topLabels) <- NULL
		which <- which.max(unlist(object$ncomp))
		ncomp <- object$ncomp[[which]]
		sdev <- object$sdev[[which]]
		importance <- matrix(sdev, ncol=ncomp,
			dimnames=list("Standard deviation", paste0("PC", 1:ncomp)))
		# totvar <- object[[which]]$totvar		
		# importance <- t(simplify2array(list(sdev,
		# 	sdev^2 / totvar,
		# 	cumsum(sdev^2 / totvar))))
		# dimnames(importance)  <- list(c("Standard deviation",
		# 		"Proportion of Variance",
		# 		"Cumulative"),
		# 	paste0("PC", seq_along(sdev)))
		out <- list(topLabels=topLabels, importance=importance,
			model=modelData(object), method=object[[1]]$method)
		class(out) <- "summary.PCA"
		out
	})

print.summary.PCA <- function(x, ...) {
	print(x$importance)
}

plot.summary.PCA <- function(x, y, ...) {
	sdev <- x$importance["Standard deviation",]
	# var <- x$importance["Proportion of Variance",]
	# cum <- x$importance["Cumulative",]
	# data <- data.frame(pc=seq_along(var), sdev=sdev, var=var, cum=cum)
	data <- data.frame(pc=seq_along(var), sdev=sdev)
	plot(sdev ~ pc, data=data, type='b', xlab="PC",
		ylab="Standard deviation", ...)
}

setMethod("summary", "PLS",
	function(object, ...) {
		topLabels <- do.call("rbind", lapply(resultData(object), function(x) {
			p <- nrow(object)
			nclasses <- ncol(x$fitted)
			ncomp <- x$ncomp
			if ( is.factor(x$y) ) {
				column <- factor(rep(seq_len(nclasses), each=p),
					labels=levels(x$classes))
			} else {
				column <- factor(rep(seq_len(nclasses), each=p),
					labels=seq_len(nclasses))
			}
			data.frame(ncomp=ncomp,
				column=column,
				coefficients=as.vector(x$coefficients),
				loadings=as.vector(x$loadings[,ncomp,drop=FALSE]),
				weights=as.vector(x$weights[,ncomp,drop=FALSE]),
				row.names=seq_len(nclasses * nrow(object)))
		}))
		row.names(topLabels) <- NULL
		accuracy <- lapply(resultData(object), function(x) {
			if ( is.factor(x$y) ) {
				.summarize.factor(x$y, x$classes)
			} else {
				.summarize.numeric(x$y, x$fitted)
			}
		})
		if ( is.factor(object[[1]]$y) ) {
			attr(accuracy, "accuracy:type") <- "factor"
		} else {
			attr(accuracy, "accuracy:type") <- "numeric"
		}
		attr(accuracy, "gridsearch:x") <- "ncomp"
		attr(accuracy, "gridsearch:xlab") <- "Number of Components"
		out <- list(topLabels=topLabels, accuracy=accuracy,
			model=modelData(object), method=object[[1]]$method)
		class(out) <- "summary.PLS"
		out
	})

print.summary.PLS <- function(x, ...) {
	print(c(x$accuracy))
}

plot.summary.PLS <- function(x, y, ...) {
	if ( attr(x$accuracy, "accuracy") == "numeric" ) {
		.plot.accuracy.numeric(x$model, x$accuracy, ...)
	} else if ( attr(x$accuracy, "accuracy") == "factor" ) {
		.plot.accuracy.factor(x$model, x$accuracy, ...)
	}
}

setMethod("summary", "OPLS",
	function(object, ...) {
		topLabels <- do.call("rbind", lapply(resultData(object), function(x) {
			p <- nrow(object)
			nclasses <- ncol(x$fitted)
			ncomp <- x$ncomp
			if ( is.factor(x$y) ) {
				column <- factor(rep(seq_len(nclasses), each=p),
					labels=levels(x$classes))
			} else {
				column <- factor(rep(seq_len(nclasses), each=p),
					labels=seq_len(nclasses))
			}
			data.frame(ncomp=ncomp,
				column=column,
				coefficients=as.vector(x$coefficients),
				loadings=as.vector(x$loadings),
				Oloadings=as.vector(x$Oloadings[,ncomp,drop=FALSE]),
				weights=as.vector(x$weights),
				Oweights=as.vector(x$Oweights[,ncomp,drop=FALSE]))
		}))
		row.names(topLabels) <- NULL
		accuracy <- lapply(resultData(object), function(x) {
			if ( is.factor(x$y) ) {
				.summarize.factor(x$y, x$classes)
			} else {
				.summarize.numeric(x$y, x$fitted)
			}
		})
		if ( is.factor(object[[1]]$y) ) {
			attr(accuracy, "accuracy:type") <- "factor"
		} else {
			attr(accuracy, "accuracy:type") <- "numeric"
		}
		attr(accuracy, "gridsearch:x") <- "ncomp"
		attr(accuracy, "gridsearch:xlab") <- "Number of Components"
		out <- list(topLabels=topLabels, accuracy=accuracy,
			model=modelData(object), method=object[[1]]$method)
		class(out) <- "summary.OPLS"
		out
	})

print.summary.OPLS <- function(x, ...) {
	print(c(x$accuracy))
}

plot.summary.OPLS <- function(x, y, ...) {
	if ( attr(x$accuracy, "accuracy") == "numeric" ) {
		.plot.accuracy.numeric(x$model, x$accuracy, ...)
	} else if ( attr(x$accuracy, "accuracy") == "factor" ) {
		.plot.accuracy.factor(x$model, x$accuracy, ...)
	}
}

setMethod("summary", "SpatialKMeans",
	function(object, ...) {
		topLabels <- do.call("rbind", lapply(resultData(object), function(x) {
			k <- x$k
			n <- tabulate(x$cluster)
			n <- rep(n, each=nrow(object))
			cluster <- factor(rep(seq_len(k), each=nrow(object)),
				labels=levels(x$cluster))
			data.frame(r=x$r, k=x$k,
				cluster=cluster,
				centers=as.vector(x$centers),
				withinss=as.vector(x$withinss),
				betweenss=as.vector(x$betweenss),
				row.names=seq_len(k * nrow(object)))
		}))
		row.names(topLabels) <- NULL
		withinss <- sapply(resultData(object), function(x) sum(x$withinss))
		betweenss <- sapply(resultData(object), function(x) sum(x$betweenss))
		totss <- sapply(resultData(object), function(x) sum(x$totss))
		time <- sapply(resultData(object), function(x) x$time[[3]])
		out <- list(topLabels=topLabels,
			withinss=withinss, betweenss=betweenss, totss=totss,
			model=modelData(object), method=object[[1]]$method, time=time)
		class(out) <- "summary.SpatialKMeans"
		out
	})

print.summary.SpatialKMeans <- function(x, ...) {
	model <- pData(x$model)
	row.names(model) <- NULL
	model[["method"]] <- x$method
	model[["time"]] <- x$time
	model[["Within-Cluster SS"]] <- x$withinss
	model[["Between-Cluster SS"]] <- x$betweenss
	model[["Total SS"]] <- x$totss
	print(model)
}

plot.summary.SpatialKMeans <- function(x, y, ...) {
	.plot.gridsearch(x="k", y=x$withinss, model=x$model,
		xlab="Number of Clusters", ylab="Within-Cluster SS", ...)
}

setMethod("summary", "SpatialShrunkenCentroids",
	function(object, ...) {
		topLabels <- do.call("rbind", lapply(resultData(object), function(x) {
			k <- x$k
			n <- table(x$classes)
			n <- rep(n, each=nrow(object))
			n[n < 2] <- NA # remove singletons and missing classes
			classes <- factor(rep(seq_len(k), each=nrow(object)),
				labels=levels(x$classes))
			p.values <- 2 * (1 - pt(abs(as.vector(x$tstatistics)), df=n - 1))
			adj.p.values <- p.adjust(p.values, method="BH")
			data.frame(r=x$r, k=x$k, s=x$s,
				classes=classes,
				centers=as.vector(x$centers),
				tstatistics=as.vector(x$tstatistics),
				p.values=p.values,
				adj.p.values=adj.p.values,
				row.names=seq_len(k * nrow(object)))
		}))
		row.names(topLabels) <- NULL
		accuracy <- lapply(resultData(object), function(x) {
			.summarize.factor(x$y, x$classes)
		})
		if ( !is.null(accuracy[[1]]) ) {
			attr(accuracy, "gridsearch:x") <- "s"
			attr(accuracy, "gridsearch:xlab") <- "Shrinkage parameter (s)"
			attr(accuracy, "accuracy:type") <- "factor"
		}
		nclasses <- sapply(resultData(object), function(x) 
			length(unique(x$classes)))
		nzfeatures <- sapply(resultData(object), function(x) {
			which <- apply(x$tstatistics, 2, function(t) any(t != 0))
			nz <- apply(x$tstatistics[,which,drop=FALSE], 2, function(t) sum(t != 0))
			round(mean(nz))
		})
		time <- sapply(resultData(object), function(x) x$time[[3]])
		out <- list(topLabels=topLabels, accuracy=accuracy,
			nclasses=nclasses, nzfeatures=nzfeatures,
			model=modelData(object), method=object[[1]]$method, time=time)
		class(out) <- "summary.SpatialShrunkenCentroids"
		out
	})

print.summary.SpatialShrunkenCentroids <- function(x, ...) {
	model <- pData(x$model)
	row.names(model) <- NULL
	model[["method"]] <- x$method
	model[["time"]] <- x$time
	model[["Predicted # of Classes"]] <- x$nclasses
	model[["Mean # of Features per Class"]] <- x$nzfeatures
	# if ( !is.null(x$accuracy[[1]]) ) {
	# 	model[["Accuracy"]] <- sapply(x$accuracy,
	# 		function(s) s["Accuracy", 1])
	# }
	print(model)
}

plot.summary.SpatialShrunkenCentroids <- function(x, y, ...) {
	if ( is.null(x$accuracy[[1]]) ) {
		.plot.gridsearch(x="s", y=x$nclasses, model=x$model,
		xlab="Shrinkage parameter (s)", ylab="Predicted # of Classes", ...)
	} else {
		.plot.accuracy.factor(x$model, x$accuracy, ...)
	}
}

setMethod("summary", "CrossValidated",
	function(object, ...) {
		acc <- lapply(resultData(object),
			function(ob) summary(ob)$accuracy)
		accuracy <- do.call("Map", c(function(...) {
			dots <- list(...)
			nfold <- length(dots)
			acc <- Reduce(`+`, dots)
			acc / nfold
		}, acc))
		attr(accuracy, "gridsearch:x") <- attr(acc[[1]], "gridsearch:x")
		attr(accuracy, "gridsearch:xlab") <- attr(acc[[1]], "gridsearch:xlab")
		attr(accuracy, "accuracy:type") <- attr(acc[[1]], "accuracy:type")
		out <- list(accuracy=accuracy, model=modelData(object[[1]]))
		class(out) <- "summary.CrossValidated"
		out
	})

print.summary.CrossValidated <- function(x, ...) {
	print(c(x$accuracy))
}

plot.summary.CrossValidated <- function(x, y, ...) {
	if ( attr(x$accuracy, "accuracy") == "numeric" ) {
		.plot.accuracy.numeric(x$model, x$accuracy, ...)
	} else if ( attr(x$accuracy, "accuracy") == "factor" ) {
		.plot.accuracy.factor(x$model, x$accuracy, ...)
	}
}

.summarize.factor <- function(y, fitted) {
	if ( is.null(y) || is.null(fitted) )
		return(NULL)
	nonmissing <- !is.na(y)
	y <- y[nonmissing]
	fitted <- fitted[nonmissing]
	accuracy <- lapply(levels(fitted), function(class) {
		true.pos <- sum(y == class & fitted == class, na.rm=TRUE)
		false.pos <- sum(y != class & fitted == class, na.rm=TRUE)
		true.neg <- sum(y != class & fitted != class, na.rm=TRUE)
		false.neg <- sum(y == class & fitted != class, na.rm=TRUE)
		c(Accuracy=(true.pos + true.neg) / length(fitted),
			Sensitivity=true.pos / (true.pos + false.neg),
			Specificity=true.neg / (false.pos + true.neg),
			FDR=false.pos / (true.pos + false.pos))
	})
	names(accuracy) <- levels(fitted)
	simplify2array(accuracy)
}

.summarize.numeric <- function(y, fitted) {
	if ( is.null(y) || is.null(fitted) )
		return(NULL)
	nonmissing <- !is.na(y)
	y <- y[nonmissing]
	if ( is.matrix(fitted) ) {
		fitted <- fitted[nonmissing,,drop=FALSE]	
	} else {
		fitted <- fitted[nonmissing]
	}
	if ( is.factor(y) )
		y <- sapply(levels(y), function(Ck) as.integer(y == Ck))
	c(SSE = sum((fitted - y)^2),
		MSE = sum((fitted - y)^2) / length(fitted),
		RMSE = sqrt(sum((fitted - y)^2) / length(fitted)))
}

.plot.accuracy.factor <- function(model, accuracy, ...) {
	x <- attr(accuracy, "gridsearch:x")
	xlab <- attr(accuracy, "gridsearch:xlab")
	accuracy <- sapply(accuracy, function(s) s["Accuracy", 1])
	.plot.gridsearch(x, y=accuracy, model=model,
		xlab=xlab, ylab="Accuracy", markfun=which.max, ...)
}

.plot.accuracy.numeric <- function(model, accuracy, ...) {
	x <- attr(accuracy, "gridsearch:x")
	xlab <- attr(accuracy, "gridsearch:xlab")
	accuracy <- sapply(accuracy, function(s) s["RMSE"])
	.plot.gridsearch(x, y=accuracy, model=model,
		xlab=xlab, ylab="RMSE", markfun=which.min, ...)
}

.plot.gridsearch <- function(x, y, model, markfun, ...) {
	model <- pData(model)
	if ( is.character(x) ) {
		i <- which(names(model) == x)
		x <- model[[x]]
		model <- model[,-i,drop=FALSE]
	}
	if ( is.character(y) ) {
		j <- which(names(model) == y)
		y <- model[[y]]
		model <- model[,-j,drop=FALSE]
	}
	if ( ncol(model) != 0 ) {
		param <- expand.grid(lapply(model, unique))
		col <- seq_len(length(unique((param[[1]]))))
		param$col <- col[as.integer(as.factor(param[[1]]))]
		if ( ncol(param) > 1 ) {
			pch <- seq_len(length(unique((param[[2]]))))
			param$pch <- pch[as.integer(as.factor(param[[2]]))]
			lty <- seq_len(length(unique((param[[2]]))))
			param$lty <- lty[as.integer(as.factor(param[[2]]))]
		} else {
			param$pch <- rep(1, nrow(param))
			param$lty <- rep(1, nrow(param))
		}
		plot(range(x), range(y), type='n', ...)
		for ( i in seq_len(nrow(param)) ) {
			par <- param[i,,drop=FALSE]
			ind <- subrows(model, subset=par[names(model)])
			xi <- x[ind]
			yi <- y[ind]
			points(xi, yi, type='b', col=par$col, pch=par$pch, lty=par$lty, ...)
			if ( !missing(markfun) ) {
				mark <- markfun(yi)
				abline(v=xi[mark], col=par$col, lty=9)
				points(xi[mark], yi[mark], col=par$col, pch=4, cex=2)
			}
		}
		legend("topright", legend=.format.data.frame(param[names(model)]),
			col=param$col, pch=param$pch, lty=param$lty)
	} else {
		plot(x, y, type='b', ...)
		if ( !missing(markfun) ) {
			mark <- markfun(y)
			abline(v=x[mark], lty=9)
			points(x[mark], y[mark], pch=4, cex=2)
		}
	}
}
