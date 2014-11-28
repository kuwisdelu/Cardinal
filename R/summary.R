
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
		sdev <- object$sdev[[which]]
		totvar <- object[[which]]$totvar
		importance <- t(simplify2array(list(sdev,
			sdev^2 / totvar,
			cumsum(sdev^2 / totvar))))
		dimnames(importance)  <- list(c("Standard deviation",
				"Proportion of Variance",
				"Cumulative"),
			paste0("PC", seq_along(sdev)))
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
	var <- x$importance["Proportion of Variance",]
	cum <- x$importance["Cumulative",]
	data <- data.frame(pc=seq_along(var), sdev=sdev, var=var, cum=cum)
	plot(var ~ pc, data=data, type='b', xlab="PC",
		ylab="Proportion of Variance")
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
		out <- list(topLabels=topLabels, accuracy=accuracy,
			model=modelData(object), method=object[[1]]$method)
		class(out) <- "summary.PLS"
		out
	})

print.summary.PLS <- function(x, ...) {
	print(x$accuracy)
}

plot.summary.PLS <- function(x, y, ...) {
	.plot.accuracy(x, y, ...)	
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
		out <- list(topLabels=topLabels, accuracy=accuracy,
			model=modelData(object), method=object[[1]]$method)
		class(out) <- "summary.OPLS"
		out
	})

print.summary.OPLS <- function(x, ...) {
	print(x$accuracy)
}

plot.summary.OPLS <- function(x, y, ...) {
	.plot.accuracy(x, y, ...)	
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
	data <- pData(x$model)
	data$withinss <- x$withinss
	param <- data.frame(r=sort(unique(data$r)))
	col <- rainbow(length(unique((data$r))))
	param$col <- col[as.integer(as.factor(param$r))]
	plot(range(data$k), range(data$withinss), type='n',
		xlab="# of Clusters (k)", ylab="Within-Cluster SS")
	for ( i in seq_len(nrow(param)) ) {
		par <- param[i,,drop=FALSE]
		dat <- data[data$r == par$r,,drop=FALSE]
		points(withinss ~ k, data=dat, type='b', col=par$col, pch=1, lty=1)
	}
	legend("topright", legend=.format.data.frame(param[,"r",drop=FALSE]),
		col=param$col, pch=1, lty=1)
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
			if ( is.null(x$y) ) {
				NULL
			} else {
				.summarize.factor(x$y, x$classes)
			}
		})
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
	print(model)
	if ( !all(sapply(x$accuracy, is.null)) ) {
		cat("\n")
		print(x$accuracy)
	}
}

plot.summary.SpatialShrunkenCentroids <- function(x, y, ...) {
	data <- pData(x$model)
	data$nclasses <- x$nclasses
	param <- expand.grid(r=sort(unique(data$r)),
		k=sort(unique(data$k)))
	col <- rainbow(length(unique((param$r))))
	param$col <- col[as.integer(as.factor(param$r))]
	pch <- seq_len(length(unique((param$k))))
	param$pch <- pch[as.integer(as.factor(param$k))]
	lty <- seq_len(length(unique((param$k))))
	param$lty <- lty[as.integer(as.factor(param$k))]
	plot(range(data$s), c(min(data$nclasses), max(data$k)), type='n',
		xlab="Shrinkage Parameter (s)", ylab="Predicted # of Classes")
	for ( i in seq_len(nrow(param)) ) {
		par <- param[i,,drop=FALSE]
		dat <- data[data$r == par$r & data$k == par$k,]
		points(nclasses ~ s, data=dat, type='b', col=par$col,
			pch=par$pch, lty=par$lty)
	}
	legend("topright", legend=.format.data.frame(param[,c("r","k")]),
		col=param$col, pch=param$pch, lty=param$lty)
}

setMethod("summary", "CrossValidated",
	function(object, ...) {
		accuracy <- lapply(resultData(object),
			function(ob) summary(ob)$accuracy)
		accuracy <- do.call("Map", c(function(...) {
			dots <- list(...)
			nfold <- length(dots)
			acc <- Reduce(`+`, dots)
			acc / nfold
		}, accuracy))
		out <- list(accuracy=accuracy, model=modelData(object[[1]]))
		class(out) <- "summary.CrossValidated"
		out
	})

print.summary.CrossValidated <- function(x, ...) {
	print(x$accuracy)
}

plot.summary.CrossValidated <- function(x, y, ...) {
	.plot.accuracy(x, y, ...)	
}

.summarize.factor <- function(y, fitted) {
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

.plot.accuracy <- function(x, y, ...) {
	data <- pData(x$model)
	accuracy <- sapply(x$accuracy, function(summ) summ["Accuracy",])
	accuracy <- as.data.frame(t(accuracy))
	data <- data.frame(data, accuracy)
	plot(data$ncomp, accuracy[,1], type='b',
		xlab="# of Components", ylab="Accuracy",
		col=1, pch=1, lty=1)
	abline(v=data$ncomp[which.max(accuracy[,1])], col=1, lty=9)
	if ( ncol(accuracy) > 2 ) {
		for ( i in seq_along(names(accuracy)) ) {
			points(data$ncomp, accuracy[,i], type='b',
				xlab="# of Components", ylab="Accuracy",
				col=i, pch=i, lty=i)
			abline(v=data$ncomp[which.max(accuracy[,i])], col=i, lty=9)
		}
		legend("topright", legend=names(accuracy),
			col=seq_len(ncol(accuracy)),
			pch=seq_len(ncol(accuracy)),
			lty=seq_len(ncol(accuracy)))
	}
}
