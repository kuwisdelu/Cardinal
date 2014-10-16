
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
		importance <- t(simplify2array(list(sdev,
			sdev^2 / sum(sdev^2),
			cumsum(sdev^2 / sum(sdev^2)))))
		dimnames(importance)  <- list(c("Standard deviation",
				"Proportion of Variance",
				"Cumulative"),
			paste0("PC", seq_along(sdev)))
		out <- list(topLabels=topLabels, importance=importance)
		class(out) <- "summary.PCA"
		out
	})

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
		out <- list(topLabels=topLabels, accuracy=accuracy)
		class(out) <- "summary.PLS"
		out
	})

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
		out <- list(topLabels=topLabels, accuracy=accuracy)
		class(out) <- "summary.OPLS"
		out
	})

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
		out <- list(topLabels=topLabels)
		class(out) <- "summary.SpatialKMeans"
		out
	})

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
		out <- list(topLabels=topLabels, accuracy=accuracy)
		class(out) <- "summary.SpatialShrunkenCentroids"
		out
	})

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
		out <- list(accuracy=accuracy)
		class(out) <- "summary.CrossValidated"
		out
	})

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
