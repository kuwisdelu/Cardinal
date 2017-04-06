
setMethod("spatialShrunkenCentroids",
	signature = c(x = "SImageSet", y = "missing"),
	function(x, y, r = 1, k = 2, s = 0,
		method = c("gaussian", "adaptive"),
		iter.max=10, ...)
	{
		method <- match.arg(method)
		iData(x) <- as.matrixlike(iData(x), supported="matrix")
		rs <- sort(r)
		ks <- sort(k)
		ss <- sort(s)
		.time.start()
		.message("spatialShrunkenCentroids: Initializing classes.")
		initial <- spatialKMeans(x, r=rs, k=ks, method=method, ...)
		result <- unlist(lapply(initial@resultData, function(init) {
			spatial <- spatial.info(x, r=init$r, method=method)
			lapply(ss, function(s) {
				.message("spatialShrunkenCentroids: Fitting r = ", init$r, ", k = ", init$k, ", s = ", s, ".")
				append(.spatialShrunkenCentroids.cluster(x,
					classes=init$cluster, centers=init$centers,
					r=init$r, k=init$k, s=s, spatial=spatial,
					iter.max=iter.max), list(method=method,
						r=init$r, k=init$k, s=s))
			})
		}), recursive=FALSE)
		.message("spatialShrunkenCentroids: Preparing results.")
		model <- AnnotatedDataFrame(data=data.frame(
				r=sapply(result, function(fit) fit$r),
				k=sapply(result, function(fit) fit$k),
				s=sapply(result, function(fit) fit$s)),
			varMetadata=data.frame(
				labelDescription=c(
					r="Neighborhood radius",
					k="Number of classes",
					s="Sparsity parameter")))
		featureNames(model) <- .format.data.frame(pData(model))
		names(result) <- .format.data.frame(pData(model))
		object <- new("SpatialShrunkenCentroids",
			pixelData=x@pixelData,
			featureData=x@featureData,
			experimentData=x@experimentData,
			protocolData=x@protocolData,
			resultData=result,
			modelData=model)
		object <- coregister(object)
		.message("spatialShrunkenCentroids: Done.")
		.time.stop()
		object
	})

setMethod("spatialShrunkenCentroids",
	signature = c(x = "SImageSet", y = "factor"),
	function(x, y, r = 1, s = 0,
		method = c("gaussian", "adaptive"),
		priors = table(y), ...)
	{
		method <- match.arg(method)
		iData(x) <- as.matrixlike(iData(x), supported="matrix")
		priors <- priors / sum(priors)
		rs <- sort(r)
		ss <- sort(s)
		nas <- is.na(y)
		newx <- x
		newy <- y
		if ( any(nas) ) {
			.message("spatialShrunkenCentroids: Removing missing observations.")
			x <- x[,!nas]
			y <- y[!nas]
		}
		.time.start()
		result <- unlist(lapply(ss, function(s) {
			.message("spatialShrunkenCentroids: Calculating shrunken centroids for s = ", s, ".")
			fit <- .spatialShrunkenCentroids.fit(x, classes=y, s=s)
			fit <- append(fit, list(y=newy, s=s, method=method, priors=priors))
			lapply(rs, function(r) {
				.message("spatialShrunkenCentroids: Expanding results for r = ", r, ".")
				append(fit, list(r=r, k=length(levels(y))))
			})
		}), recursive=FALSE)
		model <- AnnotatedDataFrame(data=data.frame(
				r=sapply(result, function(fit) fit$r),
				k=sapply(result, function(fit) fit$k),
				s=sapply(result, function(fit) fit$s)),
			varMetadata=data.frame(
				labelDescription=c(
					r="Neighborhood radius",
					k="Number of classes",
					s="Sparsity parameter")))
		featureNames(model) <- .format.data.frame(pData(model))
		names(result) <- .format.data.frame(pData(model))
		object <- new("SpatialShrunkenCentroids",
			pixelData=x@pixelData,
			featureData=x@featureData,
			experimentData=x@experimentData,
			protocolData=x@protocolData,
			resultData=result,
			modelData=model)
		.time.stop()
		predict(object, newx=newx, newy=newy)
	})

setMethod("spatialShrunkenCentroids",
	signature = c(x = "SImageSet", y = "character"),
	function(x, y, ...)
	{
		spatialShrunkenCentroids(x, factor(y), ...)
	})

setMethod("predict",
	signature = c(object = "SpatialShrunkenCentroids"),
	function(object, newx, newy, ...)
	{
		if ( !is(newx, "iSet") )
			.stop("'newx' must inherit from 'iSet'")
		iData(newx) <- as.matrixlike(iData(newx), supported="matrix")
		.time.start()
		if ( missing(newy) ) {
			missing.newy <- TRUE
		} else {
			missing.newy <- FALSE
		}
		result <- lapply(object@resultData, function(res) {
			spatial <- spatial.info(newx, r=res$r, method=res$method)
			.message("spatialShrunkenCentroids: Predicting classes for r = ", res$r, ", k = ", res$k, ", s = ", res$s, ".")
			pred <- .spatialShrunkenCentroids.predict(newx, classes=res$y,
				centers=res$centers, priors=res$priors,
				spatial=spatial, sd=res$sd)
			res[names(pred)] <- pred
			if ( !missing.newy )
				res$y <- newy
			res
		})
		.message("spatialShrunkenCentroids: Done.")
		.time.stop()
		new("SpatialShrunkenCentroids",
			pixelData=newx@pixelData,
			featureData=newx@featureData,
			experimentData=newx@experimentData,
			protocolData=newx@protocolData,
			resultData=result,
			modelData=object@modelData)
	})

setMethod("logLik", "SpatialShrunkenCentroids", function(object, ...) {
	logp <- sapply(object$probabilities, function(prob) {
		phat <- apply(prob, 1, max)
		sum(log(phat))
	})
	class(logp) <- "logLik"
	attr(logp, "df") <- sapply(object$tstatistics, function(t) {
		sum(abs(t) > 0) + length(features(object))
	})
	attr(logp, "nobs") <- length(pixels(object))
	attr(logp, "names") <- names(resultData(object))
	logp
} )

.spatialShrunkenCentroids.cluster <- function(x, classes, centers, r, k, s,
	spatial, iter.max)
{
	classes.last <- sample(classes)
	centers.last <- matrix(nrow=nrow(iData(x)), ncol=k)
	iter <- 1
	start.time <- proc.time()
	while ( any(classes != classes.last, na.rm=TRUE) && iter <= iter.max )
	{
		priors <- table(classes) / sum(table(classes))
		classes.last <- classes
		centers.last <- centers
		fit <- .spatialShrunkenCentroids.fit(x, classes=classes, s=s)
		pred <- .spatialShrunkenCentroids.predict(x=x, classes=classes,
			centers=fit$centers, priors=priors, spatial=spatial, sd=fit$sd)
		classes <- pred$classes
		centers <- fit$centers
		if ( length(unique(classes)) != length(unique(classes.last)) )
			iter <- 1
		iter <- iter + 1
	}
	list(classes=classes, probabilities=pred$probabilities, scores=pred$scores,
		centers=fit$centers, tstatistics=fit$tstatistics, sd=fit$sd,
		iter=iter, time=proc.time() - start.time)
}

.spatialShrunkenCentroids.fit <- function(x, classes, s, s0) {
	start.time <- proc.time()
	xbar <- rowMeans(iData(x))
	xbar.k <- sapply(levels(classes), function(Ck) {
		rowMeans(iData(x)[,classes==Ck,drop=FALSE]) # may introduce NaNs
	})
	sd <- .calculateWithinClassPooledSD(x, classes=classes, centroid=xbar)
	if ( missing(s0) ) s0 <- median(sd)
	xdiff <- xbar.k - xbar
	se <- .calculateWithinClassPooledSE(x, classes=classes, centroid=xbar,
		sd=sd, s0=s0)
	tstatistics.k <- xdiff / se
	tstatistics <- soft(tstatistics.k, s)
	tstatistics[is.na(tstatistics)] <- 0  # NaNs -> 0
	centers <- xbar + se * tstatistics # NaNs kept for missing class levels
	rownames(centers) <- featureNames(x)
	colnames(centers) <- levels(classes)
	rownames(tstatistics) <- featureNames(x)
	colnames(tstatistics) <- levels(classes)
	names(sd) <- featureNames(x)
	list(centers=centers, tstatistics=tstatistics,
		sd=sd, time=proc.time() - start.time)
}

.spatialShrunkenCentroids.predict <- function(x, classes, centers,
	priors, spatial, sd, s0=median(sd), .C=TRUE)
{
	start.time <- proc.time()
	scores <- .calculateSpatialDiscriminantScores(x, centers=centers,
		priors=priors, spatial=spatial, sd=sd, s0=s0, .C=.C) # NaNs -> Inf
	probabilities <- .calculateClassProbabilities(scores) # NaNs -> 0
	empty <- which(table(classes) == 0)
	clusters <- apply(probabilities, 1, function(p) {
		if ( any(is.finite(p)) ) {
			which.max(p)
		} else {
			empty[[1]]
		}
	})
	classes <- factor(clusters, levels=seq_len(nlevels(classes)), labels=levels(classes))
	names(classes) <- pixelNames(x)
	rownames(probabilities) <- pixelNames(x)
	colnames(probabilities) <- levels(classes)
	rownames(scores) <- pixelNames(x)
	colnames(scores) <- levels(classes)
	list(classes=classes, probabilities=probabilities,
		scores=scores, time=proc.time() - start.time)
}

.spatialShrunkenCentroids.reclass <- function(x, ref) {
	relevel <- x$classes
	levels(relevel) <- levels(x$classes)[ref]
	classes <- factor(relevel, levels=levels(x$classes), labels=levels(x$classes))
	probabilities <- x$probabilities[,order(ref)]
	scores <- x$scores[,order(ref)]
	centers <- x$centers[,order(ref)]
	tstatistics <- x$tstatistics[,order(ref)]
	colnames(probabilities) <- levels(x$classes)
	colnames(scores) <- levels(x$classes)
	colnames(centers) <- levels(x$classes)
	colnames(tstatistics) <- levels(x$classes)
	reclassed <- list(classes=classes,
		probabilities=probabilities, scores=scores,
		centers=centers, tstatistics=tstatistics)
	x[names(reclassed)] <- reclassed
	x
}

.calculateWithinClassPooledSE <- function(x, classes, centroid, sd, s0) {
	m.k <- sqrt((1 / table(classes)) - (1 / length(classes)))
	se <- rep(m.k, each=nrow(iData(x))) * (sd + s0)
	dim(se) <- c(nrow(x), nlevels(classes))
	se
}

.calculateWithinClassPooledSD <- function(x, classes, centroid) {
	wcss <- sqrt(rowSums(.calculateFeaturewiseWCSS(x, classes, centroid)))
	wcss / sqrt(length(classes) - nlevels(classes))
}

.calculateFeaturewiseWCSS <- function(x, classes, centroid) {
	sapply(levels(classes), function(Ck) {
		ok <- classes == Ck
		if ( any(ok) ) {
			rowSums((iData(x)[,ok,drop=FALSE] - centroid)^2)
		} else {
			rep(0, nrow(iData(x)))
		}
	})
}

.calculateSpatialDiscriminantScores <- function(x, centers,
	priors, spatial, sd, s0=median(sd), .C=TRUE)
{
	if ( .C ) {
		w <- rep(1, nrow(iData(x)))
		na.centers <- apply(centers, 2, function(x.k) any(is.na(x.k)))
		if ( any(na.centers) ) centers[,na.centers] <- 0 # handle NaNs
		spatial$beta <- t(simplify2array(spatial$beta, higher=FALSE))
		spatial$neighbors <- t(simplify2array(spatial$neighbors, higher=FALSE))
		scores <- matrix(0, nrow=ncol(x), ncol=ncol(centers))
		scores <- .C("discriminant_scores_spatial", as.double(iData(x)),
			as.integer(nrow(iData(x))), as.integer(ncol(iData(x))),
			as.integer(spatial$neighbors - 1), as.integer(ncol(spatial$neighbors)),
			as.double(w), as.double(spatial$alpha), as.double(spatial$beta),
			as.double(centers), as.integer(ncol(centers)),
			as.double(sd + s0), as.double(scores))[[12]]
		dim(scores) <- c(ncol(iData(x)), ncol(centers))
		if ( any(na.centers) ) scores[,na.centers] <- Inf # handle NaNs
	} else {
		scores <- mapply(function(i, nb, b) {
			gamma <- (spatial$alpha * b) / sum(gamma)
			gamma <- rep(gamma, each=nrow(iData(x)))
			apply(centers, 2, function(x.k) {
				if ( any(is.na(x.k)) ) {
					Inf  # handle NaNs
				} else {
					sum(gamma * (iData(x)[,nb,drop=FALSE] - x.k)^2 / (sd + s0)^2)
				}
			})
		}, seq_len(ncol(iData(x))), spatial$neighbors, spatial$beta)
		scores <- t(scores)
	}
	scores - 2 * log(rep(priors, each=ncol(iData(x))))
}

.calculateClassProbabilities <- function(scores) {
	t(apply(scores, 1, function(s) {
		pmax(exp(-0.5 * s) / sum(exp(-0.5 * s)),
			rep(0, length(s)),
			na.rm=TRUE)
	}))
}
