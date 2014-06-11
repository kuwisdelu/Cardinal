
setMethod("spatialShrunkenCentroids",
	signature = c(x = "SImageSet", y = "missing"),
	function(x, y, r = 1, k = 2:5, s = seq(from=0, to=10, by=2),
		method = c("gaussian", "adaptive"),
		iter.max=10, tol=1e-4, ...)
{
	method <- match.arg(method)
	rs <- sort(r)
	ks <- sort(k)
	ss <- sort(s)
	initial <- spatialKMeans(x, r=rs, k=ks, method=method, ...)
	out <- unlist(lapply(initial, function(init) {
		spatial <- .calculateSpatial(x, r=r)
		priors <- tabulate(init$cluster) / sum(tabulate(init$cluster))
		lapply(ss, function(s) {
			append(.clusterSpatialShrunkenCentroids(x, classes=factor(init$cluster),
				r=r, k=k, s=s, spatial=spatial, iter.max=iter.max, tol=tol),
				list(y=y, method=method, priors=priors))
		})
	}), recursive=FALSE)
	par <- AnnotatedDataFrame(data=data.frame(
			r=sapply(out, function(fit) fit$r),
			k=sapply(out, function(fit) fit$k),
			s=sapply(out, function(fit) fit$s)),
		varMetadata=data.frame(
			labelDescription=c(
				r="Neighborhood radius",
				k="Number of classes",
				s="Sparsity parameter")))
	featureNames(par) <- formatParam(pData(par))
	names(out) <- formatParam(pData(par))
	object <- new("SpatialShrunkenCentroids",
		pixelData=x@pixelData,
		featureData=x@featureData,
		experimentData=x@experimentData,
		protocolData=x@protocolData,
		resultData=out,
		modelData=par)
})

setMethod("spatialShrunkenCentroids",
	signature = c(x = "SImageSet", y = "factor"),
	function(x, y, r = 1, s = seq(from=0, to=10, by=2),
		method = c("gaussian", "adaptive"),
		priors = tabulate(y), ...)
{
	method <- match.arg(method)
	priors <- priors / sum(priors)
	rs <- sort(r)
	ss <- sort(s)
	out <- unlist(lapply(ss, function(s) {
		fit <- .calculateSpatialShrunkenCentroids(x, classes=y, s=s)
		fit <- append(fit, list(y=y, s=s, method=method, priors=priors))
		lapply(rs, function(r) append(fit, list(r=r, k=length(levels(y)))))
	}), recursive=FALSE)
	par <- AnnotatedDataFrame(data=data.frame(
			r=sapply(out, function(fit) fit$r),
			k=sapply(out, function(fit) fit$k),
			s=sapply(out, function(fit) fit$s)),
		varMetadata=data.frame(
			labelDescription=c(
				r="Neighborhood radius",
				k="Number of classes",
				s="Sparsity parameter")))
	featureNames(par) <- formatParam(pData(par))
	names(out) <- formatParam(pData(par))
	object <- new("SpatialShrunkenCentroids",
		pixelData=x@pixelData,
		featureData=x@featureData,
		experimentData=x@experimentData,
		protocolData=x@protocolData,
		resultData=out,
		modelData=par)
	predict(object, newx=x, newy=y)
})

setMethod("predict",
	signature = c(object = "SpatialShrunkenCentroids"),
	function(object, newx, newy, ...)
{
	if ( !is(newx, "iSet") )
		.stop("'newx' must inherit from 'iSet'")
	out <- lapply(object@resultData, function(ob) {
		spatial <- .calculateSpatial(newx, r=r)
		pred <- .calculateNearestSpatialShrunkenCentroids(newx, classes=ob$y,
			centers=ob$centers, priors=ob$priors, spatial=spatial, sd=ob$sd)
		ob[names(pred)] <- pred
		pred
	})
	new("SpatialShrunkenCentroids",
		pixelData=newx@pixelData,
		featureData=newx@featureData,
		experimentData=newx@experimentData,
		protocolData=newx@protocolData,
		resultData=out,
		modelData=object@modelData)
})

.clusterSpatialShrunkenCentroids <- function(x, classes, r, k, s,
	priors, spatial, iter.max, tol)
{
	classes.last <- sample(classes.last)
	centers.last <- matrix(nrow=nrow(centers.last), ncol=ncol(centers.last))
	iter <- 1
	while ( any(classes != classes.last) &&
		l2norm(centers - centers.last) > tol &&
		iter <= iter.max )
	{
		classes.last <- classes
		centers.last <- centers
		fit <- .calculateSpatialShrunkenCentroids(x, classes=classes, s=s)
		degenerates <- 0 == apply(fit$tstatistics, 2, function(t) sum(abs(t) > 0))
		if ( any(degenerates) ) {
			iter <- 1
			fit$centers <- fit$centers[,!degenerates]
			centers.last <- centers.last[,!degenerates]
		}
		refit <- .calculateNearestSpatialShrunkenCentroids(x=x, classes=classes,
			centers=fit$centers, priors=priors, spatial=spatial, sd=fit$sd)
		classes <- refit$classes
		centers <- fit$centers
		if ( ncol(centers) != ncol(centers.last) ) {
			iter <- 1
			centers.last <- centers.last[,1:ncol(centers)]
		}
		iter <- iter + 1
	}
	list(classes=classes, probabilities=refit$probabilities,
		scores=refit$scores, k=k, s=s, r=r, centers=fit$centers,
		tstatistics=fit$tstatistics, sd=fit$sd)
}

.calculateSpatialShrunkenCentroids <- function(x, classes, s, sd, s0=median(sd)) {
	xbar <- rowMeans(iData(x))
	xbar.k <- sapply(levels(classes), function(Ck) {
		rowMeans(iData(x)[,classes==Ck,drop=FALSE])
	})
	m.k <- sqrt((1 / tabulate(classes)) - (1 / length(classes)))
	if ( missing(sd) )
		sd <- .calculateWithinClassPooledSD(x, classes=classes, centroid=xbar)
	xdiff <- xbar.k - xbar # matrix - vector
	se <- rep(m.k, each=nrow(iData(x))) * (sd + s0)
	dim(se) <- dim(xbar.k)
	tstatistics.k <- xdiff / se
	tstatistics <- soft(tstatistics.k, s)
	centers <- xbar + se * tstatistics
	list(centers=centers,
		tstatistics=tstatistics,
		sd=sd)
}

.calculateNearestSpatialShrunkenCentroids <- function(x, classes, centers,
	priors, spatial, sd, s0=median(sd), .C=TRUE)
{
	scores <- .calculateSpatialDiscriminantScores(x, centers=centers,
		priors=priors, spatial=spatial, sd=sd, s0=s0, .C=.C)
	probabilities <- .calculateClassProbabilities(scores)
	classes <- factor(apply(probabilities, 1, which.max))
	levels(classes) <- levels(classes)
	list(classes=classes,
		probabilities=probabilities,
		scores=scores)
}

.calculateWithinClassPooledSD <- function(x, classes, centroid) {
	K <- length(levels(classes))
	wcss <- sqrt(rowSums(.calculateFeaturewiseWCSS(x, classes, centroid)))
	wcss / sqrt(length(classes) - K)
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
	priors, spatial, sd, s0=median(si), .C=TRUE)
{
	if ( .C ) {
		w <- rep(1, nrow(iData(x)))
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
	} else {
		scores <- mapply(function(i, nb, b) {
			gamma <- (spatial$alpha * b) / sum(gamma)
			gamma <- rep(gamma, each=nrow(iData(x)))
			apply(centers, 2, function(x.k) {
				sum(gamma * (x[,nb,drop=FALSE] - x.k)^2 / (sd + s0)^2)
			})
		}, seq_len(ncol(iData(x))), spatial$neighbors, spatial$beta)
		scores <- t(scores)
	}
	scores - 2 * log(rep(priors, each=ncol(iData(x))))
}

.calculateClassProbabilities <- function(scores) {
	t(apply(scores, 1, function(s) {
		exp(-0.5 * s) / sum(exp(-0.5 * s))
	}))
}


