
setMethod("plot",
	signature = c(x = "SpatialShrunkenCentroids", y = "missing"),
	function(x, formula = ~ Feature,
		model = pData(modelData(x)),
		superpose = TRUE,
		auto.key = TRUE,
		...,
		mode = c("centers", "tstatistics"),
		classes = levels(unlist(x$classes)),
		type = 'h',
		col = rainbow(nlevels(unlist(x$classes))),
		lattice = FALSE)
	{
		if ( is.list(model) || any(names(model) %in% varLabels(modelData(x))) ) {
			parameters <- model
		} else {
			parameters <- pData(modelData(x))[model,,drop=FALSE]
		}
		mode <- match.arg(mode)
		model <- .parseFormula(formula)
		formula <- paste(" ~ ", names(model$right))
		formula <- as.formula(paste(c(formula, "model"), collapse="|"))
		nclasses <- sapply(x$classes, function(Ck) length(levels(Ck)))
		ntimes <- as.vector(unlist(mapply(function(i, Ck) rep(i, Ck),
			seq_len(nrow(modelData(x))), nclasses)))
		df <- pData(modelData(x))[ntimes,]
		df$classes <- factor(as.vector(unlist(sapply(x$classes, levels))),
			levels=levels(unlist(x$classes)), labels=levels(unlist(x$classes)))
		centers <- matrix(as.vector(unlist(x$centers)), nrow=nrow(x))
		tstatistics <- matrix(as.vector(unlist((x$tstatistics))), nrow=nrow(x))
		centers <- SImageSet(data=centers,
			coord=df)
		tstatistics <- SImageSet(data=tstatistics,
			coord=df)
		fData(centers) <- fData(x)
		fData(tstatistics) <- fData(x)
		model <- factor(names(resultData(x))[ntimes],
			levels=names(resultData(x)),
			labels=names(resultData(x)))
		pData(centers)$model <- model
		pData(tstatistics)$model <- model
		if ( is.data.frame(parameters) ) {
			which <- unlist(apply(parameters, 1, function(par) {
				do.call("pixels", c(list(centers), par))
			}))
		} else {
			which <- do.call("pixels", c(list(centers), parameters))
		}
		centers <- centers[,which]
		tstatistics <- tstatistics[,which]
		if ( mode == "centers" ) {
			obj <- centers
		} else if ( mode == "tstatistics" ) {
			obj <- tstatistics
		}
		pixel <- pixels(obj, classes=classes)
		significant <- as.vector(apply(iData(tstatistics)[,pixel], 2,
			function(t) abs(t) > 0))
		plot(obj, formula=formula, pixel=pixel, pixel.groups=classes,
			groups=significant, col=col, type=type, superpose=superpose,
			auto.key=auto.key, fun=identity, lattice=lattice, ...)
	})

setMethod("plot",
	signature = c(x = "SpatialShrunkenCentroids", y = "formula"),
	function(x, y, ...) {
		plot(x, formula = y, ...)
	})

setMethod("image",
	signature = c(x = "SpatialShrunkenCentroids"),
	function(x, formula = ~ x * y,
		model = pData(modelData(x)),
		superpose = TRUE,
		auto.key = TRUE,
		...,
		mode = c("probabilities", "scores"),
		classes = unique(unlist(x$classes)),
		col = rainbow(nlevels(unlist(x$classes))),
		lattice = FALSE)
	{
		if ( is.list(model) || any(names(model) %in% varLabels(modelData(x))) ) {
			parameters <- model
		} else {
			parameters <- pData(modelData(x))[model,,drop=FALSE]
		}
		mode <- match.arg(mode)
		model <- .parseFormula(formula)
		formula <- paste(" ~ ", paste(names(model$right), collapse="*"))
		formula <- as.formula(paste(c(formula, "model"), collapse="|"))
		nclasses <- sapply(x$classes, function(Ck) length(levels(Ck)))
		ntimes <- as.vector(unlist(mapply(function(i, Ck) rep(i, Ck),
			seq_len(nrow(modelData(x))), nclasses)))
		df <- pData(modelData(x))[ntimes,]
		df$classes <- factor(as.vector(unlist(sapply(x$classes, levels))),
			levels=levels(unlist(x$classes)), labels=levels(unlist(x$classes)))
		probabilities <- matrix(as.vector(unlist(x$probabilities)), nrow=ncol(x))
		scores <- matrix(as.vector(unlist(x$scores)), nrow=ncol(x))
		probabilities <- SImageSet(data=t(probabilities),
			coord=coord(x))
		scores <- SImageSet(data=t(scores),
			coord=coord(x))
		model <- factor(names(resultData(x))[ntimes],
			levels=names(resultData(x)),
			labels=names(resultData(x)))
		fData(probabilities) <- df
		fData(probabilities)$model <- model
		fData(scores) <- df
		fData(scores)$model <- model
		if ( is.data.frame(parameters) ) {
			which <- unlist(apply(parameters, 1, function(par) {
				do.call("features", c(list(probabilities), par))
			}))
		} else {
			which <- do.call("features", c(list(probabilities), parameters))
		}
		probabilities <- probabilities[which,]
		scores <- scores[which,]
		if ( mode == "probabilities" ) {
			obj <- probabilities
		} else if ( mode == "scores" ) {
			obj <- scores
		}
		feature <- features(obj, classes=classes)
		image(obj, formula=formula, feature=feature, feature.groups=classes,
			col=col, superpose=superpose, auto.key=auto.key,
			fun=identity, lattice=lattice, ...)
	})


