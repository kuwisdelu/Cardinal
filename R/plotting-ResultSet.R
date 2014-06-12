
setMethod("plot",
	signature = c(x = "SpatialShrunkenCentroids", y = "missing"),
	function(x, parameters,
		mode = c("centers", "tstatistics"),
		tmin = 0,
		classes = unique(unlist(x$classes)),
		col = rainbow(max(unlist(x$k))),
		...)
	{
		mode <- match.arg(mode)
		nclasses <- sapply(x$classes, function(Ck) length(levels(Ck)))
		ntimes <- as.vector(mapply(function(i, Ck) rep(i, Ck),
			seq_len(nrow(modelData(x))), nclasses))
		df <- pData(modelData(x))[ntimes,]
		df$classes <- as.vector(sapply(x$classes, levels))
		centers <- simplify2array(x$centers)
		tstatistics <- simplify2array(x$tstatistics)
		dim(centers) <- c(dim(centers)[1], prod(dim(centers)[-1]))
		dim(tstatistics) <- c(dim(tstatistics)[1], prod(dim(tstatistics)[-1]))
		centers <- MSImageSet(spectra=centers,
			coord=df, mz=fData(x)[["mz"]])
		tstatistics <- MSImageSet(spectra=tstatistics,
			coord=df, mz=fData(x)[["mz"]])
		pData(centers)$model <- names(resultData(x))[ntimes]
		pData(tstatistics)$model <- names(resultData(x))[ntimes]
		if ( !missing(parameters) ) {
			subset <- do.call("pixels", c(list(centers), parameters))
			centers <- centers[,subset]
			tstatistics <- tstatistics[,subset]
		}
		if ( mode == "centers" ) {
			obj <- centers
		} else if ( mode == "tstatistics" ) {
			obj <- tstatistics
		}
		pixel <- pixels(obj, classes=classes)
		significant <- as.vector(abs(unlist(spectra(tstatistics)[,pixel])) > tmin)
		plot(obj, ~ mz | model, pixel=pixel, pixel.groups=classes,
			groups=significant, col=col, type='h', superpose=TRUE,
			lattice=TRUE, ...)
	})


setMethod("image",
	signature = c(x = "SpatialShrunkenCentroids"),
	function(x, parameters,
		mode = c("probabilities", "scores"),
		classes = unique(unlist(x$classes)),
		col = rainbow(max(unlist(x$k))),
		...)
	{
		mode <- match.arg(mode)
		nclasses <- sapply(x$classes, function(Ck) length(levels(Ck)))
		ntimes <- as.vector(mapply(function(i, Ck) rep(i, Ck),
			seq_len(nrow(modelData(x))), nclasses))
		df <- pData(modelData(x))[ntimes,]
		df$classes <- as.vector(sapply(x$classes, levels))
		probabilities <- simplify2array(x$probabilities)
		scores <- simplify2array(x$scores)
		dim(probabilities) <- c(dim(probabilities)[1], prod(dim(probabilities)[-1]))
		dim(scores) <- c(dim(scores)[1], prod(dim(scores)[-1]))
		probabilities <- MSImageSet(spectra=t(probabilities),
			coord=coord(x))
		scores <- MSImageSet(spectra=t(scores),
			coord=coord(x))
		fData(probabilities) <- df
		fData(probabilities)$model <- names(resultData(x))[ntimes]
		fData(scores) <- df
		fData(scores)$model <- names(resultData(x))[ntimes]
		if ( !missing(parameters) ) {
			subset <- do.call("features", c(list(probabilities), parameters))
			probabilities <- probabilities[subset,]
			scores <- scores[subset,]
		}
		if ( mode == "probabilities" ) {
			obj <- probabilities
		} else if ( mode == "scores" ) {
			obj <- scores
		}
		feature <- features(obj, classes=classes)
		image(obj, ~ x * y | model, feature=feature, feature.groups=classes,
			col=col, superpose=TRUE, lattice=TRUE, ...)
	})


