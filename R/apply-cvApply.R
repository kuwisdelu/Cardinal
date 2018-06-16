
setMethod("cvApply", "SImageSet",
	function(.x, .y, .fun, .fold = sample, ...) {
		# setup
		x <- .x
		y <- .y
		# get cv folds and method
		.fun <- match.fun(.fun)
		.fold <- tryCatch(eval(substitute(.fold), envir=pData(x),
			enclos=environment(.fun)), error=function(e) eval(.fold))
		# loop through folds
		result <- lapply(sort(unique(.fold)), function(k) {
			.message("!!---- Fold ", k, " ----!!")
			if ( length(dim(y)) == 2 ) {
				fit <- .fun(x[,k!=.fold], y[k!=.fold,,drop=FALSE], ...)
				predict(fit,
					newx=x[,k==.fold],
					newy=y[k==.fold,,drop=FALSE], ...)
			} else {
				fit <- .fun(x[,k!=.fold], y[k!=.fold], ...)
				predict(fit,
					newx=x[,k==.fold],
					newy=y[k==.fold], ...)
			}
		})
		names(result) <- sort(unique(.fold))
		model <- AnnotatedDataFrame(data=data.frame(
				fold=sort(unique(.fold))),
			varMetadata=data.frame(
				labelDescription=c(
					fold="Fold")))
		new("CrossValidated",
			pixelData=x@pixelData,
			featureData=x@featureData,
			experimentData=x@experimentData,
			protocolData=x@protocolData,
			resultData=result,
			modelData=model)
	})
