
# setMethod("crossValidate", "MSImagingExperiment",
# 	function(.x, .y, .fun,
# 		.fold = run(.x),
# 		.predict = predict,
# 		.process = !centroided(.x),
# 		BPPARAM = bpparam(), ...)
# 	{
# 		.fun <- match.fun(.fun)
# 		.predict <- match.fun(.predict)
# 		# modify functions for preprocessing
# 		.myfun <- .fun
# 		.mypredict <- .predict
# 		# apply cross-validation
# 		callNextMethod(.x, .y, .fun=.myfun, .fold=.fold,
# 			.predict=.mypredict, BPPARAM=BPPARAM, ...)
# 	})

setMethod("crossValidate", "SparseImagingExperiment",
	function(.x, .y, .fun, .fold = run(.x),
		BPPARAM = bpparam(), ...)
	{
		# get folds and response
		yname <- "..response.."
		.y <- force(.y)
		fname <- "..fold.."
		.fold <- as.factor(.fold)
		# apply cross-validation
		cv <- cvApply(.x, .y, .fun=.fun, .fold=.fold,
			.simplify=TRUE, BPPARAM=BPPARAM, ...)
		# collate results
		models <- attr(cv[[1]], "models")
		reference <- lapply(cv, function(f) attr(f, "reference"))
		if ( !is.numeric(fitted) ) {
			isClassification <- TRUE
			.y <- as.factor(.y)
			pos <- levels(.y)[1]
		} else {
			isClassification <- FALSE
		}
		results <- lapply(1:nrow(models), function(i) {
			fitted <- lapply(cv, function(f) f[[i]])
			names(fitted) <- levels(.fold)
			if ( isClassification ) {
				acc <- mapply(function(pred, ref) {
					mean(pred == ref, na.rm=TRUE)
				}, fitted, reference)
				sens <- mapply(function(pred, ref) {
					sensitivity(pred, ref, positive=pos)
				}, fitted, reference)
				spec <- mapply(function(pred, ref) {
					specificity(pred, ref, positive=pos)
				}, fitted, reference)
				res <- list(fitted=fitted, accuracy=acc,
					sensitivity=sens, specificity=spec)
			} else {
				rmse <- mapply(function(pred, ref) {
					sqrt(mean((pred - ref)^2, na.rm=TRUE))
				}, fitted, reference)
				mae <- mapply(function(pred, ref) {
					mean(abs(pred - ref), na.rm=TRUE)
				}, fitted, reference)
				res <- list(fitted=fitted, rmse=rmse, mae=mae)
			}
			res
		})
		# return results
		out <- .CrossValidated2(
			imageData=.SimpleImageArrayList(),
			featureData=featureData(.x),
			elementMetadata=pixelData(.x),
			metadata=list(
				parameters=names(models),
				responseName=yname,
				foldsName=fname,
				positiveClass=pos),
			resultData=as(results, "List"),
			modelData=models)
		pixelData(out)[[yname]] <- .y
		pixelData(out)[[fname]] <- .fold
		if ( isClassification ) {
			modelData(out)$accuracy <- sapply(results,
				function(res) mean(res$accuracy, na.rm=TRUE))
			modelData(out)$sensitivity <- sapply(results,
				function(res) mean(res$sensitivity, na.rm=TRUE))
			modelData(out)$specificity <- sapply(results,
				function(res) mean(res$specificity, na.rm=TRUE))
		} else {
			modelData(out)$rmse <- sapply(results,
				function(res) mean(res$rmse, na.rm=TRUE))
			modelData(out)$mae <- sapply(results,
				function(res) mean(res$mae, na.rm=TRUE))
		}
		out
	})

setMethod("cvApply", "SparseImagingExperiment",
	function(.x, .y, .fun,
		.fold = run(.x),
		.predict = predict,
		.fitted = fitted,
		.simplify = TRUE,
		BPREDO = list(),
		BPPARAM = bpparam(), ...)
	{
		# get functions
		BPPARAM <- .protectNestedBPPARAM(BPPARAM)
		.fun <- match.fun(.fun)
		.predict <- match.fun(.predict)
		.fitted <- match.fun(.fitted)
		.fold <- as.factor(.fold)
		# apply cross-validation
		bplapply(levels(.fold), function(fi, BPPARAM) {
			.message(">>>> fold: ", fi, " <<<<")
			if ( is.null(dim(.y)) ) {
				# vector response
				fit <- .fun(.x[,fi!=.fold,drop=FALSE],
					.y[fi!=.fold], BPPARAM = BPPARAM, ...)
				pred <- .predict(fit, .x[,fi==.fold,drop=FALSE],
					.y[fi==.fold], BPPARAM = BPPARAM, ...)
				ref <- .y[fi==.fold]
			} else {
				# matrix response
				fit <- .fun(.x[,fi!=.fold,drop=FALSE],
					.y[fi!=.fold,,drop=FALSE], BPPARAM = BPPARAM, ...)
				pred <- .predict(fit, .x[,fi==.fold,drop=FALSE],
					.y[fi==.fold,,drop=FALSE], BPPARAM = BPPARAM, ...)
				ref <- .y[fi==.fold,,drop=FALSE]
			}
			if ( .simplify ) {
				params <- metadata(fit)$parameters
				if ( !is.null(params) ) {
					models <- modelData(fit)[params]
				} else {
					models <- modelData(fit)
				}
				pred <- .fitted(pred)
				attr(pred, "models") <- models
				attr(pred, "reference") <- ref
			}
			.message("<<<< done: ", fi, " >>>>\n")
			pred
		}, BPREDO=BPREDO, BPPARAM=BPPARAM)
	})

setMethod("cvApply", "SImageSet",
	function(.x, .y, .fun, .fold = sample, ...)
	{
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
