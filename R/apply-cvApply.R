
setMethod("crossValidate", "MSImagingExperiment",
	function(.x, .y, .fun,
		.fold = run(.x),
		.predict = predict,
		.process = FALSE,
		.processControl = list(),
		.peaks = NULL,
		BPPARAM = bpparam(), ...)
	{
		.fun <- match.fun(.fun)
		.predict <- match.fun(.predict)
		# get peakAlign and peakBin arguments
		alignArgs <- list()
		binArgs <- list()
		if ( "tolerance" %in% names(.processControl) ) {
			alignArgs$tolerance <- .processControl$tolerance
			binArgs$tolerance <- .processControl$tolerance
			.processControl$tolerance <- NULL
		} else if ( "units" %in% names(.processControl) ) {
			alignArgs$units <- .processControl$units
			binArgs$units <- .processControl$units
			.processControl$units <- NULL
		} else if ( "type" %in% names(.processControl) ) {
			binArgs$type <- .processControl$type
			.processControl$tyle <- NULL
		}
		# get peakFilter arguments
		if ( length(.processControl) > 1L ) {
			filterArgs <- .processControl
		} else {
			filterArgs <- list()
		}
		# pre-processing
		if ( .process ) {
			if ( is.null(.peaks) ) {
				FUN <- function(x, y, ..., BPPARAM) {
					peaks <- do.call(peakAlign, c(list(x), alignArgs))
					peaks <- do.call(peakFilter, c(list(peaks), filterArgs))
					peaks <- process(peaks, BPPARAM=BPPARAM)
					.fun(peaks, y, ..., BPPARAM=BPPARAM)
				}
				PREDICT <- function(fit, newx, newy, ..., BPPARAM) {
					peaks <- do.call(peakAlign, c(list(newx, mz(fData(fit))), alignArgs))
					peaks <- process(peaks, BPPARAM=BPPARAM)
					.predict(fit, peaks, newy, ..., BPPARAM=BPPARAM)
				}
			} else {
				if ( is(.peaks, "MSImagingExperiment") ) {
					iData(.x, "..peaks..") <- iData(.peaks)
				} else {
					iData(.x, "..peaks..") <- .peaks
				}
				FUN <- function(x, y, ..., BPPARAM) {
					peaks <- x
					imageData(peaks) <- ImageArrayList(iData(x, "..peaks.."))
					peaks <- do.call(peakAlign, c(list(peaks), alignArgs))
					peaks <- do.call(peakFilter, c(list(peaks), filterArgs))
					peaks <- process(peaks, BPPARAM=BPPARAM)
					x <- do.call(peakBin, c(list(x, mz(peaks)), binArgs))
					x <- process(x, BPPARAM=BPPARAM)
					.fun(x, y, ..., BPPARAM=BPPARAM)
				}
				PREDICT <- function(fit, newx, newy, ..., BPPARAM) {
					newx <- do.call(peakBin, c(list(newx, mz(fData(fit))), binArgs))
					newx <- process(newx, BPPARAM=BPPARAM)
					.predict(fit, newx, newy, ..., BPPARAM=BPPARAM)
				}
			}
		} else {
			FUN <- .fun
			PREDICT <- .predict
		}
		# apply cross-validation
		callNextMethod(.x, .y, .fun=FUN, .fold=.fold,
			.predict=PREDICT, BPPARAM=BPPARAM, ...)
	})

setMethod("crossValidate", "SparseImagingExperiment",
	function(.x, .y, .fun, .fold = run(.x),
		BPPARAM = bpparam(), ...)
	{
		# get cross-validation folds
		.fold <- as.factor(.fold)
		# apply cross-validation
		cv <- cvApply(.x, .y, .fun=.fun, .fold=.fold,
			.simplify=TRUE, BPPARAM=BPPARAM, ...)
		# collate results
		models <- attr(cv[[1]], "models")
		reference <- lapply(cv, function(f) attr(f, "y"))
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
					sensitivity(ref, pred, positive=pos)
				}, fitted, reference)
				spec <- mapply(function(pred, ref) {
					specificity(ref, pred, positive=pos)
				}, fitted, reference)
				res <- list(y=reference,
					fitted=fitted, accuracy=acc,
					sensitivity=sens, specificity=spec)
			} else {
				rmse <- mapply(function(pred, ref) {
					sqrt(mean((pred - ref)^2, na.rm=TRUE))
				}, fitted, reference)
				mae <- mapply(function(pred, ref) {
					mean(abs(pred - ref), na.rm=TRUE)
				}, fitted, reference)
				res <- list(y=reference, fitted=fitted,
					rmse=rmse, mae=mae)
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
				`positive class`=pos),
			resultData=as(results, "List"),
			modelData=models)
		pixelData(out)[["..response.."]] <- .y
		pixelData(out)[["..fold.."]] <- .fold
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
		.simplify = FALSE,
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
				fit <- .fun(.x[,fi!=.fold,drop=FALSE], .y[fi!=.fold],
					BPPARAM = BPPARAM, ...) # train
				pred <- .predict(fit, .x[,fi==.fold,drop=FALSE],
					.y[fi==.fold], BPPARAM = BPPARAM, ...) # test
				data <- .y[fi==.fold] # save test data
			} else {
				# matrix response
				fit <- .fun(.x[,fi!=.fold,drop=FALSE], .y[fi!=.fold,,drop=FALSE],
					BPPARAM = BPPARAM, ...) # train
				pred <- .predict(fit, .x[,fi==.fold,drop=FALSE],
					.y[fi==.fold,,drop=FALSE], BPPARAM = BPPARAM, ...) # test
				data <- .y[fi==.fold,,drop=FALSE]  # save test data
			}
			if ( .simplify ) {
				params <- metadata(fit)$parameters
				if ( !is.null(params) ) {
					models <- modelData(fit)[params]
				} else {
					models <- modelData(fit)
				}
				pred <- .fitted(pred) # extract predicted values
				attr(pred, "models") <- models
				attr(pred, "y") <- data # observed values
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
