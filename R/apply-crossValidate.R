
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
					iData(.x, ".peaks") <- iData(.peaks)
				} else {
					iData(.x, ".peaks") <- .peaks
				}
				FUN <- function(x, y, ..., BPPARAM) {
					peaks <- x
					imageData(peaks) <- ImageArrayList(iData(x, ".peaks"))
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
		cv <- .cv_collate(cv, .y, .fold)
		# return results
		out <- .CrossValidated2(
			imageData=.SimpleImageArrayList(),
			featureData=featureData(.x),
			elementMetadata=pixelData(.x),
			metadata=list(
				parameters=names(cv$models),
				type=cv$type),
			resultData=as(cv$results, "List"),
			modelData=cv$models)
		if ( is.factor(.y) || is.character(.y) ) {
			pixelData(out)$.response <- as.factor(.y)
		} else {
			.y <- as.matrix(.y)
			i <- if (ncol(.y) > 1) seq_len(ncol(.y)) else ""
			nms <- paste0(".response", i)
			pixelData(out)[nms] <- as.data.frame(.y)
		}
		pixelData(out)$..fold.. <- .fold
		if ( cv$type == "classification" )
			metadata(out)[["positive class"]] <- cv$pos
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
			if ( .simplify )
				pred <- .cv_simplify(pred, data, .fitted)
			.message("<<<< done: ", fi, " >>>>\n")
			pred
		}, BPREDO=BPREDO, BPPARAM=BPPARAM)
	})

setAs("CrossValidated", "CrossValidated2",
	function(from) {
		cv <- lapply(resultData(from), function(pred) {
			if ( is(pred, "PLS") ) {
				pred <- as(pred, "PLS2")
			} else if ( is(pred, "OPLS") ) {
				pred <- as(pred, "OPLS2")
			} else if ( is(pred, "SpatialShrunkenCentroids") ) {
				pred <- as(pred, "SpatialShrunkenCentroids2")
			} else {
				pred <- .coerce_ImagingResult(pred,
					"SparseImagingResult")
			}
			.cv_simplify(pred, .fitted=fitted)
		})
		y <- attr(cv[[1]], "y")
		cv <- .cv_collate(cv, y, .fold=pData(from)$sample)
		out <- .CrossValidated2(
			imageData=.SimpleImageArrayList(),
			featureData=XDataFrame(fData(from)),
			elementMetadata=PositionDataFrame(
				coord=DataFrame(coord(from)[,coordLabels(from)],
					row.names=NULL),
				run=pixelData(from)$sample),
			metadata=list(
				parameters=names(cv$models),
				type=cv$type),
			resultData=as(cv$results, "List"),
			modelData=cv$models)
		pixelData(out)$.fold <- pData(from)$sample
		if ( cv$type == "classification" )
			metadata(out)[["positive class"]] <- cv$pos
		out
	})

.cv_collate <- function(cv, y, .fold) {
	models <- attr(cv[[1]], "models")
	reference <- lapply(cv, function(f) attr(f, "y"))
	if ( !is.numeric(y) ) {
		type <- "classification"
		y <- as.factor(y)
		pos <- levels(y)[1]
	} else {
		type <- "regression"
	}
	results <- lapply(1:nrow(models), function(i) {
		fitted <- lapply(cv, function(f) f[[i]])
		names(fitted) <- levels(.fold)
		if ( type == "classification" ) {
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
	if ( type == "classification" ) {
		list(results=results, models=models, type=type, pos=pos)
	} else {
		list(results=results, models=models, type=type)
	}
}

.cv_simplify <- function(pred, data, .fitted) {
	if ( missing(data) )
		data <- pixelData(pred)$.response
	models <- modelData(pred)
	pred <- .fitted(pred)
	attr(pred, "models") <- models
	attr(pred, "y") <- data
	pred
}

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
