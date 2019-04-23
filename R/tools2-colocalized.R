

setMethod("colocalized",
	signature = c("MSImagingExperiment", "missing"),
	function(object, mz, ...)
	{
		ref <- features(object, mz=mz)
		colocalized(object, ref=ref, ...)
	})

setMethod("colocalized",
	signature = c("SparseImagingExperiment", "ANY"),
	function(object, ref, n = 10,
		sort.by = c("correlation", "M1", "M2"),
		threshold = median,
		BPPARAM = bpparam(), ...)
	{
		.checkForIncompleteProcessing(object)
		threshold <- match.fun(threshold)
		sort.by <- match.arg(sort.by)
		# check if feature ID or vector
		if ( length(ref) == 1L && is.wholenumber(ref) ) {
			.message("using feature ", ref, " as reference")
			ref <- as.numeric(iData(object)[ref,])
		}
		if ( length(ref) != ncol(object) ) {
			.stop("length of reference [", length(ref),
				"] does not match object [", ncol(object), "]")
		}
		# create logical mask
		if ( !is.logical(ref) ) {
			ref2 <- ref > threshold(ref)
		} else {
			ref2 <- ref
		}
		.message("measuring image colocalization...")
		results <- featureApply(object, function(xi) {
			corr <- cor(ref, xi)
			xi <- xi > threshold(xi)
			M1 <- Mscore(ref2, xi, type=1)
			M2 <- Mscore(ref2, xi, type=2)
			c(correlation=corr, M1=M1, M2=M2)
		}, .simplify=TRUE, BPPARAM=BPPARAM)
		results <- as.data.frame(t(results))
		out <- cbind(as.data.frame(fData(object)), results)
		sort.by <- which(names(out) %in% sort.by)
		order <- order(out[,sort.by], decreasing=TRUE)
		if ( is.finite(n) )
			order <- head(order, n=n)
		out <- out[order,,drop=FALSE]
		SummaryDataFrame(as.list(out), .rownumbers=TRUE,
			.summary="Colocalized features:")
	})

setMethod("colocalized",
	signature = c("SpatialDGMM", "ANY"),
	function(object, ref, n = 10,
		sort.by = c("Mscore", "M1", "M2"),
		threshold = median,
		BPPARAM = bpparam(), ...)
	{
		threshold <- match.fun(threshold)
		sort.by <- match.arg(sort.by)
		# check if feature ID or vector
		if ( length(ref) == 1L && is.wholenumber(ref) ) {
			.message("using feature ", ref, " as reference")
			i1 <- which(ref == modelData(object)$feature)
			.message("using model ", .format.data.labels(modelData(object)[i1,]))
			est <- resultData(object, i1, "estimates")
			i2 <- tapply(est$mean, est$group, is.max)
			i2 <- which(as.logical(unlist(i2)))
			class <- resultData(object, i1, "class")
			.message("using class ", paste0(est$class[i2], collapse=", "))
			ref <- class %in% est$class[i2]
		}
		if ( length(ref) != ncol(object) ) {
			.stop("length of reference [", length(ref),
				"] does not match object [", ncol(object), "]")
		}
		# create logical mask
		if ( !is.logical(ref) )
			ref <- ref > threshold(ref)
		groups <- pixelData(object)$..group..
		models <- modelData(object)
		ii <- seq_len(nrow(models))
		.message("measuring segmentation colocalization...")
		results <- bpmapply(function(res, i) {
			measures <- mapply(function(gi, ci) {
				xi <- (res$class == ci)[gi == groups]
				ref2 <- ref[gi == groups]
				score <- Mscore(ref2, xi)
				M1 <- Mscore(ref2, xi, type=1)
				M2 <- Mscore(ref2, xi, type=2)
				c(Mscore=score, M1=M1, M2=M2)
			}, res$estimates$group, res$estimates$class)
			measures <- as.data.frame(t(measures))
			mapping <- data.frame(
				group=res$estimates$group,
				class=res$estimates$class)
			feature <- models[i,"feature"]
			fData <- as.list(fData(object)[feature,,drop=FALSE])
			model <- as.list(models[i,,drop=FALSE])
			data.frame(fData, model, mapping, measures)
		}, resultData(object), ii, SIMPLIFY=FALSE, BPPARAM=BPPARAM)
		out <- do.call("rbind", results)
		sort.by <- which(names(out) %in% sort.by)
		order <- order(out[,sort.by], decreasing=TRUE)
		if ( is.finite(n) )
			order <- head(order, n=n)
		out <- out[order,,drop=FALSE]
		SummaryDataFrame(as.list(out), .rownumbers=TRUE,
			.summary="Colocalized features:")
	})

