
# Extract top-ranked features

setMethod("topFeatures", "SpatialShrunkenCentroids2",
	function(object, n = 10, ..., model = modelData(object))
	{
		r <- modelData(object)$r
		k <- modelData(object)$k
		s <- modelData(object)$s
		out <- mapply(function(ri, ki, si, res) {
			class <- res$class
			class <- rep(levels(class), each=nrow(object))
			if ( is.null(ki) ) {
				out1 <- DataFrame(as.list(fData(object)), r=ri, s=si)
			} else {
				out1 <- DataFrame(as.list(fData(object)), r=ri, k=ki, s=si)
			}
			out2 <- DataFrame(class=class,
				centers=as.vector(res$centers),
				statistic=as.vector(res$centers))
			cbind(out1, out2)
		}, r, k, s, resultData(object), SIMPLIFY=FALSE)
		if ( !is.null(names(model)) ) {
			model <- model[names(model) %in% names(modelData(object))]
			model <- subset_rows(modelData(object), as.list(model))
		}
		if ( length(model) > 1 )
			.warning("more than 1 model selected")
		out <- do.call("rbind", out[model])
		if ( !is.null(match.call(expand.dots=FALSE)$...) )
			out <- Cardinal::filter(out, ...)
		order <- order(out$statistic, decreasing=TRUE)
		if ( is.finite(n) )
			order <- head(order, n=n)
		out <- out[order,,drop=FALSE]
		SummaryDataFrame(as.list(out),
			.summary="Top-ranked features:")
	})

setMethod("topFeatures", "MeansTest",
	function(object, n = 10, ..., p.adjust = "BH")
	{
		tests <- summary(object)
		fData <- as.data.frame(fData(object))
		out <- cbind(fData, modelData(object))
		out$LR <- tests$LR
		out$PValue <- tests$PValue
		out$AdjP <- p.adjust(out$PValue, method=p.adjust)
		fixed <- metadata(object)$fixed
		fixed[[2]] <- NULL
		contrast <- paste(deparse(fixed), "vs ~1")
		if ( !is.null(match.call(expand.dots=FALSE)$...) )
			out <- Cardinal::filter(out, ...)
		order <- order(out$AdjP)
		if ( is.finite(n) )
			order <- head(order, n=n)
		out <- out[order,,drop=FALSE]
		SummaryDataFrame(as.list(out),
			.summary=paste0("Top-ranked tests: ", contrast))
	})

setMethod("topFeatures", "SegmentationTest",
	function(object, n = 10, ..., model=modelData(object), p.adjust = "BH")
	{
		tests <- summary(object)
		fData <- as.data.frame(fData(object))
		out <- cbind(fData, modelData(object))
		out$LR <- tests$LR
		out$PValue <- tests$PValue
		fixed <- metadata(object)$fixed
		fixed[[2]] <- NULL
		contrast <- paste(deparse(fixed), "vs ~1")
		if ( !is.null(names(model)) ) {
			model <- model[names(model) %in% names(modelData(object))]
			i <- subset_rows(out, as.list(model))
			out <- out[i,,drop=FALSE]
		}
		out$AdjP <- p.adjust(out$PValue, method=p.adjust)
		if ( length(unique(out$r)) > 1 || length(unique(out$k)) > 1 )
			.warning("more than 1 model per feature selected")
		if ( !is.null(match.call(expand.dots=FALSE)$...) )
			out <- Cardinal::filter(out, ...)
		order <- order(out$AdjP)
		if ( is.finite(n) )
			order <- head(order, n=n)
		out <- out[order,,drop=FALSE]
		SummaryDataFrame(as.list(out),
			.summary=paste0("Top-ranked tests: ", contrast))
	})


