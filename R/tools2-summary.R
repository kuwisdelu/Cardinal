
# Summarize analysis results

setMethod("summary", "PCA2",
	function(object, ...)
	{
		SummaryDataFrame(
			Component=seq_len(modelData(object)$ncomp[1L]),
			`Standard deviation`=resultData(object, 1L, "sdev"),
			.summary="Principal components analysis:\n")
	})

setMethod("summary", "PLS2",
	function(object, ...)
	{
		if ( metadata(object)$type == "classification" ) {
			y <- pixelData(object)$..response..
			pos <- levels(as.factor(y))[1L]
			acc <- sapply(resultData(object), function(res)
				mean(res$class == y, na.rm=TRUE))
			sens <- sapply(resultData(object), function(res)
				sensitivity(y, res$class, positive=pos))
			spec <- sapply(resultData(object), function(res)
				specificity(y, res$class, positive=pos))
			description <- paste0(" Classification on ", nlevels(y), " classes: ")
			description <- paste0(description, paste0(levels(y), collapse=" "))
			out <- SummaryDataFrame(
				`Number of Components`=modelData(object)$ncomp,
				Accuracy=acc, Sensitivity=sens, Specificity=spec)
		} else {
			# do something
			nm <- grepl("..response..", names(pixelData(object)))
			y <- as.matrix(pixelData(object)[,nm,drop=FALSE])
			rmse <- sapply(resultData(object), function(res)
				sqrt(mean((res$fitted - y)^2)), na.rm=TRUE)
			mae <- sapply(resultData(object), function(res)
				mean(abs(res$fitted - y)), na.rm=TRUE)
			if ( ncol(y) > 1L ) {
				description <- paste0(" Regression with ", ncol(y), " response variables")
			} else {
				description <- paste0(" Regression with ", ncol(y), " response variable")
			}
			out <- SummaryDataFrame(
				`Number of Components`=modelData(object)$ncomp,
				RMSE=rmse, MAE=mae)
		}
		method <- paste0(" Method = ", metadata(object)$method, "\n")
		out@summary <-list("Projection to latent components:\n",
			description, method)
		out
	})

setMethod("summary", "SpatialFastmap2",
	function(object, ...)
	{
		r <- modelData(object)$r
		ncomp <- modelData(object)$ncomp
		out <- mapply(function(res, ri, nc) {
			DataFrame(`Radius (r)`=ri, Component=1:nc,
				`Pivot 1`=res$pivots[,1],
				`Pivot 2`=res$pivots[,2],
				`Standard deviation`=res$sdev,
				check.names=FALSE)
		}, resultData(object), r, ncomp, SIMPLIFY=FALSE)
		out <- do.call("rbind", out)
		SummaryDataFrame(out,
			.summary=list("Spatially-aware FastMap projection:\n",
				paste0(" Method = ", metadata(object)$method),
				paste0(" Distance = ", metadata(object)$dist, "\n")))
	})

setMethod("summary", "SpatialKMeans2",
	function(object, ...)
	{
		SummaryDataFrame(
			`Radius (r)`=modelData(object)$r,
			`Num Clusters (k)`=modelData(object)$k,
			.summary=list("Spatially-aware K-means clustering:\n",
				paste0(" Method = ", metadata(object)$method),
				paste0(" Distance = ", metadata(object)$dist, "\n")))
	})

setMethod("summary", "SpatialShrunkenCentroids2",
	function(object, ...)
	{
		y <- pixelData(object)$..response..
		num_features <- sapply(resultData(object),
			function(res) mean(colSums(res$statistic != 0)))
		num_features <- round(num_features, digits=2)
		if ( is.null(y) ) {
			description <- " Segmentation / clustering"
			num_segments <- sapply(resultData(object),
				function(res) nlevels(res$class))
			SummaryDataFrame(
				`Radius (r)`=modelData(object)$r,
				`Init (k)`=modelData(object)$k,
				`Lambda (s)`=modelData(object)$s,
				`Num Classes`=num_segments,
				`Num Features / Class`=num_features,
				.summary=list("Spatially-aware nearest shrunken centroids:\n",
					description, paste0(" Method = ", metadata(object)$method),
					paste0(" Distance = ", metadata(object)$dist, "\n")))
		} else {
			description <- paste0(" Classification on ", nlevels(y), " classes: ")
			description <- paste0(description, paste0(levels(y), collapse=" "))
			pos <- levels(as.factor(y))[1L]
			acc <- sapply(resultData(object), function(res)
				mean(res$class == y, na.rm=TRUE))
			sens <- sapply(resultData(object), function(res)
				sensitivity(y, res$class, positive=pos))
			spec <- sapply(resultData(object), function(res)
				specificity(y, res$class, positive=pos))
			SummaryDataFrame(
				`Radius (r)`=modelData(object)$r,
				`Lambda (s)`=modelData(object)$s,
				`Num Features / Class`=num_features,
				Accuracy=acc, Sensitivity=sens, Specificity=spec,
				.summary=list("Spatially-aware nearest shrunken centroids:\n",
					description, paste0(" Method = ", metadata(object)$method),
					paste0(" Distance = ", metadata(object)$dist, "\n")))
		}
	})

setMethod("summary", "SpatialDGMM",
	function(object, ...)
	{
		groups <- pixelData(object)$..group..
		num_segments <- sapply(resultData(object), function(res)
				nlevels(res$class) / nlevels(res$estimates$group))
		num_segments <- round(num_segments, digits=2)
		if ( nlevels(groups) > 1L ) {
			description <- paste0(" Segmentation on ",
				.spaste("%d groups: %s", levels(groups)))
		} else {
			description <- paste0(" Segmentation on ",
				.spaste("%d group: %s", levels(groups)))
		}
		SummaryDataFrame(
			`Radius (r)`=modelData(object)$r,
			`Init (k)`=modelData(object)$k,
			`Feature`=modelData(object)$feature,
			`Num Classes / Group`=num_segments,
			.summary=list("Spatially-aware Dirichlet Gaussian mixture models:\n",
				description, paste0(" Method = ", metadata(object)$method),
				paste0(" Distance = ", metadata(object)$dist, "\n")))
	})

setMethod("summary", "MeansTest",
	function(object, ...)
	{
		groups <- pixelData(object)$..group..
		lrt <- .meansTest_LRT(object)
		if ( nlevels(groups) > 1L ) {
			description <- paste0("\n Summarized ",
				.spaste("%d groups: %s", levels(groups)))
		} else {
			description <- paste0("\n Summarized ",
				.spaste("%d group: %s", levels(groups)))
		}
		fixed <- metadata(object)$fixed
		random <- metadata(object)$random
		fixed[[2]] <- NULL
		fixed <- paste0(" Fixed effects: ", deparse(fixed))
		if ( !is.null(random) )
			random <- paste0(" Random effects: ", deparse(random))
		test <- paste0("\n Likelihood ratio test for fixed effects:\n")
		SummaryDataFrame(
			`Feature`=modelData(object)$feature,
			LR=round(lrt$LR, digits=4), PValue=lrt$PValue,
			FDR=p.adjust(lrt$PValue, method="BH"),
			.summary=list("Means-summarized linear model testing:\n",
				fixed, random, description, test))
	})

setMethod("summary", "SegmentationTest",
	function(object, ...)
	{
		groups <- pixelData(object)$..group..
		lrt <- .meansTest_LRT(object)
		if ( nlevels(groups) > 1L ) {
			description <- paste0("\n Summarized ",
				.spaste("%d groups: %s", levels(groups)))
		} else {
			description <- paste0("\n Summarized ",
				.spaste("%d group: %s", levels(groups)))
		}
		fixed <- metadata(object)$fixed
		random <- metadata(object)$random
		fixed[[2]] <- NULL
		fixed <- paste0(" Fixed effects: ", deparse(fixed))
		if ( !is.null(random) )
			random <- paste0(" Random effects: ", deparse(random))
		test <- paste0("\n Likelihood ratio test for fixed effects:\n")
		SummaryDataFrame(
			`Radius (r)`=modelData(object)$r,
			`Init (k)`=modelData(object)$k,
			`Feature`=modelData(object)$feature,
			LR=round(lrt$LR, digits=4), PValue=lrt$PValue,
			FDR=p.adjust(lrt$PValue, method="BH"),
			.summary=list("Segmentation-based linear model testing:\n",
				fixed, random, description, test))
	})

