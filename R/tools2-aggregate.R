
## Summarize the pixels or features of an imaging dataset

setMethod("aggregate", "SparseImagingExperiment",
	function(x, by = c("feature", "pixel"), FUN,
		groups = NULL, tform = identity, as = "ImagingExperiment",
		BPPARAM = getCardinalBPPARAM(), ...)
	{
		.checkForIncompleteProcessing(x)
		by <- match.arg(by)
		if ( by == "feature" ) {
			len <- ncol(x)
			df <- fData(x)[,integer(),drop=FALSE]
		} else if ( by == "pixel" ) {
			len <- nrow(x)
			df <- pData(x)[,integer(),drop=FALSE]
		}
		if ( !is.null(groups) ) {
			groups <- rep_len(groups, len)
			groups <- as.factor(groups)
		}
		as <- match.arg(as, c("ImagingExperiment", "DataFrame"))
		groupnames <- levels(groups)
		ngroups <- nlevels(groups)
		statnames <- c(
			"prod",
			"mean", "sum",
			"sd", "var",
			"min", "max",
			"all", "any",
			"nnzero")
		streamstats <- is.character(FUN) && all(FUN %in% statnames)
		if ( streamstats ) {
			fnames <- names(FUN)
			if ( is.null(fnames) ) {
				fnames <- FUN
			} else {
				ind <- which(!nzchar(fnames))
				fnames[ind] <- FUN[ind]
			}
		} else if ( is.function(FUN) ) {
			fnames <- deparse(substitute(FUN))
			FUN <- list(FUN)
		} else {
			fnames <- names(FUN)
			if ( is.null(fnames) ) {
				fnames <- paste0("FUN.", seq_along(FUN))
			} else {
				ind <- which(!nzchar(fnames))
				fnames[ind] <- paste0("FUN.", ind)
			}
			FUN <- lapply(FUN, match.fun)
		}
		FUNLIST <- setNames(FUN, fnames)
		if ( is.null(groups) ) {
			cnames <- fnames
		} else {
			cnames <- paste0(rep(fnames, each=ngroups), ".",
				rep(unlist(groupnames), times=length(fnames)))
		}
		if ( streamstats ) {
			y <- .aggregate_stats(x, by=by, STATS=FUNLIST, ...,
				groups=groups, tform=tform, BPPARAM=BPPARAM)
		} else {
			y <- .aggregate_funs(x, by=by, FUNLIST=FUNLIST, ...,
				groups=groups, tform=tform, BPPARAM=BPPARAM)
		}
		y <- do.call(cbind, y)
		df[cnames] <- y
		if ( as == "ImagingExperiment" ) {
			if ( is.null(groups) ) {
				mcols <- data.frame(FUN=fnames)
			} else {
				mcols <- expand.grid(group=groupnames, FUN=fnames)
				mcols <- rev(mcols)
			}
			dimnames(y) <- NULL
			if ( by == "pixel" ) {
				fData <- DataFrame(mcols)
				iData <- ImageArrayList(t(y))
				names(iData) <- names(imageData(x))[1]
				ans <- .SparseImagingSummary(
					imageData=iData,
					featureData=fData,
					elementMetadata=df)
			} else if ( by == "feature" ) {
				pData <- PositionDataFrame(
					coord=expand.grid(x=1:nrow(mcols), y=1),
					run=factor(1), mcols)
				iData <- ImageArrayList(y)
				names(iData) <- names(imageData(x))[1]
				ans <- .SparseImagingSummary(
					imageData=iData,
					featureData=df,
					elementMetadata=pData)
			}
		} else {
			ans <- df
		}
		ans
	})

setMethod("aggregate", "MSImagingExperiment", function(x, ...) {
	ans <- callNextMethod()
	if ( is(ans, "ImagingExperiment") ) {
		if ( !is(featureData(ans), "MassDataFrame") )
			fData(ans) <- MassDataFrame(mz=1:nrow(ans), fData(ans))
		ans <- as(ans, "MSImagingSummary")
		centroided(ans) <- centroided(x)
	}
	ans
})

summarizePixels <- function(x, FUN = "mean", ...) {
	aggregate(x, by="pixel", FUN=FUN, ...)
}

summarizeFeatures <- function(x, FUN = "mean", ...) {
	aggregate(x, by="feature", FUN=FUN, ...)
}

.aggregate_funs <- function(object, by, FUNLIST, groups, tform, BPPARAM, ...) {
	fun <- function(x, ...) {
		lapply(FUNLIST, function(FUN) {
			if ( is.null(groups) ) {
				y <- FUN(tform(x), ...)
			} else {
				y <- .tapply(tform(x), groups, FUN, ...)
			}
			unlist(y)
		})
	}
	labels <- paste0("[", names(FUNLIST), "]")
	if ( by == "pixel" ) {
		.message("summarizing ", paste0(labels, collapse=" "), " by pixel ...")
		ans <- pixelApply(object, .fun=fun, ...,
			.simplify=FALSE, BPPARAM=BPPARAM)
	} else if ( by == "feature" ) {
		.message("summarizing ", paste0(labels, collapse=" "), " by feature ...")
		ans <- featureApply(object, .fun=fun, ...,
			.simplify=FALSE, BPPARAM=BPPARAM)
	}
	ans <- do.call(c, ans)
	ans <- lapply(names(FUNLIST), function(nm) {
		a <- do.call(rbind, ans[names(ans) %in% nm])
		rownames(a) <- NULL
		drop(a)
	})
	names(ans) <- names(FUNLIST)
	ans
}

.aggregate_stats <- function(object, by, STATS, groups, tform, BPPARAM) {
	labels <- paste0("[", names(STATS), "]")
	if ( by == "pixel" ) {
		.message("summarizing ", paste0(labels, collapse=" "), " by pixel ...")
		ans <- colStats(iData(object), stat=STATS, groups=groups,
			na.rm=TRUE, tform=tform, drop=FALSE,
			chunks=getCardinalNumBlocks(),
			verbose=getCardinalVerbose(),
			BPPARAM=BPPARAM)
	} else if ( by == "feature" ) {
		.message("summarizing ", paste0(labels, collapse=" "), " by feature ...")
		ans <- rowStats(iData(object), stat=STATS, groups=groups,
			na.rm=TRUE, tform=tform, drop=FALSE,
			chunks=getCardinalNumBlocks(),
			verbose=getCardinalVerbose(),
			BPPARAM=BPPARAM)
	}
	ans
}

