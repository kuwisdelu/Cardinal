
## Summarize a DataFrame

summarise.DataFrame <- function(.data, ...)
	{
		summarize(as(.data, "XDataFrame"), ...)
	}

summarise.XDataFrame <- function(.data, ...)
	{
		x <- summarize(.XDataFrame_to_tbl(.data), ...)
		x <- tryCatch(as(x, class(.data)),
			error=function(e) as(x, "XDataFrame"))
		x
	}

summarise.SummaryDataFrame <- function(.data, ...)
	{
		summarize(as.data.frame(.data), ...)
	}

## Summarize the pixels or features of an imaging dataset

summarise.SparseImagingExperiment <- function(.data, ...,
			.by = c("feature", "pixel"), .groups = NULL,
			.stat = "mean", .tform = identity,
			.as = "ImagingExperiment",
			BPPARAM = bpparam())
	{
		.checkForIncompleteProcessing(.data)
		.by <- match.arg(.by)
		if ( .by == "feature" ) {
			len <- ncol(.data)
			df <- fData(.data)[,integer(),drop=FALSE]
		} else if ( .by == "pixel" ) {
			len <- nrow(.data)
			df <- pData(.data)[,integer(),drop=FALSE]
		}
		dots <- match.call(expand.dots=FALSE)$...
		if ( !is.null(.groups) ) {
			.groups <- rep_len(.groups, len)
			.groups <- as.factor(.groups)
		}
		.as <- match.arg(.as, c("ImagingExperiment", "DataFrame"))
		.df <- switch(.as, ImagingExperiment=FALSE, DataFrame=TRUE)
		groupnames <- levels(.groups)
		ngroups <- nlevels(.groups)
		expr <- eval(substitute(alist(...)))
		attr(expr, "environment") <- new.env(parent=parent.frame(1))
		xnm <- character()
		if ( length(expr) > 0L ) {
			enm <- sapply(substitute(...()), deparse)
			if ( !is.null(names(expr)) ) {
				nz <- nzchar(names(expr))
				enm[nz] <- names(expr)[nz]
			}
			names(expr) <- enm
			if ( !is.null(.groups) ) {
				enm <- paste0(rep(enm, each=ngroups), ".",
					rep(unlist(groupnames), times=length(enm)))
			}
			ans.e <- .summarize_expr(.data, .by, .groups, expr, .tform, BPPARAM)
			xnm <- c(xnm, names(expr))
			if ( .df )
				df[,enm] <- ans.e
		} else {
			ans.e <- NULL
		}
		if ( !missing(.stat) || length(expr) == 0 ) {
			snm <- names(.stat)
			statnames <- c("min", "max", "mean", "sum", "sd", "var")
			.stat <- match.arg(.stat, statnames, several.ok=TRUE)
			if ( is.null(snm) ) {
				snm <- .stat
			} else {
				zch <- !nzchar(snm)
				snm[zch] <- .stat[zch]
			}
			names(.stat) <- snm
			if ( !is.null(.groups) ) {
				snm <- paste0(rep(snm, each=ngroups), ".",
					rep(unlist(groupnames), times=length(snm)))
			}
			ans.s <- .summarize_stat(.data, .by, .groups, .stat, .tform, BPPARAM)
			xnm <- c(xnm, names(.stat))
			if ( .df )
				df[,snm] <- ans.s
		} else {
			ans.s <- NULL
		}
		if ( .df ) {
			ans <- df
		} else {
			if ( is.null(.groups) ) {
				mcols <- data.frame(summary=xnm)
			} else {
				mcols <- expand.grid(group=groupnames, summary=xnm)
				mcols <- rev(mcols)
			}
			ans <- do.call(cbind, c(ans.e, ans.s))
			dimnames(ans) <- NULL
			if ( .by == "pixel" ) {
				fData <- DataFrame(mcols)
				iData <- ImageArrayList(t(ans))
				names(iData) <- names(imageData(.data))[1]
				ans <- .SparseImagingSummary(
					imageData=iData,
					featureData=fData,
					elementMetadata=pixelData(.data))
			} else if ( .by == "feature" ) {
				pData <- PositionDataFrame(
					coord=expand.grid(x=1:nrow(mcols), y=1),
					run=factor(1), mcols)
				iData <- ImageArrayList(ans)
				names(iData) <- names(imageData(.data))[1]
				ans <- .SparseImagingSummary(
					imageData=iData,
					featureData=featureData(.data),
					elementMetadata=pData)
			}
		}
		ans
	}

summarise.MSImagingExperiment <- function(.data, ...) {
	ans <- NextMethod()
	if ( is(ans, "ImagingExperiment") ) {
		if ( !is(featureData(ans), "MassDataFrame") )
			fData(ans) <- MassDataFrame(mz=1:nrow(ans), fData(ans))
		ans <- as(ans, "MSImagingSummary")
		centroided(ans) <- centroided(.data)
	}
	ans
}

.summarize_expr <- function(object, by, groups, expr, tform, BPPARAM) {
	fun <- function(x, ...) {
		env <- attr(expr, "environment")
		lapply(expr, function(e) {
			if ( is.null(groups) ) {
				y <- .do_dot_expr(tform(x), what=e, env=env)
			} else {
				y <- .tapply(tform(x), groups, .do_dot_expr, what=e, env=env)
			}
			unlist(y)
		})
	}
	labels <- paste0("[", names(expr), "]")
	if ( by == "pixel" ) {
		.message("summarizing ", paste0(labels, collapse=" "), " by pixel ...")
		ans <- pixelApply(object, .fun=fun,
			.simplify=FALSE, BPPARAM=BPPARAM)
	} else if ( by == "feature" ) {
		.message("summarizing ", paste0(labels, collapse=" "), " by feature ...")
		ans <- featureApply(object, .fun=fun,
			.simplify=FALSE, BPPARAM=BPPARAM)
	}
	ans <- do.call(c, ans)
	ans <- lapply(names(expr), function(nm) {
		a <- do.call(rbind, ans[names(ans) %in% nm])
		rownames(a) <- NULL
		drop(a)
	})
	names(ans) <- names(expr)
	ans
}

.summarize_stat <- function(object, by, groups, stat, tform, BPPARAM) {
	labels <- paste0("[", names(stat), "]")
	if ( by == "pixel" ) {
		.message("summarizing ", paste0(labels, collapse=" "), " by pixel ...")
		ans <- colStats(iData(object), stat=stat, groups=groups,
			na.rm=TRUE, tform=tform, drop=FALSE,
			chunks=getOption("Cardinal.numblocks"),
			verbose=getOption("Cardinal.progress"),
			BPPARAM=BPPARAM)
	} else if ( by == "feature" ) {
		.message("summarizing ", paste0(labels, collapse=" "), " by feature ...")
		ans <- rowStats(iData(object), stat=stat, groups=groups,
			na.rm=TRUE, tform=tform, drop=FALSE,
			chunks=getOption("Cardinal.numblocks"),
			verbose=getOption("Cardinal.progress"),
			BPPARAM=BPPARAM)
	}
	ans
}

