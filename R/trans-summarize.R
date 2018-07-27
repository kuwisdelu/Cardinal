
## Summarize the pixels or features of an imaging dataset

setMethod("summarize", "SparseImagingExperiment",
	function(.data, ..., .by = c("feature", "pixel"),
			.stat = c("min", "max", "mean", "sum", "sd", "var"),
			.tform = identity,
			BPPARAM = bpparam())
	{
		.checkForIncompleteProcessing(.data)
		.by <- match.arg(.by)
		if ( .by == "feature" ) {
			ans <- fData(.data)[,integer(),drop=FALSE]
		} else if ( .by == "pixel" ) {
			ans <- pData(.data)[,integer(),drop=FALSE]
		}
		expr <- list(...)
		if ( length(expr) > 0L ) {
			ecolnames <- sapply(substitute(...()), deparse)
			if ( !is.null(names(expr)) ) {
				nz <- nzchar(names(expr))
				ecolnames[nz] <- names(expr)[nz]
			}
			names(expr) <- ecolnames
			expr <- lapply(expr, match.fun)
			ecols <- .summarize_expr(.data, .by, expr, .tform, BPPARAM)
			ans[,ecolnames] <- ecols
		}
		if ( !missing(.stat) ) {
			scolnames <- names(.stat)
			.stat <- match.arg(.stat, several.ok=TRUE)
			if ( is.null(scolnames) ) {
				scolnames <- .stat
			} else {
				zch <- !nzchar(scolnames)
				scolnames[zch] <- .stat[zch]
			}
			names(.stat) <- scolnames
			scols <- .summarize_stat(.data, .by, .stat, .tform, BPPARAM)
			ans[,scolnames] <- scols
		}
		ans
	})

.summarize_expr <- function(object, by, expr, tform, BPPARAM) {
	margin <- switch(by,
		feature = 1,
		pixel = 2)
	len <- switch(by,
		feature = nrow(object),
		pixel = ncol(object))
	fun <- function(x) lapply(expr, function(f) f(tform(x)))
	labels <- selectSome(paste0("[", names(expr), "]"))
	if ( by == "pixel" ) {
		.message("summarizing ", paste0(labels, collapse=" "), " by pixel ...")
		ans <- pixelApply(object, .fun=fun,
			.simplify=FALSE, BPPARAM=BPPARAM)
	} else if ( by == "feature" ) {
		.message("summarizing ", paste0(labels, collapse=" "), " by feature ...")
		ans <- featureApply(object, .fun=fun,
			.simplify=FALSE, BPPARAM=BPPARAM)
	}
	.bind_results(ans)
}

.summarize_stat <- function(object, by, stat, tform, BPPARAM) {
	margin <- switch(by,
		feature = 1,
		pixel = 2)
	n <- switch(by,
		feature = ncol(object),
		pixel = nrow(object))
	if ( n == 1L )
		.stop("can't summarize data when n = 1")
	colclasses <- c("matter_matc", "sparse_matc")
	rowclasses <- c("matter_matr", "sparse_matr")
	bias <- (n / (n - 1))
	fun <- function(x) {
		apply(tform(x), margin, function(xi)
			c(min=min(xi), max=max(xi),
				mean=mean(xi), sum=sum(xi),
				sd=sd(xi), var=var(xi)))
	}
	simplify <- function(ans) {
		ans <- do.call(cbind, ans)
		as.data.frame(t(ans))
	}
	tfun <- function(x) {
		apply(tform(x), margin, function(xi)
			c(min=min(xi), max=max(xi),
				sx=sum(xi), sx2=sum(xi^2)))
	}
	tsimplify <- function(ans) {
		dim <- c(dim(ans[[1]]), length(ans))
		ans <- array(unlist(ans), dim=dim)
		sx <- rowSums(ans[3,,])
		sx2 <- rowSums(ans[4,,])
		var <- ((sx2 / n) - (sx / n)^2)
		data.frame(
			min=apply(ans[1,,], 1, min),
			max=apply(ans[2,,], 1, max),
			mean=sx / n, sum=sx,
			sd=sqrt(bias) * sqrt(var),
			var=bias * var)
	}
	labels <- selectSome(paste0("[", names(stat), "]"))
	if ( by == "pixel" ) {
		.message("summarizing ", paste0(labels, collapse=" "), " by pixel ...")
		if ( inherits(iData(object), rowclasses) ) {
			ans <- featureApply(object, .fun=tfun, .blocks=TRUE,
				.simplify=tsimplify, BPPARAM=BPPARAM)
		} else {
			ans <- pixelApply(object, .fun=fun, .blocks=TRUE,
				.simplify=simplify, BPPARAM=BPPARAM)
		}
	} else if ( by == "feature" ) {
		.message("summarizing ", paste0(labels, collapse=" "), " by feature ...")
		if ( inherits(iData(object), colclasses) ) {
			ans <- pixelApply(object, .fun=tfun, .blocks=TRUE,
				.simplify=tsimplify, BPPARAM=BPPARAM)
		} else {
			ans <- featureApply(object, .fun=fun, .blocks=TRUE,
				.simplify=simplify, BPPARAM=BPPARAM)
		}
	}
	ans <- ans[,stat,drop=FALSE]
	names(ans) <- names(stat)
	ans
}

