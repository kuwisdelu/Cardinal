
## Summarize the pixels or features of an imaging dataset

setMethod("summarize", "SparseImagingExperiment",
	function(.data, ...,
			.by = c("feature", "pixel"), .group_by,
			.stat = c("min", "max", "mean",
				"sum", "sd", "var"),
			.tform = identity,
			BPPARAM = bpparam())
	{
		.checkForIncompleteProcessing(.data)
		.by <- match.arg(.by)
		if ( .by == "feature" ) {
			len <- ncol(.data)
			ans <- fData(.data)[,integer(),drop=FALSE]
			if ( missing(.group_by) ) {
				groups <- NULL
			} else {
				groups <- .group_by
			}
		} else if ( .by == "pixel" ) {
			len <- nrow(.data)
			ans <- pData(.data)[,integer(),drop=FALSE]
			if ( missing(.group_by) ) {
				groups <- NULL
			} else {
				groups <- .group_by
			}
		}
		if ( !is.null(groups) ) {
			groups <- rep_len(groups, len)
			groups <- as.factor(groups)
		}
		groupnames <- levels(groups)
		ngroups <- nlevels(groups)
		expr <- eval(substitute(alist(...)))
		attr(expr, "environment") <- new.env(parent=parent.frame(2))
		if ( length(expr) > 0L ) {
			ecolnames <- sapply(substitute(...()), deparse)
			if ( !is.null(names(expr)) ) {
				nz <- nzchar(names(expr))
				ecolnames[nz] <- names(expr)[nz]
			}
			names(expr) <- ecolnames
			if ( !is.null(groups) ) {
				ecolnames <- paste0(rep(ecolnames, each=ngroups), ".",
					rep(unlist(groupnames), times=length(ecolnames)))
			}
			ecols <- .summarize_expr(.data, .by, groups, expr, .tform, BPPARAM)
			ans[,ecolnames] <- ecols
		}
		if ( !missing(.stat) || length(expr) == 0 ) {
			scolnames <- names(.stat)
			.stat <- match.arg(.stat, several.ok=TRUE)
			if ( is.null(scolnames) ) {
				scolnames <- .stat
			} else {
				zch <- !nzchar(scolnames)
				scolnames[zch] <- .stat[zch]
			}
			names(.stat) <- scolnames
			if ( !is.null(groups) ) {
				scolnames <- paste0(rep(scolnames, each=ngroups), ".",
					rep(unlist(groupnames), times=length(scolnames)))
			}
			scols <- .summarize_stat(.data, .by, groups, .stat, .tform, BPPARAM)
			ans[,scolnames] <- scols
		}
		ans
	})

.summarize_expr <- function(object, by, groups, expr, tform, BPPARAM) {
	margin <- switch(by,
		feature = 1,
		pixel = 2)
	len <- switch(by,
		feature = nrow(object),
		pixel = ncol(object))
	fun <- function(x, ...) {
		env <- attr(expr, "environment")
		s <- lapply(expr, function(e) {
			if ( is.null(groups) ) {
				y <- .do_dot_expr(tform(x), what=e, env=env)
			} else {
				y <- .tapply(tform(x), groups, .do_dot_expr, what=e, env=env)
			}
			y
		})
		if ( !is.null(groups) )
			s <- unlist(s, recursive=FALSE)
		s
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
	.bind_as_df(ans)
}

.summarize_stat <- function(object, by, groups, stat, tform, BPPARAM) {
	margin <- switch(by,
		feature = 1,
		pixel = 2)
	if ( is.null(groups) ) {
		n <- switch(by,
			feature = ncol(object),
			pixel = nrow(object))
	} else {
		n <- as.integer(table(groups))
	}
	colclasses <- c("matter_matc", "sparse_matc")
	rowclasses <- c("matter_matr", "sparse_matr")
	bias <- (n / (n - 1))
	# summarize while iterating over major dimension
	fun <- function(x, ...) {
		ans <- .apply(tform(x), margin, function(xi) {
			if ( is.null(groups) ) {
				vapply(stat, function(f) {
					f <- match.fun(f)
					f(xi, na.rm=TRUE)
				}, numeric(1))
			} else {
				unlist(lapply(stat, function(f) {
					f <- match.fun(f)
					.tapply(xi, groups, f, na.rm=TRUE)
				}))
			}
		})
		do.call("rbind", ans)
	}
	simplify <- function(ans) {
		ans <- do.call("rbind", ans)
		as.data.frame(ans)
	}
	# summarize while iterating over minor dimension
	tfun <- function(x, ...) {
		i <- attr(x, "idx")
		ans <- .apply(tform(x), margin, function(xi) {
			s <- list()
			if ( is.null(groups) ) {
				if ( "min" %in% stat )
					s$min <- min(xi, na.rm=TRUE)
				if ( "max" %in% stat )
					s$max <- max(xi, na.rm=TRUE)
				if ( any(c("mean", "sum", "sd", "var") %in% stat) )
					s$sx1 <- sum(xi, na.rm=TRUE)
				if ( any(c("sd", "var") %in% stat) )
					s$sx2 <- sum(xi^2, na.rm=TRUE)
			} else {
				if ( "min" %in% stat )
					s$min <- .tapply(xi, groups[i], min, na.rm=TRUE)
				if ( "max" %in% stat )
					s$max <- .tapply(xi, groups[i], max, na.rm=TRUE)
				if ( any(c("mean", "sum", "sd", "var") %in% stat) )
					s$sx1 <- .tapply(xi, groups[i], sum, na.rm=TRUE)
				if ( any(c("sd", "var") %in% stat) )
					s$sx2 <- .tapply(xi^2, groups[i], sum, na.rm=TRUE)
			}
			unlist(s)
		})
		do.call("cbind", ans)
	}
	tsimplify <- function(ans) {
		nm <- rownames(ans[[1]])
		dim <- c(dim(ans[[1]]), length(ans))
		ans <- array(unlist(ans), dim=dim)
		ret <- list()
		# use order of c(min, max, mean, sum, sd, var) :
		if ( any(dm_min <- grepl("min", nm)) )
			ret$min <- apply(ans[dm_min,,,drop=FALSE], c(1, 2), min, na.rm=TRUE)
		if ( any(dm_max <- grepl("max", nm)) )
			ret$max <- apply(ans[dm_max,,,drop=FALSE], c(1, 2), max, na.rm=TRUE)
		if ( any(dm_sx1 <- grepl("sx1", nm)) ) {
			sx1 <- apply(ans[dm_sx1,,,drop=FALSE], c(1,2), sum, na.rm=TRUE)
			if ( "mean" %in% stat )
				ret$mean <- sx1 / n
			if ( "sum" %in% stat )
				ret$sum <- sx1
		}
		if ( any(dm_sx2 <- grepl("sx2", nm)) ) {
			sx2 <- apply(ans[dm_sx2,,,drop=FALSE], c(1,2), sum, na.rm=TRUE)
			var <- (sx2 / n) - (sx1 / n)^2
			if ( "sd" %in% stat )
				ret$sd <- sqrt(bias) * sqrt(var)
			if ( "var" %in% stat )
				ret$var <- bias * var
		}
		ret <- ret[stat]
		ret <- do.call("rbind", ret)
		as.data.frame(t(ret))
	}
	# summarize
	labels <- paste0("[", names(stat), "]")
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
	ans
}

