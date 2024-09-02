
#### Model-based testing of sample means ####
## -------------------------------------------

setMethod("meansTest", "ANY",
	function(x, data, fixed, random, samples,
		response = "y", reduced = ~ 1, byrow = FALSE,
		verbose = getCardinalVerbose(), chunkopts = list(),
		BPPARAM = getCardinalBPPARAM(), ...)
{
	if ( "groups" %in% ...names() ) {
		.Deprecated(old="groups", new="samples")
		samples <- list(...)$groups
	}
	samples <- as.factor(samples)
	if ( response %in% names(data) )
		.Warn("data already has column ", sQuote(response),
			" which will be ignored")
	if ( missing(fixed) ) {
		.Error("missing fixed effects")
	} else {
		to <- as.formula(paste0(response, " ~ ."))
		fixed <- update(fixed, to)
	}
	if ( missing(random) )
		random <- NULL
	n <- if (byrow) nrow(x) else ncol(x)
	# summarize response
	.Log("summarizing ", sQuote(response),
		message=verbose)
	if ( byrow ) {
		y <- rowStats(x, stat="mean", group=samples,
			verbose=verbose, chunkopts=chunkopts,
			BPPARAM=BPPARAM)
	} else {
		y <- colStats(x, stat="mean", group=samples,
			verbose=verbose, chunkopts=chunkopts,
			BPPARAM=BPPARAM)
	}
	if ( !is.matrix(y) )
		y <- t(y)
	# summarize data
	.Log("preparing data",
		message=verbose)
	data <- lapply(data, function(v) 
		{
			unlist(unname(tapply(v, samples, avg, simplify=FALSE)))
		})
	# create data list
	vars <- union(all.vars(fixed), all.vars(random))
	datalist <- apply(y, 1L, function(yi)
		{
			data[[response]] <- yi
			ok <- vars %in% names(data)
			if ( !all(ok) ) {
				labs <- paste0(vars[!ok], collapse=", ")
				.Error("couldn't find variable: ", labs)
			}
			as.data.frame(data[vars])
		})
	label <- if (n != 1L) "models" else "model"
	# fit models
	.Log("fitting ", n, " ", label,
		message=verbose)
	FIT <- .lmFit_fun(fixed, random)
	models <- chunkLapply(datalist, FIT,
		verbose=verbose, chunkopts=chunkopts,
		BPPARAM=BPPARAM, ...)
	names(models) <- if (byrow) rownames(x) else colnames(x)
	# test models
	.Log("testing ", n, " ", label,
		message=verbose)
	TEST <- .lmTest_fun(reduced, random)
	tests <- chunkMapply(TEST, models, datalist,
		verbose=verbose, chunkopts=chunkopts,
		BPPARAM=BPPARAM, ...)
	tests <- DataFrame(do.call(rbind, tests))
	# return results
	if ( anyNA(tests$statistic) )
		.Warn(sum(is.na(tests$statistic)), " tests could not be performed")
	if ( is.null(random) ) {
		mcols <- DataFrame(fixed=deparse1(fixed), tests)
	} else {
		mcols <- DataFrame(fixed=deparse1(fixed),
			random=deparse1(random), tests)
	}
	as(ResultsList(models, mcols=mcols), "MeansTest")
})

.lmFit_fun <- function(fixed, random)
{
	FIT <- isoclos(function(data, ...)
	{
		model <- NULL
		if ( is.null(random) ) {
			model <- try(lm(fixed, data=data, ...), silent=TRUE)
		} else {
			model <- try(lme(fixed, data=data,
				random=random, method="ML", ...), silent=TRUE)
		}
		if ( !inherits(model, "try-error") )
		{
			model <- update(model, . ~ .)
			model$data <- data
		}
		model
	}, CardinalEnv())
	FIT
}

.lmTest_fun <- function(reduced, random)
{
	TEST <- isoclos(function(model, data)
	{
		if ( inherits(model, "try-error") ) {
			return(c(statistic=NA, pvalue=NA))
		} else {
			full <- model
		}
		if ( inherits(model, "lm") ) {
			null <- update(full, reduced)
			num <- as.numeric(logLik(null))
			den <- as.numeric(logLik(full))
			df <- abs(null$df.residual - full$df.residual)
			LR <- -2 * (num - den)
			PValue <- pchisq(LR, df, lower.tail=FALSE)
		} else if ( inherits(model, "lme") ) {
			null <- update(full, reduced)
			aov <- anova(null, full)
			df <- abs(diff(aov[,"df"]))
			LR <- aov[2L,"L.Ratio"]
			PValue <- aov[2L,"p-value"]
		} else {
			.Error("don't know how to test model of class ",
				sQuote(class(model)))
		}
		c(statistic=LR, pvalue=PValue)
	}, CardinalEnv())
	TEST
}

setMethod("meansTest", "SpectralImagingExperiment",
	function(x, fixed, random, samples = run(x),
		response = "intensity", ...)
{
	ans <- meansTest(spectra(x, response), data=pixelData(x),
		fixed=fixed, random=random, samples=samples,
		response=response, byrow=TRUE, ...)
	names(ans) <- featureNames(x)
	featureData <- featureData(x)
	featureData$i <- seq_len(nrow(featureData))
	if ( is(featureData, "XDataFrame") ) {
		keep <- c("i", unlist(keys(featureData)))
	} else {
		keep <- "i"
	}
	mcols(ans) <- cbind(featureData[keep], mcols(ans))
	ans
})

setMethod("topFeatures", "MeansTest",
	function(object, n = Inf, sort.by = "statistic", ...)
{
	sort.by <- match.arg(sort.by)
	topf <- mcols(object)
	if ( "fixed" %in% names(topf) )
		topf$fixed <- NULL
	if ( "random" %in% names(topf) )
		topf$random <- NULL
	topf$fdr <- p.adjust(topf$pvalue, method="fdr")
	i <- order(topf[[sort.by]], decreasing=TRUE)
	topf <- topf[i,,drop=FALSE]
	head(topf, n=n)
})

setMethod("plot", c(x = "MeansTest", y = "missing"),
	function(x, i = 1L, type = "boxplot", show.obs = TRUE,
		fill = FALSE, layout = NULL, ...)
{
	type <- match.arg(type)
	plots <- lapply(x[i], .plot_boxplots,
		show.obs=show.obs, fill=fill, ...)
	if ( is.null(rownames(mcols(x))) ) {
		if ( is.null(names(i)) ) {
			names(plots) <- paste0("i = ", i)
		} else {
			names(plots) <- names(i)
		}
	} else {
		names(plots) <- rownames(mcols(x))[i]
	}
	if ( !is.null(layout) ) {
		layout <- rep_len(layout, 2L)
		nrow <- layout[1L]
		ncol <- layout[2L]
		as_facets(plots, nrow=nrow, ncol=ncol)
	} else {
		as_facets(plots)
	}
})

.plot_boxplots <- function(model, select = 1L,
	xlab = NULL, ylab = NULL, col = NULL, fill = FALSE,
	xlim = NULL, ylim = NULL, key = TRUE, grid = TRUE,
	show.obs = TRUE, engine = NULL, ...)
{
	data <- model$data
	if ( is.numeric(select) )
		select <- select + 1L
	if ( is.null(xlab) )
		xlab <- names(data)[select]
	if ( is.null(ylab) )
		ylab <- names(data)[1L]
	if ( fill ) {
		plot <- vizi(x=data[[select]], y=data[[1L]], fill=data[[select]])
	} else {
		plot <- vizi(x=data[[select]], y=data[[1L]], color=data[[select]])
	}
	plot <- add_mark(plot, "boxplot")
	if ( show.obs )
		plot <- add_mark(plot, "points")
	plot <- set_coord(plot, xlim=xlim, ylim=ylim, grid=grid)
	plot <- set_channel(plot, "x", label=xlab)
	plot <- set_channel(plot, "y", label=ylab)
	if ( fill ) {
		plot <- set_channel(plot, "fill", label="\n", scheme=col, key=key)
	} else {
		plot <- set_channel(plot, "color", label="\n", scheme=col, key=key)
	}
	if ( !is.null(engine) )
		plot <- set_engine(plot, engine)
	plot <- set_par(plot, ...)
	plot
}


#### Model-based testing of class means ####
## -----------------------------------------

setMethod("meansTest", "SpatialDGMM",
	function(x, fixed, random, class = 1L,
		response = "intensity", reduced = ~ 1,
		verbose = getCardinalVerbose(), chunkopts = list(),
		BPPARAM = getCardinalBPPARAM(), ...)
{
	data <- pixelData(x)
	samples <- as.factor(x$group)
	if ( response %in% names(data) )
		.Warn("data already has column ", sQuote(response),
			" which will be ignored")
	if ( missing(fixed) ) {
		.Error("missing fixed effects")
	} else {
		to <- as.formula(paste0(response, " ~ ."))
		fixed <- update(fixed, to)
	}
	if ( missing(random) )
		random <- NULL
	n <- length(x$class)
	# summarize data
	.Log("preparing data",
		message=verbose)
	data <- lapply(data, function(v) 
		{
			unlist(unname(tapply(v, samples, avg, simplify=FALSE)))
		})
	# create data list
	vars <- union(all.vars(fixed), all.vars(random))
	datalist <- apply(x$mu, 3L, function(yi)
		{
			data[[response]] <- yi[,class,drop=TRUE]
			as.data.frame(data[vars])
		})
	label <- if (n != 1L) "models" else "model"
	# fit models
	.Log("fitting ", n, " ", label,
		message=verbose)
	FIT <- .lmFit_fun(fixed, random)
	models <- chunkLapply(datalist, FIT,
		verbose=verbose, chunkopts=chunkopts,
		BPPARAM=BPPARAM, ...)
	names(models) <- rownames(featureData(x))
	# test models
	.Log("testing ", n, " ", label,
		message=verbose)
	TEST <- .lmTest_fun(reduced, random)
	tests <- chunkMapply(TEST, models, datalist,
		verbose=verbose, chunkopts=chunkopts,
		BPPARAM=BPPARAM, ...)
	tests <- DataFrame(do.call(rbind, tests))
	# return results
	if ( is.null(random) ) {
		mcols <- DataFrame(fixed=deparse1(fixed), tests)
	} else {
		mcols <- DataFrame(fixed=deparse1(fixed),
			random=deparse1(random), tests)
	}
	featureData <- featureData(x)
	featureData$i <- seq_len(nrow(featureData))
	if ( is(featureData, "XDataFrame") ) {
		keep <- c("i", unlist(keys(featureData)))
	} else {
		keep <- "i"
	}
	mcols <- cbind(featureData[keep], mcols)
	as(ResultsList(models, mcols=mcols), "MeansTest")
})

segmentationTest <- function(x, fixed, random, samples = run(x),
	class = 1L, response = "intensity", reduced = ~ 1, ...)
{
	if ( "groups" %in% ...names() ) {
		.Deprecated(old="groups", new="samples")
		samples <- list(...)$groups
	}
	if ( missing(fixed) )
		.Error("missing fixed effects")
	if ( !is(x, "SpatialDGMM") ) {
		if ( is(x, "SpectralImagingExperiment") ) {
			spectra(x) <- spectra(x, response)
		} else {
			.Error("'x' must be a SpectralImagingExperiment")
		}
		x <- spatialDGMM(x, groups=samples, ...)
	}
	meansTest(x, fixed=fixed, random=random, class=class,
		response=response, reduced=reduced, ...)
}

