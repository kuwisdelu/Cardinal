
#### Model-based testing of sample means ####
## -------------------------------------------

setMethod("meansTest", "ANY",
	function(x, data, fixed, random, samples,
		response = "y", reduced = ~ 1, byrow = FALSE,
		nchunks = getCardinalNChunks(),
		verbose = getCardinalVerbose(),
		BPPARAM = getCardinalBPPARAM(), ...)
{
	if ( "groups" %in% ...names() ) {
		.Deprecated(old="groups", new="samples")
		samples <- list(...)$groups
	}
	samples <- as.factor(samples)
	if ( response %in% names(data) )
		warning("data already has column ", sQuote(response),
			" which will be ignored")
	if ( missing(fixed) ) {
		stop("missing fixed effects")
	} else {
		to <- as.formula(paste0(response, " ~ ."))
		fixed <- update(fixed, to)
	}
	if ( missing(random) )
		random <- NULL
	n <- if (byrow) nrow(x) else ncol(x)
	# summarize response
	if ( verbose )
		message("summarizing ", sQuote(response))
	if ( byrow ) {
		y <- rowStats(x, stat="mean", group=samples,
			nchunks=nchunks, verbose=verbose,
			BPPARAM=BPPARAM)
	} else {
		y <- colStats(x, stat="mean", group=samples,
			nchunks=nchunks, verbose=verbose,
			BPPARAM=BPPARAM)
	}
	if ( !is.matrix(y) )
		y <- t(y)
	# summarize data
	if ( verbose )
		message("preparing data")
	data <- lapply(data, function(v) 
		{
			unlist(unname(tapply(v, samples, avg, simplify=FALSE)))
		})
	# create data list
	vars <- union(all.vars(fixed), all.vars(random))
	datalist <- apply(y, 1L, function(yi)
		{
			data[[response]] <- yi
			as.data.frame(data[vars])
		})
	# fit models
	FIT <- .lm_fit_fun(fixed, random)
	if ( verbose ) {
		lab <- if (n != 1L) "models" else "model"
		message("fitting ", n, " ", lab)
	}
	models <- chunkLapply(datalist, FIT,
		nchunks=nchunks, verbose=verbose,
		BPPARAM=BPPARAM, ...)
	# test models
	TEST <- .lm_test_fun(reduced)
	if ( verbose ) {
		lab <- if (n != 1L) "models" else "model"
		message("testing ", n, " ", lab)
	}
	tests <- chunkMapply(TEST, models, datalist,
		nchunks=nchunks, verbose=verbose,
		BPPARAM=BPPARAM, ...)
	tests <- DataFrame(do.call(rbind, tests))
	# return results
	if ( is.null(random) ) {
		mcols <- DataFrame(
			fixed=deparse1(fixed), tests)
	} else {
		mcols <- DataFrame(
			fixed=deparse1(fixed),
			random=deparse1(random), tests)
	}
	as(ResultsList(models, mcols=mcols), "MeansTest")
})

.lm_fit_fun <- function(fixed, random)
{
	FIT <- function(data, ...)
	{
		model <- NULL
		if ( is.null(random) ) {
			try(fit <- lm(fixed, data=data, ...), silent=TRUE)
			try(model <- update(fit, . ~ ., data=data), silent=TRUE)
		} else {
			try(fit <- lme(fixed, data=data, random=random, ...), silent=TRUE)
			try(model <- update(fit, . ~ ., data=data, method="ML"), silent=TRUE)
		}
		if ( !is.null(model) )
		{
			model$model <- NULL
			model$data <- data
		}
		model
	}
	FIT
}

.lm_test_fun <- function(reduced)
{
	TEST <- function(model, data)
	{
		if ( is.null(model) ) {
			return(c(LR=NA, PValue=NA))
		} else {
			full <- model
		}
		if ( inherits(model, "lm") ) {
			null <- update(full, reduced, data=data)
			num <- as.numeric(logLik(null))
			den <- as.numeric(logLik(full))
			df <- abs(null$df.residual - full$df.residual)
			LR <- -2 * (num - den)
			PValue <- pchisq(LR, df, lower.tail=FALSE)
		} else if ( inherits(model, "lme") ) {
			null <- update(full, reduced, data=data, method="ML")
			aov <- anova(null, full)
			df <- abs(diff(aov[,"df"]))
			LR <- aov[2L,"L.Ratio"]
			PValue <- aov[2L,"p-value"]
		} else {
			stop("don't know how to test model of class ",
				sQuote(class(model)))
		}
		c(LR=LR, PValue=PValue)
	}
	TEST
}

setMethod("meansTest", "SpectralImagingExperiment",
	function(x, fixed, random, samples = run(x),
		response = "intensity", ...)
{
	ans <- meansTest(spectra(x, response), data=pixelData(x),
		fixed=fixed, random=random, samples=samples,
		response=response, byrow=TRUE, ...)
	featureData <- featureData(x)
	featureData$i <- seq_len(nrow(featureData))
	if ( is(featureData, "XDataFrame") ) {
		nms <- c(union(unlist(keys(featureData)), names(mcols(ans))))
	} else {
		nms <- names(mcols(ans))
	}
	mcols(ans) <- cbind(featureData, mcols(ans))[nms]
	ans
})

#### Model-based testing of class means ####
## -----------------------------------------

setMethod("meansTest", "SpatialDGMM",
	function(x, fixed, random, class = 1L,
		response = "intensity", reduced = ~ 1,
		nchunks = getCardinalNChunks(),
		verbose = getCardinalVerbose(),
		BPPARAM = getCardinalBPPARAM(), ...)
{
	data <- pixelData(x)
	samples <- as.factor(x$group)
	if ( response %in% names(data) )
		warning("data already has column ", sQuote(response),
			" which will be ignored")
	if ( missing(fixed) ) {
		stop("missing fixed effects")
	} else {
		to <- as.formula(paste0(response, " ~ ."))
		fixed <- update(fixed, to)
	}
	if ( missing(random) )
		random <- NULL
	n <- length(x$class)
	# summarize data
	if ( verbose )
		message("preparing data")
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
	# fit models
	FIT <- .lm_fit_fun(fixed, random)
	if ( verbose ) {
		lab <- if (n != 1L) "models" else "model"
		message("fitting ", n, " ", lab)
	}
	models <- chunkLapply(datalist, FIT,
		nchunks=nchunks, verbose=verbose,
		BPPARAM=BPPARAM, ...)
	# test models
	TEST <- .lm_test_fun(reduced)
	if ( verbose ) {
		lab <- if (n != 1L) "models" else "model"
		message("testing ", n, " ", lab)
	}
	tests <- chunkMapply(TEST, models, datalist,
		nchunks=nchunks, verbose=verbose,
		BPPARAM=BPPARAM, ...)
	tests <- DataFrame(do.call(rbind, tests))
	# return results
	featureData <- featureData(x)
	featureData$i <- seq_len(nrow(featureData))
	if ( is.null(random) ) {
		mcols <- DataFrame(
			fixed=deparse1(fixed), tests)
	} else {
		mcols <- DataFrame(
			fixed=deparse1(fixed),
			random=deparse1(random), tests)
	}
	if ( is(featureData, "XDataFrame") ) {
		nms <- c(union(unlist(keys(featureData)), names(mcols)))
	} else {
		nms <- names(mcols)
	}
	mcols <- cbind(featureData, mcols)[nms]
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
		stop("missing fixed effects")
	if ( !is(x, "SpatialDGMM") ) {
		if ( is(x, "SpectralImagingExperiment") ) {
			spectra(x) <- spectra(x, response)
		} else {
			stop("'x' must be a SpectralImagingExperiment")
		}
		x <- spatialDGMM(x, groups=samples, ...)
	}
	meansTest(x, fixed=fixed, random=random, class=class,
		response=response, reduced=reduced, ...)
}

