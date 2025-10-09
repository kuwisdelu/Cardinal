
#### Model-based testing of sample means ####
## -------------------------------------------

setMethod("meansTest", "ANY",
	function(x, data, fixed, random, samples,
		response = "y", reduced = ~ 1, byrow = FALSE,
		use_lmer = FALSE,
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
	FIT <- .lmFit_fun(fixed, random, use_lmer)
	models <- chunkLapply(datalist, FIT,
		verbose=verbose, chunkopts=chunkopts,
		BPPARAM=BPPARAM, ...)
	names(models) <- if (byrow) rownames(x) else colnames(x)
	# test models (skip for lmer models)
	if ( !use_lmer ) {
		.Log("testing ", n, " ", label,
			message=verbose)
		TEST <- .lmTest_fun(reduced, random, use_lmer)
		tests <- chunkMapply(TEST, models, datalist,
			verbose=verbose, chunkopts=chunkopts,
			BPPARAM=BPPARAM, ...)
		tests <- DataFrame(do.call(rbind, tests))
		# return results
		if ( anyNA(tests$statistic) )
			.Warn(sum(is.na(tests$statistic)), " tests could not be performed")
	} else {
		# For lmer models, create empty test results and check for singular fits
		singular <- sapply(models, function(m) {
			if ( inherits(m, "lmerMod") ) {
				lme4::isSingular(m)
			} else {
				NA
			}
		})
		tests <- DataFrame(statistic=rep(NA_real_, n), pvalue=rep(NA_real_, n), singular=singular)
	}
	if ( is.null(random) ) {
		mcols <- DataFrame(fixed=deparse1(fixed), tests)
	} else {
		mcols <- DataFrame(fixed=deparse1(fixed),
			random=deparse1(random), tests)
	}
	as(ResultsList(models, mcols=mcols), "MeansTest")
})

.lmFit_fun <- function(fixed, random, use_lmer = FALSE)
{
	FIT <- isoclos(function(data, ...)
	{
		model <- NULL
		if ( is.null(random) ) {
			model <- try(lm(fixed, data=data, ...), silent=TRUE)
		} else {
			if ( use_lmer ) {
				fixed_terms <- as.character(fixed)[3]
				random_formula_char <- as.character(random)
				random_part <- trimws(random_formula_char[2])
				# Check if random part already has parentheses (simple or multiple terms)
				# If it starts with '(' assume it's already in lmer format
				if ( !grepl("^\\(", random_part) ) {
					random_part <- paste0("(", random_part, ")")
				}
				response <- as.character(fixed)[2]
				lmer_formula <- as.formula(paste0(response, " ~ ", fixed_terms, " + ", random_part))
				control <- lme4::lmerControl(check.conv.singular = lme4::.makeCC("ignore", tol = 1e-4))
				# Use lmerTest::lmer for Satterthwaite df by default
				model <- try(lmerTest::lmer(lmer_formula, data=data, REML=TRUE, control=control, ...), silent=TRUE)
			} else {
				model <- try(lme(fixed, data=data,
					random=random, method="ML", ...), silent=TRUE)
			}
		}
		if ( !inherits(model, "try-error") )
		{
			if ( !inherits(model, "lmerMod") ) {
				model <- update(model, . ~ .)
				model$data <- data
			} else {
				# For lmer models, store data differently
				attr(model, "data") <- data
			}
		}
		model
	}, CardinalEnv())
	FIT
}

.lmTest_fun <- function(reduced, random, use_lmer = FALSE)
{
	TEST <- isoclos(function(model, data)
	{
		if ( inherits(model, "try-error") ) {
			return(c(statistic=NA, pvalue=NA))
		} else {
			full <- model
		}
		if ( inherits(model, "lmerMod") ) {
			.Error("likelihood ratio tests for lmer models are not supported")
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
		use_lmer = FALSE,
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
	FIT <- .lmFit_fun(fixed, random, use_lmer)
	models <- chunkLapply(datalist, FIT,
		verbose=verbose, chunkopts=chunkopts,
		BPPARAM=BPPARAM, ...)
	names(models) <- rownames(featureData(x))
	# test models (skip for lmer models)
	if ( !use_lmer ) {
		.Log("testing ", n, " ", label,
			message=verbose)
		TEST <- .lmTest_fun(reduced, random, use_lmer)
		tests <- chunkMapply(TEST, models, datalist,
			verbose=verbose, chunkopts=chunkopts,
			BPPARAM=BPPARAM, ...)
		tests <- DataFrame(do.call(rbind, tests))
	} else {
		# For lmer models, create empty test results and check for singular fits
		singular <- sapply(models, function(m) {
			if ( inherits(m, "lmerMod") ) {
				lme4::isSingular(m)
			} else {
				NA
			}
		})
		tests <- DataFrame(statistic=rep(NA_real_, n), pvalue=rep(NA_real_, n), singular=singular)
	}
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


#### Contrasts for mixed effects models ####
## ------------------------------------------

contrastTest <- function(object, specs, method = "pairwise", emm_adjust = "none",
	verbose = getCardinalVerbose(), chunkopts = list(),
	BPPARAM = getCardinalBPPARAM(), ...)
{
	if ( !is(object, "MeansTest") )
		.Error("'object' must be a MeansTest object")
	# Check if models were fit with lm or lmer
	has_lm_or_lmer <- any(sapply(object, function(m) {
		inherits(m, "lm") || inherits(m, "lmerMod")
	}))
	if ( !has_lm_or_lmer ) {
		.Error("contrastTest() requires models fit with lm or use_lmer = TRUE")
	}
	# Check for specs
	if ( missing(specs) )
		.Error("missing 'specs' argument for emmeans")
	
	n <- length(object)
	label <- if (n != 1L) "contrasts" else "contrast"
	
	# Compute contrasts
	.Log("computing ", n, " ", label,
		message=verbose)
	# Use lapply instead of chunkLapply to avoid serialization issues with lmer models
	# Capture emmeans::contrast to avoid namespace conflicts
	emmeans_contrast <- emmeans::contrast
	# Set default df method to satterthwaite to avoid pbkrtest warnings
	old_emm_options <- emmeans::emm_options(lmer.df = "satterthwaite")
	on.exit(emmeans::emm_options(old_emm_options), add = TRUE)
	
	contrasts <- lapply(object, function(model) {
		if ( inherits(model, "try-error") ) {
			return(NULL)
		}
		if ( !inherits(model, "lm") && !inherits(model, "lmerMod") ) {
			return(NULL)
		}
		# Compute emmeans
		emm <- try(emmeans::emmeans(model, specs=specs, ...), silent=TRUE)
		if ( inherits(emm, "try-error") ) {
			return(NULL)
		}
		# Compute contrasts
		contr <- try(emmeans_contrast(emm, method=method, adjust=emm_adjust), silent=TRUE)
		if ( inherits(contr, "try-error") ) {
			return(NULL)
		}
		contr
	})
	names(contrasts) <- names(object)
	
	# Extract statistics into wide DataFrame
	.Log("extracting statistics",
		message=verbose)
	stats_list <- lapply(contrasts, .extract_contrast_stats)
	
	# Check if all have same structure
	if ( length(stats_list) > 0L && !is.null(stats_list[[1L]]) ) {
		# Get column names from first non-NULL result
		first_valid <- stats_list[!sapply(stats_list, is.null)][[1L]]
		col_names <- names(first_valid)
		
		# Create matrix with all results
		# Use matrix() instead of rbind() to preserve non-syntactic column names
		stats_mat <- matrix(NA_real_, nrow=length(stats_list), ncol=length(col_names))
		for ( i in seq_along(stats_list) ) {
			if ( !is.null(stats_list[[i]]) ) {
				stats_mat[i, ] <- stats_list[[i]]
			}
		}
		colnames(stats_mat) <- col_names
		stats_df <- DataFrame(stats_mat, check.names=FALSE)
	} else {
		# All contrasts failed
		stats_df <- DataFrame()
	}
	
	# Combine with existing mcols
	# Drop carryover 'statistic' and 'pvalue' columns from MeansTest
	mcols_old <- mcols(object)
	if ( "statistic" %in% names(mcols_old) )
		mcols_old$statistic <- NULL
	if ( "pvalue" %in% names(mcols_old) )
		mcols_old$pvalue <- NULL
	if ( ncol(stats_df) > 0L ) {
		mcols_new <- cbind(mcols_old, stats_df)
	} else {
		mcols_new <- mcols_old
	}
	
	# Return ContrastResults
	x <- SimpleList(contrasts)
	new("ContrastResults", x, elementMetadata=mcols_new,
		elementType=class(x[[1L]])[1L])
}

.extract_contrast_stats <- function(contrast_obj)
{
	if ( is.null(contrast_obj) ) {
		return(NULL)
	}
	# Convert to data frame
	contr_df <- as.data.frame(contrast_obj)
	
	# Get contrast names
	if ( "contrast" %in% names(contr_df) ) {
		contr_names <- as.character(contr_df$contrast)
	} else {
		# Use row names if no contrast column
		contr_names <- rownames(contr_df)
		if ( is.null(contr_names) ) {
			contr_names <- paste0("contrast", seq_len(nrow(contr_df)))
		}
	}
	
	# Extract estimate and p-value columns
	estimate_col <- NULL
	pvalue_col <- NULL
	
	if ( "estimate" %in% names(contr_df) ) {
		estimate_col <- contr_df$estimate
	}
	if ( "p.value" %in% names(contr_df) ) {
		pvalue_col <- contr_df$p.value
	}
	
	# Create named vector with results
	result <- numeric(0)
	if ( !is.null(estimate_col) ) {
		names_est <- paste0(contr_names, ".estimate")
		result <- c(result, setNames(estimate_col, names_est))
	}
	if ( !is.null(pvalue_col) ) {
		names_pval <- paste0(contr_names, ".pvalue")
		result <- c(result, setNames(pvalue_col, names_pval))
	}
	
	if ( length(result) == 0L ) {
		return(NULL)
	}
	
	result
}


setMethod("topFeatures", "ContrastResults",
	function(object, n = Inf, sort.by = NULL, ...)
{
	# Start from mcols and drop design descriptors
	topf <- mcols(object)
	if ( "fixed" %in% names(topf) )
		topf$fixed <- NULL
	if ( "random" %in% names(topf) )
		topf$random <- NULL

	# Identify p-value columns of form "[contrast].pvalue"
	pval_cols <- grep("\\.pvalue$", names(topf), value=TRUE)
	if ( length(pval_cols) == 0L )
		.Error("no pvalue columns found in contrast results")

	# Add FDR-adjusted columns alongside each p-value column
	for ( pv in pval_cols ) {
		fdr_name <- sub("\\.pvalue$", ".fdr", pv)
		# Ensure numeric for p.adjust
		vals <- as.numeric(topf[[pv]])
		topf[[fdr_name]] <- p.adjust(vals, method="fdr")
	}

	# Determine sort column after adding FDR columns
	if ( is.null(sort.by) ) {
		# Default: first pvalue column that is not all NA
		non_na <- NULL
		for ( pv in pval_cols ) {
			if ( !all(is.na(topf[[pv]])) ) { non_na <- pv; break }
		}
		if ( is.null(non_na) )
			.Error("all pvalue columns are NA; cannot determine default sort column")
		sort.by <- non_na
	} else if ( is.numeric(sort.by) ) {
		if ( length(sort.by) != 1L )
			.Error("'sort.by' must be a single column index")
		i <- as.integer(sort.by)
		if ( i < 1L || i > ncol(topf) )
			.Error("'sort.by' index out of range")
		sort.by <- names(topf)[i]
	} else if ( is.character(sort.by) ) {
		if ( length(sort.by) != 1L )
			.Error("'sort.by' must be a single column name")
		if ( !(sort.by %in% names(topf)) )
			.Error("column ", sQuote(sort.by), " not found in results")
	} else {
		.Error("'sort.by' must be NULL, a column name, or a column index")
	}

	# Sort ascending with NA last
	i <- order(topf[[sort.by]], na.last=TRUE)
	topf <- topf[i,,drop=FALSE]
	head(topf, n=n)
})

