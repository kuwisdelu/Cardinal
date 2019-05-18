
setMethod("meansTest", "SparseImagingExperiment",
	function(x, fixed, random, groups = run(x),
		BPPARAM = bpparam(), ...)
	{
		e <- environment(fixed)
		args <- .parseFormula2(fixed)
		vars <- names(args$rhs)[sapply(args$rhs, is.language)]
		if ( !all(vars %in% names(pixelData(x))) )
			.stop("all variables in formula must appear in pixelData")
		if ( !is.null(args$lhs) )
			.stop("lhs of formula must be empty")
		if ( !is.null(args$g) )
			.stop("conditioning variables via | not allowed")
		groups <- as.factor(rep_len(groups, ncol(x)))
		fixed <- paste0(".response ~", deparse(fixed[[2]]))
		fixed <- as.formula(fixed)
		environment(fixed) <- e
		if ( missing(random) ) {
			random <- NULL
			mixed <- FALSE
		} else {
			mixed <- TRUE
		}
		testData <- .meansTest_testdata(x, groups, BPPARAM=BPPARAM)
		results <- lapply(testData, function(data) {
			if ( mixed ) {
				fit <- try(lme(fixed=fixed, random=random, data=data, ...), silent=TRUE)
			} else {
				fit <- try(lm(formula=fixed, data=data, ...), silent=TRUE)
			}
			list(model=fit, data=as(data, "DataFrame"))
		})
		models <- DataFrame(feature=1:nrow(x))
		out <- .MeansTest(
			imageData=.SimpleImageArrayList(),
			featureData=featureData(x),
			elementMetadata=pixelData(x),
			metadata=list(
				mapping=list(
					feature=NULL,
					pixel=NULL),
				fixed=fixed, random=random),
			resultData=as(results, "List"),
			modelData=models)
		errors <- sapply(results, function(res) inherits(res$model, "try-error"))
		if ( any(errors) )
			.warning("there were 1 or more errors while fitting models")
		pixelData(out)$.group <- groups
		out
	})

.meansTest_withmeans <- function(x, jitter = TRUE) {
	resultData(x) <- endoapply(resultData(x),
		function(res) {
			mean <- res$data$.response[pData(x)$.group]
			if ( jitter ) {
				res$mean <- jitter(mean)
			} else {
				res$mean <- mean
			}
			res
		})
	x
}

.meansTest_LRT <- function(object, BPPARAM) {
	tests <- bplapply(resultData(object), function(res) {
		data <- res$data
		full <- res$model
		if ( inherits(full, "lm") ) {
			null <- update(full, . ~ 1, data=data)
			numer <- sum(residuals(null)^2) - sum(residuals(full)^2)
			denom <- sum(residuals(full)^2) / full$df.residual
			df <- abs(null$df.residual - full$df.residual)
			LR <- numer / denom
			PValue <- pchisq(LR, df, lower.tail = FALSE)
		} else if ( inherits(full, "lme") ) {
			full <- update(full, . ~ ., data=data, method="ML")
			null <- update(full, . ~ 1, data=data, method="ML")
			aov <- anova(null, full)
			df <- abs(diff(aov[,"df"]))
			LR <- aov[2,"L.Ratio"]
			PValue <- aov[2,"p-value"]
		} else {
			PValue <- NULL
			LR <- NULL
		}
		list(LR=LR, DF=df, PValue=PValue)
	}, BPPARAM=BPPARAM)
	LR <- sapply(tests, function(tt) {
		if ( is.null(tt) ) {
			NA_real_
		} else {
			tt$LR
		}
	})
	DF <- sapply(tests, function(tt) tt$df)
	PValue <- sapply(tests, function(tt) {
		if ( is.null(tt) ) {
			NA_real_
		} else {
			tt$PValue
		}
	})
	list(LR=LR, DF=DF, PValue=PValue)
}

.meansTest_testdata <- function(x, groups, BPPARAM) {
	response <- summarize(x, .stat="mean", .group_by=groups, BPPARAM=BPPARAM)
	response <- t(as.matrix(response, slots=FALSE))
	pdata <- as.data.frame(pData(x), slots=FALSE)
	pdata <- cbind(data.frame(run=run(x)), pdata)
	vars <- lapply(names(pdata), function(nm) {
		newvar <- sapply(levels(groups), function(gi) {
			var <- pdata[[nm]]
			if ( is.numeric(var) ) {
				mean(var[groups == gi], na.rm=TRUE)
			} else {
				Mode(var[groups == gi], na.rm=TRUE)
			}
		})
		newvar
	})
	names(vars) <- names(pdata)
	vars <- as.data.frame(vars)
	apply(response, 2, function(resp) {
		out <- data.frame(.response=resp, .group=levels(groups))
		out <- cbind(out, vars)
		row.names(out) <- NULL
		out
	})
}
