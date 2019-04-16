
setMethod("meansTest", "SparseImagingExperiment",
	function(x, fixed, random, groups = run(x),
		BPPARAM = bpparam(), ...)
	{
		e <- environment(fixed)
		args <- .parseFormula2(fixed)
		fnames <- names(args$rhs)
		if ( !all(fnames %in% names(pixelData(x))) )
			.stop("all variables in formula must appear in pixelData")
		if ( !is.null(args$lhs) )
			.stop("lhs of formula must be empty")
		if ( !is.null(args$g) )
			.stop("conditioning variables via | not allowed")
		fixed <- paste0("..response.. ~", deparse(fixed[[2]]))
		fixed <- as.formula(fixed)
		environment(fixed) <- e
		groups <- as.factor(groups)
		if ( missing(random) ) {
			random <- NULL
			mixed <- FALSE
		} else {
			mixed <- TRUE
		}
		testData <- .meansTest_testdata(x, groups, BPPARAM=BPPARAM)
		results <- lapply(testData, function(data) {
			if ( mixed ) {
				fit <- lme(fixed=fixed, random=random, data=data, ...)
			} else {
				fit <- lm(formula=fixed, data=data, ...)
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
				parameters="feature",
				fixed=fixed, random=random),
			resultData=as(results, "List"),
			modelData=models)
		modelData(out)$p.value <- sapply(results, function(res) {
			f <- summary(res$model)$fstatistic
			p <- pf(f[1], f[2], f[3], lower.tail=FALSE)
			round(p, digits=6)
		})
		adj.p <- p.adjust(modelData(out)$p.value, method="BH")
		modelData(out)$adj.p.value <- round(adj.p, digits=6)
		pixelData(out)$..group.. <- groups
		out
	})

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
		out <- data.frame(..response..=resp, ..group..=levels(groups))
		out <- cbind(out, vars)
		row.names(out) <- NULL
		out
	})
}
