
setMethod("segmentationTest", "SparseImagingExperiment",
	function(x, fixed, random, groups = run(x),
		classControl = c("Mscore", "Ymax"),
		BPPARAM = bpparam(), ...)
	{
		args <- .parseFormula2(fixed)
		fnames <- names(args$rhs)
		if ( !all(fnames %in% names(pixelData(x))) )
			.stop("all variables in formula must appear in pixelData")
		if ( !is.null(args$lhs) )
			.stop("lhs of formula must be empty")
		if ( !is.null(args$g) )
			.stop("conditioning variables via | not allowed")
		init <- spatialDGMM(x, groups=groups, BPPARAM=BPPARAM, ...)
		segmentationTest(init, fixed=fixed, random=random,
			classControl=classControl, ...)
	})

setMethod("segmentationTest", "SpatialDGMM",
	function(x, fixed, random, model = modelData(x),
		classControl = c("Mscore", "Ymax"), ...)
	{
		if ( !is.numeric(model) ) {
			if ( is.null(metadata(x)$parameters) ) {
				estimates <- names(modelData(x))
			} else {
				estimates <- metadata(x)$parameters
			}
			model <- model[names(model) %in% estimates]
			model <- subset_rows(modelData(x), as.list(model))
		}
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
		fc <- fnames[!sapply(pixelData(x)[fnames], is.numeric)]
		if ( length(fc) == 0L )
			.stop("at least one variable must be non-numeric")
		if ( is.character(classControl) ) {
			classControl <- match.arg(classControl)
			classControl <- .spatialDGMM_getclasses(x, fc, classControl)
			classControl <- classControl[model]
		} else {
			classControl <- .segmentationTest_getclasses(classControl)
		}
		if ( missing(random) ) {
			random <- NULL
			mixed <- FALSE
		} else {
			mixed <- TRUE
		}
		fullData <- .segmentationTest_testdata(x)[model]
		results <- mapply(function(data, ctrl, res) {
			data <- subset_data(data, ctrl)
			if ( mixed ) {
				fit <- lme(fixed=fixed, random=random, data=data, ...)
			} else {
				fit <- lm(formula=fixed, data=data, ...)
			}
			subset <- res$class %in% data$..class..
			mapping <- replace(res$class, !subset, NA_integer_)
			list(model=fit, data=as(data, "DataFrame"),
				mapping=droplevels(mapping))
		}, fullData, classControl, resultData(x), SIMPLIFY=FALSE)
		models <- modelData(x)[model,metadata(x)$parameters,drop=FALSE]
		out <- .SegmentationTest(
			imageData=.SimpleImageArrayList(),
			featureData=featureData(x),
			elementMetadata=pixelData(x),
			metadata=list(
				mapping=list(
					feature=NULL,
					pixel="mapping"),
				parameters=metadata(x)$parameters,
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
		out
	})

.segmentationTest_testdata <- function(results) {
	i <- which(names(pData(results)) %in% "..group..")
	groups <- pData(results)[[i]]
	pdata <- as.data.frame(pData(results)[,-i,drop=FALSE], slots=FALSE)
	pdata <- cbind(data.frame(run=run(results)), pdata)
	lapply(resultData(results), function(res) {
		vars <- lapply(names(pdata), function(nm) {
			newvar <- sapply(levels(res$class), function(ci) {
				cl <- res$class == ci
				var <- pdata[[nm]]
				if ( is.numeric(var) ) {
					mean(var[cl], na.rm=TRUE)
				} else {
					Mode(var[cl], na.rm=TRUE)
				}
			})
			newvar
		})
		out <- res$estimates[,c("mean", "group", "class")]
		names(out) <- c("..response..", "..group..", "..class..")
		out[names(pdata)] <- vars
		out
	})
}

.segmentationTest_getclasses <- function(control) {
	lapply(control, function(ctrl) {
		data.frame(..group..=names(ctrl),
			..class..=as.character(ctrl))
	})
}

.spatialDGMM_getclasses <- function(results, fc, method) {
	groups <- pixelData(results)$..group..
	lapply(resultData(results), function(res) {
		if ( method == "Mscore" ) {
			out1 <- lapply(fc, function(nm) {
				f <- pData(results)[[nm]]
				if ( is.logical(f) ) {
					sc <- sapply(levels(res$class), function(ci) {
						gi <- res$estimates$group[res$estimates$class == ci][1L]
						cg <- (res$class == ci)[groups == gi]
						fg <- f[groups == gi]
						Mscore(cg, fg)
					})
					sc <- as.data.frame(sc)
					names(sc) <- nm
				} else {
					f <- as.factor(f)
					sc <- lapply(levels(f), function(fi) {
						scores <- sapply(levels(res$class), function(ci) {
							gi <- res$estimates$group[res$estimates$class == ci][1L]
							cg <- (res$class == ci)[groups == gi]
							fg <- (f == fi)[groups == gi]
							Mscore(cg, fg)
						})
						scores
					})
					sc <- as.data.frame(sc)
					names(sc) <- paste0(nm, ":", levels(f))
				}
				sc
			})
			out1 <- do.call("cbind", out1)
		} else {
			out1 <- res$estimates[,"mean",drop=FALSE]
		}
		out2 <- res$estimates[,c("group", "class")]
		names(out2) <- c("..group..", "..class..")
		out <- cbind(out2, out1)
		score_sums <- rowSums(out[,-c(1,2),drop=FALSE])
		matches <- tapply(score_sums, out[["..group.."]], is.max)
		out[unlist(matches),c("..group..", "..class..")]
	})
}

