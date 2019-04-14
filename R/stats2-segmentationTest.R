
setMethod("segmentationTest", "SpatialDGMM",
	function(x, fixed, random,
		model = modelData(x),
		classControl = NULL, ...)
	{
		if ( !is.numeric(model) ) {
			if ( is.null(metadata(x)$modelParam) ) {
				params <- names(modelData(x))
			} else {
				params <- metadata(x)$modelParam
			}
			model <- model[names(model) %in% params]
			model <- subset_rows(modelData(x), as.list(model))
		}
		if ( missing(random) ) {
			mixed <- FALSE
		} else {
			mixed <- TRUE
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
		fullData <- .spatialDGMM_testdata(x)[model]
		if ( is.null(classControl) ) {
			classControl <- .spatialDGMM_matchclasses(x, fc)[model]
		} else {
			# do something
		}
		testData <- mapply(function(data, ctrl) subset_data(data, ctrl),
			fullData, classControl, SIMPLIFY=FALSE)
		out <- lapply(testData, function(data) {
			if ( mixed ) {
				fit <- lme(fixed=fixed, random=random, data=data, ...)
			} else {
				fit <- lm(formula=fixed, data=data, ...)
			}
			list(model=fit, data=data)
		})
		out
	})

.spatialDGMM_testdata <- function(results) {
	i <- which(names(pData(results)) %in% metadata(results)$groupsName)
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
		out <- res$params[,c("group", "class")]
		names(out) <- c("..group..", "..class..")
		out[["..response.."]] <- res$params[["mean"]]
		out[names(pdata)] <- vars
		out
	})
}

.spatialDGMM_matchclasses <- function(results, fc, select = TRUE) {
	groups <- pData(results)[[metadata(results)$groupsName]]
	lapply(resultData(results), function(res) {
		out <- lapply(fc, function(nm) {
			f <- pData(results)[[nm]]
			if ( is.logical(f) ) {
				sc <- sapply(levels(res$class), function(ci) {
					gi <- res$params$group[res$params$class == ci][1L]
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
						gi <- res$params$group[res$params$class == ci][1L]
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
		out1 <- do.call("cbind", out)
		out2 <- res$params[,c("group", "class")]
		names(out2) <- c("..group..", "..class..")
		out <- cbind(out2, out1)
		if ( select ) {
			stot <- rowSums(out[,-c(1,2)])
			matches <- tapply(stot, out[["..group.."]], is.max)
			out[unlist(matches),c("..group..", "..class..")]
		} else {
			out
		}
	})
}

