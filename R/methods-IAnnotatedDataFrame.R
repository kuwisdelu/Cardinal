
setMethod("initialize", "IAnnotatedDataFrame",
	function(.Object,
			data = data.frame(),
			varMetadata = data.frame(),
			...) {
		if ( missing(data) ) {
			data <- data.frame(sample=factor())
		} else if ( is.null(data[["sample"]]) ) {
			data[["sample"]] <- factor(rep(1, nrow(data)))
		}
		reqLabelTypes <- c("dim", "sample", "pheno")
		if ( missing(varMetadata) )
			varMetadata <- data.frame(labelType=factor(rep(NA, ncol(data)), levels=reqLabelTypes),
				row.names=names(data))
		if ( is.null(varMetadata[["labelType"]]) ) {
			varMetadata[["labelType"]] <- factor(rep(NA, ncol(data)), levels=reqLabelTypes)
		} else if ( !all(reqLabelTypes %in% levels(varMetadata[["labelType"]])) ) {
			levels(varMetadata[["labelType"]]) <- union(levels(varMetadata[["labelType"]]), reqLabelTypes)
		}
		if ( !"sample" %in% row.names(varMetadata) ) {
			if ( nrow(varMetadata) < ncol(data) ) {
				varMetadata["sample","labelType"] <- "sample"
			} else if ( !all(row.names(varMetadata) == names(data)) ) {
				row.names(varMetadata) <- names(data)
			}
		}
		if ( is.na(varMetadata["sample","labelType"]) )
			varMetadata["sample","labelType"] <- "sample"
		.Object <- callNextMethod(.Object,
			data=data,
			varMetadata=varMetadata,
			...)
		.Object@varMetadata[["labelType"]][is.na(.Object@varMetadata[["labelType"]])] <- "pheno"
		if ( validObject(.Object) )
			.Object
	})

IAnnotatedDataFrame <- function(data, varMetadata,
	dimLabels=c("pixelNames", "pixelColumns"), ...)
{
	reqLabelTypes <- c("dim", "sample", "pheno")
	if ( missing(data) )
		data <- data.frame(sample=factor())
	if ( missing(varMetadata) )
		varMetadata <- data.frame(labelType=factor(rep(NA, ncol(data)), levels=reqLabelTypes),
			row.names=names(data))
	.IAnnotatedDataFrame(data=data,
		varMetadata=varMetadata,
		dimLabels=dimLabels,
		...)
}

setValidity("IAnnotatedDataFrame", function(object) {
	msg <- validMsg(NULL, NULL)
	if ( is.null(object@data[["sample"]]) )
		msg <- validMsg(msg, "required column 'sample' missing from data")
	if ( !is.factor(object@data[["sample"]]) )
		msg <- validMsg(msg, "required column 'sample' must be a factor")
	labelType <- object@varMetadata[["labelType"]]
	if ( is.null(labelType) )
		msg <- validMsg(msg, "required column 'labelType' missing from varMetadata")
	reqLabelTypes <- c("dim", "sample", "pheno")
	if ( !is.factor(labelType) || !all(reqLabelTypes %in% levels(labelType)) )
		msg <- validMsg(msg, paste("column 'labelType' must be a factor with levels:",
			paste(reqLabelTypes, collapse=", ")))
	if (is.null(msg)) TRUE else msg
})

setMethod("sampleNames", "IAnnotatedDataFrame",
	function(object) levels(object[["sample"]]))

setReplaceMethod("sampleNames", "IAnnotatedDataFrame",
	function(object, value) {
		levels(object[["sample"]]) <- value
		object
	})

setMethod("pixelNames", "IAnnotatedDataFrame",
	function(object) row.names(pData(object)))

setReplaceMethod("pixelNames", "IAnnotatedDataFrame",
	function(object, value) {
		if (length(value) != dim(pData(object))[1])
			.stop("number of new names (", length(value), ") ",
				"should equal number of rows in AnnotatedDataFrame (",
				dim(object)[1], ")")
		row.names(pData(object)) <- value
		object
	})

setMethod("coordLabels", "IAnnotatedDataFrame",
	function(object) {
		coordLabelTypes <- c("dim")
		sampleLabelTypes <- c("sample")
		isCoord <- varMetadata(object)[["labelType"]] %in% coordLabelTypes
		isCoord[varLabels(object) %in% sampleLabelTypes] <- FALSE
		varLabels(object)[isCoord]
	})

setReplaceMethod("coordLabels", "IAnnotatedDataFrame",
	function(object, value) {
		coordLabelTypes <- c("dim")
		sampleLabelTypes <- c("sample")
		isCoord <- varMetadata(object)[["labelType"]] %in% coordLabelTypes
		isCoord[varLabels(object) %in% sampleLabelTypes] <- FALSE
		varLabels(object)[isCoord] <- value
		object
	})

setMethod("coord", "IAnnotatedDataFrame",
	function(object) {
		coordLabelTypes <- c("dim")
		isCoord <- varMetadata(object)[["labelType"]] %in% coordLabelTypes
		pData(object)[isCoord]
	})

setReplaceMethod("coord", "IAnnotatedDataFrame",
	function(object, value) {
		coordLabelTypes <- c("dim")
		isCoord <- varMetadata(object)[["labelType"]] %in% coordLabelTypes
		pData(object)[isCoord] <- value
		object
	})

setMethod("[", "IAnnotatedDataFrame",
	function(x, i, j, ..., drop = FALSE) {
		x <- callNextMethod(x, i, j, ..., drop=drop)
		pData(x)$sample <- factor(pData(x)$sample)
		x
	})

setMethod("combine",
	signature = c(x = "IAnnotatedDataFrame", y = "IAnnotatedDataFrame"),
	function(x, y, ...) {
		if ( any(duplicated(rbind(coord(x), coord(y)))) )
			.stop("IAnnotatedDataFrame contain pixels with duplicate coordinates")
		if ( !identical(varLabels(x), varLabels(y)) )
			.stop("IAnnotatedDataFrame 'varLabels' must match")
		if ( !identical(varMetadata(x), varMetadata(y)) )
			.stop("IAnnotatedDataFrame 'varMetadata' must match")
		new(class(x),
			data=rbind(pData(x), pData(y)),
			varMetadata=varMetadata(x),
			dimLabels=dimLabels(x))
	})

