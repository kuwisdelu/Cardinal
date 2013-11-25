
setMethod("initialize",
	signature(.Object = "PAnnotatedDataFrame"),
	function(.Object,
			data = data.frame(),
			varMetadata = data.frame(),
			...) {
		if ( missing(data) )
			data <- data.frame(sample=factor())
		if ( is.null(data[["sample"]]) )
			data[["sample"]] <- factor(rep(1, nrow(data)))
		reqLabelTypes <- c("spatial2d", "spatial3d", "dimension", "sample", "pheno")
		if ( missing(varMetadata) )
			varMetadata <- data.frame(labelType = factor(levels=reqLabelTypes), row.names=names(data))
		if ( is.null(varMetadata[["labelType"]]) )
			varMetadata[["labelType"]] <- factor(rep(NA, ncol(data)), levels=reqLabelTypes)
		if ( !all(reqLabelTypes %in% levels(varMetadata[["labelType"]])) )
			levels(varMetadata[["labelType"]]) <- unique(c(levels(varMetadata[["labelType"]]), reqLabelTypes))
		if ( !"sample" %in% row.names(varMetadata) )
			varMetadata["sample","labelType"] <- "sample"
		.Object <- callNextMethod(.Object,
			data=data,
			varMetadata=varMetadata,
			...)
		.Object@varMetadata["sample","labelType"] <- "sample"
		.Object@varMetadata[["labelType"]][is.na(.Object@varMetadata[["labelType"]])] <- "pheno"
		if ( validObject(.Object) )
			.Object
	})

PAnnotatedDataFrame <- function(data, varMetadata,
	dimLabels=c("pixelNames", "pixelColumns"),
	...)
{
	reqLabelTypes <- c("spatial2d", "spatial3d", "dimension", "sample", "pheno")
	if ( missing(data) )
		data <- data.frame(sample=factor())
	if ( missing(varMetadata) )
		varMetadata <- data.frame(labelType=factor(rep(NA, ncol(data)),
			levels=reqLabelTypes), row.names=names(data))
	if ( !is.null(varMetadata[["labelType"]]) && !is.factor(varMetadata[["labelType"]]) )
		varMetadata[["labelType"]] <- as.factor(varMetadata[["labelType"]])
	.PAnnotatedDataFrame(data=data,
		varMetadata=varMetadata,
		dimLabels=dimLabels,
		...)
}
