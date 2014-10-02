
setMethod("topIons", "CrossValidated",
	function(object, ...)
	{
		lapply(resultData(object), topIons, ...)
	})

setMethod("topIons", "PCA",
	function(object, n = 6,
		model = pData(modelData(object)),
		sort.by = "loadings",
		decreasing = TRUE,
		adjust = identity,
		plot = FALSE,
		...)
	{
		sort.by <- match.arg(sort.by)
		.topIons(object=object, n=n, model=model, filter=NULL,
			sort.by=sort.by, decreasing=decreasing, adjust=adjust)
	})

setMethod("topIons", "PLS",
	function(object, n = 6,
		model = pData(modelData(object)),
		column = levels(summary(object)$summary$column),
		sort.by = c("coefficients", "loadings", "weights"),
		decreasing = TRUE,
		adjust = identity,
		plot = FALSE,
		...)
	{
		sort.by <- match.arg(sort.by)
		if ( is.numeric(column) )
			column <- levels(summary(object)$summary$column)[column]
		filter <- list(column=column)
		.topIons(object=object, n=n, model=model, filter=filter,
			sort.by=sort.by, decreasing=decreasing, adjust=adjust)
	})

setMethod("topIons", "OPLS",
	function(object, n = 6,
		model = pData(modelData(object)),
		column = levels(summary(object)$summary$column),
		sort.by = c("coefficients", "loadings", "Oloadings",
			"weights", "Oweights"),
		decreasing = TRUE,
		adjust = identity,
		plot = FALSE,
		...)
	{
		sort.by <- match.arg(sort.by)
		if ( is.numeric(column) )
			column <- levels(summary(object)$summary$column)[column]
		filter <- list(column=column)
		.topIons(object=object, n=n, model=model, filter=filter,
			sort.by=sort.by, decreasing=decreasing, adjust=adjust)
	})

setMethod("topIons", "SpatialKMeans",
	function(object, n = 6,
		model = pData(modelData(object)),
		cluster = levels(summary(object)$summary$cluster),
		sort.by = "centers",
		decreasing = TRUE,
		adjust = identity,
		plot = FALSE,
		...)
	{
		sort.by <- match.arg(sort.by)
		if ( is.numeric(cluster) )
			cluster <- levels(summary(object)$summary$cluster)[cluster]
		filter <- list(cluster=cluster)
		.topIons(object=object, n=n, model=model, filter=filter,
			sort.by=sort.by, decreasing=decreasing, adjust=adjust)
	})

setMethod("topIons", "SpatialShrunkenCentroids",
	function(object, n = 6,
		model = pData(modelData(object)),
		classes = levels(summary(object)$summary$classes),
		sort.by = c("tstatistics", "p.values"),
		decreasing = sort.by != "p.values",
		adjust = identity,
		plot = FALSE,
		...)
	{
		sort.by <- match.arg(sort.by)
		if ( is.numeric(classes) )
			classes <- levels(summary(object)$summary$classes)[classes]
		filter <- list(classes=classes)
		.topIons(object=object, n=n, model=model, filter=filter,
			sort.by=sort.by, decreasing=decreasing, adjust=adjust)
	})

.topIons <- function(object, n, model,
	sort.by, decreasing, adjust, filter)
{
	if ( is.list(model) || any(names(model) %in% varLabels(modelData(object))) ) {
		model <- model
	} else {
		model <- pData(modelData(object))[model,,drop=FALSE]
	}
	top <- summary(object)$summary
	top <- data.frame(mz=rep(fData(object)$mz, length.out=nrow(top)), top)
	filter <- append(model, filter)
	top <- subdata(top, subset=filter)
	adjust <- match.fun(adjust)
	top <- top[order(adjust(top[[sort.by]]), decreasing=decreasing),]
	top <- top[seq_len(n),]
	row.names(top) <- NULL
	top
}

