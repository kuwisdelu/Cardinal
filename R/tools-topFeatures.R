
setMethod("topFeatures", "ResultSet",
	function(object, n = 6,
		model = pData(modelData(object)),
		type = c('+', '-', 'b'),
		sort.by = fvarLabels(object),
		filter = list(),
		...)
	{
		.Deprecated_Cardinal1("ResultSet")
		type <- match.arg(type)
		if ( is.list(model) || any(names(model) %in% varLabels(modelData(object))) ) {
			model <- model
		} else {
			model <- pData(modelData(object))[model,,drop=FALSE]
		}
		topFeatures <- summary(object)$topFeatures
		fData <- do.call(rbind, rep(list(fData(object))))
		topFeatures <- data.frame(fData, topFeatures, row.names=seq_len(nrow(topFeatures)))
		sort.by <- match.arg(sort.by[[1]], choices=names(topFeatures))
		filter <- append(model, filter)
		topFeatures <- subset_data(topFeatures, subset=filter)
		topFeatures <- switch(type,
			`+` = topFeatures[order(topFeatures[[sort.by]], decreasing=TRUE),],
			`-` = topFeatures[order(topFeatures[[sort.by]], decreasing=FALSE),],
			'b' = topFeatures[order(abs(topFeatures[[sort.by]]), decreasing=TRUE),])
		row.names(topFeatures) <- NULL
		head(topFeatures, n=n, ...)
	})

setMethod("topFeatures", "PCA",
	function(object, n = 6,
		sort.by = "loadings",
		...)
	{
		callNextMethod(object=object, n=n, sort.by=sort.by, ...)
	})

setMethod("topFeatures", "PLS",
	function(object, n = 6,
		sort.by = c("coefficients", "loadings", "weights"),
		...)
	{
		callNextMethod(object=object, n=n, sort.by=sort.by, ...)
	})

setMethod("topFeatures", "OPLS",
	function(object, n = 6,
		sort.by = c("coefficients",
			"loadings", "Oloadings",
			"weights", "Oweights"),
		...)
	{
		callNextMethod(object=object, n=n, sort.by=sort.by, ...)
	})

setMethod("topFeatures", "SpatialKMeans",
	function(object, n = 6,
		sort.by = c("betweenss", "withinss"),
		...)
	{
		callNextMethod(object=object, n=n, sort.by=sort.by, ...)
	})

setMethod("topFeatures", "SpatialShrunkenCentroids",
	function(object, n = 6,
		sort.by = c("tstatistics", "p.values"),
		...)
	{
		callNextMethod(object=object, n=n, sort.by=sort.by, ...)
	})

setMethod("topFeatures", "CrossValidated",
	function(object, ...)
	{
		lapply(resultData(object), topFeatures, ...)
	})

