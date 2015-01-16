
setMethod("topLabels", "ResultSet",
	function(object, n = 6,
		model = pData(modelData(object)),
		type = c('+', '-', 'b'),
		sort.by = fvarLabels(object),
		filter = list(),
		...)
	{
		type <- match.arg(type)
		if ( is.list(model) || any(names(model) %in% varLabels(modelData(object))) ) {
			model <- model
		} else {
			model <- pData(modelData(object))[model,,drop=FALSE]
		}
		topLabels <- summary(object)$topLabels
		fData <- do.call(rbind, rep(list(fData(object))))
		topLabels <- data.frame(fData, topLabels, row.names=seq_len(nrow(topLabels)))
		sort.by <- match.arg(sort.by[[1]], choices=names(topLabels))
		filter <- append(model, filter)
		topLabels <- subdata(topLabels, subset=filter)
		topLabels <- switch(type,
			`+` = topLabels[order(topLabels[[sort.by]], decreasing=TRUE),],
			`-` = topLabels[order(topLabels[[sort.by]], decreasing=FALSE),],
			'b' = topLabels[order(abs(topLabels[[sort.by]]), decreasing=TRUE),])
		row.names(topLabels) <- NULL
		head(topLabels, n=n, ...)
	})

setMethod("topLabels", "PCA",
	function(object, n = 6,
		sort.by = "loadings",
		...)
	{
		callNextMethod(object=object, n=n, sort.by=sort.by, ...)
	})

setMethod("topLabels", "PLS",
	function(object, n = 6,
		sort.by = c("coefficients", "loadings", "weights"),
		...)
	{
		callNextMethod(object=object, n=n, sort.by=sort.by, ...)
	})

setMethod("topLabels", "OPLS",
	function(object, n = 6,
		sort.by = c("coefficients",
			"loadings", "Oloadings",
			"weights", "Oweights"),
		...)
	{
		callNextMethod(object=object, n=n, sort.by=sort.by, ...)
	})

setMethod("topLabels", "SpatialKMeans",
	function(object, n = 6,
		sort.by = c("betweenss", "withinss"),
		...)
	{
		callNextMethod(object=object, n=n, sort.by=sort.by, ...)
	})

setMethod("topLabels", "SpatialShrunkenCentroids",
	function(object, n = 6,
		sort.by = c("tstatistics", "p.values"),
		...)
	{
		callNextMethod(object=object, n=n, sort.by=sort.by, ...)
	})

setMethod("topLabels", "CrossValidated",
	function(object, ...)
	{
		lapply(resultData(object), topLabels, ...)
	})
