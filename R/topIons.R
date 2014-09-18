
setMethod("topIons", "SpatialShrunkenCentroids",
	function(object, n = 6,
		model = pData(modelData(object)),
		classes = levels(summary(object)$classes),
		sort.by = c("tstatistics", "p.values"),
		plot = FALSE,
		...)
	{
		if ( plot ) stop("reference object 'ref' required for plot = TRUE")
		if ( is.list(model) || any(names(model) %in% varLabels(modelData(object))) ) {
			model <- model
		} else {
			model <- pData(modelData(object))[model,,drop=FALSE]
		}
		top <- summary(object)
		top <- data.frame(mz=rep(fData(object)$mz, length.out=nrow(top)), top)
		if ( is.numeric(classes) )
			classes <- levels(top$classes)[classes]
		filter <- append(model, list(classes=classes))
		top <- subdata(top, subset=filter)
		sort.by <- match.arg(sort.by)
		top <- switch(sort.by,
			tstatistics = top[order(top$tstatistics, decreasing=TRUE),],
			p.values = top[order(top$p.values, decreasing=FALSE),],
			top[order(top[[sort.by]]),])
		top <- top[seq_len(n),]
		row.names(top) <- NULL
		top
	})
