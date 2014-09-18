
setMethod("summary", "SpatialShrunkenCentroids",
	function(object, ...) {
		out <- do.call("rbind", lapply(resultData(object), function(x, m) {
			k <- x$k
			n <- tabulate(x$classes)
			n <- rep(n, each=nrow(object))
			classes <- factor(rep(seq_len(k), each=nrow(object)),
				labels=levels(x$classes))
			p.values <- 2 * (1 - pt(abs(as.vector(x$tstatistics)), df=n - 1))
			adj.p.values <- p.adjust(p.values, method="BH")
			data.frame(r=x$r, k=x$k, s=x$s,
				classes=classes,
				centers=as.vector(x$centers),
				tstatistics=as.vector(x$tstatistics),
				p.values=p.values,
				adj.p.values=adj.p.values,
				row.names=seq_len(k * nrow(object)))
		}))
		row.names(out) <- NULL
		out
	})

