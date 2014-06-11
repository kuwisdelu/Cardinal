
# #### implement the trellis plotting methods ####

# setMethod("trellisPlot", "MSImageSet", function(x, pixel, coord, by,
# 	main=metaData(x)[["name"]], xlab="m/z", ylab="intensity",
# 	type=if( isPeaks(x) ) c("h", "p") else "l", fun=mean,
# 	nlabels=if( isPeaks(x) ) 6 else 0, pch=20, cex=0.6,
# 	filter=c("none", "gaussian", "ma", "sgolay", "kaiser"), ...)
# {
# 	filter <- filterFunction(filter)
# 	if ( missing(by) ) {
# 		if ( !missing(coord) ) {
# 			pixel <- as.list(pixels(x, coord))
# 		} else if ( !missing(pixel) ) {
# 			coord <- NULL # necessary to make coord() work
# 		} else {
# 			stop("either 'pixel', 'coord', or 'by' must be specified")
# 		}
# 		if ( !is.list(pixel) ) pixel <- list(pixel)
# 		if ( any(unlist(sapply(pixel, is.na))) ) stop("invalid pixel")
# 		spectra <- list()
# 		by <- list()
# 		for ( i in seq_along(pixel) ) {
# 			by[[i]] <- formatCoord(coord(x)[pixel[[i]],])
# 			spectra[[i]] <- apply(x@spectra$spectra[,pixel[[i]],drop=FALSE], 1, mean)
# 			spectra[[i]] <- filter(spectra[[i]], mz(x), ...)
# 		}
# 		spectra <- as.numeric(unlist(spectra))
# 		by <- factor(unlist(by))
# 		npanels <- length(by)
# 		by <- rep(by, each=numFeatures(x))
# 		spectra.plot <- data.frame(spectra=spectra, mz=rep(mz(x), npanels), by=by)
# 		xyplot(spectra ~ mz | by, data=spectra.plot, main=main, xlab=xlab, ylab=ylab,
# 			pch=pch, cex=cex, panel=function(x, y, ...) {
# 				panel.abline(h=0, lwd=0.2)
# 				panel.xyplot(x, y, type=type[[1]], ...)
# 				if ( length(type) > 1 ) {
# 					panel.xyplot(x, y, type=type[[2]], ...)
# 				}
# 				if ( nlabels > 0 ) {
# 					top <- head(order(y, decreasing=TRUE), n=nlabels)
# 					offset <- diff(range(y)) / 75
# 					panel.text(x[top], offset + y[top], labels=round(x[top],
# 						digits=2), ...)
# 				}
# 			}, ...)
# 	} else {
# 		pixel <- lapply(sort(unique(by)), function(i) which(by == i))
# 		trellisPlot(x, pixel=pixel, main=main, xlab=xlab, ylab=ylab, type=type, fun=fun,
# 			filter=filter, pch=pch, cex=cex, ...)
# 	}
# } )

# setMethod("trellisPlot", "MSImageSegmentation", function(x, which=metaData(x)[["parnames"]],
# 	mode=c("centroids", "tstatistics"), main=metaData(x)[["name"]], xlab="m/z", ylab=mode,
# 	col=rainbow(max(x[["nclasses"]][which])), type=c("h", "p"),
# 	key=list(text=list(metaData(x)[["labels"]][1:length(col)]),
# 	points=list(pch=pch, col=col), columns=min(4, length(col))),
# 	climits=FALSE, nlabels=6, class.mask=NULL, pch=20, cex=0.6, ...)
# {
# 	mode <- match.arg(mode)
# 	which <- match.which(x, which)
# 	p <- unique(sapply(x$tstatistics, nrow))
# 	ks <- sapply(x$tstatistics, ncol)
# 	names.plot <- rep(metaData(x)[["parnames"]], sapply(x$tstatistics, length))
# 	names.plot <- factor(names.plot, levels=unique(names.plot))
# 	classes.plot <- lapply(ks, function(k) lapply(1:k, function(ki) rep(ki, p)))
# 	classes.plot <- factor(unlist(classes.plot), levels=x@metaData$levels,
# 		labels=x@metaData$labels)
# 	if ( climits ) se <- unlist(calculateSE(x)[which])
# 	tstatistics <- unlist(x$tstatistics[which])
# 	data.plot <- data.frame(x=x@metaData$mz, y=unlist(x[[mode]]),
# 		classes=classes.plot, parameters=names.plot)
# 	if ( !is.null(class.mask) ) {
# 		data.plot <- data.plot[data.plot$classes %in% class.mask,]
# 	}
# 	if ( !is.character(which) ) which <- metaData(x)[["parnames"]][which]
# 	if ( length(type) != 2 ) type <- rep(type, 2)
# 	data.plot <- data.plot[data.plot$parameters %in% which,]
# 	xyplot(y ~ x | parameters, data=data.plot, main=main, xlab=xlab, ylab=ylab,
# 		key=key, groups=classes, panel=function(x, y, subscripts, groups, ...) {
# 			panel.abline(h=0, lwd=0.2)
# 			panel.xyplot(x, y, col="gray", type=type[[1]], pch=pch, cex=cex, lwd=0.2)
# 			nonzero <- tstatistics[subscripts] != 0
# 			panel.xyplot(x[nonzero], y[nonzero], col=col[groups[subscripts][nonzero]],
# 				type=type[[1]], pch=pch, cex=cex, lwd=0.2)
# 			panel.xyplot(x[nonzero], y[nonzero], type=type[[2]], pch=pch, cex=cex,
# 				col=col[groups[subscripts][nonzero]], ...)
# 			if ( climits && mode == "centroids" ) {
# 				panel.xyplot(x[nonzero], y[nonzero]+2*se[subscripts][nonzero],
# 					pch=2, cex=cex, col=col[groups[subscripts][nonzero]], ...)
# 				panel.xyplot(x[nonzero], y[nonzero]-2*se[subscripts][nonzero],
# 					pch=6, cex=cex, col=col[groups[subscripts][nonzero]], ...)
# 			}
# 			if ( nlabels > 0 ) {
# 				ymat <- matrix(y, ncol=length(unique(groups[subscripts])))
# 				ytop <- apply(ymat, 1, function(yi) {
# 					j <- which.max(abs(yi))
# 					sign(yi[j]) * max(abs(yi))
# 				} )
# 				top <- head(order(abs(ytop), decreasing=TRUE), n=nlabels)
# 				offset <- diff(range(y)) / 25
# 				panel.text(x[top], sign(ytop[top]) * offset + ytop[top],
# 					labels=round(x[top], digits=2), cex=cex, ...)
# 			}
# 		}, ...)
# } )

