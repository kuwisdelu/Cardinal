
#### implement the plotting methods ####

setMethod("plot", c("MSImageSet", "missing"), function(x, pixel, coord,
	main=metaData(x)[["name"]], xlab="m/z", ylab="intensity", col="black", pch=20, cex=0.6,
	type=if( isPeaks(x) ) c("h", "p") else "l", sub=formatCoord(coord),
	col.sub="gray", fun=mean, filter=c("none", "gaussian", "ma", "sgolay", "kaiser"),
	window=5, nlabels=if( isPeaks(x) ) 6 else 0, add=FALSE, ...)
{
	filter <- filterFunction(filter)
	if ( missing(pixel) && missing(coord) ) {
		stop("either 'pixel' or 'coord' must be specified")
	} else if ( missing(pixel) ) {
		pixel <- pixels(x, coord)
		coord <- coord(x)[pixel,]
	} else if ( missing(coord) ) {
		coord <- NULL
		coord <- coord(x)[pixel,]
	}
	if ( any(is.na(pixel)) ) stop("invalid pixel")
	if ( length(pixel) > 1 ) {
		spectra.plot <- apply(x@spectra$spectra[,pixel], 1, fun)
	} else {
		spectra.plot <- x@spectra$spectra[,pixel]
	}
	spectra.plot <- filter(spectra.plot, mz(x), window=window, ...)
	if ( add ) {
		suppressWarnings(points(mz(x), spectra.plot, xlab=xlab, ylab=ylab, pch=pch, cex=cex,
			type=type[[1]], col=col, sub=sub, col.sub=col.sub, ...))
	} else {
		suppressWarnings(plot(mz(x), spectra.plot, main=main, xlab=xlab, ylab=ylab, pch=pch, cex=cex,
			type=type[[1]], col=col, sub=sub, col.sub=col.sub, ...))
	}
	if ( isPeaks(x) && length(type) > 1 ) {
		points(mz(x), spectra.plot, type=type[[2]], col=col, pch=pch, cex=cex)
	}
	if ( nlabels > 0 ) {
		top.ions <- head(order(spectra.plot, decreasing=TRUE), n=nlabels)
		offset <- diff(range(spectra.plot)) / 75
		text(mz(x)[top.ions], offset + spectra.plot[top.ions], labels=round(mz(x)[top.ions], digits=2), cex=cex)
	}
	abline(h=0, lwd=0.2)
	invisible(NULL)
} )

setMethod("plot", c("MSImageSegmentation","missing"), function(x, which=1,
	mode=c("centroids", "tstatistics"), main=metaData(x)[["name"]],
	xlab="m/z", ylab=mode, col=rainbow(max(x[["nclasses"]][which])), type=c("h", "p"),
	sub=metaData(x)[["parnames"]][[which]], col.sub="gray", pch=20, cex=0.6,
	climits=FALSE, nlabels=6, class.mask=NULL, legend=TRUE, add=FALSE, ...)
{
	mode <- match.arg(mode)
	which <- match.which(x, which)
	if ( is.null(class.mask) ) class.mask <- 1:x$nclasses[[which]]
	if ( length(type) != 2 ) type <- rep(type, 2)
	if ( !add ) {
		plot(range(x@metaData$mz), range(x[[mode]][[which]][,class.mask]), type="n",
			main=main, xlab=xlab, ylab=ylab, sub=sub, col.sub=col.sub, ...)
	}
	if ( climits ) si <- calculateSE(x)
	for ( i in class.mask ) {
		intensities <- x[[mode]][[which]][,i]
		mz <- x@metaData$mz
		points(mz, intensities, type=type[[1]], pch=pch, cex=cex, lwd=0.2, col="gray")
		intensities <- intensities[x$tstatistics[[which]][,i] != 0]
		mz <- mz[x$tstatistics[[which]][,i] != 0]
		points(mz, intensities, type=type[[1]], pch=pch, cex=cex, lwd=0.2, col=col[[i]])
		points(mz, intensities, type=type[[2]], pch=pch, cex=cex, col=col[[i]])
		if ( climits && mode == "centroids") {
			se <- si[[which]][,i]
			se <- se[x$tstatistics[[which]][,i] != 0]
			points(mz, intensities+2*se, pch=2, cex=cex, col=col[[i]])
			points(mz, intensities-2*se, pch=6, cex=cex, col=col[[i]])
		}
	}
	if ( nlabels > 0 ) {
		selected <- abs(sign(x[["tstatistics"]][[which]][,class.mask,drop=FALSE]))
		top.ions <- apply(selected * x[[mode]][[which]][,class.mask,drop=FALSE], 1, function(yi) {
			j <- which.max(abs(yi))
			sign(yi[j]) * max(abs(yi))
		} )
		top <- head(order(abs(top.ions), decreasing=TRUE), n=nlabels)
		offset <- diff(range(x[[mode]][[which]][,class.mask])) / 50
		text(x@metaData$mz[top], sign(top.ions[top]) * offset + top.ions[top],
			labels=round(x@metaData$mz[top], digits=2), cex=cex, ...)
	}
	abline(h=0, lwd=0.2)
	if ( legend ) {
		legend("topright", legend=x@metaData$labels[1:length(col)], pch=pch, col=col,
			bg=rgb(1, 1, 1, 0.75))
	}
	invisible(NULL)
} )

setMethod("plot", c("MSPeakFrame", "missing"), function(x, dist=FALSE, xlab="m/z",
	ylab="intensity", nlabels=6, type=c("h", "p"), col="black", pch=20, cex=0.6,
	lty.dist=1, lwd.dist=0.5, col.dist="black", add=FALSE, ...)
{
	if ( nrow(x@peaks) == 0 ) stop("empty peak list")
	if ( length(type) != 2 ) type <- rep(type, 2)
	if ( !add ) plot(range(x$mz), range(x$intensity), type='n', xlab=xlab, ylab=ylab)
	points(x$mz, x$intensity, type=type[[1]], col=col, pch=pch, cex=cex)
	points(x$mz, x$intensity, type=type[[2]], col=col, pch=pch, cex=cex)
	abline(h=0, lwd=0.2)
	if ( dist ) {
		for ( i in 1:nrow(x@peaks) ) {
			mz.lower <- x$mu[i] - 3 * x$sigma[i]
			mz.upper <- x$mu[i] + 3 * x$sigma[i]
			mz <- seq(from=mz.lower, to=mz.upper, length.out=100)
			peak <- dnorm(mz, mean=x$mu[i], sd=x$sigma[i])
			peak <- peak * x$intensity[i] / max(peak)
			lines(mz, peak, col=col.dist, lty=lty.dist, lwd=lwd.dist)
		}
	}
	if ( nlabels > 0 ) {
		top.ions <- head(order(x$intensity, decreasing=TRUE), n=nlabels)
		offset <- max(x$intensity) / 75
		text(x$mz[top.ions], offset + x$intensity[top.ions], labels=round(x$mz[top.ions], digits=2), cex=cex)
	}
	invisible(NULL)
} )

setMethod("plot", c("MSPeakList", "missing"), function(x, dist=FALSE, nlabels=0,
	col=sample(rainbow(length(peaks(x)))), col.dist=col, add=FALSE, ...)
{
	x <- x[sapply(x@peaks, function(p) nrow(p@peaks)) > 0]
	plot(mergePeaks(x), type="n", nlabels=0, add=add)
	for ( i in seq_along(x@peaks) ) {
		plot(x[[i]], dist=dist, nlabels=nlabels, col=col[[i]],
			col.dist=col.dist[[i]], add=TRUE, ...)
	}
	invisible(NULL)
} )

#### helper functions ####

# makePlot <- function(plot) {
# 	if ( isTRUE(plot) || plot == "add" ) {
# 		TRUE
# 	} else {
# 		FALSE
# 	}
# }

# addPlot <- function(plot) {
# 	if ( plot == "add" ) {
# 		TRUE
# 	} else {
# 		FALSE
# 	}
# }
