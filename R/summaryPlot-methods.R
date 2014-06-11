
# #### implement summary plotting methods ####

# setMethod("summaryPlot", c("MSImageSegmentation", "MSImageSet"), function(object, reference,
# 	which=1, class.mask=1:object[["nclasses"]][[which]], true.labels=FALSE, mask.missing=FALSE,
# 	n=6, mode=c("classes", "probabilities"), output=c("centroids", "tstatistics"), main=metaData(object)[["name"]],
# 	sliceDimNames=metaData(reference)[["coordDimNames"]][c(1,2)], fixCoord=list(),
# 	col=rainbow(max(object[["nclasses"]][which])), layout, ...)
# {
# 	which <- match.which(object, which)
# 	nplots <- n + length(output)
# 	if ( missing(layout) ) {
# 		nrow <- max(ceiling(sqrt(nplots)), 2)
# 		ncol <- ceiling(nplots / ceiling(sqrt(nplots)))
# 	} else {
# 		nrow <- layout[[2]]
# 		ncol <- layout[[1]]
# 	}
# 	layout <- matrix(1:(nrow * ncol), nrow=nrow, ncol=ncol, byrow=TRUE)
# 	layout(layout)
# 	mar.old <- par()$mar
# 	par(mar=c(2.5,2.5,1.5,1.5))
# 	if ( is.character(class.mask) && !is.null(object$labels) ) {
# 		class.mask <- which(levels(object$labels) %in% class.mask)
# 	}
# 	for ( i in class.mask ) {
# 		mzs <- summary(object, which=which, n=n, class.mask=i)[[1]]$mz
# 		if ( length(mzs) > object$nfeatures[[which]][i] ) {
# 			mzs <- mzs[object$nfeatures[[which]][i]]
# 		}
# 		image(object, which=which, mode=mode, class.mask=i, col=col, legend=FALSE,
# 			sliceDimNames=sliceDimNames, fixCoord=fixCoord, main=main,
# 			true.labels=true.labels, mask.missing=mask.missing)
# 		if ( "centroids" %in% output ) {
# 			irange <- range(object[["centroids"]][[which]])
# 			plot(object, which=which, mode="centroids", class.mask=i, col=col,
# 				main="Mean Spectrum", ylim=irange, nlabels=n, legend=FALSE)
# 			abline(v=mzs, lty=3, lwd=0.2)
# 		}
# 		if ( "tstatistics" %in% output ) {
# 			tmax <- max(abs(object[["tstatistics"]][[which]]))
# 			trange <- c(-tmax, tmax)
# 			plot(object, which=which, mode="tstatistics", class.mask=i, col=col,
# 				main="t-statistics", ylim=trange, nlabels=n, legend=FALSE)
# 			abline(v=mzs, lty=3, lwd=0.2)
# 		}
# 		for ( mzi in mzs ) {
# 			image(reference, mz=mzi, main=formatMZ(mzi),
# 				sliceDimNames=sliceDimNames, fixCoord=fixCoord, ...)
# 		}
# 	}
# 	par(mar=mar.old)
# 	layout(1)
# } )
