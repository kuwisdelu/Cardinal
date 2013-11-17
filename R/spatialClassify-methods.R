
#### implement shorthand for spatially-aware classification with s = 0 ####

setMethod("spatialClassify", "MSImageSet", function(object, labels, r,
	method=c("gaussian", "adaptive"), autoDimNames=NULL, ...)
{
	outlist <- spatialSparseClassify(object, labels=labels, r=r, s=0,
		method=method, autoDimNames=autoDimNames, ...)
	return(outlist)
} )

#### implement methods for spatially-aware feature-sparse classification ####

setMethod("spatialSparseClassify", "MSImageSet", function(object, labels, r, s,
	method=c("gaussian", "adaptive"), autoDimNames=NULL, ...)
{
	method <- match.arg(method)
	priors <- calculateClassSizes(labels, na.rm=TRUE)
	priors <- rep(list(priors / sum(priors)), length(s))
	# prepare for classification
	s <- sort(s)
	nonmissing <- is.finite(as.integer(labels))
	classes <- labels[nonmissing]
	spectra <- object@spectra$spectra[,nonmissing,drop=FALSE]
	# calculate the shrunken centroids
	tryVerboseMessage("Calculating shrunken centroids...", precedes.progress.output=FALSE)
	results <- lapply(s, function(si) {
		tryVerboseMessage("r = ", r, ", s = ", si, "...", precedes.progress.output=FALSE)
		calculateShrunkenCentroids(spectra, classes=classes, s=si, isNxP=FALSE)
	} )
	# aggregate the results
	names(results) <- paste("r = ", r, ", k = ", rep(length(unique(classes)),
		length(results)), ", s = ", s, sep="")
	outlist <- list(centroids=sapply(results, function(x) x$centroids, simplify=FALSE, USE.NAMES=TRUE),
		tstatistics=sapply(results, function(x) x$tstatistics, simplify=FALSE, USE.NAMES=TRUE),
		sd=sapply(results, function(x) x$sd, simplify=FALSE, USE.NAMES=TRUE),
		dof=sapply(results, function(x) sum(abs(x$tstatistics) > 0) + numFeatures(object), USE.NAMES=TRUE),
		nclasses=sapply(results, function(x) ncol(x$centroids), USE.NAMES=TRUE),
		nfeatures=sapply(results, function(x) apply(x$tstatistics, 2, function(t) sum(abs(t) > 0)), simplify=FALSE, USE.NAMES=TRUE))
	outlist$priors <- priors
	names(outlist$priors) <- names(results)
	metaData <- list(name=metaData(object)[["name"]], mz=mz(object),
		coord=coord(object), r=r, method=method, autoDimNames=autoDimNames,
		levels=1:length(unique(classes)), parnames=names(results), npar=length(results))
	metaData$parameters <- data.frame(r=r, k=rep(length(unique(classes)), length(results)), s=s)
	if ( is.factor(labels) ) {
		metaData$labels <- levels(labels)
		metaData$isfactor <- TRUE
	} else {
		metaData$labels <- paste(1:length(unique(classes)))
		metaData$isfactor <- FALSE
	}
	fit <- new("MSImageSegmentation", .Data=outlist, metaData=metaData)
	attr(fit, "names") <- names(outlist)
	# return the fitted segmentation
	fit <- predict(fit, newx=object, newy=labels, autoDimNames=autoDimNames)
	return(fit)
} )
