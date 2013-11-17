
#### implement summary methods ####

setMethod("summary", "MSImageSegmentation", function(object,
	which=metaData(object)[["parnames"]], n=20, reshape=TRUE,
	sort.by=c("tstatistic", "intensity", "p.value", "mz"),
	class.mask=NULL, ...)
{
	outlist <- list()
	sort.by <- match.arg(sort.by)
	which <- match.which(object, which)
	if ( n == Inf ) {
		n <- sapply(object$tstatistics, length)
	} else {
		n <- rep(n, length(object$tstatistics))
	}
	if ( is.numeric(class.mask) && !is.null(object$labels) ) {
		class.mask <- levels(object$labels)[[class.mask]]
	}
	for ( i in seq_along(object$classes[which]) ) {
		outlist[[i]] <- data.frame(mz=rep(object@metaData$mz,
			ncol(object$tstatistics[[which[i]]])))
		outlist[[i]]$class <- factor(rep(1:ncol(object$tstatistics[[which[i]]]),
			each=nrow(object$tstatistics[[which[i]]])), levels=object@metaData$levels,
			labels=object@metaData$labels)
		outlist[[i]]$intensity <- as.vector(object$centroid[[which[i]]])
		outlist[[i]]$se <- as.vector(calculateSE(object)[[which[i]]])
		outlist[[i]]$tstatistic <- as.vector(object$tstatistics[[which[i]]])
		nsize <- calculateClassSizes(object$classes[[which[i]]])
		outlist[[i]]$p.value <- 2 * (1 - pt(abs(as.vector(object$tstatistics[[which[i]]])),
			df=rep(nsize, each=nrow(object$tstatistics[[which[i]]]))))
		outlist[[i]]$adj.p.value <- p.adjust(outlist[[i]]$p.value, method="BH")
		if ( reshape ) {
			outlist[[i]] <- reshape(outlist[[i]], v.names=c("intensity", "se",
				"tstatistic", "p.value", "adj.p.value"), timevar="class", idvar="mz",
				direction="wide")
			if ( is.null(class.mask) ) {
				ordering <- switch(sort.by,
					"mz" = head(order(outlist[[i]]["mz"]), n=n[[i]]),
					"p.value" = head(order(apply(outlist[[i]][,grep("p.value",
						names(outlist[[i]]))], 1, min), (-1) * apply(abs(outlist[[i]][,grep("tstatistic",
						names(outlist[[i]]))]), 1, max)), n=n[[i]]),
					"intensity" = head(order(apply(outlist[[i]][grep("intensity",
						names(outlist[[i]]))], 1, max), decreasing=TRUE), n=n[[i]]),
					"tstatistic" = head(order(apply(outlist[[i]][,grep("tstatistic",
						names(outlist[[i]]))], 1, max), decreasing=TRUE), n=n[[i]])
				)
			} else {
				ordering <- switch(sort.by,
					"mz" = head(order(outlist[[i]]["mz"]), n=n[[i]]),
					"p.value" = head(order(apply(outlist[[i]][,paste("p.value",
						class.mask, sep="."), drop=FALSE], 1, min),
						(-1) * apply(abs(outlist[[i]][,paste("tstatistic",
						class.mask, sep="."), drop=FALSE]), 1, max)), n=n[[i]]),
					"intensity" = head(order(apply(outlist[[i]][paste("intensity",
						class.mask, sep="."), drop=FALSE], 1, max), decreasing=TRUE), n=n[[i]]),
					"tstatistic" = head(order(apply(outlist[[i]][,paste("tstatistic",
						class.mask, sep="."), drop=FALSE], 1, max), decreasing=TRUE), n=n[[i]])
				)
			}
		} else {
			ordering <- switch(sort.by,
				"mz" = head(order(outlist[[i]]["mz"]), n=n[[i]]),
				"p.value" = head(order(outlist[[i]]["p.value"], (-1) * abs(outlist[[i]]["tstatistic"])),
					n=n[[i]]),
				"tstatistic" = head(order(outlist[[i]]["tstatistic"],
					decreasing=TRUE), n=n[[i]]),
				"intensity" = head(order(outlist[[i]]["intensity"],
					decreasing=TRUE), n=n[[i]])
			)
		}
		outlist[[i]] <- outlist[[i]][ordering,]
	}
	names(outlist) <- names(object$classes[which])
	return(outlist)
} )

setMethod("summary", "CrossValidation", function(object, ...) {
	summary <- as.data.frame(sapply(object, function(z) z$MSEP,
		simplify=FALSE, USE.NAMES=TRUE))
	summary$CV <- rowMeans(summary)
	return(summary)
} )

#### helper functions ####

calculateSE <- function(object) {
	p <- length(object@metaData$mz)
	if ( is.null(object$labels) ) {
		classes <- object$classes
	} else {
		classes <- rep(list(object$labels[!is.na(object$labels)]), length(object$classes))
	}
	se <- list()
	for ( i in seq_along(classes) ) {
		si <- object$sd[[i]]
		s0 <- median(si)
		mk <- sqrt(1 / calculateClassSizes(classes[[i]]) - 1 / length(classes[[i]]))
		se[[i]] <- rep(mk, each=p) * (si + s0)
		dim(se[[i]]) <- c(p, length(unique(classes[[i]])))
	}
	names(se) <- names(object$classes)
	se
}
