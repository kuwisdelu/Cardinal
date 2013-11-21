
#### implement the log-likelihood plotting methods ####

setMethod("likPlot", "MSImageSegmentation", function(object,
	output=c("loglik", "aic", "bic", "nclasses"),
	col=rainbow(max(metaData(object)[["levels"]])),
	layout, ...)
{
	k <- metaData(object)$parameters$k
	s <- metaData(object)$parameters$s
	nc <- object$nclasses
	lik <- logLik(object)
	aic <- -2 * lik + 2 * attr(lik, "df")
	bic <- -2 * lik + attr(lik, "df") * log(attr(lik, "nobs"))
	if ( missing(layout) ) {
		layout <- matrix(1:4, ncol=2, nrow=2, byrow=TRUE)
	} else {
		layout <- matrix(1:4, ncol=layout[[1]], nrow=layout[[2]], byrow=TRUE)
	}
	layout(layout)
	if ( "loglik" %in% output ) {
		plot(range(s), range(lik, na.rm=TRUE), type="n", xlab="sparsity parameter", ylab="log-likelihood", main="Likelihood")
		for ( ki in unique(k) ) {
			points(unique(s), lik[k == ki], type="b", col=col[ki])
		}
		legend("topright", legend=paste("k =", unique(k)), fill=col[unique(k)])
	}
	if ( "aic" %in% output ) {
		plot(range(s), range(aic, na.rm=TRUE), type="n", xlab="sparsity parameter", ylab="AIC", main="AIC")
		for ( ki in unique(k) ) {
			points(unique(s), aic[k == ki], type="b", col=col[ki])
		}
		legend("topright", legend=paste("k =", unique(k)), fill=col[unique(k)])
	}
	if ( "bic" %in% output ) {
		plot(range(s), range(bic, na.rm=TRUE), type="n", xlab="sparsity parameter", ylab="BIC", main="BIC")
		for ( ki in unique(k) ) {
			points(unique(s), bic[k == ki], type="b", col=col[ki])
		}
		legend("topright", legend=paste("k =", unique(k)), fill=col[unique(k)])
	}
	if ( "nclasses" %in% output ) {
		plot(range(s), c(1, max(k)), type="n", xlab="sparsity parameter", ylab="classes", main="Number of Classes")
		for ( ki in unique(k) ) {
			points(unique(s), nc[k == ki], type="b", col=col[ki])
		}
		legend("topright", legend=paste("k =", unique(k)), fill=col[unique(k)])
	}
	layout(1)
} )

