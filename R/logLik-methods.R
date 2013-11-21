
#### implement log-likelihood ####

setMethod("logLik", "MSImageSegmentation", function(object, ...) {
	logLik <- sapply(object$probabilities, function(prob) {
		phat <- apply(prob, 1, max)
		sum(log(phat))
	} )
	class(logLik) <- "logLik"
	attr(logLik, "df") <- object$dof
	attr(logLik, "nobs") <- sapply(object$classes, length)
	attr(logLik, "names") <- metaData(object)[["parnames"]]
	return(logLik)
} )
