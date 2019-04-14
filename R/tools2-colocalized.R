
# not exported yet

setMethod("colocalized",
	signature = c("SpatialDGMM", "logical"),
	function(object, ref, ...)
	{
		# do something
	})

.spatialDGMM_match <- function(results, ref) {
	groups <- pData(results)[[metadata(results)$groupsName]]
	lapply(resultData(results), function(res) {
		scores <- sapply(1:nlevels(res$class), function(i) {
			cl <- res$class == levels(res$class)[i]
			mu <- mean(res$params$mean[as.integer(res$class)])
			sgn <- sign(res$params$mean[i] - mu)
			sgn * Mscore(cl, ref)
		})
		res$params[,"score"] <- scores
		res$params
	})
}
