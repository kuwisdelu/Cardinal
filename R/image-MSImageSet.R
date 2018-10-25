
#### Image plotting for MSImageSet ####

setMethod("image",
	signature = c(x = "MSImageSet"),
	function(x, formula = ~ x * y,
		feature = features(x, mz=mz),
		feature.groups,
		mz,
		plusminus,
		...)
	{
		if ( !missing(mz) && missing(feature.groups) ) {
			feature.groups <- featureNames(x)[feature]
			if ( !missing(plusminus) && plusminus != 0 ) {
				newfeatures <- lapply(mz, function(mzi) {
					seq(from=features(x, mz=mzi-plusminus),
						to=features(x, mz=mzi+plusminus),
						by=sign(plusminus))
				})
				feature.groups <- unlist(mapply(function(group, feature) {
					rep(group, length(feature))
				}, feature.groups, newfeatures))
				feature <- unlist(newfeatures)
			}
		} else if ( missing(feature.groups) ) {
			feature.groups <- NULL
		}
		if ( missing(feature) && missing(mz) && missing(formula) )
			.stop("image: 'feature' or 'mz' must be specified")
		callNextMethod(x,
			formula=formula,
			feature=feature,
			feature.groups=feature.groups,
			...)
	})
