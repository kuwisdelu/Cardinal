
# #### implement methods for creating a factor from ROIs ####

# setMethod("roiBind", "logical", function(object, ..., labels=NULL,
# 	na.replace=FALSE, ordered=FALSE)
# {
# 	rois <- as.integer(rep(NA, length(object)))
# 	oblist <- list(object, ...)
# 	for ( i in seq_along(oblist) ) {
# 		rois[oblist[[i]]] <- i
# 	}
# 	if ( na.replace ) {
# 		rois[is.na(rois)] <- length(oblist) + 1
# 		levels <- 1:(length(oblist) + 1)
# 	} else {
# 		levels <- seq_along(oblist)
# 	}
# 	if ( is.null(labels) ) labels <- levels
# 	return(factor(rois, levels=levels, labels=labels))
# } )
