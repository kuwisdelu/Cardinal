
#### Spatially-aware Gaussian mixture models ####
## ---------------------------------------------

setMethod("spatialDGMM", "ANY",
	function(x, coord, i, r = 1, k = 2, groups = NULL,
		weights = c("gaussian", "adaptive"),
		neighbors = findNeighbors(coord, r=r, groups=groups),
		annealing = TRUE, compress = TRUE, byrow = FALSE,
		verbose = getCardinalVerbose(), chunkopts = list(),
		BPPARAM = getCardinalBPPARAM(), ...)
{
	weights <- match.arg(weights)
	if ( "method" %in% ...names() ) {
		.Deprecated(old="method", new="weights")
		weights <- list(...)$method
	}
	if ( is.character(weights) ) {
		.Log("computing ", weights, " weights",
			message=verbose)
		nbwts <- spatialWeights(x=x,
			coord=coord, r=r, byrow=!byrow,
			weights=weights, neighbors=neighbors,
			verbose=verbose, chunkopts=chunkopts,
			BPPARAM=BPPARAM)
	} else {
		.Log("using custom weights",
			message=verbose)
		nbwts <- rep_len(weights, length(neighbors))
		weights <- "custom"
	}
	if ( !missing(i) && !is.null(i) ) {
		drop <- if (is.matter(x) || is.sparse(x)) NULL else FALSE
		if ( byrow ) {
			x <- x[i,,drop=drop]
		} else {
			x <- x[,i,drop=drop]
		}
	} else {
		i <- if (byrow) seq_len(nrow(x)) else seq_len(ncol(x))
	}
	k <- rev(sort(k))
	ans <- vector("list", length=length(k))
	for ( j in seq_along(k) )
	{
		lab <- if (length(i) != 1L) "models" else "model"
		.Log("fitting spatial Gaussian mixture ",
			lab, " for k = ", k[j],
			message=verbose)
		ans[[j]] <- sgmixn(NULL, NULL, x, r=r, k=k[j], group=groups,
			weights=nbwts, neighbors=neighbors, byrow=byrow,
			annealing=annealing, compress=compress,
			verbose=verbose, chunkopts=chunkopts,
			BPPARAM=BPPARAM, ...)
		ans[[j]]$weights <- weights
		ans[[j]]$r <- r
		ans[[j]]$k <- k[j]
	}
	names(ans) <- paste0("k=", k)
	lab <- if (length(k) != 1L || length(i) != 1L) "models" else "model"
	.Log("returning spatial Gaussian mixture ", lab,
		message=verbose)
	if ( length(ans) > 1L ) {
		ResultsList(ans,
			mcols=DataFrame(r=r, k=k, weights=weights))
	} else {
		ans[[1L]]
	}
})

setMethod("spatialDGMM", "SpectralImagingExperiment", 
	function(x, i, r = 1, k = 2, groups = run(x),
		weights = c("gaussian", "adaptive"),
		neighbors = findNeighbors(coord(x), r=r, groups=groups), ...)
{
	if ( length(processingData(x)) > 0L )
		.Warn("pending processing steps will be ignored")
	ans <- spatialDGMM(spectra(x),
		coord=coord(x), i=i, r=r, k=k, groups=groups,
		neighbors=neighbors, weights=weights, byrow=TRUE, ...)
	if ( missing(i) || is.null(i) ) {
		f <- function(a) as(SpatialResults(a, x), "SpatialDGMM")
	} else {
		f <- function(a) as(SpatialResults(a, x,
			featureData=featureData(x)[i,,drop=FALSE],
			pixelData=pixelData(x)), "SpatialDGMM")
	}
	if ( is(ans, "ResultsList") ) {
		ResultsList(lapply(ans, f), mcols=mcols(ans))
	} else {
		f(ans)
	}
})

setMethod("logLik", "SpatialDGMM",
	function(object, ...) logLik(object@model))

setMethod("plot", c(x = "SpatialDGMM", y = "missing"),
	function(x, i = 1L, type = "density",
		layout = NULL, free = "", ...)
{
	type <- match.arg(type)
	ndim <- length(dim(x$mu))
	mu <- apply(x$mu, ndim, identity, simplify=FALSE)
	sigma <- apply(x$sigma, ndim, identity, simplify=FALSE)
	mu <- mu[i]
	sigma <- sigma[i]
	plots <- Map(.plot_density, mu, sigma, MoreArgs=list(...))
	if ( is.null(featureNames(x)) ) {
		if ( is.null(names(i)) ) {
			names(plots) <- paste0("i = ", i)
		} else {
			names(plots) <- names(i)
		}
	} else {
		names(plots) <- featureNames(x)[i]
	}
	if ( !is.null(layout) ) {
		layout <- rep_len(layout, 2L)
		nrow <- layout[1L]
		ncol <- layout[2L]
		as_facets(plots, nrow=nrow, ncol=ncol, free=free)
	} else {
		as_facets(plots, free=free)
	}
})

setMethod("image", c(x = "SpatialDGMM"),
	function(x, i = 1L, type = "class",
		layout = NULL, free = "", ...)
{
	type <- match.arg(type)
	FUN <- function(y, ...) .plot_image_results(x, y=as.factor(y), ...)
	plots <- lapply(x$class[i], FUN, ...)
	if ( is.null(featureNames(x)) ) {
		names(plots) <- paste0("i = ", i)
	} else {
		names(plots) <- featureNames(x)[i]
	}
	if ( !is.null(layout) ) {
		layout <- rep_len(layout, 2L)
		nrow <- layout[1L]
		ncol <- layout[2L]
		as_facets(plots, nrow=nrow, ncol=ncol, free=free)
	} else {
		as_facets(plots, free=free)
	}
})

.plot_density <- function(mu, sigma, n = 256L,
	xlab = "", ylab = "Density", col = NULL,
	xlim = NULL, ylim = NULL, key = TRUE,
	grid = TRUE, engine = NULL, ...)
{
	plot <- vizi()
	cls <- factor(colnames(mu))
	for ( i in seq_len(nrow(mu)) )
	{
		for ( j in seq_len(ncol(mu)) )
		{
			lower <- mu[i,j] - 3 * sigma[i,j]
			upper <- mu[i,j] + 3 * sigma[i,j]
			x <- seq(lower, upper, length.out=n)
			px <- dnorm(x, mean=mu[i,j], sd=sigma[i,j])
			plot <- add_mark(plot, "lines",
				x=x, y=px, color=cls[j])
		}
	}
	plot <- set_coord(plot, xlim=xlim, ylim=ylim, grid=grid)
	plot <- set_channel(plot, "x", label=xlab)
	plot <- set_channel(plot, "y", label=ylab)
	plot <- set_channel(plot, "color", label="\n",
		limits=levels(cls), scheme=col, key=key)
	if ( !is.null(engine) )
		plot <- set_engine(plot, engine)
	plot <- set_par(plot, ...)
	plot
}

