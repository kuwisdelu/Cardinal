
setMethod("spatialApply", "SparseImagingExperiment",
	function(.object, .r, .fun, ...,
			.blocks = FALSE,
			.simplify = TRUE,
			.use.names = TRUE,
			.outpath = NULL,
			.params = NULL,
			.init = NULL,
			BPREDO = list(),
			BPPARAM = bpparam())
	{
		.checkForIncompleteProcessing(.object)
		.fun <- match.fun(.fun)
		output <- !is.null(.outpath)
		if ( is.list(.init) ) {
			spatial <- .init$spatial
		} else {
			spatial <- .spatialInfo(.object, r=.r, weights=FALSE)
		}
		if ( .blocks ) {
			if ( !is.logical(.simplify) )
				reduce_blocks <- match.fun(.simplify)
			.simplify <- !is.logical(.simplify)
			if ( !is.numeric(.blocks) )
				.blocks <- getOption("Cardinal.nblocks")
			if ( is.list(.init) ) {
				idx <- .init$idx
			} else {
				idx <- .findSpatialBlocks(.object, groups=run(.object),
					nblocks=.blocks, neighbors=spatial$neighbors)
			}
		} else {
			if ( is.list(.init) ) {
				idx <- .init$idx
			} else {
				idx <- spatial$neighbors
				for ( i in seq_along(idx) )
					attr(idx[[i]], "centers") <- i
			}
		}
		pid <- ipcid()
		if ( output ) {
			.outpath <- .outpath[1L]
			.message("using outpath = ", .outpath)
			rwrite <- .remote_writer(pid, .outpath)
		}
		progress <- is(BPPARAM, "SerialParam") && !bpprogressbar(BPPARAM)
		if ( progress )
			.message(progress="start", max=length(idx))
		ans <- bplapply(idx, function(i) {
			suppressPackageStartupMessages(require(Cardinal))
			x <- iData(.object)[,i,drop=FALSE]
			ci <- attr(i, "centers")
			ni <- lapply(spatial$neighbors[ci], match, i)
			attr(x, "idx") <- i
			attr(x, "mcols") <- fData(.object)
			attr(x, "centers") <- match(ci, i)
			attr(x, "neighbors") <- ni
			attr(x, "offsets") <- spatial$offsets[ci]
			attr(x, "params") <- .params[ci]
			res <- .fun(x, ...)
			if ( output )
				res <- rwrite(res)
			if ( progress )
				.message(progress="increment")
			res
		}, BPREDO=BPREDO, BPPARAM=BPPARAM)
		if ( progress )
			.message(progress="stop")
		if ( output )
			ans <- .remote_collect(ans, .outpath, .simplify)
		if ( .use.names && !.blocks ) {
			if ( output && is(ans, "matter_mat") ) {
				colnames(ans) <- pixelNames(.object)
			} else {
				names(ans) <- pixelNames(.object)
			}
		}
		if ( .simplify ) {
			if ( .blocks ) {
				ci <- lapply(idx, attr, "centers")
				attr(ans, "idx") <- ci
				ans <- reduce_blocks(ans)
			} else if ( !output ) {
				ans <- drop(simplify2array(ans))
			}
		}
		if ( isTRUE(.init) ) {
			init <- list(spatial=spatial, idx=idx)
			attr(ans, "init") <- init
		}
		ipcremove(pid)
		ans
	})
