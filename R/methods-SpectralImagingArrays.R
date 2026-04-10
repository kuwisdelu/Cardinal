
#### SpectralImagingArrays ####
## ----------------------------

# Class for a list of (unprocessed) spectra
# _without_ any aligned feature information
# following Vector semantics (length is # of elements/spectra)

SpectralImagingArrays <- function(spectraData = SimpleList(),
	pixelData = PositionDataFrame(), metadata = list(),
	centroided = NA, continuous = FALSE)
{
	spectraData <- SpectraArrays(spectraData)
	if ( length(spectraData) != 0L )
	{
		spectra <- spectraData[[1L]]
		if ( missing(pixelData) )
		{
			colnames <- colnames(spectra)
			coord <- expand.grid(x=seq_len(length(spectra)), y=1L)
			pixelData <- PositionDataFrame(coord=coord, row.names=colnames)
		}
	}
	new("SpectralImagingArrays",
		spectraData=spectraData,
		elementMetadata=pixelData,
		metadata=metadata,
		centroided=centroided,
		continuous=continuous,
		processingQueue=list(),
		processingVariables=character(),
		processingChunkSize=NA_integer_)
}

.valid_SpectralImagingArrays <- function(object)
{
	errors <- NULL
	if ( length(object@spectraData) > 0L )
	{
		nr_spectra <- nrow(object@spectraData)
		nr_pixelData <- nrow(object@elementMetadata)
		if ( length(dim(object@spectraData)) != 2L )
			errors <- c(errors, paste0("number of dimensions of spectraData [",
				length(dim(object@spectraData)), "] must be 2"))
		if ( nr_spectra != nr_pixelData )
			errors <- c(errors, paste0("number of rows in spectraData [",
				nr_spectra, "] must match number of rows in pixelData [",
				nr_pixelData, "]"))
		lens <- lapply(as.list(object@spectraData), lengths)
		lens <- lapply(lens, unname)
		lens_ok <- vapply(lens, identical, logical(1L), lens[[1L]])
		if ( !all(lens_ok) )
			errors <- c(errors, paste0("all spectra arrays must have ",
				"identical lengths for arrays belonging to the same pixels"))
	}
	if ( length(object@processingQueue) > 0L )
	{
		is_ps <- vapply(object@processingQueue, is, logical(1L), "ProcessingStep")
		if ( !all(is_ps) )
			errors <- c(errors, paste0("all processingQueue elements ",
				"must be ProcessingStep objects"))
	}
	if ( !all(object@processingVariables %in% names(object@elementMetadata)) )
	{
		errors <- c(errors, "processingVariables must be columns of pixelData")
	}
	if ( length(object@processingChunkSize) != 1L )
	{
		errors <- c(errors, "processingChunkSize must be numeric(1)")
	}
	if ( length(object@continuous) != 1L )
	{
		errors <- c(errors, "continuous must be logical(1)")
	}
	if ( isTRUE(object@continuous) && length(object@spectraData) > 0L )
	{
		lens <- lapply(as.list(object@spectraData), lengths)
		lens <- lapply(lens, unique)
		if ( length(unlist(unique(lens))) != 1L )
			errors <- c(errors, paste0("all spectra arrays must have",
				"identical lengths for all pixels when continuous=TRUE"))
	}
	if ( is.null(errors) ) TRUE else errors
}

setValidity("SpectralImagingArrays", .valid_SpectralImagingArrays)

.paste_head_tail <- function(x, n = 6L, collapse = ", ")
{
	paste0(.select_head_tail(x, n), collapse=collapse)
}

.print_queued_processing <- function(object)
{
	if ( length(processingData(object)) > 0L )
	{
		cat(sprintf("processingData(%d): %s\n", length(processingData(object)),
			.paste_head_tail(names(processingData(object)))))
	}
}

setMethod("show", "SpectralImagingArrays",
	function(object) {
		# dimensions
		label <- if(length(object) != 1L) "spectra" else "spectrum"
		cat(class(object), "with", length(object), label, "\n")
		# spectraData()
		cat(sprintf("spectraData(%d): %s\n", length(spectraData(object)),
			.paste_head_tail(names(spectraData(object)))))
		# pixelData()
		cat(sprintf("pixelData(%d): %s\n", length(pixelData(object)),
			.paste_head_tail(names(pixelData(object)))))
		# coord()
		if ( length(object) > 0L )
		{
			lims <- vapply(coord(object), range, numeric(2L))
			lims <- paste0(coordNames(object), " = ", lims[1L,], "...", lims[2L,])
			cat(sprintf("coord(%d): %s\n", length(coordNames(object)),
				.paste_head_tail(lims)))
		}
		# runNames()
		cat(sprintf("runNames(%d): %s\n", length(runNames(object)),
			.paste_head_tail(runNames(object))))
		# processingData()
		.print_queued_processing(object)
		# metadata()
		if ( length(metadata(object)) > 0L )
		{
			cat(sprintf("metadata(%d): %s\n", length(metadata(object)),
				.paste_head_tail(names(metadata(object)))))
		}
	})

# find spectra by position
setMethod("pixels", "SpectralImagingArrays",
	function(object, ..., coord, run, tolerance = NA,
		env = NULL)
	{
		if ( is.null(env) )
			env <- parent.frame(2)
		i <- callNextMethod(object, ..., env=env)
		if ( !missing(coord) || !missing(run) ) {
			pos <- .find_positions(object, coord, run, tolerance)
			i <- intersect(i, pos)
		}
		setNames(i, pixelNames(object)[i])
	})

.find_positions <- function(object, coord, run, tol = NA)
{
	index <- seq_len(nrow(pixelData(object)))
	if ( !missing(coord) && !is.null(coord) ) {
		if ( missing(run) )
			run <- NULL
		coord <- as.list(coord)
		if ( length(coord) != ncol(coord(object)) )
			.Error("length of coord [", length(coord), "] does not ",
				"match object coord [", ncol(coord(object)), "]")
		if ( is.null(names(coord)) ) {
			names(coord) <- coordNames(object)
		} else {
			coord <- coord[coordNames(object)]
		}
		coord <- as.data.frame(coord)
		if ( is.na(tol) )
			tol <- 0.5 * vapply(coord(object), estres, numeric(1L))
		i_coord <- kdsearch(coord, coord(object), tol=tol)
		if ( any(lengths(i_coord) != 1L) )
		{
			for ( j in which(lengths(i_coord) != 1L) )
			{
				badmatch <- paste0(names(coord), " = ",
					unlist(coord[j,]), collapse=", ")
				if ( length(i_coord[[j]]) > 1L ) {
					i_run <- run(object)[i_coord[[j]]]
					dup <- !setequal(unique(i_run), i_run)
					if ( dup )
						.Warn("multiple matches for coord ", badmatch)
				} else {
					k <- as.vector(knnsearch(coord[j,], coord(object), k=1L))
					k_coord <- as.list(coord(object)[k,])
					nearmatch <- paste0(coordNames(object), " = ",
						unlist(k_coord), collapse=", ")
					.Warn("no match for coord ", badmatch, "; ",
						"nearest is ", nearmatch)
				}
			}
		}
		i_coord <- unique(unlist(i_coord))
		index <- intersect(i_coord, index)
	}
	if ( !missing(run) && !is.null(run) ) {
		if ( missing(coord) )
			coord <- NULL
		if ( !is.character(run) && !is.factor(run) )
			run <- runNames(object)[run]
		i_run <- which(run(object) %in% run)
		index <- intersect(i_run, index)
	}
	index
}

# Spectra array access

setMethod("spectra", "SpectralImagingArrays",
	function(object, i = 1L, ..., BPPARAM = getCardinalBPPARAM())
	{
		if ( length(processingData(object)) > 0L ) {
			spectrapply(object, `[[`, i, ..., BPPARAM=BPPARAM)
		} else {
			callNextMethod()
		}
	})

setReplaceMethod("spectra", "SpectralImagingArrays",
	function(object, i = 1L, ..., value) {
		if ( length(processingData(object)) > 0L ) {
			.Error("can't replace spectra with queued processing steps")
		} else {
			callNextMethod()
		}
	})

## Basic getters and setters

setMethod("lengths", "SpectralImagingArrays",
	function(x, use.names = TRUE) {
		if ( length(spectraData(x)) > 0L ) {
			lengths(spectraData(x)[[1L]], use.names=use.names)
		} else {
			integer(0L)
		}
	})

setMethod("processingData", "SpectralImagingArrays",
	function(object, ...) object@processingQueue)
setReplaceMethod("processingData", "SpectralImagingArrays",
	function(object, ..., value) {
		object@processingQueue <- value
		if ( validObject(object) )
			object
	})

setMethod("processingChunkSize", "SpectralImagingArrays",
	function(object, ...) object@processingChunkSize)
setReplaceMethod("processingChunkSize", "SpectralImagingArrays",
	function(object, ..., value) {
		object@processingChunkSize <- value
		if ( validObject(object) )
			object
	})

setMethod("processingChunkFactor", "SpectralImagingArrays",
	function(object, ...)
{
	if ( is.na(processingChunkSize(object)) ) {
		.processingChunkFactor(length(object), getCardinalChunksize())
	} else {
		.processingChunkFactor(length(object), processingChunkSize(object))
	}
})

.processingChunkFactor <- function(length.out, chunkSize)
{
	if ( is.na(chunkSize) || chunkSize > length.out ) {
		f <- as.factor(rep.int(1L, length.out))
	} else {
		chunkIds <- seq_len(ceiling(length.out / chunkSize))
		f <- as.factor(rep(chunkIds, each=chunkSize, length.out=length.out))
	}
	f
}

## Vector-like subsetting

.subset_SpectralImagingArrays <- function(x, i)
{
	if ( is.character(i) || is.factor(i) )
		i <- match(i, pixelNames(x))
	x@spectraData <- x@spectraData[i,,drop=FALSE]
	x@elementMetadata <- x@elementMetadata[i,,drop=FALSE]
	if ( validObject(x) )
		x
}

setMethod("[", "SpectralImagingArrays",
	function(x, i, j, ..., drop = TRUE) {
		if ( !missing(drop) && isTRUE(drop) )
			.Warn("'drop' ignored when subsetting ", class(x))
		.subset_SpectralImagingArrays(x, i)
	})

setMethod("subset", "SpectralImagingArrays",
	function(x, subset, ...)
	{
		pdata <- as.env(pixelData(x), enclos=parent.frame(2))
		if ( !missing(subset) ) {
			i <- eval(substitute(subset), envir=pdata)
			if ( !is.logical(i) && !is.numeric(i) )
				.Error("'subset' must specify logical or numeric indices")
		}
		if ( missing(subset) ) {
			x
		} else {
			x[i]
		}
	})

## Iteration

.spectrapply_SpectralImagingArrays <- function(object, ITEMFUN, ...)
{
	X <- .list_SpectralImagingArrays(object, withProcessing=TRUE)
	lapply(X, ITEMFUN, ...)
}

.chunkapply_SpectralImagingArrays <- function(object, CHUNKFUN, ..., f, verbose)
{
	ITER <- .iter_SpectralImagingArrays(object, f, verbose)
	.bpiterate(ITER, CHUNKFUN, ...)
}

setMethod("spectrapply", "SpectralImagingArrays",
	function(object, FUN, ...,
		f = processingChunkFactor(object),
		REDUCE, init, reduce.in.order = TRUE,
		verbose = getCardinalVerbose(),
		BPPARAM = getCardinalBPPARAM())
	{
		if ( missing(REDUCE) )
			REDUCE <- c
		if ( missing(init) )
			init <- NULL
		.chunkapply_SpectralImagingArrays(object, ...,
			CHUNKFUN=.spectrapply_SpectralImagingArrays, ITEMFUN=FUN,
			REDUCE=REDUCE, init=init, reduce.in.order=reduce.in.order,
			f=f, verbose=verbose, BPPARAM=BPPARAM)
	})

.iter_SpectralImagingArrays <- function(x, f, verbose)
{
	if ( !is.factor(f) || length(f) != length(x) )
		stop("'f' must be a factor along 'x'")
	i <- 1L
	function() {
		if ( i == 1L )
			.Log("iterating over ", nlevels(f), " chunk(s)", message=verbose)
		if ( i <= nlevels(f) ) {
			.Log("processing chunk ", sQuote(levels(f)[i]), message=verbose)
			fi <- which(f == levels(f)[i])
			chunk <- .subset_SpectralImagingArrays(x, fi)
		} else {
			chunk <- NULL
		}
		i <<- i + 1
		chunk
	}
}

.list_SpectralImagingArrays <- function(object, withProcessing)
{
	X <- .zipup(spectraData(object))
	if ( length(object@processingQueue) > 0L && withProcessing ) {
		.process_spectra_list(X,
			queue=object@processingQueue,
			mcols=pixelData(object)[object@processingVariables])
	} else {
		X
	}
}

## combine

.bind_SpectralImagingArrays <- function(x, y)
{
	if ( !is.null(x) && !is.null(y) ) {
		new(class(x), x,
			spectraData=c(spectraData(x), spectraData(y)),
			elementMetadata=rbind(pixelData(x), pixelData(y)),
			metadata=c(metadata(x), metadata(y)),
			centroided=centroided(x) && centroided(y),
			continuous=NA,
			processingQueue=list(),
			processingVariables=character(),
			processingChunkSize=NA_integer_)
	} else if ( is.null(x) ) {
		y
	} else if ( is.null(y) ) {
		x
	} else {
		NULL
	}
}

setMethod("combine", c("SpectralImagingArrays", "SpectralImagingArrays"),
	function(x, y, ...) .bind_SpectralImagingArrays(x, y))

setMethod("combine", c("SpectralImagingArrays", "NULL"),
	function(x, y, ...) x)

setMethod("combine", c("NULL", "SpectralImagingArrays"),
	function(x, y, ...) y)

setMethod("c", "SpectralImagingArrays",
	function(x, ...)
	{
		objects <- list(...)
		if ( length(objects) > 0L ) {
			for ( i in seq_along(objects) )
				x <- combine(x, objects[[i]])
		}
		x
	})
