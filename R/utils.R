
# Check for no missing values
nomissing <- function(x) {
	all(!is.na(x))
}

# Check for all missing values
allmissing <- function(x) {
	all(is.na(x))
}

# Check for whole numbers in a vector
is.wholenumber <- function(x, tol = .Machine$double.eps^0.5)  {
	if ( is.numeric(x) ) {
		abs(x - round(x)) < tol
	} else {
		FALSE
	}
}

# Check if a variable is discrete
is.discrete <- function(x) {
	is.factor(x) || is.character(x)
}

# Check if a variable is continuous
is.numeric_vector <- function(x)  {
	is.numeric(x) && is.vector(x)
}

# Round to an arbitrary precision
roundnear <- function(x, precision=0.1) {
	round(x / precision) * precision
}

# L1 norm of a vector
l1norm <- function(x) sum(abs(x))

# L2 norm of a vector
l2norm <- function(x) sqrt(sum(x^2))

# Soft thresholding
soft <- function(x, delta) sign(x) * pmax(0, abs(x) - delta)

# Regularize a matrix of probabilities
regpr <- function(x, lambda = 0.1) {
	x <- x + lambda
	x / rowSums(x)
}

# Cube root
cbrt <- function(x) x^(1/3)

# Return positive part
pos <- function(x) pmax(x, 0)

# Return negative part
neg <- function(x) pmin(x, 0)

# Combined (pooled) mean of multiple subsets
combinedMean <- function(xbar, n) {
	sum(xbar * n) / sum(n)
}

# Combined (pooled) variance of multiple subsets
combinedVar <- function(xbar, var, n) {
	xc <- combinedMean(xbar, n)
	(sum(n * var) + sum(n * (xbar - xc)^2)) / sum(n)
}

# Grouped mean, e.g., of a histogram
groupMean <- function(x, f) {
	if ( any(f < 0) ) f[f < 0] <- 0
	n <- sum(f)
	sum(f * x) / n
}

# Grouped variance, e.g., of a histogram
groupVar <- function(x, f) {
	if ( any(f < 0) ) f[f < 0] <- 0
	n <- sum(f)
	xbar <- sum(f * x) / n
	Sfx2 <- sum(f * x^2)
	(Sfx2 / n) - xbar^2
}

# Mode of a vector
Mode <- function(x, na.rm = FALSE) {
	if ( na.rm )
		x <- x[!is.na(x)]
	ux <- unique(x)
	ux[which.max(tabulate(match(x, ux)))]
}

# Kurtosis of a vector
kurtosis <- function(x, na.rm=FALSE) {
	if ( na.rm ) x <- x[!is.na(x)]
	n <- length(x)
	n * sum((x - mean(x))^4) / (sum((x - mean(x))^2)^2)
}

# Bisection search along a sequence
bisection.seq <- function(x, fun, ..., iter.max=20, epsilon=1e-6) {
	if ( fun(x[1], ...) < fun(x[length(x)], ...) ) {
		lo <- 1
		hi <- length(x)
		to.lo <- floor
		to.hi <- ceiling
	} else {
		lo <- length(x)
		hi <- 1
		to.lo <- ceiling
		to.hi <- floor
	}
	i <- round((lo + hi) / 2, digits=0)
	iter <- 1
	while ( iter <= iter.max && l2norm(fun(x[i], ...)) > epsilon ) {
		if ( fun(x[i], ...) > 0 ) {
			hi <- i
			i <- to.lo((lo + hi) / 2)
		} else {
			lo <- i
			i <- to.hi((lo + hi) / 2)
		}
		iter <- iter + 1
	}
	i
}

# Sensitivity (true positive rate)
sensitivity <- function(ref, pred, positive = levels(ref)[1]) {
	nas <- is.na(ref) | is.na(pred)
	if ( any(nas) ) {
		ref <- ref[!nas]
		pred <- pred[!nas]
	}
	real_pos <- ref %in% positive
	pred_pos <- pred %in% positive
	sum(real_pos & pred_pos) / sum(real_pos)
}

# Specificity (true negative rate)
specificity <- function(ref, pred, positive = levels(ref)[1]) {
	nas <- is.na(ref) | is.na(pred)
	if (any(nas)) {
		ref <- ref[!nas]
		pred <- pred[!nas]
	}
	real_neg <- !ref %in% positive
	pred_neg <- !pred %in% positive
	sum(real_neg & pred_neg) / sum(real_neg)
}

# Vectorized maximum
is.max <- function(x) {
	i <- seq_along(x)
	i == which.max(x)
}

# Vectorized maximum
is.min <- function(x) {
	i <- seq_along(x)
	i == which.min(x)
}

# Match score between two logical vectors
Mscore <- function(a, b, type=3) {
	switch(type,
		sum(a & b, na.rm=TRUE) / sum(a, na.rm=TRUE),
		sum(a & b, na.rm=TRUE) / sum(b, na.rm=TRUE),
		sum(a & b, na.rm=TRUE) / sum(a | b, na.rm=TRUE))
}

# Returns a list of approximately even subsets of a vector
split_blocks <- function(x, blocks) {
	blocksize <- max(1L, length(x) / blocks)
	n <- floor(length(x) / blocksize) + 1L
	ints <- floor(seq(from=1L, to=length(x) + 1L, length.out=n))
	begin <- ints[-length(ints)]
	end <- ints[-1L] - 1L
	mapply(function(i, j) x[i:j],
		begin, end, SIMPLIFY=FALSE)
}

# Affine transformation on a data.frame of coordinates
affine <- function(x, translate=c(0,0), rotate=0,
	angle=c("degrees", "radians"), grid=TRUE)
{
	angle <- match.arg(angle)
	theta <- -rotate
	if ( angle == "degrees" ) theta <- theta * pi / 180
	# translate center of mass to be near origin
	tt <- sapply(x, function(xs) mean(xs))
	new.x <- t(as.matrix(x)) - tt
	# rotate around origin
	A <- matrix(c(cos(theta), sin(theta), -sin(theta), cos(theta)), nrow=2)
	new.x <- A %*% new.x
	# translate back and do requested translation
	new.x <- t(new.x + tt + translate)
	# remove negative coordinates and round to integers
	if ( grid ) {
		new.x <- round(new.x)
		new.x[new.x < 1] <- 1
	}
	# return data.frame of new coordinates
	new.x <- as.data.frame(new.x)
	names(new.x) <- names(x)
	new.x
}

# Returns the two nearest local maxima to the given points
nearest_locmax <- function(x, m, ...) {
	max1 <- locmax(x, ...)
	max2 <- rev(locmax(rev(x), ...))
	vals <- unique(c(1L, sort(union(max1, max2)), length(x)))
	lower <- findInterval(m, vals, all.inside=TRUE)
	upper <- lower + 1L
	list(lower=vals[lower], upper=vals[upper])
}

# Alignment of two vectors by absolute difference
diffAlign <- function(x, y, diff.max, ...) {
	if ( length(diff.max) < length(y) )
		diff.max <- rep(diff.max, length.out=y)
	aligned <- data.frame(x=rep(NA, length(y)), y=seq_along(y))
	aligned$x <- mapply(function(yi, di) {
		if ( length(x) == 0 )
			return(NA)
		diffs <- abs(x - yi)
		which <- which.min(diffs)
		if ( diffs[which] <= di ) {
			match <- which
		} else {
			match <- NA
		}
		match
	}, y, diff.max)
	aligned <- aligned[!is.na(aligned$x),]
	aligned
}

# Alignment of two vectors using dynamic programming
dynamicAlign <- function(x, y, gap=0, score=function(x, y) ifelse(x==y, 1, -1), ... ) {
	gap <- as.numeric(gap)
	x.mat <- matrix(x, byrow=TRUE, ncol=length(x), nrow=length(y))
	y.mat <- matrix(y, byrow=FALSE, ncol=length(x), nrow=length(y))
	similarity.mat <- score(x.mat, y.mat)
	score.mat <- matrix(0, ncol=length(x) + 1, nrow=length(y) + 1)
	score.mat[1,] <- c(0, cumsum(rep(gap, length(x))))
	score.mat[,1] <- c(0, cumsum(rep(gap, length(y))))
	tracking.mat <- matrix(0L, ncol=length(x) + 1, nrow=length(y) + 1)
	tracking.mat[,1] <- 0L
	tracking.mat[1,] <- 1L
	tracking.mat[1,1] <- 2L
	aligned <- .Call("C_dynAlign", similarity.mat,
		score.mat, tracking.mat, gap, PACKAGE="Cardinal")
	names(aligned) <- c("x", "y")
	aligned$x <- aligned$x[aligned$x > 0]
	aligned$y <- aligned$y[aligned$y > 0]
	aligned
}

# A sequence with half-bin-widths in ppm (parts-per-million)
# x = bin center, y = bin half-window, K = ppm
# y[n] = K * x[n] * 1e-6
# y[n+1] = (1e-6 * K) * (x[n] - y[n])) / (1 - (1e-6 * K))
# x[n+1] = x[n] + y[n] + y[n+1]
# => x[n] ((1 + K * 1e-6) / (1 - K * 1e-6))^n * x[0]
# log x[n] = n log {(1 + K * 1e-6) / (1 - K * 1e-6)} + log x[0]
# => n = (log x[n] - log x[0]) / log {(1 + K * 1e-6) / (1 - K * 1e-6)}
seq.ppm <- function(from, to, ppm) {
	length.out <- (log(to) - log(from)) / log((1 + 1e-6 * ppm) / (1 - 1e-6 *ppm))
	length.out <- floor(1 + length.out)
	i <- seq_len(length.out)
	from * ((1 + 1e-6 * ppm) / (1 - 1e-6 * ppm))^(i-1)
}


## Convert names of data types to their size in number of bytes
Csizeof <- function(type) {
	vapply(type, function(t) {
		switch(t,
			`16-bit integer` = 2L,
			`32-bit integer` = 4L,
			`64-bit integer` = 8L,
			`32-bit float` = 4L,
			`64-bit float` = 8L,
			stop("unrecognized binary type"))
	}, integer(1))
}

Ctypeof <- function(type) {
	vapply(type, function(t) {
		switch(t,
			`16-bit integer` = "short",
			`32-bit integer` = "int",
			`64-bit integer` = "long",
			`32-bit float` = "float",
			`64-bit float` = "double",
			stop("unrecognized binary type"))
	}, character(1))
}

Nametypeof <- function(type) {
	vapply(type, function(t) {
		switch(t,
			`short` = "16-bit integer",
			`int` = "32-bit integer",
			`long` = "64-bit integer",
			`float` = "32-bit float",
			`double` = "64-bit float",
			stop("unrecognized binary type"))
	}, character(1))
}

make.uuid <- function(id, uppercase=TRUE) {
	id <- as.character(id)
	id <- paste(paste0(id[1:4], collapse=""),
		paste0(id[5:6], collapse=""),
		paste0(id[7:8], collapse=""),
		paste0(id[9:10], collapse=""),
		paste0(id[11:16], collapse=""), sep="-")
	if ( uppercase ) {
		toupper(id)
	} else {
		tolower(id)
	}
}

## Make an annotation (factor) from regions-of-interest (logical)
make.annotation <- function(...) {
	regions <- data.frame(...)
	names <- names(regions)
	x <- as.character(rep(NA, nrow(regions)))
	for ( nm in names ) {
		x[regions[[nm]]] <- nm
	}
	if ( length(regions) == 1 ) {
		x[is.na(x)] <- paste("NOT", names[1])
	}
	as.factor(x)
}

## Match methods to their workhorse functions
match.method <- function(method, options) {
	if ( is.function(method) ) {
		method
	} else if ( is.character(method) ) {
		if ( missing(options) ) {
			method[1L]
		} else {
			matched <- pmatch(method[1L], options)
			if ( is.na(matched) ) {
				method[1L]
			} else {
				options[matched]
			}	
		}
	} else if ( is.null(method) ) {
		options[1L]
	} else {
		stop("method matching failed")
	}
}

## Programmatic friendly version of base::subset
subset_data <- function(data, subset, select, drop=FALSE) {
	subset <- subset_rows(data, subset=subset)
	if ( missing(select) ) {
		data[subset,,drop=drop]
	} else {
		data[subset,select,drop=drop]
	}
}

## Programmatic friendly version of base::subset (only return row indices)
subset_rows <- function(data, subset) {
	j <- setNames(seq_along(subset), names(subset))
	sub <- lapply(j, function(i) {
		data[[names(subset)[[i]]]] %in% subset[[i]]
	})
	sub <- as.data.frame(sub)
	which(apply(sub, 1, all))
}

## Evaluate a function after capturing unwanted ... arguments
wrap <- function(exprs, ..., signature) {
	.local <- function() {
		eval(substitute(exprs, env=parent.frame()))
	}
	environment(.local) <- parent.frame()
	if ( is.function(signature) ) {
		formals(.local) <- formals(signature)
	} else {
		formals(.local) <- signature
	}
	.local(...)
}

## Gets the current .Random.seed
getRNGStream <- function() {
	if ( exists(".Random.seed", envir=globalenv()) ) {
		get(".Random.seed", envir=globalenv())
	} else {
		NULL
	}
}

## Sets the .Random.seed
setRNGStream <- function(seed = NULL) {
	if ( !is.null(seed) && is.integer(seed) )
		assign(".Random.seed", seed, envir=globalenv())
}

## Generates RNG seeds for parallel computation
generateRNGStreams <- function(n = 1) {
	seeds <- vector("list", n)
	s <- getRNGStream()
	if ( is.null(s) ) {
		seeds[1] <- list(NULL)
	} else {
		seeds[[1]] <- s
	}
	if ( "L'Ecuyer-CMRG" %in% RNGkind() ) {
		for ( i in seq_len(n)[-1] ) {
			s <- nextRNGStream(seeds[[i - 1]])
			if ( is.null(s) ) {
				seeds[i] <- list(NULL)
			} else {
				seeds[[i]] <- s
			}
		}
	}
	seeds
}
