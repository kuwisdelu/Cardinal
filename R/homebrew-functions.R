
#### assorted helper functions ####

# match parameters in an MSImageSegmentation
match.which <- function(x, which) {
	if ( is.list(which) ) {
		lwhich <- lapply(names(which), function(n) {
			 metaData(x)$parameters[[n]] %in% which[[n]]
		} )
		if ( length(lwhich) > 1 ) {
			lwhich <- do.call(`&`, lwhich)
		} else {
			lwhich <- lwhich[[1]]
		}
		which <- which(lwhich)
	} else if ( is.character(which) ) {
		which <- which(metaData(x)[["parnames"]] %in% which)
	}
	which
}

# bin a signal
bin <- function(x, lbound, ubound, sum=FALSE) {
	x.new <- mapply(function(l, u) {
		sum(x[l:u], na.rm=TRUE)
	}, lbound, ubound)
	if ( sum ) {
		x.new
	} else {
		x.new / abs(ubound - lbound + 1)
	}
}

# returns a list of approximately even subsets of a vector
intervals <- function(x, blocks) {
	ints <- floor(seq(from=1, to=length(x)+1, length.out=blocks))
	begin <- ints[-length(ints)]
	end <- ints[-1] - 1
	apply(cbind(begin, end), 1, function(i) x[i[1]:i[2]])
}

# find the combined mean deviation of multiple samples
combinedMean <- function(xbar, n) {
	sum(xbar * n) / sum(n)
}

# find the combined mean deviation of multiple samples
combinedVar <- function(xbar, var, n) {
	xc <- combinedMean(xbar, n)
	(sum(n * var) + sum(n * (xbar - xc)^2)) / sum(n)
}

# find the grouped mean, e.g., of a histogram
groupMean <- function(x, f) {
	if ( any(f < 0) ) f[f < 0] <- 0
	n <- sum(f)
	sum(f * x) / n
}

# find the grouped variance, e.g., of a histogram
groupVar <- function(x, f) {
	if ( any(f < 0) ) f[f < 0] <- 0
	n <- sum(f)
	xbar <- sum(f * x) / n
	Sfx2 <- sum(f * x^2)
	(Sfx2 / n) - xbar^2
}

# gives the L1 norm of a vector
l1norm <- function(x) sum(abs(x))

# gives the L2 norm of a vector
l2norm <- function(x) sqrt(sum(x^2))

# soft thresholding
soft <- function(x, delta) sign(x) * pmax(0, abs(x)-delta)

