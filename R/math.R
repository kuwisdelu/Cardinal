
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

# Combined mean deviation of multiple samples
combinedMean <- function(xbar, n) {
	sum(xbar * n) / sum(n)
}

# Combined mean deviation of multiple samples
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

# Bin a signal
bin <- function(x, lbound, ubound, fun=sum) {
	mapply(function(l, u) {
		fun(x[l:u], na.rm=TRUE)
	}, lbound, ubound)
}

# Returns a list of approximately even subsets of a vector
intervals <- function(x, blocks) {
	ints <- floor(seq(from=1, to=length(x)+1, length.out=blocks))
	begin <- ints[-length(ints)]
	end <- ints[-1] - 1
	apply(cbind(begin, end), 1, function(i) x[i[1]:i[2]])
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

# Logical local maxima in a window
localMaximaLogical <- function(x, window=5, .C=TRUE, ...) {
	halfWindow <- floor(window / 2)
	if ( .C ) {
		is.max <- integer(length(x))
		is.max <- as.logical(.C("localMaxima", as.double(x), as.integer(is.max),
			as.integer(length(x)), as.integer(halfWindow))[[2]])
	} else {
		is.max <- sapply(seq(from=1+halfWindow, to=length(x)-halfWindow, by=1),
			function(i) which.max(x[(i-halfWindow):(i+halfWindow)]) == halfWindow + 1)
		is.max <- c(rep(FALSE, halfWindow), is.max, rep(FALSE, halfWindow))
	}
	is.max
}

# Local maxima in a window
localMaxima <- function(x, t, ...) {
	if ( missing(t) ) t <- seq_along(x)
	t[localMaximaLogical(x, ...)]
}

# Returns the two nearest local maxima to the given points
nearestLocalMaxima <- function(x, t, tout, ...) {
	locmax <- localMaxima(x, t, ...)
	locmax <- unique(c(min(t), locmax, max(t)))
	limits <- sapply(tout, function(ti) {
		lower <- which(diff(sign(ti - locmax)) < 0)
		if ( length(lower) > 1 )
			lower <- lower[[1]]
		upper <- lower + 1
		c(locmax[lower], locmax[upper])
	})
	list(lbound=limits[1,], ubound=limits[2,])
}

# Alignment of two vectors by absolute difference
diffAlign <- function(x, y, diff.max, ...) {
	aligned <- data.frame(x=rep(NA, length(y)), y=seq_along(y))
	aligned$x <- sapply(y, function(yi) {
		if ( length(x) == 0 )
			return(NA)
		diffs <- abs(x - yi)
		which <- which.min(diffs)
		if ( diffs[which] <= diff.max ) {
			match <- which
		} else {
			match <- NA
		}
		match
	})
	aligned <- aligned[!is.na(aligned$x),]
	aligned
}

# Alignment of two vectors using dynamic programming
dynamicAlign <- function(x, y, gap=0, score=function(x, y) 1 / (1 + abs(x - y)), ... ) {
	x.mat <- matrix(x, byrow=TRUE, ncol=length(x), nrow=length(y))
	y.mat <- matrix(y, byrow=FALSE, ncol=length(x), nrow=length(y))
	similarity.mat <- score(x.mat, y.mat)
	score.mat <- matrix(0, ncol=length(x) + 1, nrow=length(y) + 1)
	score.mat[1,] <- c(0, cumsum(rep(gap, length(x))))
	score.mat[,1] <- c(0, cumsum(rep(gap, length(y))))
	tracking.mat <- matrix(0, ncol=length(x) + 1, nrow=length(y) + 1)
	tracking.mat[,1] <- 0
	tracking.mat[1,] <- 1
	tracking.mat[1,1] <- 2
	out.align <- .C("dynamicAlign", as.double(score.mat), as.integer(tracking.mat),
		as.double(similarity.mat), as.integer(nrow(score.mat)), as.integer(ncol(score.mat)),
		as.double(gap), integer(length(x)), integer(length(y)))
	x.align <- out.align[[7]]
	y.align <- out.align[[8]]
	aligned <- cbind(y.align[y.align > 0], x.align[x.align > 0])
	colnames(aligned) <- c("x", "y")
	aligned
}
