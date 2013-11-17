
#### function for alignment via dynamic programming ####

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
	x.match <- out.align[[7]]
	y.match <- out.align[[8]]
	matched <- cbind(y.match[y.match > 0], x.match[x.match > 0])
	colnames(matched) <- c("x", "y")
	return(matched)
}
