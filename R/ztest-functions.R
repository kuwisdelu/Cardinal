
#### function for a z-test with known standard errors ####

ztest <- function(mu1, sigma1, mu2, sigma2) {
	sigma <- sqrt(sigma1^2 + sigma2^2)
	statistic <- abs(mu1 - mu2) / sigma
	1 - pnorm(statistic)
}

ztests <- function(theta1, theta2) {
	n1 <- length(theta1$mu)
	n2 <- length(theta2$mu)
	theta1$sigma <- rep(theta1$sigma, length.out=n1)
	theta2$sigma <- rep(theta2$sigma, length.out=n2)
	vapply(1:n1, function(i) {
		vapply(1:n2, function(j) {
			ztest(theta1$mu[[i]], theta1$sigma[[i]], theta2$mu[[j]], theta2$sigma[[j]])
		}, numeric(1))
	}, numeric(n2))
}
