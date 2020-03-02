
setMethod("spatialDGMM", "SparseImagingExperiment",
	function(x, r = 1, k = 3, groups = run(x),
		method = c("gaussian", "adaptive"),
		dist = "chebyshev", annealing = TRUE,
		init = c("kmeans", "gmm"), p0 = 0.05,
		iter.max = 100, tol = 1e-9,
		BPPARAM = bpparam(), ...)
	{
		.checkForIncompleteProcessing(x)
		BPPARAM <- .protectNestedBPPARAM(BPPARAM)
		groups <- as.factor(rep_len(groups, ncol(x)))
		method <- match.arg(method)
		init <- match.arg(init)
		.message("calculating spatial weights...")
		r.gweights <- list(r=r, w=lapply(r, function(ri) {
			bplapply(levels(groups), function(gi, BPPARAM) {
				xg <- x[,groups == gi]
				spatialWeights(xg, r=ri, dist=dist,
					method=method, BPPARAM=BPPARAM)
			}, BPPARAM=BPPARAM)
		}))
		results <- list()
		par <- expand.grid(k=k, r=r)
		.message("segmenting feature images...")
		for ( i in 1:nrow(par) ) {
			rngseeds <- generateRNGStreams(nrow(x))
			gweights <- r.gweights$w[[which(r.gweights$r == par$r[i])]]
			results[[i]] <- featureApply(x, function(xi) {
				fid <- attr(xi, "idx")
				.message("r = ", par$r[i], ", k = ", par$k[i],
					", ", "feature = ", fid, " ", appendLF = FALSE)
				res <- .spatialDGMM_cluster(xi=xi, k=par$k[i], groups=groups,
					group.weights=gweights, annealing=annealing,
					init=init, iter.max=iter.max, tol=tol, p0=p0,
					seed=rngseeds[fid], ...)
				res <- .spatialDGMM_collate(x, res, groups)
				res
			}, .simplify=FALSE, .verbose=FALSE, BPPARAM=BPPARAM[[1]])
		}
		results <- do.call("c", results)
		models <- DataFrame(rev(expand.grid(feature=1:nrow(x), k=k, r=r)))
		out <- .SpatialDGMM(
			imageData=.SimpleImageArrayList(),
			featureData=featureData(x),
			elementMetadata=pixelData(x),
			metadata=list(
				mapping=list(
					feature=NULL,
					pixel=c("class", "pixel")),
				method=method, dist=dist),
			resultData=as(results, "List"),
			modelData=models)
		pixelData(out)$.group <- groups
		out
	})

 .spatialDGMM_cluster <- function(xi, k, method, groups, group.weights,
							annealing, init, iter.max, tol, p0, seed)
{
 	oseed <- getRNGStream()
	on.exit(setRNGStream(oseed))
	setRNGStream(seed)
	results <- mapply(function(gi, w) {
		xgi <- as.numeric(xi)[groups == gi]
		res <- .spatialDGMM_fit1(xgi, k=k, weights=w,
			annealing=annealing, init=init, iter.max=iter.max,
			tol=tol, p0=p0, trace=FALSE, verbose=FALSE)
		.message(".", appendLF=FALSE)
		res
	}, levels(groups), group.weights, SIMPLIFY=FALSE)
	.message(" ")
	names(results) <- levels(groups)
	results
}

.spatialDGMM_collate <- function(x, results, groups, rename = FALSE) {
	cnames <- unlist(lapply(levels(groups), function(gi)
		paste(gi, seq_along(results[[gi]]$params$mu), sep=".")))
	class <- character(length(groups))
	for ( gi in levels(groups) ) {
		class[groups == gi] <- paste(gi, results[[gi]]$class, sep=".")
	}
	class <- factor(class, levels=cnames)
	if ( !rename ) {
		cnames <- paste0(seq_along(cnames))
		levels(class) <- cnames
	}
	probability <- mapply(function(res, gi) {
		probs <- matrix(NA_real_,
			nrow=length(groups),
			ncol=ncol(res$probability))
		probs[groups == gi,] <- res$probability
		probs
	}, results, levels(groups), SIMPLIFY=FALSE)
	probability <- do.call("cbind", probability)
	colnames(probability) <- cnames
	estimates <- do.call("rbind", mapply(function(res, gi) {
		data.frame(group=gi,
			class=NA_character_,
			mean=res$params$mu,
			var=res$params$sigma)
	}, results, levels(groups), SIMPLIFY=FALSE))
	row.names(estimates) <- cnames
	estimates$class <- cnames
	list(estimates=estimates, probability=probability, class=class)
}

.spatialDGMM_withmeans <- function(x) {
	resultData(x) <- endoapply(resultData(x),
		function(res) {
			prob <- res$probability
			mean <- rep(res$estimates$mean, each=ncol(x))
			mean <- matrix(mean, nrow=nrow(prob), ncol=ncol(prob))
			res$mean <- rowSums(mean * prob)
			res
		})
	x
}
 
.spatialDGMM_fit1 <- function(xi, k, weights, annealing, init,
								iter.max, tol, p0, trace, verbose)
{
	# initialize with ordinary Gaussian mixture model
	gmm <- Mclust(xi, G=1:k, modelNames="V", verbose=FALSE)
	K <- gmm$G
	N <- length(xi)
	# initialize parameters (theta: mu, sigma, alpha, beta)
	km <- try(kmeans(xi, centers=gmm$parameters$mean), silent=TRUE)
	if ( inherits(km, "try-error") ) {
		var_ok <- FALSE
		ncl <- 0
	} else {
		var_ok <- all(tapply(xi, km$cluster, var) > 0)
		ncl <- tabulate(km$cluster)
	}
	if ( init == "kmeans" && K > 1 && all(ncl > 1) && var_ok ) {
		mu <- as.numeric(km$centers)
		sigma <- as.numeric(tapply(xi, km$cluster, var))
	} else {
		mu <- gmm$parameters$mean
		sigma <- gmm$parameters$variance$sigmasq
	}
	alpha <- rep(1, K)
	beta <- 1
	# initialize p(x|mu, sigma)
	px <- matrix(0, nrow=length(xi), ncol=length(mu))
	for ( j in 1:length(mu) )
		px[,j] <- sqrt(1/(2 * pi * sigma[j])) * exp(-(xi - mu[j])^2 / (2 * sigma[j]))
	# initialize prior probability
	prior <- matrix(1 / K, nrow=N, ncol=K)
	# initialize posterior probability p(z)
	y <- regpr(px * prior / rowSums(px * prior), lambda=p0)
	# trace
	trace_out <- list(
		mu=matrix(ncol=K, nrow=iter.max),
		sigma=matrix(ncol=K, nrow=iter.max),
		alpha=matrix(ncol=K, nrow=iter.max),
		beta=matrix(ncol=1, nrow=iter.max))
	# log-likelihood
	loglik <- rep(NA_real_, iter.max)
	error <- rep(NA_real_, iter.max)
	error_old <- Inf
	error_new <- Inf
	# SA temperature
	tt <- 1
	# fit with EM + gradient descent + SA
	for ( i in 1:iter.max ) {
		## E-step
		E <- .spatialDGMM_Estep(xi, mu=mu, sigma=sigma,
			alpha=alpha, beta=beta, y=y, p0=p0, weights=weights)
		# update spatial posterior
		ybar <- E$ybar
		# update prior
		prior <- E$prior
		# update posterior
		y <- E$y
		# log-likelihood
		loglik[i] <- E$loglik
		error[i] <- E$error
		error_new <- error[i]
		if ( verbose )
			message("iter = ", i, "; error = ", error[i], "; loglik = ", loglik[i])
		# check for convergence
		if ( error_old - error_new < tol )
			break
		## M-step
		linesearch <- function(eta) {
			M <- .spatialDGMM_Mstep(xi, mu=mu, sigma=sigma,
				alpha=alpha, beta=beta, y=y, ybar=ybar, eta=exp(eta))
			E <- .spatialDGMM_Estep(xi, mu=M$mu, sigma=M$sigma,
				alpha=M$alpha, beta=M$beta, y=y, p0=p0, weights=weights)
			structure(E$error, loglik=E$loglik)
		}
		# find log(eta) that optimizes in direction of gradient
		leta <- suppressWarnings(optimize(linesearch, c(-100, 0)))
		eta <- exp(leta$minimum)
		M <- .spatialDGMM_Mstep(xi, mu=mu, sigma=sigma,
			alpha=alpha, beta=beta, y=y, ybar=ybar, eta=eta)
		# update parameters
		mu <- M$mu
		sigma <- M$sigma
		alpha <- M$alpha
		beta <- M$beta
		## simulated annealing
		if ( annealing ) {
			loglik_a <- loglik[i]
			for ( j in 1:length(mu) ) {
				mu_a <- mu
				mu_a[j] <- rnorm(1, mean=mu[j], sd=sqrt(sigma[j] * tt))
				E <- .spatialDGMM_Estep(xi, mu=mu_a, sigma=sigma,
					alpha=alpha, beta=beta, y=y, p0=p0, weights=weights)
				if ( E$loglik > loglik_a ) {
					if ( verbose )
						message("simulated annealing: loglik = ", E$loglik)
					loglik_a <- E$loglik
					mu <- mu_a
				}
			}
			tt <- tt - (1 / iter.max)
		}
		# update trace
		trace_out$mu[i,] <- mu
		trace_out$sigma[i,] <- sigma
		trace_out$alpha[i,] <- alpha
		trace_out$beta[i,] <- beta
		error_old <- error_new
	}
	# calculate class assignments
	z <- apply(y, 1, which.max)
	if ( trace ) {
		params <- list(mu=mu, sigma=sigma,
			alpha=alpha, beta=beta, trace=trace_out)
	} else {
		params <- list(mu=mu, sigma=sigma,
			alpha=alpha, beta=beta)
	}
	list(params=params, probability=y, class=z)
}

.spatialDGMM_Estep <- function(xi, mu, sigma, alpha, beta, y, p0, weights)
{
	# calculate spatial posterior probability p(z|neighbors)
	ybar <- t(.spatialFilter(t(y), weights, attr(weights, "neighbors")))
	# calculate p(x|mu, sigma)
	px <- matrix(0, nrow=length(xi), ncol=length(mu))
	for ( j in 1:length(mu) )
		px[,j] <- sqrt(1/(2 * pi * sigma[j])) * exp(-(xi - mu[j])^2 / (2 * sigma[j]))
	# calculate prior probability
	prior <- t(alpha^2 * t(ybar)^beta)
	prior <- prior / rowSums(prior)
	# calculate new posterior probability p(z)
	y <- regpr(px * prior / rowSums(px * prior), lambda=p0)
	# log-likelihood and error function
	loglik <- sum(log(rowSums(prior * px)))
	error <- -sum(log(rowSums((prior * px)^y)))
	list(ybar=ybar, prior=prior, y=y, error=error, loglik=loglik)
}

.spatialDGMM_Mstep <- function(xi, mu, sigma, alpha, beta, y, ybar, eta)
{
	# initialize gradient
	diff <- list(
		mu=rep(1, length(mu)),
		sigma=rep(1, length(mu)),
		alpha=rep(1, length(mu)),
		beta=1)
	# calculate gradient
	for ( j in 1:length(mu) ) {
		diff$mu[j] <- sum(y[,j] * (mu[j] - xi) / sigma[j])
		diff$sigma[j] <- sum(y[,j] * ((1 / sigma[j]) - (xi - mu[j])^2 / (sigma[j]^2))) / 2
		dalpha_p <- y * (ybar[,j])^beta / rowSums(t(t((ybar)^beta) * alpha^2))
		dalpha_p[is.na(dalpha_p)] <- 1
		diff$alpha[j] <- -sum(2 * y[,j] / alpha[j]) + 2 * alpha[j] * sum(dalpha_p)
	}
	diff$beta <- sum(y * (-log(ybar) + rowSums(t(t((ybar)^beta) * alpha^2) * log(ybar)) / rowSums(t(t((ybar)^beta) * alpha^2))))
	# update parameters
	mu <- mu - eta * diff$mu
	sigma <- sigma - eta * diff$sigma
	alpha <- alpha - eta * diff$alpha
	beta <- beta - eta * diff$beta
	list(mu=mu, sigma=sigma, alpha=alpha, beta=beta, diff=diff)
}

