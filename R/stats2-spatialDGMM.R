
setMethod("spatialDGMM", "SparseImagingExperiment",
	function(x, r = 1, k = 3, groups = run(x),
		method = c("gaussian", "adaptive"),
		dist = "chebyshev", annealing = TRUE,
		iter.max = 100, tol = 1e-9,
		collate.results = TRUE,
		BPPARAM = bpparam(), ...)
	{
		.checkForIncompleteProcessing(x)
		BPPARAM <- .protectNestedBPPARAM(BPPARAM)
		method <- match.arg(method)
		groups <- as.factor(groups)
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
			progress <- getOption("Cardinal.progress")
			options(Cardinal.progress=FALSE)
			results[[i]] <- featureApply(x, function(xi) {
				fid <- attr(xi, "idx")
				.message("r = ", par$r[i], ", k = ", par$k[i],
					", ", "feature = ", fid, " ", appendLF = TRUE)
				res <- .spatialDGMM_cluster(xi=xi, k=par$k[i], groups=groups,
					group.weights=gweights, annealing=annealing,
					iter.max=iter.max, tol=tol, seed=rngseeds[fid], ...)
				if ( collate.results )
					res <- .spatialDGMM_collate(res, groups)
				res
			}, .simplify=FALSE, BPPARAM=BPPARAM[[1]])
			options(Cardinal.progress=progress)
		}
		results <- do.call("c", results)
		models <- DataFrame(rev(expand.grid(feature=1:nrow(x), k=k, r=r)))
		models$mz <- mz(x)
		models$num_segments <- sapply(results, function(res) {
			nlevels(res$class)
		})
		if ( collate.results ) {
			resultType <- list(
				feature=character(),
				pixel=c("class", "pixel"))
		} else {
			resultType <- NULL
		}
		.SpatialDGMM(
			imageData=.SimpleImageArrayList(),
			featureData=featureData(x),
			elementMetadata=pixelData(x),
			metadata=list(
				resultType=resultType,
				modelParam=c("r", "k", "feature"),
				method=method, dist=dist),
			resultData=as(results, "List"),
			modelData=models)
	})

 .spatialDGMM_cluster <- function(xi, k, method, groups, group.weights,
									annealing, iter.max, tol, seed)
{
 	oseed <- getRNGStream()
	on.exit(setRNGStream(oseed))
	setRNGStream(seed)
	results <- mapply(function(gi, w) {
		xgi <- as.numeric(xi)[groups == gi]
		.spatialDGMM_fit1(xgi, k=k, neighbors=attr(w, "neighbors"), weights=w,
			annealing=annealing, iter.max=iter.max, tol=tol, trace=FALSE)
	}, levels(groups), group.weights, SIMPLIFY=FALSE)
	names(results) <- levels(groups)
	results
}

.spatialDGMM_collate <- function(results, groups) {
	classnames <- unlist(lapply(levels(groups),
		function(gi) paste(gi, unique(results[[gi]]$class), sep=".")))
	class <- character(length(groups))
	for ( gi in levels(groups) ) {
		class[groups == gi] <- paste(gi, results[[gi]]$class, sep=".")
	}
	class <- factor(class, levels=classnames)
	probability <- lapply(levels(groups), function(gi) {
		probs <- matrix(NA_real_, nrow=length(groups),
			ncol=ncol(results[[gi]]$probability))
		probs[groups == gi,] <- results[[gi]]$probability
		probs
	})
	probability <- do.call("cbind", probability)
	colnames(probability) <- classnames
	params <- list(
		mu=setNames(unlist(lapply(results,
			function(res) res$params$mu)), classnames),
		sigma=setNames(unlist(lapply(results,
			function(res) res$params$sigma)), classnames),
		alpha=setNames(unlist(lapply(results,
			function(res) res$params$alpha)), classnames),
		beta=setNames(unlist(lapply(results,
			function(res) res$params$beta)), levels(groups)))
	list(params=params, class=class, probability=probability)
}
 
.spatialDGMM_fit1 <- function(xi, k, neighbors, weights, annealing, tol,
								iter.max, trace = FALSE, verbose = FALSE)
{
	# simplify up spatial weights
	weights <- lapply(weights, function(wt) wt$alpha * wt$beta)
	spatial <- list(neighbors=neighbors, weights=weights)
	# initial step size
	eta <- 1e-3
	# initialize with ordinary Gaussian mixture model
	gmm <- Mclust(xi, G=1:k, modelNames="V", verbose=FALSE)
	K <- gmm$G
	N <- length(xi)
	# initialize parameters (theta: mu, sigma, alpha, beta)
	mu <- gmm$parameters$mean
	sigma <- gmm$parameters$variance$sigmasq
	alpha <- rep(1, K)
	beta <- 1
	# initialize prior probability
	prior <-matrix(1 / K, nrow=N, ncol=K)
	# initialize p(x|mu, sigma)
	px <- matrix(0, nrow=length(xi), ncol=length(mu))
	for ( j in 1:length(mu) )
		px[,j] <- sqrt(1/(2 * pi * sigma[j])) * exp(-(xi - mu[j])^2 / (2 * sigma[j]))
	# initialize posterior probability p(z)
	y <- regpr(px * prior / rowSums(px * prior))
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
			alpha=alpha, beta=beta, y=y, spatial=spatial)
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
				alpha=M$alpha, beta=M$beta, y=y, spatial=spatial)
			structure(E$error, loglik=E$loglik)
		}
		# find log(eta) that optimizes in direction of gradient
		leta <- suppressWarnings(optim(par=log(eta),
			fn=linesearch, method="Brent", lower=-100, upper=0))
		eta <- exp(leta$par)
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
					alpha=alpha, beta=beta, y=y, spatial=spatial)
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

.spatialDGMM_Estep <- function(xi, mu, sigma, alpha, beta, y, spatial)
{
	# calculate spatial posterior probability p(z|neighbors)
	ybar <- mapply(function(nb, wt) {
		colSums(wt * y[nb,,drop=FALSE]) / sum(wt)
	}, spatial$neighbors, spatial$weights)
	if ( is.matrix(ybar) ) {
		ybar <- t(ybar)
	} else {
		ybar <- as.matrix(ybar)
	}
	# calculate p(x|mu, sigma)
	px <- matrix(0, nrow=length(xi), ncol=length(mu))
	for ( j in 1:length(mu) )
		px[,j] <- sqrt(1/(2 * pi * sigma[j])) * exp(-(xi - mu[j])^2 / (2 * sigma[j]))
	px <- regpr(px)
	# calculate prior probability
	prior <- t(alpha^2 * t(ybar)^beta)
	prior <- prior / rowSums(prior)
	# calculate new posterior probability p(z)
	y <- regpr(px * prior / rowSums(px * prior))
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

