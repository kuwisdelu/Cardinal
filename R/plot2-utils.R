
plot_density <- function(mean, var, cnames, n = 100, plot = TRUE)
{
	len <- max(length(mean), length(var))
	if ( missing(cnames) )
		cnames <- paste(seq_len(len))
	means <- rep_len(mean, len)
	sds <- sqrt(rep_len(var, len))
	rg <- range(c(means - 2 * sds, means + 2 * sds))
	x <- seq(from=rg[1], to=rg[2], length.out=n)
	data <- mapply(function(mu, sd, ns) {
		dens <- dnorm(x, mean=mu, sd=sd)
		DataFrame(x=x, density=dens, class=ns)
	}, means, sds, cnames, SIMPLIFY=FALSE)
	data <- do.call("rbind", data)
	if ( plot ) {
		plot(data, density ~ x, groups=class)
	} else {
		data
	}
}

