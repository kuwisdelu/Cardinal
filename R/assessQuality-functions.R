
#### functions for assessing spectral quality ####

assessQualityFunction <- function(method) {
	if ( is.function(method) ) {
		return(method)
	} else if ( is.character(method) ) {
		if ( length(method) > 1 ) method <- method[[1]]
		switch(method,
			"snr" = assessQualitySNR,
			match.fun(method)
		)
	} else {
		stop("could not find matching function for ", substitute(method))
	}
}

assessQualitySNR <- function(x, t, noise=c("sd", "mad", "adaptive-sd", "adaptive-mad",
	"limpic", "supersmoother"), score=max, ...)
{
	fun <- estimateNoiseFunction(noise)
	noise <- fun(x, t, ...)
	score <- match.fun(score)
	score(x / noise)
}
