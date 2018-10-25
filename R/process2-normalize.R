
#### Normalize spectra ####
## ------------------------

setMethod("normalize", "MSImagingExperiment",
	function(object, method = "tic", ...)
	{
		fun <- normalize.method2(method)
		object <- process(object, fun=fun, ...,
			label="normalize", kind="pixel",
			delay=TRUE)
		object
	})

normalize.method2 <- function(method) {
	if ( is.character(method) ) {
		method <- match.method(method, c("tic"))
		switch(method,
			tic = normalize.tic2,
			match.fun(method))
	} else {
		match.fun(method)
	}
}

normalize.tic2 <- normalize.tic
