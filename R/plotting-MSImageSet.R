
setMethod("plot",
	signature = c(x = "MSImageSet", y = "missing"),
	function(x, formula = ~mz, ...) {
		callNextMethod(x, formula=formula, ...)
	})
