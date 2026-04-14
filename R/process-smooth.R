
## Smoothing

setMethod("smooth", "SpectralImagingData",
	function(x,
		method = c("gaussian", "bilateral", "adaptive",
			"diff", "guide", "pag", "sgolay", "ma"), ...)
{
	method <- match.arg(method)
	FUN <- .smooth_fun[[method, exact=FALSE]]
	addProcessing(x, FUN,
		label="smoothing",
		metadata=list(method=method), ...)
})

.smooth_fun <- list(
	gaussian = function(x, t, ...) 
		matter::filt1_gauss(x, ...),
	bilateral = function(x, t, ...) 
		matter::filt1_bi(x, ...),
	adaptive = function(x, t, ...) 
		matter::filt1_adapt(x, ...),
	diff = function(x, t, ...) 
		matter::filt1_diff(x, ...),
	guide = function(x, t, ...) 
		matter::filt1_guide(x, ...),
	pag = function(x, t, ...) 
		matter::filt1_pag(x, ...),
	sgolay = function(x, t, ...) 
		matter::filt1_sg(x, ...),
	ma = function(x, t, ...) 
		matter::filt1_ma(x, ...))
