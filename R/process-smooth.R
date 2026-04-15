
## Smoothing

setMethod("smooth", "MSImagingArrays", 
	function(x,
		method = c("gaussian", "bilateral", "adaptive",
			"diff", "guide", "pag", "sgolay", "ma"), ...)
	{
		method <- match.arg(method)
		addProcessing(x,
			FUN=.smooth_FUN(method),
			id="smoothing", ...)
	})

.smooth_FUN <- function(method)
{
	switch(method,
		gaussian = function(x, ...) {
			x$intensity <- matter::filt1_gauss(x$intensity, ...)
			x
		},
		bilateral = function(x, ...) {
			x$intensity <- matter::filt1_bi(x$intensity, ...)
			x
		},
		adaptive = function(x, ...) {
			x$intensity <- matter::filt1_adapt(x$intensity, ...)
			x
		},
		diff = function(x, ...) {
			x$intensity <- matter::filt1_diff(x$intensity, ...)
			x
		},
		guide = function(x, ...) {
			x$intensity <- matter::filt1_guide(x$intensity, ...)
			x
		},
		pag = function(x, ...) {
			x$intensity <- matter::filt1_pag(x$intensity, ...)
			x
		},
		sgolay = function(x, ...) {
			x$intensity <- matter::filt1_sg(x$intensity, ...)
			x
		},
		ma = function(x, ...) {
			x$intensity <- matter::filt1_bi(x$intensity, ...)
			x
		})
}
