
#### Estimate domain ####
## ----------------------

estimateDomain <- function(xlist,
	units = c("relative", "absolute"),
	BPPARAM = getCardinalBPPARAM())
{
	units <- match.arg(units)
	ref <- switch(units, relative="x", absolute="abs")
	FUN <- function(x) {
		x <- x[!is.na(x)]
		res <- unname(estres(x, ref=ref))
		c(min=min(x), max=max(x), res=res)
	}
	ans <- chunkLapply(xlist, FUN,
		nchunks=getCardinalNChunks(),
		verbose=getCardinalVerbose(),
		BPPARAM=BPPARAM)
	ans <- do.call(rbind, ans)
	from <- floor(min(ans[,1L], na.rm=TRUE))
	to <- ceiling(max(ans[,2L], na.rm=TRUE))
	by <- median(ans[,3L], na.rm=TRUE)
	by <- switch(units,
		relative=round(2 * by, digits=6L) * 0.5,
		absolute=round(by, digits=4L))
	switch(units,
		relative=seq_rel(from, to, by=by),
		absolute=seq.default(from, to, by=by))
}
