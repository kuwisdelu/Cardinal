
Binmat <- function(
	files,
	nrow, ncol,
	offsets = 0,
	extents = rep(nrow, ncol),
	datatype = c("16-bit integer",
		"32-bit integer",
		"64-bit integer",
		"32-bit float",
		"64-bit float"),
	dimnames = NULL,
	...)
{
	datamode <- Ctypeof(datatype)
	matter_mat(paths=files,
		offset=offsets,
		extent=extents,
		nrow=nrow, ncol=ncol,
		datamode=datamode,
		dimnames=dimnames, ...)
}
