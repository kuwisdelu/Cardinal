
#### Returns a simulated image of spectral signals ####
## 'data' is a factor or integer matrix
## 'coord' is a data.frame of coordinates
## 'peaks' is the number of peaks
## 'delta' is the effect size
## 'as' is the kind of S4 object to create
##-----------------------------------------------
generateImage <- function(data = factor(1),
	coord = expand.grid(
		x = 1:max(1, nrow(data)),
		y = 1:max(1, ncol(data))),
	peaks = length(levels(as.factor(data))),
	delta = 10,
	as = c("SImageSet", "MSImageSet"),
	...)
{
	intensities <- rep(1, peaks)
	sd <- 0.1
	as <- match.arg(as)
	if ( is.array(data) ) {
		coord <- coord[is.finite(data),]
		data <- as.factor(data[is.finite(data)])
	}
	xs <- lapply(levels(data), function(i) {
		n <- sum(data == i)
		intensities[which(i == levels(data))] <- intensities[1] + delta * sd
		generateSpectrum(n, intensities=intensities, sd=sd, ...)
	})
	t <- xs[[1]]$t
	x <- matrix(nrow=length(t), ncol=length(data))
	for ( i in levels(data) ) {
		x[,data == i] <- xs[[which(i == levels(data))]]$x
	}
	if ( as == "SImageSet" ) {
		out <- SImageSet(data=x, coord=coord)
		fData(out)$t <- t
	} else if ( as == "MSImageSet") {
		out <- MSImageSet(spectra=x, mz=t, coord=coord)
	}
	out
}
