
#### functions for coregistration of images ####

findTransformation <- function(object, sliceDimNames,
	pixels.reference, pixels.target)
{
	img1 <- createMSImageSet(spectra=intensities(object, pixel=pixels.reference, drop=TRUE),
		mz=mz(object), coord=coord(object)[pixels.reference,sliceDimNames])
	img2 <- createMSImageSet(spectra=intensities(pig3d.peaks, pixel=pixels.target, drop=TRUE),
		mz=mz(object), coord=coord(object)[pixels.target,sliceDimNames])
	s1 <- intensities(img1)
	fn <- function(par) {
		new.coord <- affine(coord(img2), translate=c(par[1], par[2]), rotate=par[3])
		coord(img2) <- new.coord
		img2 <- regeneratePositions(img2)
		s2 <- intensities(img2)
		d2 <- min(dim(s1)[2], dim(s2)[2])
		d3 <- min(dim(s1)[3], dim(s2)[3])
		-sum((log(s1[,1:d2,1:d3]+1) * log(s2[,1:d2,1:d3]+1))^2, na.rm=TRUE)
	}
	offset <- sapply(coord(img1)[,sliceDimNames], mean) - sapply(coord(img2)[,sliceDimNames], mean)
	trace <- getOption("Cardinal.verbose.output")
	optim(par=c(offset[1],offset[2],0), fn=fn,control=list(trace=trace))$par
}

rotateSlice <- function(object, pixel, sliceDimNames, translate, rotate, ...)
{
	coord(object)[pixel,sliceDimNames] <- affine(coord(object)[pixel,sliceDimNames],
		translate=translate, rotate=rotate, ...)
	regeneratePositions(object)
}
