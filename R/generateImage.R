
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
	.Defunct("simulateImage")
}
