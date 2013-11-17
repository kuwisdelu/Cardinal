
#### functions for spatial transformations ####

affine <- function(x, translate=c(0,0), rotate=0,
	angle=c("degrees", "radians"), grid=TRUE)
{
	angle <- match.arg(angle)
	theta <- -rotate
	if ( angle == "degrees" ) theta <- theta * pi / 180
	# translate center of mass to be near origin
	tt <- sapply(x, function(xs) mean(xs))
	new.x <- t(as.matrix(x)) - tt
	# rotate around origin
	A <- matrix(c(cos(theta), sin(theta), -sin(theta), cos(theta)), nrow=2)
	new.x <- A %*% new.x
	# translate back and do requested translation
	new.x <- t(new.x + tt + translate)
	# remove negative xinates and round to integers
	if ( grid ) {
		new.x <- round(new.x)
		new.x[new.x < 1] <- 1
	}
	# return data.frame of new coordinates
	new.x <- as.data.frame(new.x)
	names(new.x) <- names(x)
	new.x
}
