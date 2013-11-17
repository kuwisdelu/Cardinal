
#### S4-style classes with complete interface ####

setClass("MSPeakFrame",
	representation=representation(
		peaks="data.frame",
		metaData="list"
	),
	prototype=prototype(
		peaks=data.frame(),
		metaData=list()
	)
)

setClass("MSPeakList",
	representation=representation(
		peaks="list",
		metaData="list"
	),
	prototype=prototype(
		peaks=list(),
		metaData=list(name=character(),
			creationDate=date(),
			method=character(),
			call=list()
		)
	)
)

setClass("MSPeakAlignment",
	contains="MSPeakList"
)

setClass("MSImageSet",
	representation=representation(
		spectra="environment",
		peaks="MSPeakFrame",
		featureData="data.frame",
		pixelData="data.frame",
		metaData="list"
	),
	prototype=prototype(
		spectra=new.env(parent=globalenv()),
		peaks=new("MSPeakFrame"),
		featureData=data.frame(),
		pixelData=data.frame(),
		metaData=list(name=character(),
			creationDate=date(),
			mzDimName=character(), # necessary for functionality
			coordDimNames=character(), # necessary for functionality
			positionArray=array(), # necessary for functionality
			isPeaks=logical(),
			isBinned=logical(),
			isResampled=logical(),
			history=list()
		)
	)
)

setClass("MSImageSegmentation",
	contains="list",
	slots=list(
		metaData="list"
	)
)

#### S3-style classes used as return values by methods ####

setOldClass("CrossValidation")

setOldClass("KMeans")

setOldClass("PCA")

setOldClass("PLS")

setOldClass("OPLS")

#### end all classes ####

