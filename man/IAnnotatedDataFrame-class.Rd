\name{IAnnotatedDataFrame-class}
\docType{class}

\alias{class:IAnnotatedDataFrame}
\alias{IAnnotatedDataFrame}
\alias{IAnnotatedDataFrame-class}

\alias{coord,IAnnotatedDataFrame-method}
\alias{coord<-,IAnnotatedDataFrame-method}
\alias{coordNames,IAnnotatedDataFrame-method}
\alias{coordNames<-,IAnnotatedDataFrame-method}
\alias{initialize,IAnnotatedDataFrame-method}
\alias{pixelNames,IAnnotatedDataFrame-method}
\alias{pixelNames<-,IAnnotatedDataFrame-method}
\alias{sampleNames,IAnnotatedDataFrame-method}
\alias{sampleNames<-,IAnnotatedDataFrame-method}

\title{Class Containing Measured Variables and Their Meta-Data Description for Imaging Experiments.}

\description{
	An \code{IAnnotatedDataFrame} is an extension of an \code{\linkS4class{AnnotatedDataFrame}} as defined in the 'Biobase' package modified to reflect that individual rows in \code{data} represent pixels rather than samples, and many pixels will come from a single sample. Additionally, it keeps track of the coordinates of the pixel represented by each row.
}

\usage{    
## Instance creation
IAnnotatedDataFrame(data, varMetadata,
	dimLabels=c("pixelNames", "pixelColumns"),
	\dots)

## Additional methods documented below
}

\arguments{
	
	\item{data}{A \code{data.frame} of the pixels (rows) and measured variables (columns). Omitting this will yield an empty \code{IAnnotatedDataFrame} with zero rows.}

	\item{varMetadata}{A \code{data.frame} with columns describing the measured variables in \code{data}. Generated automatically if missing.}

	\item{dimLabels}{Aesthetic labels for the rows and columns in the \code{show} method.}

	\item{\dots}{Additional arguments passed to the \code{initialize} method.}

}

\section{Extends}{
   Class \code{\linkS4class{AnnotatedDataFrame}}, directly.
   Class \code{\linkS4class{Versioned}}, by class "AnnotatedDataFrame", distance 2.
}

\section{Creating Objects}{
	\code{IAnnotatedDataFrame} instances are usually created through \code{IAnnotatedDataFrame()}.
}

\section{Slots}{
	\describe{
		\item{\code{data}:}{Object of class \code{data.frame} containing pixels (rows) and measured variables (columns). Contains at least one column named 'sample' which is a \code{factor} and gives the sample names for each pixel. The sample names can be set using \code{sampleNames<-}. Inherited from \linkS4class{AnnotatedDataFrame}.}

		\item{\code{varMetadata}:}{Object of class \code{data.frame} with number of rows equal to the number of columns in \code{data}. Contains at least two columns, one named 'labelDescription' giving a textual description of each variable, and an additional one named 'labelType' describing the type of variable. The 'labelType' is a factor with levels \code{"spatial2d", "spatial3d", "dimension", "sample", "pheno"}. Inherited from \linkS4class{AnnotatedDataFrame.}}
		
		\item{\code{dimLabels}:}{Object of class \code{character}  of length 2 that provides labels for the rows and columns in the \code{show} method. Inherited from \linkS4class{AnnotatedDataFrame}.}

		\item{\code{.__classVersion__}:}{A \code{Versions} object describing the version of the class used to created the instance. Intended for developer use.}
	}
}

\section{Methods}{
	Class-specific methods:
	\describe{
		\item{\code{sampleNames(object)}, \code{sampleNames(object)<-}:}{Return or set the sample names in the object, as determined by the factor levels of the 'sample' variable in \code{data}.}

		\item{\code{pixelNames(object)}, \code{pixelNames(object)<-}:}{Return or set the pixel names (the rows of \code{data}).}

		\item{\code{coordNames(object)}, \code{coordNames(object)<-}:}{Return or set the names of the pixel coodinates. These are the subset of \code{varLabels(object) for which the corresponding variables have a 'labelType' of 'spatial2d', 'spatial3d', or 'dimension'.}}

		\item{\code{coord(object)}, \code{coord(object)<-}:}{Return or set the coodinates. This is a \code{data.frame} containing the subset of columns of \code{data} for which the variables have a 'labelType' of 'spatial2d', 'spatial3d', or 'dimension'.}
	}
}

\author{Kyle D. Bemis}

\seealso{
	\code{\link[Biobase]{AnnotatedDataFrame}},
	\code{\link{MSImageSet}}
}

\examples{
## Create an IAnnotatedDataFrame object
IAnnotatedDataFrame()

## Simple IAnnotatedDataFrame
df1 <- IAnnotatedDataFrame(data=expand.grid(x=1:3, y=1:3),
	varMetadata=data.frame(labelType=c("spatial2d", "spatial2d")))
pData(df1)
varMetadata(df1)

# Example of possible experiment data
coord <- expand.grid(x=1:3, y=1:3)
df2 <- IAnnotatedDataFrame(data=
	data.frame(rbind(coord, coord), sample=factor(rep(1:2, each=nrow(coord)))),
	varMetadata=data.frame(labelType=c("spatial2d", "spatial2d")))
df2$diagnosis <- factor(rbinom(nrow(df2), 1, 0.5), labels=c("normal", "cancer"))
varMetadata(df2)["diagnosis", "labelDescription"] <- "disease pathology"
df2[["time", labelDescription="time measured"]] <- rep(date(), nrow(df2))
pData(df2)
varMetadata(df2)

# Change labels and pixel coord
coordNames(df2) <- c("x1", "x2")
pixelNames(df2) <- paste("p", 1:nrow(df2), sep="")
sampleNames(df2) <- c("subject A", "subject B")
coord(df2) <- coord(df2)[nrow(df2):1,]
pData(df2)
}

\keyword{classes}
