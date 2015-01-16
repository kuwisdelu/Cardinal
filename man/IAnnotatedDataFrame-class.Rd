\name{IAnnotatedDataFrame-class}
\docType{class}

\alias{class:IAnnotatedDataFrame}
\alias{IAnnotatedDataFrame}
\alias{IAnnotatedDataFrame-class}

\alias{coord,IAnnotatedDataFrame-method}
\alias{coord<-,IAnnotatedDataFrame-method}
\alias{coordLabels,IAnnotatedDataFrame-method}
\alias{coordLabels<-,IAnnotatedDataFrame-method}
\alias{initialize,IAnnotatedDataFrame-method}
\alias{pixelNames,IAnnotatedDataFrame-method}
\alias{pixelNames<-,IAnnotatedDataFrame-method}
\alias{sampleNames,IAnnotatedDataFrame-method}
\alias{sampleNames<-,IAnnotatedDataFrame-method}
\alias{sampleNames<-,IAnnotatedDataFrame,ANY-method}
\alias{combine,IAnnotatedDataFrame,IAnnotatedDataFrame-method}
\alias{[,IAnnotatedDataFrame,ANY,ANY,ANY-method}

\title{Class Containing Measured Variables and Their Meta-Data Description for Imaging Experiments}

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

\section{Slots}{
	\describe{
		\item{\code{data}:}{Object of class \code{data.frame} containing pixels (rows) and measured variables (columns). Contains at least one column named 'sample' which is a \code{factor} and gives the sample names for each pixel. The sample names can be set using \code{sampleNames<-}. Inherited from \linkS4class{AnnotatedDataFrame}.}

		\item{\code{varMetadata}:}{Object of class \code{data.frame} with number of rows equal to the number of columns in \code{data}. Contains at least two columns, one named 'labelDescription' giving a textual description of each variable, and an additional one named 'labelType' describing the type of variable. The 'labelType' is a factor with levels \code{"dim", "sample", "pheno"}. Inherited from \linkS4class{AnnotatedDataFrame}}
		
		\item{\code{dimLabels}:}{Object of class \code{character}  of length 2 that provides labels for the rows and columns in the \code{show} method. Inherited from \linkS4class{AnnotatedDataFrame}.}

		\item{\code{.__classVersion__}:}{A \code{Versions} object describing the version of the class used to created the instance. Intended for developer use.}
	}
}

\section{Extends}{
   Class \code{\linkS4class{AnnotatedDataFrame}}, directly.
   Class \code{\linkS4class{Versioned}}, by class "AnnotatedDataFrame", distance 2.
}

\section{Creating Objects}{
	\code{IAnnotatedDataFrame} instances are usually created through \code{IAnnotatedDataFrame()}.
}

\section{Methods}{
	Class-specific methods:
	\describe{
		\item{\code{sampleNames(object)}, \code{sampleNames(object)<-}:}{Return or set the sample names in the object, as determined by the factor levels of the 'sample' variable in \code{data}.}

		\item{\code{pixelNames(object)}, \code{pixelNames(object)<-}:}{Return or set the pixel names (the rows of \code{data}).}

		\item{\code{coordLabels(object)}, \code{coordLabels(object)<-}:}{Return or set the names of the pixel coodinates. These are the subset of \code{varLabels(object) for which the corresponding variables have a 'labelType' of 'dim'.} Note that this will \emph{never} include 'sample', even if the 'sample' variable has type 'dim'. (See details.)}

		\item{\code{coord(object)}, \code{coord(object)<-}:}{Return or set the coodinates. This is a \code{data.frame} containing the subset of columns of \code{data} for which the variables have a 'labelType' of 'dim'.}
	}

	Standard generic methods:
    \describe{
        \item{\code{combine(x, y, ...)}:}{Combine two or more \code{IAnnotatedDataFrame} objects. The objects are combined similarly to 'rbind' for \code{data.frame} objects. Pixels coordinates are checked for uniqueness. The 'varLabels' and 'varMetadata' must match.}
    }
}

\details{
	The key difference between a \code{IAnnotatedDataFrame} and a \code{AnnotatedDataFrame} is that an \code{IAnnotatedDataFrame} makes a distinction between samples and pixels, recognizing that rows belong to pixels, many of which may belong to the same sample. Therefore, \code{data} contains a required column called 'sample', which indicates the sample to which the pixel belongs, and \code{varMetadata} contains an additional required column called 'labelType', which indicates whether a variable is a spatial dimensions ('dim') or a phenotype ('pheno') or a sample ('sample'). The 'labelType' of the 'sample' variable depends on the structure of the experiment. See below for details.

	The 'labelType' for 'sample' will be 'sample' in the case of a 2D imaging experiment with a single sample. The 'labelType' for 'sample' will be 'dim' in the case of a 2D imaging experiment with multiple samples, since the 'sample' will be acting as a proxy spatial coordinate. Note however that in this case, the result of a call to \code{coordLabels} will \emph{not} include 'sample'.

	It is possible to compare the results of \code{names(coord(object))} and \code{coordLabels(object)} to distinguish between coordinate types that should be considered independent. It will be assumed a spatial relationship exists for all variables returned by \code{coordLabels(object)}, but this is not necessarily true for all variables returned by \code{names(coord(object))}. This is required, because every row in the \code{data.frame} returned by \code{coord(object)} should be unique and correspond to a unique pixel.

	The suggested structure for 3D imaging experiments is to create an additional variable solely to refer to the spatial dimension (e.g., 'z') and treat it separately from the 'sample'. Therefore, in a 3D imaging experiment with a single sample, the 'labelType' for 'sample' would be 'sample'.
}

\author{Kyle D. Bemis}

\seealso{
	\code{\link[Biobase]{AnnotatedDataFrame}},
	\code{\link{iSet}},
	\code{\link{SImageSet}}
	\code{\link{MSImageSet}}
}

\examples{
## Create an IAnnotatedDataFrame object
IAnnotatedDataFrame()

## Simple IAnnotatedDataFrame
df1 <- IAnnotatedDataFrame(data=expand.grid(x=1:3, y=1:3),
	varMetadata=data.frame(labelType=c("dim", "dim")))
pData(df1)
varMetadata(df1)

# Example of possible experiment data
coord <- expand.grid(x=1:3, y=1:3)
df2 <- IAnnotatedDataFrame(data=
	data.frame(rbind(coord, coord), sample=factor(rep(1:2, each=nrow(coord)))),
	varMetadata=data.frame(labelType=c("dim", "dim")))
df2$diagnosis <- factor(rbinom(nrow(df2), 1, 0.5), labels=c("normal", "cancer"))
varMetadata(df2)["diagnosis", "labelDescription"] <- "disease pathology"
df2[["time", labelDescription="time measured"]] <- rep(date(), nrow(df2))
pData(df2)
varMetadata(df2)

# Change labels and pixel coord
coordLabels(df2) <- c("x1", "x2")
pixelNames(df2) <- paste("p", 1:nrow(df2), sep="")
sampleNames(df2) <- c("subject A", "subject B")
coord(df2) <- coord(df2)[nrow(df2):1,]
pData(df2)
}

\keyword{classes}
