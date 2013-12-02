\name{MIAPE-Imaging-class}
\docType{class}

\alias{class:MIAPE-Imaging}
\alias{MIAPE-Imaging}
\alias{MIAPE-Imaging-class}

\alias{show,MIAPE-Imaging-method}
\alias{msInfo,MIAPE-Imaging-method}
\alias{expemail,MIAPE-Imaging-method}
\alias{exptitle,MIAPE-Imaging-method}
\alias{ionSource,MIAPE-Imaging-method}
\alias{ionSourceDetails,MIAPE-Imaging-method}
\alias{analyser,MIAPE-Imaging-method}
\alias{analyserDetails,MIAPE-Imaging-method}
\alias{analyzer,MIAPE-Imaging-method}
\alias{analyzerDetails,MIAPE-Imaging-method}
\alias{detectorType,MIAPE-Imaging-method}
\alias{instrumentModel,MIAPE-Imaging-method}
\alias{instrumentManufacturer,MIAPE-Imaging-method}
\alias{instrumentCustomisations,MIAPE-Imaging-method}
\alias{abstract,MIAPE-Imaging-method}
\alias{samples,MIAPE-Imaging-method}
\alias{pubMedIds,MIAPE-Imaging-method}
\alias{pubMedIds<-,MIAPE-Imaging-method}
\alias{otherInfo,MIAPE-Imaging-method}
\alias{expinfo,MIAPE-Imaging-method}
\alias{notes,MIAPE-Imaging-method}
\alias{notes<-,MIAPE-Imaging-method}
\alias{combine,MIAPE-Imaging,MIAPE-Imaging-method}

\alias{imagingInfo}
\alias{imagingInfo,MIAPE-Imaging-method}
\alias{specimenOrigin}
\alias{specimenOrigin,MIAPE-Imaging-method}
\alias{specimenType}
\alias{specimenType,MIAPE-Imaging-method}
\alias{stainingMethod}
\alias{stainingMethod,MIAPE-Imaging-method}
\alias{tissueThickness}
\alias{tissueThickness,MIAPE-Imaging-method}
\alias{tissueWash}
\alias{tissueWash,MIAPE-Imaging-method}
\alias{embeddingMethod}
\alias{embeddingMethod,MIAPE-Imaging-method}
\alias{inSituChemistry}
\alias{inSituChemistry,MIAPE-Imaging-method}
\alias{matrixDeposition}
\alias{matrixDeposition,MIAPE-Imaging-method}
\alias{pixelSize}
\alias{pixelSize,MIAPE-Imaging-method}
\alias{scanType}
\alias{scanType,MIAPE-Imaging-method}
\alias{scanPattern}
\alias{scanPattern,MIAPE-Imaging-method}
\alias{scanDirection}
\alias{scanDirection,MIAPE-Imaging-method}
\alias{lineScanDirection}
\alias{lineScanDirection,MIAPE-Imaging-method}
\alias{imageShape}
\alias{imageShape,MIAPE-Imaging-method}

\title{Class for Storing Mass Spectrometry Imaging Experiment Information.}

\description{
    The Minimum Information About a Proteomics Experiment for Imaging. The current implementation is based in part on the imzML specification and on a working document for an MS imaging MIAPE (see References). Subject to change.
}

\section{Slots}{
    \describe{
        \item{\code{title}:}{Object of class \code{character} containing a single-sentence experiment title. Inherited from \code{\linkS4class{MIAPE}}.}

        \item{\code{abstract}:}{Object of class \code{character} containing an abstract describing the experiment. Inherited from \code{\linkS4class{MIAPE}}.}
        
        \item{\code{url}:}{Object of class \code{character} containing a URL for the experiment. Inherited from \code{\linkS4class{MIAPE}}.}
        
        \item{\code{pubMedIds}:}{Object of class \code{character} listing strings of PubMed identifiers of papers relevant to the dataset. Inherited from \code{\linkS4class{MIAPE}}.}

        \item{\code{samples}:}{Object of class \code{list} containing information about the samples. Inherited from \code{\linkS4class{MIAPE}}.}
        
        \item{\code{preprocessing}:}{Object of class \code{list} containing information about the pre-processing steps used on the raw data from this experiment. Inherited from \code{\linkS4class{MIAPE}}.}
        
        \item{\code{other}:}{Object of class \code{list} containing other information for which none of the above slots does not applies. Inherited from \code{\linkS4class{MIAPE}}.}
        
        \item{\code{dateStamp}:}{Object of class \code{character}, giving the date on which the work described was initiated; given in the standard 'YYYY-MM-DD' format (with hyphens). Inherited from \code{\linkS4class{MIAPE}}.}
        
        \item{\code{name}:}{Object of class \code{character} containing the name of the (stable) primary contact person for this data set; this could be the experimenter, lab head, line manager, \ldots Inherited from \code{\linkS4class{MIAPE}}.}
        
        \item{\code{lab}:}{Object of class \code{character} containing the laboratory where the experiment was conducted. Inherited from \code{\linkS4class{MIAPE}}.}
        
        \item{\code{contact}:}{Object of class \code{character} containing contact information for lab and/or experimenter. Inherited from \code{\linkS4class{MIAPE}}.}
        
        \item{\code{email}:}{Object of class \code{character} containing email contact information for the primary contact person (see \code{name} above). Inherited from \code{\linkS4class{MIAPE}}.} 
        
        \item{\code{instrumentModel}:}{Object of class \code{character} indicating the model of the mass spectrometer used to generate the data. Inherited from \code{\linkS4class{MIAPE}}.}
        
        \item{\code{instrumentManufacturer}:}{Object of class \code{character} indicating the manufacturing company of the mass spectrometer. Inherited from \code{\linkS4class{MIAPE}}.}
        
        \item{\code{instrumentCustomisations}:}{Object of class \code{character} describing any significant (i.e. affecting behaviour) deviations from the manufacturer's specification for the mass spectrometer. Inherited from \code{\linkS4class{MIAPE}}.}
        
        \item{\code{softwareName}:}{Object of class \code{character} with the instrument management and data analysis package(s) name(s). Inherited from \code{\linkS4class{MIAPE}}.}
        
        \item{\code{softwareVersion}:}{Object of class \code{character} with the instrument management and data analysis package(s) version(s). Inherited from \code{\linkS4class{MIAPE}}.}
        
        \item{\code{switchingCriteria}:}{Not used. Inherited from \code{\linkS4class{MIAPE}}.}
        
        \item{\code{isolationWidth}:}{Not used. Inherited from \code{\linkS4class{MIAPE}}.}
        
        \item{\code{parameterFile}:}{Object of class \code{character} giving the location and name under which the mass spectrometer's parameter settings file for the run is stored, if available. Ideally this should be a URI+filename, or most preferably an LSID, where feasible. Inherited from \code{\linkS4class{MIAPE}}.}
        
        \item{\code{ionSource}:}{Object of class \code{character} describing the ion source (MALDI, DESI, \ldots). Inherited from \code{\linkS4class{MIAPE}}.}
        
        \item{\code{ionSourceDetails}:}{Object of class \code{character} describing the relevant details about the ion source. See MIAPE-MI document for more details. Inherited from \code{\linkS4class{MIAPE}}.}
        
        \item{\code{analyser}:}{Object of class \code{character} describing the analyzer type (Quadrupole, time-of-flight, ion trap, \ldots). Inherited from \code{\linkS4class{MIAPE}}.}
        
        \item{\code{analyserDetails}:}{Object of class \code{character} describing the relevant details about the analyzer. See MIAPE-MI document for more details. Inherited from \code{\linkS4class{MIAPE}}.}
        
        \item{\code{collisionGas}:}{Object of class \code{character} describing the composition of the gas used to fragment ions in the collision cell. Inherited from \code{\linkS4class{MIAPE}}.}
        
        \item{\code{collisionPressure}:}{Object of class \code{numeric} providing the pressure (in bars) of the collision gas. Inherited from \code{\linkS4class{MIAPE}}.}
        
        \item{\code{collisionEnergy}:}{Object of class \code{character} specifying for the process of imparting a particular impetus to ions with a given m/z value, as they travel into the collision cell for fragmentation. Inherited from \code{\linkS4class{MIAPE}}.}
        
        \item{\code{detectorType}:}{Object of class \code{character} describing the type of detector used in the machine (microchannel plate, channeltron, \ldots). Inherited from \code{\linkS4class{MIAPE}}.}
        
        \item{\code{detectorSensitivity}:}{Object of class \code{character} giving and appropriate measure of the sensitivity of the described detector (e.g. applied voltage). Inherited from \code{\linkS4class{MIAPE}}.}

        \item{\code{specimenOrigin}:}{Object of class \code{character} describing the specimen origin (institution, \ldots).}

        \item{\code{specimenType}:}{Object of class \code{character} describing the specimen type (species, organ, \ldots).}

        \item{\code{stainingMethod}:}{Object of class \code{character} describing the staining method, if any, applied to the sample (H&E, \ldots).}

        \item{\code{tissueThickness}:}{Object of class \code{numeric} giving the tissue thickness in micrometers (um).}

        \item{\code{tissueWash}:}{Object of class \code{character} describing the wash method (spray, dipping, \ldots).}

        \item{\code{embeddingMethod}:}{Object of class \code{character} describing the embedding method (if any); this could be paraffin, \ldots}

        \item{\code{inSituChemistry}:}{Object of class \code{character} describing any on-sample chemistry (tryptic digest, \ldots)}

        \item{\code{matrixDeposition}:}{Object of class \code{character} describing how the matrix was applied, if applicable}

        \item{\code{pixelSize}:}{Object of class \code{numeric} describing the size of the pixels in micrometers (um).}

        \item{\code{scanType}:}{Object of class \code{character} describing the scan type. This must be either 'horizontal line scan' or 'vertical line scan'. See the imzML specifications for more details.}

        \item{\code{scanPattern}:}{Object of class \code{character} describing the scan type. This must be one of 'flyback', 'meandering', or 'random access'. See the imzML specifications for more details.}

        \item{\code{scanDirection}:}{Object of class \code{character} describing the scan type. This must be one of 'bottom up', 'left right', 'right left', or 'top down'. See the imzML specifications for more details.}

        \item{\code{lineScanDirection}:}{Object of class \code{character} describing the scan type. This must be one of 'linescan bottom up', 'linescan left right', 'linescan right left', or 'linescan top down'. See the imzML specifications for more details.}

        \item{\code{imageShape}:}{Object of class \code{character} describing the image shape (rectangular, free form, \ldots). See the imzML specifications for more details.}
    }
}

\section{Extends}{
   Class \code{\linkS4class{MIAPE}}, directly.
   Class \code{\linkS4class{MIAxE}}, by class "MIAPE", distance 2.
   Class \code{\linkS4class{Versioned}}, by class "MIAxE", distance 3.
}

\section{Creating Objects}{
    \code{MIAPE-Imaging} instances can be created through \code{new("MIAPE-Imaging")}. In general, instances should not be created by the user, but are automatically generated when reading an external file to create an \code{\linkS4class{MSImageSet}} object.
}

\section{Methods}{
    Inherited from \code{\linkS4class{MIAPE}}:
    \describe{
        \item{\code{msInfo}:}{Displays 'MIAPE-MS' information.}

        \item{\code{abstract}:}{An accessor function for \code{abstract}. }
        
        \item{\code{expinfo}:}{An accessor function for \code{name}, \code{lab}, \code{contact}, \code{title}, and \code{url}.} 
        
        \item{\code{notes(object), notes(object) <- value}:}{Accessor functions for \code{other}. \code{notes(object) <- character} \emph{appends} character to notes; use \code{notes(object) <- list} to replace the notes entirely.} 
        
        \item{\code{otherInfo}:}{An accessor function for \code{other}.} 
        
        \item{\code{preproc}:}{An accessor function for \code{preprocessing}.} 
        
        \item{\code{pubMedIds(object), pubMedIds(object) <- value}:}{Accessor function for \code{pubMedIds}.} 
        
        \item{\code{expemail}:}{An accessor function for  \code{email} slot.}
        
        \item{\code{exptitle}:}{An accessor function for \code{title} slot.}
        
        \item{\code{analyzer}:}{ An accessor function for \code{analyser} slot. \code{analyser} is also available.}
        
        \item{\code{analyzerDetails}:}{ An accessor function for \code{analyserDetails} slot. \code{analyserDetails} is also available. }
        
        \item{\code{detectorType}:}{ An accessor function for \code{detectorType} slot.}
        
        \item{\code{ionSource}:}{ An accessor function for \code{ionSource} slot.}
        
        \item{\code{ionSourceDetails}:}{ An accessor function for \code{ionSourceDetails} slot.}
        
        \item{\code{instrumentModel}:}{ An accessor function for \code{instrumentModel} slot.}
        
        \item{\code{instrumentManufacturer}:}{ An accessor function for \code{instrumentManufacturer} slot.}
        
        \item{\code{instrumentCustomisations}:}{ An accessor function for \code{instrumentCustomisations} slot.}
    }

    Class-specific methods:
    \describe{
        \item{\code{imagingInfo}:}{Displays 'MIAPE-Imaging' information.}

        \item{\code{specimenOrigin}:}{Accessor function for the \code{specimenOrigin} slot.}

        \item{\code{specimenType}:}{Accessor function for the \code{specimenType} slot.}

        \item{\code{stainingMethod}:}{Accessor function for the \code{stainingMethod} slot.}

        \item{\code{tissueThickness}:}{Accessor function for the \code{tissueThickness} slot.}

        \item{\code{tissueWash}:}{Accessor function for the \code{tissueWash} slot.}

        \item{\code{embeddingMethod}:}{Accessor function for the \code{embeddingMethod} slot.}

        \item{\code{inSituChemistry}:}{Accessor function for the \code{inSituChemistry} slot.}

        \item{\code{matrixDeposition}:}{Accessor function for the \code{matrixDeposition} slot.}

        \item{\code{pixelSize}:}{Accessor function for the \code{pixelSize} slot.}

        \item{\code{scanType}:}{Accessor function for the \code{scanType} slot.}

        \item{\code{scanPattern}:}{Accessor function for the \code{scanPattern} slot.}

        \item{\code{scanDirection}:}{Accessor function for the \code{scanDirection} slot.}

        \item{\code{lineScanDirection}:}{Accessor function for the \code{linScanDirection} slot.}

        \item{\code{imageShape}:}{Accessor function for the \code{imageShape} slot.}
    }

    Standard generic methods:
    \describe{
        \item{\code{show}:}{Displays object content as ASCII text.}

        \item{\code{combine(x, y, ...)}:}{Combine two or more \code{MIAPE-Imaging} objects.}
    }
}

\author{Kyle D. Bemis}

\references{
  Imaging MIAPE working document: \url{http://imzml.org/index.php?option=com_fireboard&Itemid=53&func=view&catid=516&id=737}

  imzML specification: \url{http://www.imzml.org/index.php?option=com_content&view=article&id=187&Itemid=67}
}

\seealso{
    \code{\linkS4class{MIAPE}},
    \code{\linkS4class{MSImageSet}}
}

\examples{
showClass("MIAPE-Imaging")
}

\keyword{classes}
