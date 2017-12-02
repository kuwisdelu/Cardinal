
#### Write imzML files ####
## ----------------------

setMethod("writeImzML", "MSImageSet",
	function(object, name, folder = getwd(), merge = FALSE,
		mz.type = "32-bit float", intensity.type = "32-bit float", ...)
	{
		if ( merge )
			stop("'merge = TRUE' is not currently supported")
		if ( length(sampleNames(object)) > 1 ) {
			samples <- sampleNames(object)
			result <- sapply(samples, function(nm) {
				tmp <- object[,pData(object)$sample == nm]
				nm2 <- paste0(name, "-", nm)
				writeImzML(tmp, nm2, folder, ...)
			})
			return(invisible(result))
		}
		# check for files
		xmlpath <- normalizePath(file.path(folder, paste(name, ".imzML", sep="")),
			mustWork=FALSE)
		if ( file.exists(xmlpath) ) .stop("writeImzML: ", xmlpath, " already exists")
		ibdpath <- normalizePath(file.path(folder, paste(name, ".ibd", sep="")),
			mustWork=FALSE)
		if ( file.exists(ibdpath) ) .stop("writeImzML: ", ibdpath, " already exists")
		# read ibd file
		.log("writeImzML: Reading ibd file '", ibdpath, "'")
		mzml <- .writeIbd.MSImageSet(object, ibdpath, mz.type, intensity.type)
		# read imzML file
		.log("writeImzML: Writing imzML file '", xmlpath, "'")
		result <- .writeImzML(mzml, xmlpath)
		invisible(result)
	})

.writeImzML <- function(metadata, file) {
	tmpl <- .templateImzML(metadata)
	result <- .Call("writeImzML", metadata, tmpl,
		normalizePath(file, mustWork=FALSE))
	invisible(result)
}

.writeIbd.MSImageSet <- function(x, file, mzType, intensityType) {
	if ( is.null(file) )
		return(info)
	file <- normalizePath(file, mustWork=FALSE)
	if ( file.exists(file) )
		.stop(paste0("file '", file, "' already exists"))
	if ( !file.create(file) )
		.stop(paste0("couldn't create file '", file, "'"))
	info <- .metadataImzML(x, mzType, intensityType)
	matter.cast.warning <- getOption("matter.cast.warning")
	options(matter.cast.warning=FALSE)
	id <- uuid(uppercase=TRUE)
	pid <- matter_vec(length=16, paths=file, filemode="rb+", datamode="raw")
	pid[] <- id$bytes
	pmz <- matter_vec(length=nrow(x), paths=file, filemode="rb+",
		offset=info$mzArrayList[["external offset"]][1],
		extent=info$mzArrayList[["external array length"]][1],
		datamode=modeof.ibdtype(mzType))
	pmz[] <- mz(x)
	pspectra <- matter_mat(nrow=nrow(x), ncol=ncol(x), paths=file, filemode="rb+",
		offset=info$intensityArrayList[["external offset"]],
		extent=info$intensityArrayList[["external array length"]],
		datamode=modeof.ibdtype(intensityType))
	for ( i in seq_len(ncol(x)) )
		pspectra[,i] <- spectra(x)[,i]
	options(matter.cast.warning=matter.cast.warning)
	hash <- checksum(pspectra, algo="sha1")
	info$experimentMetadata[["universally unique identifier"]] <- paste0("{", id$string, "}")
	info$experimentMetadata[["ibd SHA-1"]] <- toupper(as.character(hash))
	info
}

.metadataImzML <- function(x, mzType, intensityType) {
	scanList <- coord(x)[coordLabels(x)]
	positionNames <- c("position x", "position y", "position z")
	names(scanList) <- positionNames[seq_along(coordLabels(x))]
	mzArrayList <- data.frame(
		"external offset"=rep(16, ncol(x)),
		"external array length"=rep(nrow(x), ncol(x)),
		"external encoded length"=rep(sizeof.ibdtype(mzType) * nrow(x), ncol(x)),
		"binary data type"=rep(mzType, ncol(x)),
		stringsAsFactors=FALSE, check.names=FALSE)
	intensityArrayList <- data.frame(
		"external offset"=rep(16 + sizeof.ibdtype(mzType) * nrow(x), ncol(x)),
		"external array length"=rep(nrow(x), ncol(x)),
		"external encoded length"=rep(sizeof.ibdtype(intensityType) * nrow(x), ncol(x)),
		"binary data type"=rep(intensityType, ncol(x)),
		stringsAsFactors=FALSE, check.names=FALSE)
	offset <- c(0, cumsum(intensityArrayList[["external encoded length"]][-ncol(x)]))
	intensityArrayList[["external offset"]] <- offset + intensityArrayList[["external offset"]]
	spectrumType <- ifelse(centroided(x), "centroid spectrum", "profile spectrum")
	experimentMetadata <- list(
		"spectrum representation"=spectrumType,
		"ibd binary type"="continuous")
	list(experimentMetadata=experimentMetadata,
		scanList=scanList,
		mzArrayList=mzArrayList,
		intensityArrayList=intensityArrayList)
}

.templateImzML <- function(x, version = packageVersion("Cardinal"))
{
	mzml <- 
	'<?xml version="1.0" encoding="ISO-8859-1"?>
	<mzML version="1.1" xmlns="http://psi.hupo.org/ms/mzml" xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xsi:schemaLocation="http://psi.hupo.org/ms/mzml http://psidev.info/files/ms/mzML/xsd/mzML1.1.0_idx.xsd">
		<cvList count="3">
			<cv id="MS" fullName="Proteomics Standards Initiative Mass Spectrometry Ontology" version="1.3.1" URI="http://psidev.info/ms/mzML/psi-ms.obo"/>
			<cv id="UO" fullName="Unit Ontology" version="1.15" URI="http://obo.cvs.sourceforge.net/obo/obo/ontology/phenotype/unit.obo"/>
			<cv id="IMS" fullName="Imaging MS Ontology" version="0.9.1" URI="http://www.maldi-msi.org/download/imzml/imagingMS.obo"/>
		</cvList>
		<fileDescription>
			<fileContent>
				<cvParam cvRef="MS" accession="MS:1000579" name="MS1 spectrum" value=""/>
			</fileContent>
		</fileDescription>
		<referenceableParamGroupList count="4">
			<referenceableParamGroup id="spectrum1">
				<cvParam cvRef="MS" accession="MS:1000579" name="MS1 spectrum" value=""/>
				<cvParam cvRef="MS" accession="MS:1000511" name="ms level" value="0"/>
			</referenceableParamGroup>
			<referenceableParamGroup id="scan1">
				<cvParam cvRef="MS" accession="MS:1000093" name="increasing m/z scan" value=""/>
				<cvParam cvRef="MS" accession="MS:1000095" name="linear" value=""/>
			</referenceableParamGroup>
			<referenceableParamGroup id="mzArray">
				<cvParam cvRef="MS" accession="MS:1000576" name="no compression" value=""/>
				<cvParam cvRef="MS" accession="MS:1000514" name="m/z array" value="" unitCvRef="MS" unitAccession="MS:1000040" unitName="m/z"/>
				<cvParam cvRef="IMS" accession="IMS:1000101" name="external data" value="true"/>
			</referenceableParamGroup>
			<referenceableParamGroup id="intensityArray">
				<cvParam cvRef="MS" accession="MS:1000576" name="no compression" value=""/>
				<cvParam cvRef="MS" accession="MS:1000515" name="intensity array" value="" unitCvRef="MS" unitAccession="MS:1000131" unitName="number of counts"/>
				<cvParam cvRef="IMS" accession="IMS:1000101" name="external data" value="true"/>
			</referenceableParamGroup>
		</referenceableParamGroupList>
		<sampleList count="1">
			<sample id="sample1" name="Sample1">
				<cvParam cvRef="MS" accession="MS:1000001" name="sample number" value="1"/>
			</sample>
		</sampleList>
		<softwareList count="1">
			<software id="Cardinal" version="%s">
				<cvParam cvRef="MS" accession="MS:1000799" name="custom unreleased software tool" value=""/>
			</software>
		</softwareList>
		<scanSettingsList count="1">
			<scanSettings id="scansettings1">
				<cvParam cvRef="IMS" accession="IMS:1000042" name="max count of pixel x" value="%d"/>
				<cvParam cvRef="IMS" accession="IMS:1000043" name="max count of pixel y" value="%d"/>
			</scanSettings>
		</scanSettingsList>
		<instrumentConfigurationList count="1">
			<instrumentConfiguration id="IC1">
		    </instrumentConfiguration>
		</instrumentConfigurationList>
		<dataProcessingList count="1">
			<dataProcessing id="CardinalWriteImzML">
				<processingMethod order="1" softwareRef="Cardinal">
					<cvParam cvRef="MS" accession="MS:1000544" name="Conversion to mzML" value=""/>
				</processingMethod>
			</dataProcessing>
		</dataProcessingList>
		<run defaultInstrumentConfigurationRef="IC1" id="Experiment01" sampleRef="sample1">
			<spectrumList count="%d" defaultDataProcessingRef="CardinalWriteImzML">
				<spectrum id="Spectrum=1" defaultArrayLength="0" index="1">
					<referenceableParamGroupRef ref="spectrum1"/>
					<scanList count="1">
						<cvParam cvRef="MS" accession="MS:1000795" name="no combination" value=""/>
						<scan instrumentConfigurationRef="IC1">
							<referenceableParamGroupRef ref="scan1"/>
						</scan>
					</scanList>
					<binaryDataArrayList count="2">
						<binaryDataArray encodedLength="0">
							<referenceableParamGroupRef ref="mzArray"/>
							<binary/>
						</binaryDataArray>
						<binaryDataArray encodedLength="0">
							<referenceableParamGroupRef ref="intensityArray"/>
							<binary/>
						</binaryDataArray>
					</binaryDataArrayList>
				</spectrum>
			</spectrumList>
		</run>
	</mzML>'
	sprintf(mzml, version,
		"max count of x" = as.integer(max(x$scanList[["position x"]])),
		"max count of y" = as.integer(max(x$scanList[["position y"]])),
		"count" = nrow(x$scanList))
}

