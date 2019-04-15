
#### Write imzML files ####
## ----------------------

writeImzML <- function(object, name, folder = getwd(), merge = FALSE,
		mz.type = "32-bit float", intensity.type = "32-bit float", ...)
	{
		if ( merge )
			.stop("option 'merge = TRUE' is not supported")
		if ( is(object, "MSImageSet") && length(sampleNames(object)) > 1 ) {
			samples <- sampleNames(object)
			result <- sapply(samples, function(nm) {
				tmp <- object[,pData(object)$sample == nm]
				name2 <- paste0(name, "-", nm)
				writeImzML(tmp, name2, folder,
					intensity.type=intensity.type, ...)
			})
			return(invisible(result))
		}
		if ( is(object, "MSImagingExperiment") && length(runNames(object)) > 1 ) {
			runs <- runNames(object)
			result <- sapply(runs, function(id) {
				tmp <- object[,run(object) == id]
				name2 <- paste0(name, "-", id)
				writeImzML(tmp, name2, folder,
					intensity.type=intensity.type, ...)
			})
			return(invisible(result))
		}
		# check for files
		xmlpath <- normalizePath(file.path(folder, paste(name, ".imzML", sep="")),
			mustWork=FALSE)
		if ( file.exists(xmlpath) ) .stop("file ", xmlpath, " already exists")
		ibdpath <- normalizePath(file.path(folder, paste(name, ".ibd", sep="")),
			mustWork=FALSE)
		if ( file.exists(ibdpath) ) .stop("file ", ibdpath, " already exists")
		# read ibd file
		.message("writing ibd file '", ibdpath, "'")
		info <- .writeIbd(object, ibdpath, mz.type, intensity.type)
		# read imzML file
		.message("writing imzML file '", xmlpath, "'")
		result <- .writeImzML(info, xmlpath)
		if ( result )
			.message("done.")
		invisible(result)
	}

.writeIbd <- function(x, file, mz.type, intensity.type) {
	file <- normalizePath(file, mustWork=FALSE)
	if ( !file.create(file) )
		.stop(paste0("couldn't create file '", file, "'"))
	info <- msiInfo(x, mz.type=mz.type, intensity.type=intensity.type)
	warn <- getOption("matter.cast.warning")
	options(matter.cast.warning=FALSE)
	id <- uuid(uppercase=FALSE)
	pid <- matter_vec(length=16, paths=file, filemode="rw", datamode="raw")
	pid[] <- id$bytes
	if ( metadata(info)[["ibd binary type"]] == "continuous" ) {
		# write 'continuous' imzML
		pmz <- matter_vec(length=nrow(x), paths=file, filemode="rw",
			offset=mzData(info)[["external offset"]][1L],
			extent=mzData(info)[["external array length"]][1L],
			datamode=Ctypeof(mz.type))
		pmz[] <- mz(x)
		pspectra <- matter_mat(nrow=nrow(x), ncol=ncol(x), paths=file, filemode="rw",
			offset=imageData(info)[["external offset"]],
			extent=imageData(info)[["external array length"]],
			datamode=Ctypeof(intensity.type))
		for ( i in seq_len(ncol(x)) )
			pspectra[,i] <- iData(x)[,i]
	} else if ( metadata(info)[["ibd binary type"]] == "processed" ) {
		# write 'processed' imzML
		pmz <- matter_list(paths=file, filemode="rw",
			offset=mzData(info)[["external offset"]],
			extent=mzData(info)[["external array length"]],
			datamode=Ctypeof(mz.type))
		pspectra <- matter_list(paths=file, filemode="rw",
			offset=imageData(info)[["external offset"]],
			extent=imageData(info)[["external array length"]],
			datamode=Ctypeof(intensity.type))
		for ( i in seq_len(ncol(x)) ) {
			pmz[[i]] <- mzData(x)[[i]]
			pspectra[[i]] <- peakData(x)[[i]]
		}
	} else {
		.stop("invalid 'ibd binary type' found")
	}
	options(matter.cast.warning=warn)
	hash <- checksum(pspectra, algo="sha1")
	metadata(info)[["universally unique identifier"]] <- paste0("{", id$string, "}")
	metadata(info)[["ibd SHA-1"]] <- tolower(as.character(hash))
	info
}

.writeImzML <- function(info, file) {
	metadata <- as.list(info)
	template <- .templateImzML(info)
	result <- .Call("C_writeImzML", metadata, template,
		normalizePath(file, mustWork=FALSE), PACKAGE="Cardinal")
	invisible(result)
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
				<cvParam cvRef="IMS" accession="IMS:1000046" name="pixel size x" value="%f"/>
				<cvParam cvRef="IMS" accession="IMS:1000047" name="pixel size y" value="%f"/>
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
		"max count of x" = as.integer(max(scans(x)[["position x"]])),
		"max count of y" = as.integer(max(scans(x)[["position y"]])),
		"pixel size x" = min(diff(sort(unique(scans(x)[["position x"]])))),
		"pixel size x" = min(diff(sort(unique(scans(x)[["position y"]])))),
		"count" = nrow(scans(x)))
}

