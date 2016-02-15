
#include <R.h>
#include <Rdefines.h>

#include <stdio.h>
#include <stdlib.h>

#include "OBO-IMS.h"
#include "OBO-MS.h"
#include "OBO-UO.h"

#include "pugixml.h"
#include "utils.h"

// utility functions for reading ibd

template<typename CType, typename RType>
SEXP readContinuousMzArray(const char * filename, double offset, int length)
{
	FILE * pfile = fopen(filename, "rb");
	if ( pfile == NULL ) return R_NilValue;
	fseek(pfile, (long) offset, SEEK_SET);
	SEXP data;
	PROTECT(data = allocVector(DataType<RType>(), length));
	RType * pdata = DataPtr<RType>(data);
	CType tmp[length];
	fread(tmp, sizeof(CType), length, pfile);
	fclose(pfile);
	for ( int i = 0; i < length; i++ )
	{
		pdata[i] = (RType) tmp[i];
	}
	UNPROTECT(1);
	return data;
}

template<typename CType, typename RType>
SEXP readContinuousIntensityArray(const char * filename, double offset, int nrow, int ncol)
{
	FILE * pfile = fopen(filename, "rb");
	if ( pfile == NULL ) return R_NilValue;
	fseek(pfile, (long) offset, SEEK_SET);
	SEXP data;
	PROTECT(data = allocMatrix(DataType<RType>(), nrow, ncol));
	RType * pdata = DataPtr<RType>(data);
	CType tmp[nrow];
	for ( int j = 0; j < ncol; j++ )
	{
		fread(tmp, sizeof(CType), nrow, pfile);
		for ( int i = 0; i < nrow; i++ )
		{
			pdata[(nrow * j) + i] = (RType) tmp[i];
		}
	}
	fclose(pfile);
	UNPROTECT(1);
	return data;
}

template<typename CType, typename RType>
SEXP readProcessedIbdArray(const char * filename, double * offset, int * length, int count)
{
	FILE * pfile = fopen(filename, "rb");
	if ( pfile == NULL ) return R_NilValue;
	SEXP list;
	PROTECT(list = NEW_LIST(count));
	SEXP data;
	RType * pdata;
	CType * tmp;
	for ( int j = 0; j < count; j++ )
	{
		tmp = (CType *) Calloc(length[j], CType);
		fseek(pfile, (long) offset[j], SEEK_SET);
		fread(tmp, sizeof(CType), length[j], pfile);
		PROTECT(data = allocVector(DataType<RType>(), length[j]));
		pdata = DataPtr<RType>(data);
		for ( int i = 0; i < length[j]; i++ )
		{
			pdata[i] = (RType) tmp[i];
		}
		SET_VECTOR_ELT(list, j, data);
		UNPROTECT(1);
		Free(tmp);
	}
	fclose(pfile);
	UNPROTECT(1);
	return list;
}

// utility functions for parsing imzML

void insert_referenceableParamGroup(pugi::xml_node node)
{
	pugi::xml_node refGroupList = node.root().child("mzML").child("referenceableParamGroupList");
	pugi::xml_node refGroup = node.child("referenceableParamGroupRef");
	while ( refGroup )
	{
		pugi::xml_node refGroupList_elt = refGroupList.find_child_by_attribute("id", refGroup.attribute("ref").value());
		for ( pugi::xml_node refGroup_elt = refGroupList_elt.first_child();
			refGroup_elt;
			refGroup_elt = refGroup_elt.next_sibling() )
		{
			node.insert_copy_before(refGroup_elt, refGroup);
		}
		node.remove_child(refGroup);
		refGroup = node.child("referenceableParamGroupRef");
	}
}

const char * find_ibd_binary_type(pugi::xml_node node)
{
	if ( node.find_child_by_attribute("cvParam", "accession", IMS_CONTINUOUS_ID) )
		return IMS_CONTINUOUS_NAME;
	else if ( node.find_child_by_attribute("cvParam", "accession", IMS_PROCESSED_ID) )
		return IMS_PROCESSED_NAME;
	return "";
}

const char * find_ibd_identification(pugi::xml_node node)
{
	return(node.find_child_by_attribute("cvParam", "accession",
		IMS_UNIVERSALLY_UNIQUE_IDENTIFIER_ID).attribute("value").value());
}

const char * find_contact_name(pugi::xml_node node)
{
	return(node.find_child_by_attribute("cvParam", "accession",
		MS_CONTACT_NAME_ID).attribute("value").value());
}

const char * find_contact_organization(pugi::xml_node node)
{
	return(node.find_child_by_attribute("cvParam", "accession",
		MS_CONTACT_ORGANIZATION_ID).attribute("value").value());
}

const char * find_line_scan_direction(pugi::xml_node node)
{
	if ( node.find_child_by_attribute("cvParam", "accession", IMS_LINESCAN_RIGHT_LEFT_ID) )
		return IMS_LINESCAN_RIGHT_LEFT_NAME;
	else if ( node.find_child_by_attribute("cvParam", "accession", IMS_LINESCAN_LEFT_RIGHT_ID) )
		return IMS_LINESCAN_LEFT_RIGHT_NAME;
	else if ( node.find_child_by_attribute("cvParam", "accession", IMS_LINESCAN_BOTTOM_UP_ID) )
		return IMS_LINESCAN_BOTTOM_UP_NAME;
	else if ( node.find_child_by_attribute("cvParam", "accession", IMS_LINESCAN_TOP_DOWN_ID) )
		return IMS_LINESCAN_TOP_DOWN_NAME;
	return "";
}

const char * find_scan_direction(pugi::xml_node node)
{
	if ( node.find_child_by_attribute("cvParam", "accession", IMS_BOTTOM_UP_ID) )
		return IMS_BOTTOM_UP_NAME;
	else if ( node.find_child_by_attribute("cvParam", "accession", IMS_TOP_DOWN_ID) )
		return IMS_TOP_DOWN_NAME;
	else if ( node.find_child_by_attribute("cvParam", "accession", IMS_LEFT_RIGHT_ID) )
		return IMS_LEFT_RIGHT_NAME;
	else if ( node.find_child_by_attribute("cvParam", "accession", IMS_RIGHT_LEFT_ID) )
		return IMS_RIGHT_LEFT_NAME;
	else if ( node.find_child_by_attribute("cvParam", "accession", IMS_NO_DIRECTION_ID) )
		return IMS_NO_DIRECTION_NAME;
	return "";
}

const char * find_scan_pattern(pugi::xml_node node)
{
	if ( node.find_child_by_attribute("cvParam", "accession", IMS_MEANDERING_ID) )
		return IMS_MEANDERING_NAME;
	else if ( node.find_child_by_attribute("cvParam", "accession", IMS_RANDOM_ACCESS_ID) )
		return IMS_RANDOM_ACCESS_NAME;
	else if ( node.find_child_by_attribute("cvParam", "accession", IMS_FLYBACK_ID) )
		return IMS_FLYBACK_NAME;
	return "";
}

const char * find_scan_type(pugi::xml_node node)
{
	if ( node.find_child_by_attribute("cvParam", "accession", IMS_HORIZONTAL_LINE_SCAN_ID) )
		return IMS_HORIZONTAL_LINE_SCAN_NAME;
	else if ( node.find_child_by_attribute("cvParam", "accession", IMS_VERTICAL_LINE_SCAN_ID) )
		return IMS_VERTICAL_LINE_SCAN_NAME;
	return "";
}

int find_position_x(pugi::xml_node node)
{
	return(node.find_child_by_attribute("cvParam", "accession",
		IMS_POSITION_X_ID).attribute("value").as_int());
}

int find_position_y(pugi::xml_node node)
{
	return(node.find_child_by_attribute("cvParam", "accession",
		IMS_POSITION_Y_ID).attribute("value").as_int());
}

int find_position_z(pugi::xml_node node)
{
	return(node.find_child_by_attribute("cvParam", "accession",
		IMS_POSITION_Z_ID).attribute("value").as_int());
}

double find_3D_position_x(pugi::xml_node node)
{
	return(node.find_child_by_attribute("userParam", "name",
		"3DPositionX").attribute("value").as_double());
}

double find_3D_position_y(pugi::xml_node node)
{
	return(node.find_child_by_attribute("userParam", "name",
		"3DPositionY").attribute("value").as_double());
}

double find_3D_position_z(pugi::xml_node node)
{
	return(node.find_child_by_attribute("userParam", "name",
		"3DPositionZ").attribute("value").as_double());
}

double find_external_offset(pugi::xml_node node)
{
	return(node.find_child_by_attribute("cvParam", "accession",
		IMS_EXTERNAL_OFFSET_ID).attribute("value").as_double());
}

int find_external_array_length(pugi::xml_node node)
{
	return(node.find_child_by_attribute("cvParam", "accession",
		IMS_EXTERNAL_ARRAY_LENGTH_ID).attribute("value").as_int());
}

int find_external_encoded_length(pugi::xml_node node)
{
	return(node.find_child_by_attribute("cvParam", "accession",
		IMS_EXTERNAL_ENCODED_LENGTH_ID).attribute("value").as_int());
}

const char * find_scan_polarity(pugi::xml_node node)
{
	if ( node.find_child_by_attribute("cvParam", "accession", MS_NEGATIVE_SCAN_ID) )
		return MS_NEGATIVE_SCAN_NAME;
	else if ( node.find_child_by_attribute("cvParam", "accession", MS_POSITIVE_SCAN_ID) )
		return MS_POSITIVE_SCAN_NAME;
	return "";
}

const char * find_spectrum_representation(pugi::xml_node node)
{
	if ( node.find_child_by_attribute("cvParam", "accession", MS_PROFILE_SPECTRUM_ID) )
		return MS_PROFILE_SPECTRUM_NAME;
	else if ( node.find_child_by_attribute("cvParam", "accession", MS_CENTROID_SPECTRUM_ID) )
		return MS_CENTROID_SPECTRUM_NAME;
	return "";
}

const char * find_binary_data_type(pugi::xml_node node)
{
	if ( node.find_child_by_attribute("cvParam", "accession", MS_32_BIT_INTEGER_ID) )
		return MS_32_BIT_INTEGER_NAME;
	else if ( node.find_child_by_attribute("cvParam", "accession", MS_64_BIT_INTEGER_ID) )
		return MS_64_BIT_INTEGER_NAME;
	else if ( node.find_child_by_attribute("cvParam", "accession", MS_32_BIT_FLOAT_ID) )
		return MS_32_BIT_FLOAT_NAME;
	else if ( node.find_child_by_attribute("cvParam", "accession", MS_64_BIT_FLOAT_ID) )
		return MS_64_BIT_FLOAT_NAME;
	else if ( node.find_child_by_attribute("cvParam", "accession", IMS_32_BIT_INTEGER_ID) )
		return IMS_32_BIT_INTEGER_NAME;
	else if ( node.find_child_by_attribute("cvParam", "accession", IMS_64_BIT_INTEGER_ID) )
		return IMS_64_BIT_INTEGER_NAME;
	return "";
}

SEXP parse_fileContent(pugi::xml_node node)
{
	if ( !node )
		return R_NilValue;
	else
		insert_referenceableParamGroup(node);

	SEXP fileContent, fileContentNames;
	PROTECT(fileContent = NEW_LIST(2));
	PROTECT(fileContentNames = NEW_STRING(2));

	SET_STRING_ELT(fileContentNames, 0, mkChar(IMS_UNIVERSALLY_UNIQUE_IDENTIFIER_NAME));
	SET_STRING_ELT(fileContentNames, 1, mkChar(IMS_IBD_BINARY_TYPE_NAME));

	SET_VECTOR_ELT(fileContent, 0, mkString(find_ibd_identification(node)));
	SET_VECTOR_ELT(fileContent, 1, mkString(find_ibd_binary_type(node)));

	setAttrib(fileContent, R_NamesSymbol, fileContentNames);
	UNPROTECT(2);

	return fileContent;
}

SEXP parse_sourceFile(pugi::xml_node node)
{
	if ( !node )
		return R_NilValue;
	else
		insert_referenceableParamGroup(node);

	SEXP sourceFile, sourceFileNames;
	PROTECT(sourceFile = NEW_LIST(2));
	PROTECT(sourceFileNames = NEW_STRING(2));

	SET_STRING_ELT(sourceFileNames, 0, mkChar("name"));
	SET_STRING_ELT(sourceFileNames, 1, mkChar("location"));

	SET_VECTOR_ELT(sourceFile, 0, mkString(node.attribute("name").value()));
	SET_VECTOR_ELT(sourceFile, 1, mkString(node.attribute("location").value()));

	setAttrib(sourceFile, R_NamesSymbol, sourceFileNames);
	UNPROTECT(2);

	return sourceFile;
}

SEXP parse_sourceFileList(pugi::xml_node node)
{
	if ( !node )
		return R_NilValue;
	else
		insert_referenceableParamGroup(node);

	int count = node.attribute("count").as_int();

	SEXP sourceFileList, sourceFileListNames;
	PROTECT(sourceFileList = NEW_LIST(count));
	PROTECT(sourceFileListNames = NEW_STRING(count));

	pugi::xml_node sourceFileNode = node.first_child();
	int i = 0;
	while ( i < count && sourceFileNode )
	{
		SET_STRING_ELT(sourceFileListNames, i, mkChar(sourceFileNode.attribute("id").value()));
		SET_VECTOR_ELT(sourceFileList, i, parse_sourceFile(sourceFileNode));

		sourceFileNode = sourceFileNode.next_sibling();
		i++;
	}
	setAttrib(sourceFileList, R_NamesSymbol, sourceFileListNames);
	UNPROTECT(2);

	return sourceFileList;
}

SEXP parse_contact(pugi::xml_node node)
{
	if ( !node )
		return R_NilValue;
	else
		insert_referenceableParamGroup(node);

	SEXP contact, contactNames;
	PROTECT(contact = NEW_LIST(2));
	PROTECT(contactNames = NEW_STRING(2));

	SET_STRING_ELT(contactNames, 0, mkChar(MS_CONTACT_NAME_NAME));
	SET_STRING_ELT(contactNames, 1, mkChar(MS_CONTACT_ORGANIZATION_NAME));

	SET_VECTOR_ELT(contact, 0, mkString(find_contact_name(node)));
	SET_VECTOR_ELT(contact, 1, mkString(find_contact_organization(node)));

	setAttrib(contact, R_NamesSymbol, contactNames);
	UNPROTECT(2);

	return contact;
}

SEXP parse_fileDescription(pugi::xml_node node)
{
	if ( !node )
		return R_NilValue;
	else
		insert_referenceableParamGroup(node);

	SEXP fileDescription, fileDescriptionNames;
	PROTECT(fileDescription = NEW_LIST(3));
	PROTECT(fileDescriptionNames = NEW_STRING(3));

	SET_STRING_ELT(fileDescriptionNames, 0, mkChar("fileContent"));
	SET_STRING_ELT(fileDescriptionNames, 1, mkChar("souceFileList"));
	SET_STRING_ELT(fileDescriptionNames, 2, mkChar("contact"));

	SET_VECTOR_ELT(fileDescription, 0, parse_fileContent(node.child("fileContent")));
	SET_VECTOR_ELT(fileDescription, 1, parse_sourceFileList(node.child("sourceFileList")));
	SET_VECTOR_ELT(fileDescription, 2, parse_contact(node.child("contact")));

	setAttrib(fileDescription, R_NamesSymbol, fileDescriptionNames);
	UNPROTECT(2);

	return fileDescription;
}

SEXP parse_sampleList(pugi::xml_node node)
{
	if ( !node )
		return R_NilValue;
	else
		insert_referenceableParamGroup(node);

	int count = node.attribute("count").as_int();

	SEXP sampleList, sampleListNames;
	PROTECT(sampleList = NEW_LIST(count));
	PROTECT(sampleListNames = NEW_STRING(count));

	pugi::xml_node sampleNode = node.first_child();
	int i = 0;
	while ( i < count && sampleNode )
	{
		SET_STRING_ELT(sampleListNames, i, mkChar(sampleNode.attribute("id").value()));

		sampleNode = sampleNode.next_sibling();
		i++;
	}
	setAttrib(sampleList, R_NamesSymbol, sampleListNames);
	UNPROTECT(2);

	return sampleList;
}

SEXP parse_softwareList(pugi::xml_node node)
{
	if ( !node )
		return R_NilValue;
	else
		insert_referenceableParamGroup(node);

	int count = node.attribute("count").as_int();

	SEXP softwareList, softwareListNames;
	PROTECT(softwareList = NEW_LIST(count));
	PROTECT(softwareListNames = NEW_STRING(count));

	pugi::xml_node softwareNode = node.first_child();
	int i = 0;
	while ( i < count && softwareNode )
	{
		SET_STRING_ELT(softwareListNames, i, mkChar(softwareNode.attribute("id").value()));

		softwareNode = softwareNode.next_sibling();
		i++;
	}
	setAttrib(softwareList, R_NamesSymbol, softwareListNames);
	UNPROTECT(2);

	return softwareList;
}

SEXP parse_scanSettings(pugi::xml_node node)
{
	if ( !node )
		return R_NilValue;
	else
		insert_referenceableParamGroup(node);

	SEXP scanSettings, scanSettingsNames;
	PROTECT(scanSettings = NEW_LIST(4));
	PROTECT(scanSettingsNames = NEW_STRING(4));

	SET_STRING_ELT(scanSettingsNames, 0, mkChar(IMS_LINE_SCAN_DIRECTION_NAME));
	SET_STRING_ELT(scanSettingsNames, 1, mkChar(IMS_LINESCAN_SEQUENCE_NAME));
	SET_STRING_ELT(scanSettingsNames, 2, mkChar(IMS_SCAN_PATTERN_NAME));
	SET_STRING_ELT(scanSettingsNames, 3, mkChar(IMS_SCAN_TYPE_NAME));

	SET_VECTOR_ELT(scanSettings, 0, mkString(find_line_scan_direction(node)));
	SET_VECTOR_ELT(scanSettings, 1, mkString(find_scan_direction(node)));
	SET_VECTOR_ELT(scanSettings, 2, mkString(find_scan_pattern(node)));
	SET_VECTOR_ELT(scanSettings, 3, mkString(find_scan_type(node)));

	setAttrib(scanSettings, R_NamesSymbol, scanSettingsNames);
	UNPROTECT(2);

	return scanSettings;
}

SEXP parse_scanSettingsList(pugi::xml_node node)
{
	if ( !node )
		return R_NilValue;
	else
		insert_referenceableParamGroup(node);

	int count = node.attribute("count").as_int();

	SEXP scanSettingsList, scanSettingsListNames;
	PROTECT(scanSettingsList = NEW_LIST(count));
	PROTECT(scanSettingsListNames = NEW_STRING(count));

	pugi::xml_node scanSettingsNode = node.first_child();
	int i = 0;
	while ( i < count && scanSettingsNode )
	{
		SET_STRING_ELT(scanSettingsListNames, i, mkChar(scanSettingsNode.attribute("id").value()));
		SET_VECTOR_ELT(scanSettingsList, i, parse_scanSettings(scanSettingsNode));

		scanSettingsNode = scanSettingsNode.next_sibling();
		i++;
	}
	setAttrib(scanSettingsList, R_NamesSymbol, scanSettingsListNames);
	UNPROTECT(2);

	return scanSettingsList;
}

SEXP parse_instrumentConfigurationList(pugi::xml_node node)
{
	if ( !node )
		return R_NilValue;
	else
		insert_referenceableParamGroup(node);

	int count = node.attribute("count").as_int();

	SEXP instrumentConfigurationList, instrumentConfigurationListNames;
	PROTECT(instrumentConfigurationList = NEW_LIST(count));
	PROTECT(instrumentConfigurationListNames = NEW_STRING(count));

	pugi::xml_node instrumentConfigurationNode = node.first_child();
	int i = 0;
	while ( i < count && instrumentConfigurationNode )
	{
		SET_STRING_ELT(instrumentConfigurationListNames, i, mkChar(instrumentConfigurationNode.attribute("id").value()));

		instrumentConfigurationNode = instrumentConfigurationNode.next_sibling();
		i++;
	}
	setAttrib(instrumentConfigurationList, R_NamesSymbol, instrumentConfigurationListNames);
	UNPROTECT(2);

	return instrumentConfigurationList;
}

SEXP parse_dataProcessingList(pugi::xml_node node)
{
	if ( !node )
		return R_NilValue;
	else
		insert_referenceableParamGroup(node);

	int count = node.attribute("count").as_int();

	SEXP dataProcessingList, dataProcessingListNames;
	PROTECT(dataProcessingList = NEW_LIST(count));
	PROTECT(dataProcessingListNames = NEW_STRING(count));

	pugi::xml_node dataProcessingNode = node.first_child();
	int i = 0;
	while ( i < count && dataProcessingNode )
	{
		SET_STRING_ELT(dataProcessingListNames, i, mkChar(dataProcessingNode.attribute("id").value()));

		dataProcessingNode = dataProcessingNode.next_sibling();
		i++;
	}
	setAttrib(dataProcessingList, R_NamesSymbol, dataProcessingListNames);
	UNPROTECT(2);

	return dataProcessingList;
}

SEXP parse_scan(pugi::xml_node node)
{
	if ( !node )
		return R_NilValue;
	else
		insert_referenceableParamGroup(node);

	SEXP scan, scanNames;
	PROTECT(scan = NEW_LIST(6));
	PROTECT(scanNames = NEW_STRING(6));

	SET_STRING_ELT(scanNames, 0, mkChar(IMS_POSITION_X_NAME));
	SET_STRING_ELT(scanNames, 1, mkChar(IMS_POSITION_Y_NAME));
	SET_STRING_ELT(scanNames, 2, mkChar(IMS_POSITION_Z_NAME));
	SET_STRING_ELT(scanNames, 3, mkChar("3DPositionX"));
	SET_STRING_ELT(scanNames, 4, mkChar("3DPositionY"));
	SET_STRING_ELT(scanNames, 5, mkChar("3DPositionZ"));

	SET_VECTOR_ELT(scan, 0, ScalarInteger(find_position_x(node)));
	SET_VECTOR_ELT(scan, 1, ScalarInteger(find_position_y(node)));
	SET_VECTOR_ELT(scan, 2, ScalarInteger(find_position_z(node)));
	SET_VECTOR_ELT(scan, 3, ScalarReal(find_3D_position_x(node)));
	SET_VECTOR_ELT(scan, 4, ScalarReal(find_3D_position_y(node)));
	SET_VECTOR_ELT(scan, 5, ScalarReal(find_3D_position_z(node)));

	setAttrib(scan, R_NamesSymbol, scanNames);
	UNPROTECT(2);

	return(scan);
}

SEXP parse_scanList(pugi::xml_node node)
{
	if ( !node )
		return R_NilValue;
	else
		insert_referenceableParamGroup(node);

	SEXP scanList, scanListNames;
	PROTECT(scanList = NEW_LIST(1));
	PROTECT(scanListNames = NEW_STRING(1));

	SET_STRING_ELT(scanListNames, 0, mkChar("scan"));

	SET_VECTOR_ELT(scanList, 0, parse_scan(node.child("scan"))); // assume 1 scan

	setAttrib(scanList, R_NamesSymbol, scanListNames);
	UNPROTECT(2);

	return(scanList);
}

SEXP parse_binaryDataArray(pugi::xml_node node)
{
	if ( !node )
		return R_NilValue;
	else
		insert_referenceableParamGroup(node);

	SEXP binaryDataArray, binaryDataArrayNames;
	PROTECT(binaryDataArray = NEW_LIST(4));
	PROTECT(binaryDataArrayNames = NEW_STRING(4));

	SET_STRING_ELT(binaryDataArrayNames, 0, mkChar(IMS_EXTERNAL_OFFSET_NAME));
	SET_STRING_ELT(binaryDataArrayNames, 1, mkChar(IMS_EXTERNAL_ARRAY_LENGTH_NAME));
	SET_STRING_ELT(binaryDataArrayNames, 2, mkChar(IMS_EXTERNAL_ENCODED_LENGTH_NAME));
	SET_STRING_ELT(binaryDataArrayNames, 3, mkChar(MS_BINARY_DATA_TYPE_NAME));

	SET_VECTOR_ELT(binaryDataArray, 0, ScalarReal(find_external_offset(node)));
	SET_VECTOR_ELT(binaryDataArray, 1, ScalarInteger(find_external_array_length(node)));
	SET_VECTOR_ELT(binaryDataArray, 2, ScalarInteger(find_external_encoded_length(node)));
	SET_VECTOR_ELT(binaryDataArray, 3, mkString(find_binary_data_type(node)));

	setAttrib(binaryDataArray, R_NamesSymbol, binaryDataArrayNames);
	UNPROTECT(2);

	return(binaryDataArray);
}

SEXP parse_binaryDataArrayList(pugi::xml_node node)
{
	if ( !node )
		return R_NilValue;
	else
		insert_referenceableParamGroup(node);

	SEXP binaryDataArrayList, binaryDataArrayListNames;
	PROTECT(binaryDataArrayList = NEW_LIST(2));
	PROTECT(binaryDataArrayListNames = NEW_STRING(2));

	SET_STRING_ELT(binaryDataArrayListNames, 0, mkChar(MS_M_Z_ARRAY_NAME));
	SET_STRING_ELT(binaryDataArrayListNames, 1, mkChar(MS_INTENSITY_ARRAY_NAME));

	for ( pugi::xml_node binaryDataArrayNode = node.first_child();
		binaryDataArrayNode;
		binaryDataArrayNode = binaryDataArrayNode.next_sibling() )
	{
		insert_referenceableParamGroup(binaryDataArrayNode);
		
		// parse m/z array
		pugi::xml_node mzArrayNode = binaryDataArrayNode.find_child_by_attribute(
			"cvParam", "accession", MS_M_Z_ARRAY_ID).parent();
		if ( mzArrayNode )
			SET_VECTOR_ELT(binaryDataArrayList, 0, parse_binaryDataArray(mzArrayNode));
		
		// parse intensity array
		pugi::xml_node intensityArrayNode = binaryDataArrayNode.find_child_by_attribute(
			"cvParam", "accession", MS_INTENSITY_ARRAY_ID).parent();
		if ( intensityArrayNode )
			SET_VECTOR_ELT(binaryDataArrayList, 1, parse_binaryDataArray(intensityArrayNode));
	}

	setAttrib(binaryDataArrayList, R_NamesSymbol, binaryDataArrayListNames);
	UNPROTECT(2);

	return binaryDataArrayList;
}

SEXP parse_spectrum(pugi::xml_node node)
{
	if ( !node )
		return R_NilValue;
	else
		insert_referenceableParamGroup(node);

	SEXP spectrum, spectrumNames;
	PROTECT(spectrum = NEW_LIST(4));
	PROTECT(spectrumNames = NEW_STRING(4));

	SET_STRING_ELT(spectrumNames, 0, mkChar(MS_SCAN_POLARITY_NAME));
	SET_STRING_ELT(spectrumNames, 1, mkChar(MS_SPECTRUM_REPRESENTATION_NAME));
	SET_STRING_ELT(spectrumNames, 2, mkChar("scanList"));
	SET_STRING_ELT(spectrumNames, 3, mkChar("binaryDataArrayList"));

	SET_VECTOR_ELT(spectrum, 0, mkString(find_scan_polarity(node)));
	SET_VECTOR_ELT(spectrum, 1, mkString(find_spectrum_representation(node)));
	SET_VECTOR_ELT(spectrum, 2, parse_scanList(node.child("scanList")));
	SET_VECTOR_ELT(spectrum, 3, parse_binaryDataArrayList(node.child("binaryDataArrayList")));

	setAttrib(spectrum, R_NamesSymbol, spectrumNames);
	UNPROTECT(2);

	return spectrum;
}

SEXP parse_spectrumList(pugi::xml_node node)
{
	if ( !node )
		return R_NilValue;
	else
		insert_referenceableParamGroup(node);

	int count = node.attribute("count").as_int();

	SEXP spectrumList, spectrumListNames;
	PROTECT(spectrumList = NEW_LIST(count));
	PROTECT(spectrumListNames = NEW_STRING(count));

	pugi::xml_node spectrumNode = node.first_child();
	int i = 0;
	while ( i < count && spectrumNode  )
	{
		SET_STRING_ELT(spectrumListNames, i, mkChar(spectrumNode.attribute("id").value()));
		SET_VECTOR_ELT(spectrumList, i, parse_spectrum(spectrumNode));

		spectrumNode = spectrumNode.next_sibling();
		i++;
	}
	setAttrib(spectrumList, R_NamesSymbol, spectrumListNames);
	UNPROTECT(2);

	return spectrumList;
}

SEXP parse_run(pugi::xml_node node)
{
	if ( !node )
		return R_NilValue;
	else
		insert_referenceableParamGroup(node);

	SEXP run, runNames;
	PROTECT(run = NEW_LIST(1));
	PROTECT(runNames = NEW_STRING(1));

	SET_STRING_ELT(runNames, 0, mkChar("spectrumList"));

	SET_VECTOR_ELT(run, 0, parse_spectrumList(node.child("spectrumList")));

	setAttrib(run, R_NamesSymbol, runNames);
	UNPROTECT(2);

	return run;
}

// begin extern 'C' (for calling from R)

extern "C"
{

	SEXP parseImzML(SEXP filepath)
	{
		// read file
		const char * filename = CHAR(STRING_ELT(filepath, 0));
		FILE * pfile = fopen(filename, "rb");
		if ( pfile == NULL ) return R_NilValue;
		pugi::xml_document doc;
		pugi::xml_parse_result result = doc.load_file(filename);
		if ( !result ) return R_NilValue;

		SEXP mzML, mzMLNames;
		PROTECT(mzML = NEW_LIST(7));
		PROTECT(mzMLNames = NEW_STRING(7));

		SET_STRING_ELT(mzMLNames, 0, mkChar("fileDescription"));
		SET_STRING_ELT(mzMLNames, 1, mkChar("sampleList"));
		SET_STRING_ELT(mzMLNames, 2, mkChar("softwareList"));
		SET_STRING_ELT(mzMLNames, 3, mkChar("scanSettingsList"));
		SET_STRING_ELT(mzMLNames, 4, mkChar("instrumentConfigurationList"));
		SET_STRING_ELT(mzMLNames, 5, mkChar("dataProcessingList"));
		SET_STRING_ELT(mzMLNames, 6, mkChar("run"));

		SET_VECTOR_ELT(mzML, 0, parse_fileDescription(doc.child("mzML").child("fileDescription")));
		SET_VECTOR_ELT(mzML, 1, parse_sampleList(doc.child("mzML").child("sampleList")));
		SET_VECTOR_ELT(mzML, 2, parse_softwareList(doc.child("mzML").child("softwareList")));
		SET_VECTOR_ELT(mzML, 3, parse_scanSettingsList(doc.child("mzML").child("scanSettingsList")));
		SET_VECTOR_ELT(mzML, 4, parse_instrumentConfigurationList(doc.child("mzML").child("instrumentConfigurationList")));
		SET_VECTOR_ELT(mzML, 5, parse_dataProcessingList(doc.child("mzML").child("dataProcessingList")));
		SET_VECTOR_ELT(mzML, 6, parse_run(doc.child("mzML").child("run")));

		setAttrib(mzML, R_NamesSymbol, mzMLNames);
		UNPROTECT(2);

		return mzML;
	}

	SEXP readIbdMzArray(SEXP filepath, SEXP ibd_binary_type, SEXP binary_data_type,
		SEXP external_offset, SEXP external_array_length, SEXP external_array_count)
	{
		const char * data_type = CHARACTER_VALUE(binary_data_type);
		if ( strcmp(CHARACTER_VALUE(ibd_binary_type), IMS_CONTINUOUS_NAME) == 0 )
		{
			if ( strcmp(data_type, MS_32_BIT_INTEGER_NAME) == 0 )
			{
				return readContinuousMzArray<int, int>(CHARACTER_VALUE(filepath),
					NUMERIC_VALUE(external_offset), INTEGER_VALUE(external_array_length));
			}
			else if ( strcmp(data_type, MS_64_BIT_INTEGER_NAME) == 0 )
			{
				return readContinuousMzArray<long, int>(CHARACTER_VALUE(filepath),
					NUMERIC_VALUE(external_offset), INTEGER_VALUE(external_array_length));
			}
			else if ( strcmp(data_type, MS_32_BIT_FLOAT_NAME) == 0 )
			{
				return readContinuousMzArray<float, double>(CHARACTER_VALUE(filepath),
					NUMERIC_VALUE(external_offset), INTEGER_VALUE(external_array_length));
			}
			else if ( strcmp(data_type, MS_64_BIT_FLOAT_NAME) == 0 )
			{
				return readContinuousMzArray<double, double>(CHARACTER_VALUE(filepath),
					NUMERIC_VALUE(external_offset), INTEGER_VALUE(external_array_length));
			}
			else if ( strcmp(data_type, IMS_32_BIT_INTEGER_NAME) == 0 )
			{
				return readContinuousMzArray<int, int>(CHARACTER_VALUE(filepath),
					NUMERIC_VALUE(external_offset), INTEGER_VALUE(external_array_length));
			}
			else if ( strcmp(data_type, IMS_64_BIT_INTEGER_NAME) == 0 )
			{
				return readContinuousMzArray<long, int>(CHARACTER_VALUE(filepath),
					NUMERIC_VALUE(external_offset), INTEGER_VALUE(external_array_length));
			}
		}
		else if ( strcmp(CHARACTER_VALUE(ibd_binary_type), IMS_PROCESSED_NAME) == 0 )
		{
			if ( strcmp(data_type, MS_32_BIT_INTEGER_NAME) == 0 )
			{
				return readProcessedIbdArray<int, int>(CHARACTER_VALUE(filepath),
					REAL(external_offset), INTEGER(external_array_length),
					INTEGER_VALUE(external_array_count));
			}
			else if ( strcmp(data_type, MS_64_BIT_INTEGER_NAME) == 0 )
			{
				return readProcessedIbdArray<long, int>(CHARACTER_VALUE(filepath),
					REAL(external_offset), INTEGER(external_array_length),
					INTEGER_VALUE(external_array_count));
			}
			else if ( strcmp(data_type, MS_32_BIT_FLOAT_NAME) == 0 )
			{
				return readProcessedIbdArray<float, double>(CHARACTER_VALUE(filepath),
					REAL(external_offset), INTEGER(external_array_length),
					INTEGER_VALUE(external_array_count));
			}
			else if ( strcmp(data_type, MS_64_BIT_FLOAT_NAME) == 0 )
			{
				return readProcessedIbdArray<double, double>(CHARACTER_VALUE(filepath),
					REAL(external_offset), INTEGER(external_array_length),
					INTEGER_VALUE(external_array_count));
			}
			else if ( strcmp(data_type, IMS_32_BIT_INTEGER_NAME) == 0 )
			{
				return readProcessedIbdArray<int, int>(CHARACTER_VALUE(filepath),
					REAL(external_offset), INTEGER(external_array_length),
					INTEGER_VALUE(external_array_count));
			}
			else if ( strcmp(data_type, IMS_64_BIT_INTEGER_NAME) == 0 )
			{
				return readProcessedIbdArray<long, int>(CHARACTER_VALUE(filepath),
					REAL(external_offset), INTEGER(external_array_length),
					INTEGER_VALUE(external_array_count));
			}
		}
		return R_NilValue;
	}

	SEXP readIbdIntensityArray(SEXP filepath, SEXP ibd_binary_type, SEXP binary_data_type,
		SEXP external_offset, SEXP external_array_length, SEXP external_array_count)
	{
		const char * data_type = CHARACTER_VALUE(binary_data_type);
		if ( strcmp(CHARACTER_VALUE(ibd_binary_type), IMS_CONTINUOUS_NAME) == 0 )
		{
			if ( strcmp(data_type, IMS_16_BIT_INTEGER_NAME) == 0 )
			{
				return readContinuousIntensityArray<short, int>(CHARACTER_VALUE(filepath),
					NUMERIC_VALUE(external_offset), INTEGER_VALUE(external_array_length),
					INTEGER_VALUE(external_array_count));
			}
			else if ( strcmp(data_type, MS_32_BIT_INTEGER_NAME) == 0 )
			{
				return readContinuousIntensityArray<int, int>(CHARACTER_VALUE(filepath),
					NUMERIC_VALUE(external_offset), INTEGER_VALUE(external_array_length),
					INTEGER_VALUE(external_array_count));
			}
			else if ( strcmp(data_type, MS_64_BIT_INTEGER_NAME) == 0 )
			{
				return readContinuousIntensityArray<long, int>(CHARACTER_VALUE(filepath),
					NUMERIC_VALUE(external_offset), INTEGER_VALUE(external_array_length),
					INTEGER_VALUE(external_array_count));
			}
			else if ( strcmp(data_type, MS_32_BIT_FLOAT_NAME) == 0 )
			{
				return readContinuousIntensityArray<float, double>(CHARACTER_VALUE(filepath),
					NUMERIC_VALUE(external_offset), INTEGER_VALUE(external_array_length),
					INTEGER_VALUE(external_array_count));
			}
			else if ( strcmp(data_type, MS_64_BIT_FLOAT_NAME) == 0 )
			{
				return readContinuousIntensityArray<double, double>(CHARACTER_VALUE(filepath),
					NUMERIC_VALUE(external_offset), INTEGER_VALUE(external_array_length),
					INTEGER_VALUE(external_array_count));
			}
			else if ( strcmp(data_type, IMS_32_BIT_INTEGER_NAME) == 0 )
			{
				return readContinuousIntensityArray<int, int>(CHARACTER_VALUE(filepath),
					NUMERIC_VALUE(external_offset), INTEGER_VALUE(external_array_length),
					INTEGER_VALUE(external_array_count));
			}
			else if ( strcmp(data_type, IMS_64_BIT_INTEGER_NAME) == 0 )
			{
				return readContinuousIntensityArray<long, int>(CHARACTER_VALUE(filepath),
					NUMERIC_VALUE(external_offset), INTEGER_VALUE(external_array_length),
					INTEGER_VALUE(external_array_count));
			}
		}
		else if ( strcmp(CHARACTER_VALUE(ibd_binary_type), IMS_PROCESSED_NAME) == 0 )
		{
			if ( strcmp(data_type, IMS_16_BIT_INTEGER_NAME) == 0 )
			{
				return readProcessedIbdArray<short, int>(CHARACTER_VALUE(filepath),
					REAL(external_offset), INTEGER(external_array_length),
					INTEGER_VALUE(external_array_count));
			}
			else if ( strcmp(data_type, MS_32_BIT_INTEGER_NAME) == 0 )
			{
				return readProcessedIbdArray<int, int>(CHARACTER_VALUE(filepath),
					REAL(external_offset), INTEGER(external_array_length),
					INTEGER_VALUE(external_array_count));
			}
			else if ( strcmp(data_type, MS_64_BIT_INTEGER_NAME) == 0 )
			{
				return readProcessedIbdArray<long, int>(CHARACTER_VALUE(filepath),
					REAL(external_offset), INTEGER(external_array_length),
					INTEGER_VALUE(external_array_count));
			}
			else if ( strcmp(data_type, MS_32_BIT_FLOAT_NAME) == 0 )
			{
				return readProcessedIbdArray<float, double>(CHARACTER_VALUE(filepath),
					REAL(external_offset), INTEGER(external_array_length),
					INTEGER_VALUE(external_array_count));
			}
			else if ( strcmp(data_type, MS_64_BIT_FLOAT_NAME) == 0 )
			{
				return readProcessedIbdArray<double, double>(CHARACTER_VALUE(filepath),
					REAL(external_offset), INTEGER(external_array_length),
					INTEGER_VALUE(external_array_count));
			}
			else if ( strcmp(data_type, IMS_32_BIT_INTEGER_NAME) == 0 )
			{
				return readProcessedIbdArray<int, int>(CHARACTER_VALUE(filepath),
					REAL(external_offset), INTEGER(external_array_length),
					INTEGER_VALUE(external_array_count));
			}
			else if ( strcmp(data_type, IMS_64_BIT_INTEGER_NAME) == 0 )
			{
				return readProcessedIbdArray<long, int>(CHARACTER_VALUE(filepath),
					REAL(external_offset), INTEGER(external_array_length),
					INTEGER_VALUE(external_array_count));
			}
		}
		return R_NilValue;
	}

} // end extern 'C' (for calling from R)


