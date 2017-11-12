
#include <R.h>
#include <Rdefines.h>

#include <cstdio>
#include <cstdlib>

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

pugi::xml_node find_referenceableParamGroup(pugi::xml_node node)
{
	pugi::xml_node refGroupList = node.root().child("mzML").child("referenceableParamGroupList");
	pugi::xml_node refGroup = node.child("referenceableParamGroupRef");
	return refGroupList.find_child_by_attribute("id", refGroup.attribute("ref").value());
}

void insert_referenceableParamGroup(pugi::xml_node node)
{
	pugi::xml_node refGroup = node.child("referenceableParamGroupRef");
	pugi::xml_node refGroupList = find_referenceableParamGroup(node);
	pugi::xml_node ref = refGroupList.first_child();
	for ( ; ref; ref = ref.next_sibling() )
	{
		node.insert_copy_before(ref, refGroup);
	}
	node.remove_child(refGroup);
}

pugi::xml_node find_mzArray(pugi::xml_node spectrum)
{
	pugi::xml_node ref, id, node = spectrum.child("binaryDataArrayList").first_child();
	for ( ; node; node = node.next_sibling() )
	{
		// parse m/z array
		id =  node.find_child_by_attribute("cvParam", "accession", MS_MZ_ARRAY_ID);
		if ( id )
			return node;
		ref = find_referenceableParamGroup(node);
		id =  ref.find_child_by_attribute("cvParam", "accession", MS_MZ_ARRAY_ID);
		if ( id )
			return node;
	}
	return node;
}

pugi::xml_node find_intensityArray(pugi::xml_node spectrum)
{
	pugi::xml_node ref, id, node = spectrum.child("binaryDataArrayList").first_child();
	for ( ; node; node = node.next_sibling() )
	{
		// parse m/z array
		id =  node.find_child_by_attribute("cvParam", "accession", MS_INTENSITY_ARRAY_ID);
		if ( id )
			return node;
		ref = find_referenceableParamGroup(node);
		id =  ref.find_child_by_attribute("cvParam", "accession", MS_INTENSITY_ARRAY_ID);
		if ( id )
			return node;
	}
	return node;
}

// find experiment-level metadata

const char * find_ibd_binary_type(pugi::xml_node root)
{
	pugi::xml_node node = root.child("mzML").child("fileDescription").child("fileContent");
	if ( node.find_child_by_attribute("cvParam", "accession", IMS_CONTINUOUS_ID) )
		return IMS_CONTINUOUS_NAME;
	else if ( node.find_child_by_attribute("cvParam", "accession", IMS_PROCESSED_ID) )
		return IMS_PROCESSED_NAME;
	return "";
}

const char * find_ibd_identification(pugi::xml_node root)
{
	pugi::xml_node node = root.child("mzML").child("fileDescription").child("fileContent");
	return(node.find_child_by_attribute("cvParam", "accession",
		IMS_UNIVERSALLY_UNIQUE_IDENTIFIER_ID).attribute("value").value());
}

const char * find_ibd_md5(pugi::xml_node root)
{
	pugi::xml_node node = root.child("mzML").child("fileDescription").child("fileContent");
	return(node.find_child_by_attribute("cvParam", "accession",
		IMS_IBD_MD5_ID).attribute("value").value());
}

const char * find_ibd_sha1(pugi::xml_node root)
{
	pugi::xml_node node = root.child("mzML").child("fileDescription").child("fileContent");
	return(node.find_child_by_attribute("cvParam", "accession",
		IMS_IBD_SHA_1_ID).attribute("value").value());
}

const char * find_contact_name(pugi::xml_node root)
{
	pugi::xml_node node = root.child("mzML").child("fileDescription").child("contact");
	return(node.find_child_by_attribute("cvParam", "accession",
		MS_CONTACT_NAME_ID).attribute("value").value());
}

const char * find_contact_organization(pugi::xml_node root)
{
	pugi::xml_node node = root.child("mzML").child("fileDescription").child("contact");
	return(node.find_child_by_attribute("cvParam", "accession",
		MS_CONTACT_ORGANIZATION_ID).attribute("value").value());
}

const char * find_contact_address(pugi::xml_node root)
{
	pugi::xml_node node = root.child("mzML").child("fileDescription").child("contact");
	return(node.find_child_by_attribute("cvParam", "accession",
		MS_CONTACT_ADDRESS_ID).attribute("value").value());
}

const char * find_contact_email(pugi::xml_node root)
{
	pugi::xml_node node = root.child("mzML").child("fileDescription").child("contact");
	return(node.find_child_by_attribute("cvParam", "accession",
		MS_CONTACT_EMAIL_ID).attribute("value").value());
}

const char * find_line_scan_direction(pugi::xml_node root)
{
	pugi::xml_node node = root.child("mzML").child("scanSettingsList").child("scanSettings");
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

const char * find_scan_direction(pugi::xml_node root)
{
	pugi::xml_node node = root.child("mzML").child("scanSettingsList").child("scanSettings");
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

const char * find_scan_pattern(pugi::xml_node root)
{
	pugi::xml_node node = root.child("mzML").child("scanSettingsList").child("scanSettings");
	if ( node.find_child_by_attribute("cvParam", "accession", IMS_MEANDERING_ID) )
		return IMS_MEANDERING_NAME;
	else if ( node.find_child_by_attribute("cvParam", "accession", IMS_RANDOM_ACCESS_ID) )
		return IMS_RANDOM_ACCESS_NAME;
	else if ( node.find_child_by_attribute("cvParam", "accession", IMS_FLYBACK_ID) )
		return IMS_FLYBACK_NAME;
	else if ( node.find_child_by_attribute("cvParam", "accession", IMS_ONE_WAY_ID) )
		return IMS_ONE_WAY_NAME;
	return "";
}

const char * find_scan_type(pugi::xml_node root)
{
	pugi::xml_node node = root.child("mzML").child("scanSettingsList").child("scanSettings");
	if ( node.find_child_by_attribute("cvParam", "accession", IMS_HORIZONTAL_LINE_SCAN_ID) )
		return IMS_HORIZONTAL_LINE_SCAN_NAME;
	else if ( node.find_child_by_attribute("cvParam", "accession", IMS_VERTICAL_LINE_SCAN_ID) )
		return IMS_VERTICAL_LINE_SCAN_NAME;
	return "";
}

int find_max_count_of_pixel_x(pugi::xml_node root)
{
	pugi::xml_node node = root.child("mzML").child("scanSettingsList").child("scanSettings");
	return(node.find_child_by_attribute("cvParam", "accession",
		IMS_MAX_COUNT_OF_PIXELS_X_ID).attribute("value").as_int());
}

int find_max_count_of_pixel_y(pugi::xml_node root)
{
	pugi::xml_node node = root.child("mzML").child("scanSettingsList").child("scanSettings");
	return(node.find_child_by_attribute("cvParam", "accession",
		IMS_MAX_COUNT_OF_PIXELS_Y_ID).attribute("value").as_int());
}

// find spectrum-specific metadata

const char * find_spectrum_representation(pugi::xml_node spectrum)
{
	pugi::xml_node ref = find_referenceableParamGroup(spectrum);
	if ( spectrum.find_child_by_attribute("cvParam", "accession", MS_PROFILE_SPECTRUM_ID) )
		return MS_PROFILE_SPECTRUM_NAME;
	else if ( spectrum.find_child_by_attribute("cvParam", "accession", MS_CENTROID_SPECTRUM_ID) )
		return MS_CENTROID_SPECTRUM_NAME;
	else if ( ref )
		return find_spectrum_representation(ref);
	return "";
}

const char * find_scan_polarity(pugi::xml_node spectrum)
{
	pugi::xml_node ref = find_referenceableParamGroup(spectrum);
	if ( spectrum.find_child_by_attribute("cvParam", "accession", MS_NEGATIVE_SCAN_ID) )
		return MS_NEGATIVE_SCAN_NAME;
	else if ( spectrum.find_child_by_attribute("cvParam", "accession", MS_POSITIVE_SCAN_ID) )
		return MS_POSITIVE_SCAN_NAME;
	else if ( ref )
		return find_scan_polarity(ref);
	return "";
}

int find_position_x(pugi::xml_node spectrum)
{
	pugi::xml_node scan = spectrum.child("scanList").child("scan");
	return(scan.find_child_by_attribute("cvParam", "accession",
		IMS_POSITION_X_ID).attribute("value").as_int());
}

int find_position_y(pugi::xml_node spectrum)
{
	pugi::xml_node scan = spectrum.child("scanList").child("scan");
	return(scan.find_child_by_attribute("cvParam", "accession",
		IMS_POSITION_Y_ID).attribute("value").as_int());
}

int find_position_z(pugi::xml_node spectrum)
{
	pugi::xml_node scan = spectrum.child("scanList").child("scan");
	return(scan.find_child_by_attribute("cvParam", "accession",
		IMS_POSITION_Z_ID).attribute("value").as_int());
}

double find_3D_position_x(pugi::xml_node spectrum)
{
	pugi::xml_node scan = spectrum.child("scanList").child("scan");
	return(scan.find_child_by_attribute("userParam", "name",
		"3DPositionX").attribute("value").as_double());
}

double find_3D_position_y(pugi::xml_node spectrum)
{
	pugi::xml_node scan = spectrum.child("scanList").child("scan");
	return(scan.find_child_by_attribute("userParam", "name",
		"3DPositionY").attribute("value").as_double());
}

double find_3D_position_z(pugi::xml_node spectrum)
{
	pugi::xml_node scan = spectrum.child("scanList").child("scan");
	return(scan.find_child_by_attribute("userParam", "name",
		"3DPositionZ").attribute("value").as_double());
}

double find_external_offset(pugi::xml_node binaryDataArray)
{
	return(binaryDataArray.find_child_by_attribute("cvParam", "accession",
		IMS_EXTERNAL_OFFSET_ID).attribute("value").as_double());
}

int find_external_array_length(pugi::xml_node binaryDataArray)
{
	return(binaryDataArray.find_child_by_attribute("cvParam", "accession",
		IMS_EXTERNAL_ARRAY_LENGTH_ID).attribute("value").as_int());
}

int find_external_encoded_length(pugi::xml_node binaryDataArray)
{
	return(binaryDataArray.find_child_by_attribute("cvParam", "accession",
		IMS_EXTERNAL_ENCODED_LENGTH_ID).attribute("value").as_int());
}

const char * find_binary_data_type(pugi::xml_node binaryDataArray)
{
	pugi::xml_node ref = find_referenceableParamGroup(binaryDataArray);
	if ( binaryDataArray.find_child_by_attribute("cvParam", "accession", MS_32_BIT_INTEGER_ID) )
		return MS_32_BIT_INTEGER_NAME;
	else if ( binaryDataArray.find_child_by_attribute("cvParam", "accession", MS_64_BIT_INTEGER_ID) )
		return MS_64_BIT_INTEGER_NAME;
	else if ( binaryDataArray.find_child_by_attribute("cvParam", "accession", MS_32_BIT_FLOAT_ID) )
		return MS_32_BIT_FLOAT_NAME;
	else if ( binaryDataArray.find_child_by_attribute("cvParam", "accession", MS_64_BIT_FLOAT_ID) )
		return MS_64_BIT_FLOAT_NAME;
	else if ( binaryDataArray.find_child_by_attribute("cvParam", "accession", IMS_32_BIT_INTEGER_ID) )
		return IMS_32_BIT_INTEGER_NAME;
	else if ( binaryDataArray.find_child_by_attribute("cvParam", "accession", IMS_64_BIT_INTEGER_ID) )
		return IMS_64_BIT_INTEGER_NAME;
	else if ( ref )
		return find_binary_data_type(ref);
	return "";
}

// parse all experiment-level metadata

SEXP parse_experiment_metadata(pugi::xml_node root) {

	SEXP imzList, imzNames;

	PROTECT(imzList = NEW_LIST(15));
	PROTECT(imzNames = NEW_STRING(15));

	pugi::xml_node node = root.child("mzML").child("fileDescription").child("fileContent");

	SET_STRING_ELT(imzNames, 0, mkChar(MS_SPECTRUM_REPRESENTATION_NAME));
	SET_VECTOR_ELT(imzList, 0, mkString(find_spectrum_representation(node)));

	SET_STRING_ELT(imzNames, 1, mkChar(IMS_IBD_BINARY_TYPE_NAME));
	SET_VECTOR_ELT(imzList, 1, mkString(find_ibd_binary_type(root)));

	SET_STRING_ELT(imzNames, 2, mkChar(IMS_UNIVERSALLY_UNIQUE_IDENTIFIER_NAME));
	SET_VECTOR_ELT(imzList, 2, mkString(find_ibd_identification(root)));

	SET_STRING_ELT(imzNames, 3, mkChar(IMS_IBD_MD5_NAME));
	SET_VECTOR_ELT(imzList, 3, mkString(find_ibd_md5(root)));

	SET_STRING_ELT(imzNames, 4, mkChar(IMS_IBD_SHA_1_NAME));
	SET_VECTOR_ELT(imzList, 4, mkString(find_ibd_sha1(root)));

	SET_STRING_ELT(imzNames, 5, mkChar(MS_CONTACT_NAME_NAME));
	SET_VECTOR_ELT(imzList, 5, mkString(find_contact_name(root)));

	SET_STRING_ELT(imzNames, 6, mkChar(MS_CONTACT_ORGANIZATION_NAME));
	SET_VECTOR_ELT(imzList, 6, mkString(find_contact_organization(root)));

	SET_STRING_ELT(imzNames, 7, mkChar(MS_CONTACT_ADDRESS_NAME));
	SET_VECTOR_ELT(imzList, 7, mkString(find_contact_address(root)));

	SET_STRING_ELT(imzNames, 8, mkChar(MS_CONTACT_EMAIL_NAME));
	SET_VECTOR_ELT(imzList, 8, mkString(find_contact_email(root)));

	SET_STRING_ELT(imzNames, 9, mkChar(IMS_LINE_SCAN_DIRECTION_NAME));
	SET_VECTOR_ELT(imzList, 9, mkString(find_line_scan_direction(root)));

	SET_STRING_ELT(imzNames, 10, mkChar(IMS_LINESCAN_SEQUENCE_NAME));
	SET_VECTOR_ELT(imzList, 10, mkString(find_scan_direction(root)));

	SET_STRING_ELT(imzNames, 11, mkChar(IMS_SCAN_PATTERN_NAME));
	SET_VECTOR_ELT(imzList, 11, mkString(find_scan_pattern(root)));

	SET_STRING_ELT(imzNames, 12, mkChar(IMS_SCAN_TYPE_NAME));
	SET_VECTOR_ELT(imzList, 12, mkString(find_scan_type(root)));

	SET_STRING_ELT(imzNames, 13, mkChar(IMS_MAX_COUNT_OF_PIXELS_X_NAME));
	SET_VECTOR_ELT(imzList, 13, ScalarInteger(find_max_count_of_pixel_x(root)));

	SET_STRING_ELT(imzNames, 14, mkChar(IMS_MAX_COUNT_OF_PIXELS_Y_NAME));
	SET_VECTOR_ELT(imzList, 14, ScalarInteger(find_max_count_of_pixel_y(root)));

	setAttrib(imzList, R_NamesSymbol, imzNames);
	UNPROTECT(2);

	return imzList;
}

// parse all spectrum-level metadata

SEXP parse_spectrum_metadata(pugi::xml_node run) {

	int n = run.child("spectrumList").attribute("count").as_int();

	SEXP spectrumList, spectrumNames;

	PROTECT(spectrumList = NEW_LIST(2));
	PROTECT(spectrumNames = NEW_STRING(2));

	SEXP scanPolarity, spectrumRepresentation;

	PROTECT(scanPolarity = NEW_STRING(n));
	PROTECT(spectrumRepresentation = NEW_STRING(n));

	pugi::xml_node spectrum = run.child("spectrumList").first_child();

	int i = 0;

	while ( i < n && spectrum )
	{
		SET_STRING_ELT(spectrumRepresentation, i,
			mkChar(find_spectrum_representation(spectrum)));
		SET_STRING_ELT(scanPolarity, i,
			mkChar(find_scan_polarity(spectrum)));

		spectrum = spectrum.next_sibling();
		i++;
	}

	SET_STRING_ELT(spectrumNames, 0, mkChar(MS_SPECTRUM_REPRESENTATION_NAME));
	SET_VECTOR_ELT(spectrumList, 0, spectrumRepresentation);
	SET_STRING_ELT(spectrumNames, 1, mkChar(MS_SCAN_POLARITY_NAME));
	SET_VECTOR_ELT(spectrumList, 1, scanPolarity);

	setAttrib(spectrumList, R_NamesSymbol, spectrumNames);
	UNPROTECT(4);

	return spectrumList;

}

SEXP parse_scan_metadata(pugi::xml_node run) {

	int n = run.child("spectrumList").attribute("count").as_int();

	SEXP scanList, scanNames;

	PROTECT(scanList = NEW_LIST(6));
	PROTECT(scanNames = NEW_STRING(6));

	SEXP x, y, z;

	SEXP x3d, y3d, z3d;

	PROTECT(x = NEW_INTEGER(n));
	PROTECT(y = NEW_INTEGER(n));
	PROTECT(z = NEW_INTEGER(n));
	
	PROTECT(x3d = NEW_NUMERIC(n));
	PROTECT(y3d = NEW_NUMERIC(n));
	PROTECT(z3d = NEW_NUMERIC(n));

	int * pX = INTEGER(x);
	int * pY = INTEGER(y);
	int * pZ = INTEGER(z);

	double * pX3D = REAL(x3d);
	double * pY3D = REAL(y3d);
	double * pZ3D = REAL(z3d);

	pugi::xml_node spectrum = run.child("spectrumList").first_child();

	int i = 0;
	
	while ( i < n && spectrum )
	{
		pX[i] = find_position_x(spectrum);
		pY[i] = find_position_y(spectrum);
		pZ[i] = find_position_z(spectrum);

		pX3D[i] = find_3D_position_x(spectrum);
		pY3D[i] = find_3D_position_y(spectrum);
		pZ3D[i] = find_3D_position_z(spectrum);

		spectrum = spectrum.next_sibling();
		i++;
	}

	SET_STRING_ELT(scanNames, 0, mkChar(IMS_POSITION_X_NAME));
	SET_VECTOR_ELT(scanList, 0, x);
	SET_STRING_ELT(scanNames, 1, mkChar(IMS_POSITION_Y_NAME));
	SET_VECTOR_ELT(scanList, 1, y);
	SET_STRING_ELT(scanNames, 2, mkChar(IMS_POSITION_Z_NAME));
	SET_VECTOR_ELT(scanList, 2, z);

	SET_STRING_ELT(scanNames, 3, mkChar("3DPositionX"));
	SET_VECTOR_ELT(scanList, 3, x3d);
	SET_STRING_ELT(scanNames, 4, mkChar("3DPositionY"));
	SET_VECTOR_ELT(scanList, 4, y3d);
	SET_STRING_ELT(scanNames, 5, mkChar("3DPositionZ"));
	SET_VECTOR_ELT(scanList, 5, z3d);

	setAttrib(scanList, R_NamesSymbol, scanNames);
	UNPROTECT(8);

	return scanList;

}

SEXP parse_mz_metadata(pugi::xml_node run) {

	int n = run.child("spectrumList").attribute("count").as_int();

	SEXP binaryDataArrayList, binaryDataArrayNames;

	PROTECT(binaryDataArrayList = NEW_LIST(4));
	PROTECT(binaryDataArrayNames = NEW_STRING(4));

	SEXP offset, arrayLength, encodedLength, dataType;

	PROTECT(offset = NEW_NUMERIC(n));
	PROTECT(arrayLength = NEW_INTEGER(n));
	PROTECT(encodedLength = NEW_INTEGER(n));
	PROTECT(dataType = NEW_STRING(n));
	
	double * pOffset = REAL(offset);
	int * pArrayLength = INTEGER(arrayLength);
	int * pEncodedLength = INTEGER(encodedLength);

	pugi::xml_node spectrum = run.child("spectrumList").first_child();

	int i = 0;

	while ( i < n && spectrum )
	{
		pugi::xml_node binaryDataArray = find_mzArray(spectrum);

		pOffset[i] = find_external_offset(binaryDataArray);
		pArrayLength[i] = find_external_array_length(binaryDataArray);
		pEncodedLength[i] = find_external_encoded_length(binaryDataArray);
		
		SET_STRING_ELT(dataType, i,
			mkChar(find_binary_data_type(binaryDataArray)));

		spectrum = spectrum.next_sibling();
		i++;
	}

	SET_STRING_ELT(binaryDataArrayNames, 0, mkChar(IMS_EXTERNAL_OFFSET_NAME));
	SET_VECTOR_ELT(binaryDataArrayList, 0, offset);
	SET_STRING_ELT(binaryDataArrayNames, 1, mkChar(IMS_EXTERNAL_ARRAY_LENGTH_NAME));
	SET_VECTOR_ELT(binaryDataArrayList, 1, arrayLength);
	SET_STRING_ELT(binaryDataArrayNames, 2, mkChar(IMS_EXTERNAL_ENCODED_LENGTH_NAME));
	SET_VECTOR_ELT(binaryDataArrayList, 2, encodedLength);
	SET_STRING_ELT(binaryDataArrayNames, 3, mkChar(MS_BINARY_DATA_TYPE_NAME));
	SET_VECTOR_ELT(binaryDataArrayList, 3, dataType);

	setAttrib(binaryDataArrayList, R_NamesSymbol, binaryDataArrayNames);
	UNPROTECT(6);

	return binaryDataArrayList;

}

SEXP parse_intensity_metadata(pugi::xml_node run) {

	int n = run.child("spectrumList").attribute("count").as_int();

	SEXP binaryDataArrayList, binaryDataArrayNames;

	PROTECT(binaryDataArrayList = NEW_LIST(4));
	PROTECT(binaryDataArrayNames = NEW_STRING(4));

	SEXP offset, arrayLength, encodedLength, dataType;

	PROTECT(offset = NEW_NUMERIC(n));
	PROTECT(arrayLength = NEW_INTEGER(n));
	PROTECT(encodedLength = NEW_INTEGER(n));
	PROTECT(dataType = NEW_STRING(n));
	
	double * pOffset = REAL(offset);
	int * pArrayLength = INTEGER(arrayLength);
	int * pEncodedLength = INTEGER(encodedLength);

	pugi::xml_node spectrum = run.child("spectrumList").first_child();

	int i = 0;

	while ( i < n && spectrum )
	{
		pugi::xml_node binaryDataArray = find_intensityArray(spectrum);

		pOffset[i] = find_external_offset(binaryDataArray);
		pArrayLength[i] = find_external_array_length(binaryDataArray);
		pEncodedLength[i] = find_external_encoded_length(binaryDataArray);

		SET_STRING_ELT(dataType, i,
			mkChar(find_binary_data_type(binaryDataArray)));

		spectrum = spectrum.next_sibling();
		i++;
	}

	SET_STRING_ELT(binaryDataArrayNames, 0, mkChar(IMS_EXTERNAL_OFFSET_NAME));
	SET_VECTOR_ELT(binaryDataArrayList, 0, offset);
	SET_STRING_ELT(binaryDataArrayNames, 1, mkChar(IMS_EXTERNAL_ARRAY_LENGTH_NAME));
	SET_VECTOR_ELT(binaryDataArrayList, 1, arrayLength);
	SET_STRING_ELT(binaryDataArrayNames, 2, mkChar(IMS_EXTERNAL_ENCODED_LENGTH_NAME));
	SET_VECTOR_ELT(binaryDataArrayList, 2, encodedLength);
	SET_STRING_ELT(binaryDataArrayNames, 3, mkChar(MS_BINARY_DATA_TYPE_NAME));
	SET_VECTOR_ELT(binaryDataArrayList, 3, dataType);

	setAttrib(binaryDataArrayList, R_NamesSymbol, binaryDataArrayNames);
	UNPROTECT(6);

	return binaryDataArrayList;

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

		SEXP imzML, imzMLNames;

		PROTECT(imzML = NEW_LIST(5));
		PROTECT(imzMLNames = NEW_STRING(5));

		pugi::xml_node run = doc.child("mzML").child("run");

		SET_STRING_ELT(imzMLNames, 0, mkChar("experimentMetadata"));
		SET_VECTOR_ELT(imzML, 0, parse_experiment_metadata(doc.root()));

		SET_STRING_ELT(imzMLNames, 1, mkChar("spectrumList"));
		SET_VECTOR_ELT(imzML, 1, parse_spectrum_metadata(run));

		SET_STRING_ELT(imzMLNames, 2, mkChar("scanList"));
		SET_VECTOR_ELT(imzML, 2, parse_scan_metadata(run));

		SET_STRING_ELT(imzMLNames, 3, mkChar("mzArrayList"));
		SET_VECTOR_ELT(imzML, 3, parse_mz_metadata(run));

		SET_STRING_ELT(imzMLNames, 4, mkChar("intensityArrayList"));
		SET_VECTOR_ELT(imzML, 4, parse_intensity_metadata(run));

		setAttrib(imzML, R_NamesSymbol, imzMLNames);
		UNPROTECT(2);

		return imzML;
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


