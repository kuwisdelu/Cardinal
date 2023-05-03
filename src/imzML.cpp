
#include "Cardinal.h"

#include <cstdio>
#include <cstdlib>

#include "obo/OBO-IMS.h"
#include "obo/OBO-MS.h"
#include "obo/OBO-UO.h"

#include "pugixml.h"

// utility functions for parsing imzML

SEXP get_listElement(SEXP x, const char * name)
{
	SEXP elt = R_NilValue;
	SEXP names = Rf_getAttrib(x, R_NamesSymbol);
	for ( int i = 0; i < LENGTH(x); i++ )
		if( strcmp(CHAR(STRING_ELT(names, i)), name) == 0 ) {
			elt = VECTOR_ELT(x, i);
			break;
		}
	return elt;
}

pugi::xml_node get_referenceableParamGroup(pugi::xml_node node)
{
	pugi::xml_node refGroupList = node.root().child("mzML").child("referenceableParamGroupList");
	pugi::xml_node refGroup = node.child("referenceableParamGroupRef");
	return refGroupList.find_child_by_attribute("id", refGroup.attribute("ref").value());
}

void insert_referenceableParamGroup(pugi::xml_node node)
{
	pugi::xml_node refGroup = node.child("referenceableParamGroupRef");
	pugi::xml_node refGroupList = get_referenceableParamGroup(node);
	pugi::xml_node ref = refGroupList.first_child();
	for ( ; ref; ref = ref.next_sibling() )
	{
		node.insert_copy_before(ref, refGroup);
	}
	node.remove_child(refGroup);
}

pugi::xml_node get_mzArray(pugi::xml_node spectrum)
{
	pugi::xml_node ref, id, node = spectrum.child("binaryDataArrayList").first_child();
	for ( ; node; node = node.next_sibling() )
	{
		// parse m/z array
		id =  node.find_child_by_attribute("cvParam", "accession", MS_MZ_ARRAY_ID);
		if ( id )
			return node;
		ref = get_referenceableParamGroup(node);
		id =  ref.find_child_by_attribute("cvParam", "accession", MS_MZ_ARRAY_ID);
		if ( id )
			return node;
	}
	return node;
}

pugi::xml_node get_intensityArray(pugi::xml_node spectrum)
{
	pugi::xml_node ref, id, node = spectrum.child("binaryDataArrayList").first_child();
	for ( ; node; node = node.next_sibling() )
	{
		// parse m/z array
		id =  node.find_child_by_attribute("cvParam", "accession", MS_INTENSITY_ARRAY_ID);
		if ( id )
			return node;
		ref = get_referenceableParamGroup(node);
		id =  ref.find_child_by_attribute("cvParam", "accession", MS_INTENSITY_ARRAY_ID);
		if ( id )
			return node;
	}
	return node;
}

// get experiment-level metadata

const char * get_ibd_binary_type(pugi::xml_node root)
{
	pugi::xml_node node = root.child("mzML").child("fileDescription").child("fileContent");
	if ( node.find_child_by_attribute("cvParam", "accession", IMS_CONTINUOUS_ID) )
		return IMS_CONTINUOUS_NAME;
	else if ( node.find_child_by_attribute("cvParam", "accession", IMS_PROCESSED_ID) )
		return IMS_PROCESSED_NAME;
	return "";
}

const char * get_ibd_identification(pugi::xml_node root)
{
	pugi::xml_node node = root.child("mzML").child("fileDescription").child("fileContent");
	return(node.find_child_by_attribute("cvParam", "accession",
		IMS_UNIVERSALLY_UNIQUE_IDENTIFIER_ID).attribute("value").value());
}

const char * get_ibd_md5(pugi::xml_node root)
{
	pugi::xml_node node = root.child("mzML").child("fileDescription").child("fileContent");
	return(node.find_child_by_attribute("cvParam", "accession",
		IMS_IBD_MD5_ID).attribute("value").value());
}

const char * get_ibd_sha1(pugi::xml_node root)
{
	pugi::xml_node node = root.child("mzML").child("fileDescription").child("fileContent");
	return(node.find_child_by_attribute("cvParam", "accession",
		IMS_IBD_SHA1_ID).attribute("value").value());
}

const char * get_contact_name(pugi::xml_node root)
{
	pugi::xml_node node = root.child("mzML").child("fileDescription").child("contact");
	return(node.find_child_by_attribute("cvParam", "accession",
		MS_CONTACT_NAME_ID).attribute("value").value());
}

const char * get_contact_organization(pugi::xml_node root)
{
	pugi::xml_node node = root.child("mzML").child("fileDescription").child("contact");
	return(node.find_child_by_attribute("cvParam", "accession",
		MS_CONTACT_ORGANIZATION_ID).attribute("value").value());
}

const char * get_contact_address(pugi::xml_node root)
{
	pugi::xml_node node = root.child("mzML").child("fileDescription").child("contact");
	return(node.find_child_by_attribute("cvParam", "accession",
		MS_CONTACT_ADDRESS_ID).attribute("value").value());
}

const char * get_contact_email(pugi::xml_node root)
{
	pugi::xml_node node = root.child("mzML").child("fileDescription").child("contact");
	return(node.find_child_by_attribute("cvParam", "accession",
		MS_CONTACT_EMAIL_ID).attribute("value").value());
}

const char * get_line_scan_direction(pugi::xml_node root)
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

const char * get_scan_direction(pugi::xml_node root)
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

const char * get_scan_pattern(pugi::xml_node root)
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

const char * get_scan_type(pugi::xml_node root)
{
	pugi::xml_node node = root.child("mzML").child("scanSettingsList").child("scanSettings");
	if ( node.find_child_by_attribute("cvParam", "accession", IMS_HORIZONTAL_LINE_SCAN_ID) )
		return IMS_HORIZONTAL_LINE_SCAN_NAME;
	else if ( node.find_child_by_attribute("cvParam", "accession", IMS_VERTICAL_LINE_SCAN_ID) )
		return IMS_VERTICAL_LINE_SCAN_NAME;
	return "";
}

int get_max_count_of_pixel_x(pugi::xml_node root)
{
	pugi::xml_node node = root.child("mzML").child("scanSettingsList").child("scanSettings");
	return(node.find_child_by_attribute("cvParam", "accession",
		IMS_MAX_COUNT_OF_PIXELS_X_ID).attribute("value").as_int());
}

int get_max_count_of_pixel_y(pugi::xml_node root)
{
	pugi::xml_node node = root.child("mzML").child("scanSettingsList").child("scanSettings");
	return(node.find_child_by_attribute("cvParam", "accession",
		IMS_MAX_COUNT_OF_PIXELS_Y_ID).attribute("value").as_int());
}

// set experiment-level metadata

void set_ibd_binary_type(pugi::xml_node root, const char * value)
{
	pugi::xml_node node = root.child("mzML").child("fileDescription").child("fileContent");
	pugi::xml_node param = node.append_child("cvParam");
	param.append_attribute("cvRef") = "IMS";
	if ( strcmp(value, IMS_CONTINUOUS_NAME) == 0 )
	{
		param.append_attribute("accession") = IMS_CONTINUOUS_ID;
		param.append_attribute("name") = IMS_CONTINUOUS_NAME;
	}	
	else if ( strcmp(value, IMS_PROCESSED_NAME) == 0 )
	{
		param.append_attribute("accession") = IMS_PROCESSED_ID;
		param.append_attribute("name") = IMS_PROCESSED_NAME;
	}
	param.append_attribute("value") = "";
}

void set_ibd_identification(pugi::xml_node root, const char * value)
{
	pugi::xml_node node = root.child("mzML").child("fileDescription").child("fileContent");
	pugi::xml_node param = node.append_child("cvParam");
	param.append_attribute("cvRef") = "IMS";
	param.append_attribute("accession") = IMS_UNIVERSALLY_UNIQUE_IDENTIFIER_ID;
	param.append_attribute("name") = IMS_UNIVERSALLY_UNIQUE_IDENTIFIER_NAME;
	param.append_attribute("value") = value;
}

void set_ibd_md5(pugi::xml_node root, const char * value)
{
	pugi::xml_node node = root.child("mzML").child("fileDescription").child("fileContent");
	pugi::xml_node param = node.append_child("cvParam");
	param.append_attribute("cvRef") = "IMS";
	param.append_attribute("accession") = IMS_IBD_MD5_ID;
	param.append_attribute("name") = IMS_IBD_MD5_NAME;
	param.append_attribute("value") = value;
}

void set_ibd_sha1(pugi::xml_node root, const char * value)
{
	pugi::xml_node node = root.child("mzML").child("fileDescription").child("fileContent");
	pugi::xml_node param = node.append_child("cvParam");
	param.append_attribute("cvRef") = "IMS";
	param.append_attribute("accession") = IMS_IBD_SHA1_ID;
	param.append_attribute("name") = IMS_IBD_SHA1_NAME;
	param.append_attribute("value") = value;
}

void set_line_scan_direction(pugi::xml_node root, const char * value)
{
	pugi::xml_node node = root.child("mzML").child("scanSettingsList").child("scanSettings");
	pugi::xml_node param = node.prepend_child("cvParam");
	param.append_attribute("cvRef") = "IMS";
	if ( strcmp(value, IMS_LINESCAN_RIGHT_LEFT_NAME) == 0 )
	{
		param.append_attribute("accession") = IMS_LINESCAN_RIGHT_LEFT_ID;
		param.append_attribute("name") = IMS_LINESCAN_RIGHT_LEFT_NAME;
	}	
	else if ( strcmp(value, IMS_LINESCAN_LEFT_RIGHT_NAME) == 0 )
	{
		param.append_attribute("accession") = IMS_LINESCAN_LEFT_RIGHT_ID;
		param.append_attribute("name") = IMS_LINESCAN_LEFT_RIGHT_NAME;
	}
	else if ( strcmp(value, IMS_LINESCAN_BOTTOM_UP_NAME) == 0 )
	{
		param.append_attribute("accession") = IMS_LINESCAN_BOTTOM_UP_ID;
		param.append_attribute("name") = IMS_LINESCAN_BOTTOM_UP_NAME;
	}
	else if ( strcmp(value, IMS_LINESCAN_TOP_DOWN_NAME) == 0 )
	{
		param.append_attribute("accession") = IMS_LINESCAN_TOP_DOWN_ID;
		param.append_attribute("name") = IMS_LINESCAN_TOP_DOWN_NAME;
	}
	param.append_attribute("value") = "";
}

void set_scan_direction(pugi::xml_node root, const char * value)
{
	pugi::xml_node node = root.child("mzML").child("scanSettingsList").child("scanSettings");
	pugi::xml_node param = node.prepend_child("cvParam");
	param.append_attribute("cvRef") = "IMS";
	if ( strcmp(value, IMS_BOTTOM_UP_NAME) == 0 )
	{
		param.append_attribute("accession") = IMS_BOTTOM_UP_ID;
		param.append_attribute("name") = IMS_BOTTOM_UP_NAME;
	}	
	else if ( strcmp(value, IMS_TOP_DOWN_NAME) == 0 )
	{
		param.append_attribute("accession") = IMS_TOP_DOWN_ID;
		param.append_attribute("name") = IMS_TOP_DOWN_NAME;
	}
	else if ( strcmp(value, IMS_LEFT_RIGHT_NAME) == 0 )
	{
		param.append_attribute("accession") = IMS_LEFT_RIGHT_ID;
		param.append_attribute("name") = IMS_LEFT_RIGHT_NAME;
	}
	else if ( strcmp(value, IMS_RIGHT_LEFT_NAME) == 0 )
	{
		param.append_attribute("accession") = IMS_RIGHT_LEFT_ID;
		param.append_attribute("name") = IMS_RIGHT_LEFT_NAME;
	}
	else if ( strcmp(value, IMS_NO_DIRECTION_NAME) == 0 )
	{
		param.append_attribute("accession") = IMS_NO_DIRECTION_ID;
		param.append_attribute("name") = IMS_NO_DIRECTION_NAME;
	}
	param.append_attribute("value") = "";
}

void set_scan_pattern(pugi::xml_node root, const char * value)
{
	pugi::xml_node node = root.child("mzML").child("scanSettingsList").child("scanSettings");
	pugi::xml_node param = node.prepend_child("cvParam");
	param.append_attribute("cvRef") = "IMS";
	if ( strcmp(value, IMS_MEANDERING_NAME) == 0 )
	{
		param.append_attribute("accession") = IMS_MEANDERING_ID;
		param.append_attribute("name") = IMS_MEANDERING_NAME;
	}	
	else if ( strcmp(value, IMS_RANDOM_ACCESS_NAME) == 0 )
	{
		param.append_attribute("accession") = IMS_RANDOM_ACCESS_ID;
		param.append_attribute("name") = IMS_RANDOM_ACCESS_NAME;
	}
	else if ( strcmp(value, IMS_FLYBACK_NAME) == 0 )
	{
		param.append_attribute("accession") = IMS_FLYBACK_ID;
		param.append_attribute("name") = IMS_FLYBACK_NAME;
	}
	else if ( strcmp(value, IMS_ONE_WAY_NAME) == 0 )
	{
		param.append_attribute("accession") = IMS_ONE_WAY_ID;
		param.append_attribute("name") = IMS_ONE_WAY_NAME;
	}
	param.append_attribute("value") = "";
}

void set_scan_type(pugi::xml_node root, const char * value)
{
	pugi::xml_node node = root.child("mzML").child("scanSettingsList").child("scanSettings");
	pugi::xml_node param = node.prepend_child("cvParam");
	param.append_attribute("cvRef") = "IMS";
	if ( strcmp(value, IMS_HORIZONTAL_LINE_SCAN_NAME) == 0 )
	{
		param.append_attribute("accession") = IMS_HORIZONTAL_LINE_SCAN_ID;
		param.append_attribute("name") = IMS_HORIZONTAL_LINE_SCAN_NAME;
	}	
	else if ( strcmp(value, IMS_VERTICAL_LINE_SCAN_NAME) == 0 )
	{
		param.append_attribute("accession") = IMS_VERTICAL_LINE_SCAN_ID;
		param.append_attribute("name") = IMS_VERTICAL_LINE_SCAN_NAME;
	}
	param.append_attribute("value") = "";
}

// get spectrum-specific metadata

const char * get_spectrum_representation(pugi::xml_node spectrum)
{
	pugi::xml_node ref = get_referenceableParamGroup(spectrum);
	if ( spectrum.find_child_by_attribute("cvParam", "accession", MS_PROFILE_SPECTRUM_ID) )
		return MS_PROFILE_SPECTRUM_NAME;
	else if ( spectrum.find_child_by_attribute("cvParam", "accession", MS_CENTROID_SPECTRUM_ID) )
		return MS_CENTROID_SPECTRUM_NAME;
	else if ( ref )
		return get_spectrum_representation(ref);
	return "";
}

const char * get_scan_polarity(pugi::xml_node spectrum)
{
	pugi::xml_node ref = get_referenceableParamGroup(spectrum);
	if ( spectrum.find_child_by_attribute("cvParam", "accession", MS_NEGATIVE_SCAN_ID) )
		return MS_NEGATIVE_SCAN_NAME;
	else if ( spectrum.find_child_by_attribute("cvParam", "accession", MS_POSITIVE_SCAN_ID) )
		return MS_POSITIVE_SCAN_NAME;
	else if ( ref )
		return get_scan_polarity(ref);
	return "";
}

int get_position_x(pugi::xml_node spectrum)
{
	pugi::xml_node scan = spectrum.child("scanList").child("scan");
	return(scan.find_child_by_attribute("cvParam", "accession",
		IMS_POSITION_X_ID).attribute("value").as_int(NA_INTEGER));
}

int get_position_y(pugi::xml_node spectrum)
{
	pugi::xml_node scan = spectrum.child("scanList").child("scan");
	return(scan.find_child_by_attribute("cvParam", "accession",
		IMS_POSITION_Y_ID).attribute("value").as_int(NA_INTEGER));
}

int get_position_z(pugi::xml_node spectrum)
{
	pugi::xml_node scan = spectrum.child("scanList").child("scan");
	return(scan.find_child_by_attribute("cvParam", "accession",
		IMS_POSITION_Z_ID).attribute("value").as_int(NA_INTEGER));
}

double get_3D_position_x(pugi::xml_node spectrum)
{
	pugi::xml_node scan = spectrum.child("scanList").child("scan");
	return(scan.find_child_by_attribute("userParam", "name",
		"3DPositionX").attribute("value").as_double(NA_REAL));
}

double get_3D_position_y(pugi::xml_node spectrum)
{
	pugi::xml_node scan = spectrum.child("scanList").child("scan");
	return(scan.find_child_by_attribute("userParam", "name",
		"3DPositionY").attribute("value").as_double(NA_REAL));
}

double get_3D_position_z(pugi::xml_node spectrum)
{
	pugi::xml_node scan = spectrum.child("scanList").child("scan");
	return(scan.find_child_by_attribute("userParam", "name",
		"3DPositionZ").attribute("value").as_double(NA_REAL));
}

double get_external_offset(pugi::xml_node binaryDataArray)
{
	return(binaryDataArray.find_child_by_attribute("cvParam", "accession",
		IMS_EXTERNAL_OFFSET_ID).attribute("value").as_double());
}

int get_external_array_length(pugi::xml_node binaryDataArray)
{
	return(binaryDataArray.find_child_by_attribute("cvParam", "accession",
		IMS_EXTERNAL_ARRAY_LENGTH_ID).attribute("value").as_int());
}

int get_external_encoded_length(pugi::xml_node binaryDataArray)
{
	return(binaryDataArray.find_child_by_attribute("cvParam", "accession",
		IMS_EXTERNAL_ENCODED_LENGTH_ID).attribute("value").as_int());
}

const char * get_binary_data_type(pugi::xml_node binaryDataArray)
{
	pugi::xml_node ref = get_referenceableParamGroup(binaryDataArray);
	if ( binaryDataArray.find_child_by_attribute("cvParam", "accession", IMS_16_BIT_INTEGER_ID) )
		return IMS_16_BIT_INTEGER_NAME;
	else if ( binaryDataArray.find_child_by_attribute("cvParam", "accession", MS_32_BIT_INTEGER_ID) )
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
		return get_binary_data_type(ref);
	return "";
}

// set spectrum-specific metadata

void set_spectrum_representation(pugi::xml_node spectrum, const char * value)
{
	pugi::xml_node param = spectrum.append_child("cvParam");
	param.append_attribute("cvRef") = "MS";
	if ( strcmp(value, MS_PROFILE_SPECTRUM_NAME) == 0 )
	{
		param.append_attribute("accession") = MS_PROFILE_SPECTRUM_ID;
		param.append_attribute("name") = MS_PROFILE_SPECTRUM_NAME;
	}	
	else if ( strcmp(value, MS_CENTROID_SPECTRUM_NAME) == 0 )
	{
		param.append_attribute("accession") = MS_CENTROID_SPECTRUM_ID;
		param.append_attribute("name") = MS_CENTROID_SPECTRUM_NAME;
	}
	param.append_attribute("value") = "";
}

void set_scan_polarity(pugi::xml_node spectrum, const char * value)
{
	pugi::xml_node param = spectrum.append_child("cvParam");
	param.append_attribute("cvRef") = "MS";
	if ( strcmp(value, MS_NEGATIVE_SCAN_NAME) == 0 )
	{
		param.append_attribute("accession") = MS_NEGATIVE_SCAN_ID;
		param.append_attribute("name") = MS_NEGATIVE_SCAN_NAME;
	}	
	else if ( strcmp(value, MS_POSITIVE_SCAN_NAME) == 0 )
	{
		param.append_attribute("accession") = MS_POSITIVE_SCAN_ID;
		param.append_attribute("name") = MS_POSITIVE_SCAN_NAME;
	}
	param.append_attribute("value") = "";
}

void set_position_x(pugi::xml_node spectrum, const char * value)
{
	pugi::xml_node param = spectrum.child("scanList").child("scan").append_child("cvParam");
	param.append_attribute("cvRef") = "IMS";
	param.append_attribute("accession") = IMS_POSITION_X_ID;
	param.append_attribute("name") = IMS_POSITION_X_NAME;
	param.append_attribute("value") = value;
}

void set_position_y(pugi::xml_node spectrum, const char * value)
{
	pugi::xml_node param = spectrum.child("scanList").child("scan").append_child("cvParam");
	param.append_attribute("cvRef") = "IMS";
	param.append_attribute("accession") = IMS_POSITION_Y_ID;
	param.append_attribute("name") = IMS_POSITION_Y_NAME;
	param.append_attribute("value") = value;
}

void set_position_z(pugi::xml_node spectrum, const char * value)
{
	pugi::xml_node param = spectrum.child("scanList").child("scan").append_child("cvParam");
	param.append_attribute("cvRef") = "IMS";
	param.append_attribute("accession") = IMS_POSITION_Z_ID;
	param.append_attribute("name") = IMS_POSITION_Z_NAME;
	param.append_attribute("value") = value;
}

void set_3D_position_x(pugi::xml_node spectrum, const char * value)
{
	pugi::xml_node param = spectrum.child("scanList").child("scan").append_child("userParam");
	param.append_attribute("name") = "3DPositionX";
	param.append_attribute("value") = value;
}

void set_3D_position_y(pugi::xml_node spectrum, const char * value)
{
	pugi::xml_node param = spectrum.child("scanList").child("scan").append_child("userParam");
	param.append_attribute("name") = "3DPositionY";
	param.append_attribute("value") = value;
}

void set_3D_position_z(pugi::xml_node spectrum, const char * value)
{
	pugi::xml_node param = spectrum.child("scanList").child("scan").append_child("userParam");
	param.append_attribute("name") = "3DPositionZ";
	param.append_attribute("value") = value;
}

void set_external_offset(pugi::xml_node binaryDataArray, const char * value)
{
	pugi::xml_node binary = binaryDataArray.child("binary"); 
	pugi::xml_node param = binaryDataArray.insert_child_before("cvParam", binary);
	param.append_attribute("cvRef") = "IMS";
	param.append_attribute("accession") = IMS_EXTERNAL_OFFSET_ID;
	param.append_attribute("name") = IMS_EXTERNAL_OFFSET_NAME;
	param.append_attribute("value") = value;
}

void set_external_array_length(pugi::xml_node binaryDataArray, const char * value)
{
	pugi::xml_node binary = binaryDataArray.child("binary"); 
	pugi::xml_node param = binaryDataArray.insert_child_before("cvParam", binary);
	param.append_attribute("cvRef") = "IMS";
	param.append_attribute("accession") = IMS_EXTERNAL_ARRAY_LENGTH_ID;
	param.append_attribute("name") = IMS_EXTERNAL_ARRAY_LENGTH_NAME;
	param.append_attribute("value") = value;
}

void set_external_encoded_length(pugi::xml_node binaryDataArray, const char * value)
{
	pugi::xml_node binary = binaryDataArray.child("binary"); 
	pugi::xml_node param = binaryDataArray.insert_child_before("cvParam", binary);
	param.append_attribute("cvRef") = "IMS";
	param.append_attribute("accession") = IMS_EXTERNAL_ENCODED_LENGTH_ID;
	param.append_attribute("name") = IMS_EXTERNAL_ENCODED_LENGTH_NAME;
	param.append_attribute("value") = value;
}

void set_binary_data_type(pugi::xml_node binaryDataArray, const char * value)
{
	pugi::xml_node ref = get_referenceableParamGroup(binaryDataArray);
	pugi::xml_node param = ref.append_child("cvParam");
	if ( strcmp(value, IMS_16_BIT_INTEGER_NAME) == 0 )
	{
		param.append_attribute("cvRef") = "IMS";
		param.append_attribute("accession") = IMS_16_BIT_INTEGER_ID;
		param.append_attribute("name") = IMS_16_BIT_INTEGER_NAME;
	}
	else if ( strcmp(value, MS_32_BIT_INTEGER_NAME) == 0 )
	{
		param.append_attribute("cvRef") = "MS";
		param.append_attribute("accession") = MS_32_BIT_INTEGER_ID;
		param.append_attribute("name") = MS_32_BIT_INTEGER_NAME;
	}
	else if ( strcmp(value, MS_64_BIT_INTEGER_NAME) == 0 )
	{
		param.append_attribute("cvRef") = "MS";
		param.append_attribute("accession") = MS_64_BIT_INTEGER_ID;
		param.append_attribute("name") = MS_64_BIT_INTEGER_NAME;
	}
	else if ( strcmp(value, MS_32_BIT_FLOAT_NAME) == 0 )
	{
		param.append_attribute("cvRef") = "MS";
		param.append_attribute("accession") = MS_32_BIT_FLOAT_ID;
		param.append_attribute("name") = MS_32_BIT_FLOAT_NAME;
	}
	else if ( strcmp(value, MS_64_BIT_FLOAT_NAME) == 0 )
	{
		param.append_attribute("cvRef") = "MS";
		param.append_attribute("accession") = MS_64_BIT_FLOAT_ID;
		param.append_attribute("name") = MS_64_BIT_FLOAT_NAME;
	}
	else if ( strcmp(value, IMS_32_BIT_INTEGER_NAME) == 0 )
	{
		param.append_attribute("cvRef") = "IMS";
		param.append_attribute("accession") = IMS_32_BIT_INTEGER_ID;
		param.append_attribute("name") = IMS_32_BIT_INTEGER_NAME;
	}
	else if ( strcmp(value, IMS_64_BIT_INTEGER_NAME) == 0 )
	{
		param.append_attribute("cvRef") = "IMS";
		param.append_attribute("accession") = IMS_64_BIT_INTEGER_ID;
		param.append_attribute("name") = IMS_64_BIT_INTEGER_NAME;
	}
	param.append_attribute("value") = "";
}

// read all experiment-level metadata

SEXP read_experiment_metadata(pugi::xml_node root) {

	SEXP imzList, imzNames;

	PROTECT(imzList = Rf_allocVector(VECSXP, 15));
	PROTECT(imzNames = Rf_allocVector(STRSXP, 15));

	pugi::xml_node fileContent = root.child("mzML").child("fileDescription").child("fileContent");

	SET_STRING_ELT(imzNames, 0, Rf_mkChar(MS_SPECTRUM_REPRESENTATION_NAME));
	SET_VECTOR_ELT(imzList, 0, Rf_mkString(get_spectrum_representation(fileContent)));

	SET_STRING_ELT(imzNames, 1, Rf_mkChar(IMS_IBD_BINARY_TYPE_NAME));
	SET_VECTOR_ELT(imzList, 1, Rf_mkString(get_ibd_binary_type(root)));

	SET_STRING_ELT(imzNames, 2, Rf_mkChar(IMS_UNIVERSALLY_UNIQUE_IDENTIFIER_NAME));
	SET_VECTOR_ELT(imzList, 2, Rf_mkString(get_ibd_identification(root)));

	SET_STRING_ELT(imzNames, 3, Rf_mkChar(IMS_IBD_MD5_NAME));
	SET_VECTOR_ELT(imzList, 3, Rf_mkString(get_ibd_md5(root)));

	SET_STRING_ELT(imzNames, 4, Rf_mkChar(IMS_IBD_SHA1_NAME));
	SET_VECTOR_ELT(imzList, 4, Rf_mkString(get_ibd_sha1(root)));

	SET_STRING_ELT(imzNames, 5, Rf_mkChar(MS_CONTACT_NAME_NAME));
	SET_VECTOR_ELT(imzList, 5, Rf_mkString(get_contact_name(root)));

	SET_STRING_ELT(imzNames, 6, Rf_mkChar(MS_CONTACT_ORGANIZATION_NAME));
	SET_VECTOR_ELT(imzList, 6, Rf_mkString(get_contact_organization(root)));

	SET_STRING_ELT(imzNames, 7, Rf_mkChar(MS_CONTACT_ADDRESS_NAME));
	SET_VECTOR_ELT(imzList, 7, Rf_mkString(get_contact_address(root)));

	SET_STRING_ELT(imzNames, 8, Rf_mkChar(MS_CONTACT_EMAIL_NAME));
	SET_VECTOR_ELT(imzList, 8, Rf_mkString(get_contact_email(root)));

	SET_STRING_ELT(imzNames, 9, Rf_mkChar(IMS_LINE_SCAN_DIRECTION_NAME));
	SET_VECTOR_ELT(imzList, 9, Rf_mkString(get_line_scan_direction(root)));

	SET_STRING_ELT(imzNames, 10, Rf_mkChar(IMS_LINESCAN_SEQUENCE_NAME));
	SET_VECTOR_ELT(imzList, 10, Rf_mkString(get_scan_direction(root)));

	SET_STRING_ELT(imzNames, 11, Rf_mkChar(IMS_SCAN_PATTERN_NAME));
	SET_VECTOR_ELT(imzList, 11, Rf_mkString(get_scan_pattern(root)));

	SET_STRING_ELT(imzNames, 12, Rf_mkChar(IMS_SCAN_TYPE_NAME));
	SET_VECTOR_ELT(imzList, 12, Rf_mkString(get_scan_type(root)));

	SET_STRING_ELT(imzNames, 13, Rf_mkChar(IMS_MAX_COUNT_OF_PIXELS_X_NAME));
	SET_VECTOR_ELT(imzList, 13, Rf_ScalarInteger(get_max_count_of_pixel_x(root)));

	SET_STRING_ELT(imzNames, 14, Rf_mkChar(IMS_MAX_COUNT_OF_PIXELS_Y_NAME));
	SET_VECTOR_ELT(imzList, 14, Rf_ScalarInteger(get_max_count_of_pixel_y(root)));

	Rf_setAttrib(imzList, R_NamesSymbol, imzNames);
	UNPROTECT(2);

	return imzList;
}

// write all experiment-level metadata

void write_experiment_metadata(pugi::xml_node root, SEXP experimentMetadata)
{
	pugi::xml_node fileContent = root.child("mzML").child("fileDescription").child("fileContent");
	
	pugi::xml_node refGroupList = root.child("mzML").child("referenceableParamGroupList");
	pugi::xml_node spectrum1 = refGroupList.find_child_by_attribute("referenceableParamGroup", "id", "spectrum1");

	SEXP spectrumRepresentation = get_listElement(experimentMetadata, MS_SPECTRUM_REPRESENTATION_NAME);
	if ( !Rf_isNull(spectrumRepresentation) ) {
		set_spectrum_representation(fileContent, CHAR(Rf_asChar(spectrumRepresentation)));
		set_spectrum_representation(spectrum1, CHAR(Rf_asChar(spectrumRepresentation)));
	}

	SEXP ibdIdentification = get_listElement(experimentMetadata, IMS_UNIVERSALLY_UNIQUE_IDENTIFIER_NAME);
	if ( !Rf_isNull(ibdIdentification) )
		set_ibd_identification(root, CHAR(Rf_asChar(ibdIdentification)));

	SEXP ibdMD5 = get_listElement(experimentMetadata, IMS_IBD_MD5_NAME);
	if ( !Rf_isNull(ibdMD5) )
		set_ibd_md5(root, CHAR(Rf_asChar(ibdMD5)));

	SEXP ibdSHA1 = get_listElement(experimentMetadata, IMS_IBD_SHA1_NAME);
	if ( !Rf_isNull(ibdSHA1) )
		set_ibd_sha1(root, CHAR(Rf_asChar(ibdSHA1)));

	SEXP ibdBinaryType = get_listElement(experimentMetadata, IMS_IBD_BINARY_TYPE_NAME);
	if ( !Rf_isNull(ibdBinaryType) )
		set_ibd_binary_type(root, CHAR(Rf_asChar(ibdBinaryType)));

	SEXP lineScanDirection = get_listElement(experimentMetadata, IMS_LINE_SCAN_DIRECTION_NAME);
	if ( !Rf_isNull(lineScanDirection) )
		set_line_scan_direction(root, CHAR(Rf_asChar(lineScanDirection)));

	SEXP scanType = get_listElement(experimentMetadata, IMS_SCAN_TYPE_NAME);
	if ( !Rf_isNull(scanType) )
		set_scan_type(root, CHAR(Rf_asChar(scanType)));

	SEXP scanPattern = get_listElement(experimentMetadata, IMS_SCAN_PATTERN_NAME);
	if ( !Rf_isNull(scanPattern) )
		set_scan_pattern(root, CHAR(Rf_asChar(scanPattern)));

	SEXP scanDirection = get_listElement(experimentMetadata, IMS_LINESCAN_SEQUENCE_NAME);
	if ( !Rf_isNull(scanDirection) )
		set_scan_direction(root, CHAR(Rf_asChar(scanDirection)));
}

// read all spectrum-level metadata

SEXP read_spectrum_metadata(pugi::xml_node run) {

	int n = run.child("spectrumList").attribute("count").as_int();

	SEXP spectrumList, spectrumNames;

	PROTECT(spectrumList = Rf_allocVector(VECSXP, 2));
	PROTECT(spectrumNames = Rf_allocVector(STRSXP, 2));

	SEXP scanPolarity, spectrumRepresentation;

	PROTECT(scanPolarity = Rf_allocVector(STRSXP, n));
	PROTECT(spectrumRepresentation = Rf_allocVector(STRSXP, n));

	pugi::xml_node spectrum = run.child("spectrumList").first_child();

	int i = 0;

	while ( i < n && spectrum )
	{
		SET_STRING_ELT(spectrumRepresentation, i,
			Rf_mkChar(get_spectrum_representation(spectrum)));
		SET_STRING_ELT(scanPolarity, i,
			Rf_mkChar(get_scan_polarity(spectrum)));

		spectrum = spectrum.next_sibling();
		i++;
	}

	SET_STRING_ELT(spectrumNames, 0, Rf_mkChar(MS_SPECTRUM_REPRESENTATION_NAME));
	SET_VECTOR_ELT(spectrumList, 0, spectrumRepresentation);
	SET_STRING_ELT(spectrumNames, 1, Rf_mkChar(MS_SCAN_POLARITY_NAME));
	SET_VECTOR_ELT(spectrumList, 1, scanPolarity);

	Rf_setAttrib(spectrumList, R_NamesSymbol, spectrumNames);
	UNPROTECT(4);

	return spectrumList;

}

SEXP read_scan_metadata(pugi::xml_node run) {

	int n = run.child("spectrumList").attribute("count").as_int();

	SEXP scanList, scanNames;

	PROTECT(scanList = Rf_allocVector(VECSXP, 6));
	PROTECT(scanNames = Rf_allocVector(STRSXP, 6));

	SEXP x, y, z;

	SEXP x3d, y3d, z3d;

	PROTECT(x = Rf_allocVector(INTSXP, n));
	PROTECT(y = Rf_allocVector(INTSXP, n));
	PROTECT(z = Rf_allocVector(INTSXP, n));
	
	PROTECT(x3d = Rf_allocVector(REALSXP, n));
	PROTECT(y3d = Rf_allocVector(REALSXP, n));
	PROTECT(z3d = Rf_allocVector(REALSXP, n));

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
		pX[i] = get_position_x(spectrum);
		pY[i] = get_position_y(spectrum);
		pZ[i] = get_position_z(spectrum);

		pX3D[i] = get_3D_position_x(spectrum);
		pY3D[i] = get_3D_position_y(spectrum);
		pZ3D[i] = get_3D_position_z(spectrum);

		spectrum = spectrum.next_sibling();
		i++;
	}

	SET_STRING_ELT(scanNames, 0, Rf_mkChar(IMS_POSITION_X_NAME));
	SET_VECTOR_ELT(scanList, 0, x);
	SET_STRING_ELT(scanNames, 1, Rf_mkChar(IMS_POSITION_Y_NAME));
	SET_VECTOR_ELT(scanList, 1, y);
	SET_STRING_ELT(scanNames, 2, Rf_mkChar(IMS_POSITION_Z_NAME));
	SET_VECTOR_ELT(scanList, 2, z);

	SET_STRING_ELT(scanNames, 3, Rf_mkChar("3DPositionX"));
	SET_VECTOR_ELT(scanList, 3, x3d);
	SET_STRING_ELT(scanNames, 4, Rf_mkChar("3DPositionY"));
	SET_VECTOR_ELT(scanList, 4, y3d);
	SET_STRING_ELT(scanNames, 5, Rf_mkChar("3DPositionZ"));
	SET_VECTOR_ELT(scanList, 5, z3d);

	Rf_setAttrib(scanList, R_NamesSymbol, scanNames);
	UNPROTECT(8);

	return scanList;

}

SEXP read_mz_metadata(pugi::xml_node run) {

	int n = run.child("spectrumList").attribute("count").as_int();

	SEXP binaryDataArrayList, binaryDataArrayNames;

	PROTECT(binaryDataArrayList = Rf_allocVector(VECSXP, 4));
	PROTECT(binaryDataArrayNames = Rf_allocVector(STRSXP, 4));

	SEXP offset, arrayLength, encodedLength, dataType;

	PROTECT(offset = Rf_allocVector(REALSXP, n));
	PROTECT(arrayLength = Rf_allocVector(INTSXP, n));
	PROTECT(encodedLength = Rf_allocVector(INTSXP, n));
	PROTECT(dataType = Rf_allocVector(STRSXP, n));
	
	double * pOffset = REAL(offset);
	int * pArrayLength = INTEGER(arrayLength);
	int * pEncodedLength = INTEGER(encodedLength);

	pugi::xml_node spectrum = run.child("spectrumList").first_child();

	int i = 0;

	while ( i < n && spectrum )
	{
		pugi::xml_node binaryDataArray = get_mzArray(spectrum);

		pOffset[i] = get_external_offset(binaryDataArray);
		pArrayLength[i] = get_external_array_length(binaryDataArray);
		pEncodedLength[i] = get_external_encoded_length(binaryDataArray);
		
		SET_STRING_ELT(dataType, i,
			Rf_mkChar(get_binary_data_type(binaryDataArray)));

		spectrum = spectrum.next_sibling();
		i++;
	}

	SET_STRING_ELT(binaryDataArrayNames, 0, Rf_mkChar(IMS_EXTERNAL_OFFSET_NAME));
	SET_VECTOR_ELT(binaryDataArrayList, 0, offset);
	SET_STRING_ELT(binaryDataArrayNames, 1, Rf_mkChar(IMS_EXTERNAL_ARRAY_LENGTH_NAME));
	SET_VECTOR_ELT(binaryDataArrayList, 1, arrayLength);
	SET_STRING_ELT(binaryDataArrayNames, 2, Rf_mkChar(IMS_EXTERNAL_ENCODED_LENGTH_NAME));
	SET_VECTOR_ELT(binaryDataArrayList, 2, encodedLength);
	SET_STRING_ELT(binaryDataArrayNames, 3, Rf_mkChar(MS_BINARY_DATA_TYPE_NAME));
	SET_VECTOR_ELT(binaryDataArrayList, 3, dataType);

	Rf_setAttrib(binaryDataArrayList, R_NamesSymbol, binaryDataArrayNames);
	UNPROTECT(6);

	return binaryDataArrayList;

}

SEXP read_intensity_metadata(pugi::xml_node run) {

	int n = run.child("spectrumList").attribute("count").as_int();

	SEXP binaryDataArrayList, binaryDataArrayNames;

	PROTECT(binaryDataArrayList = Rf_allocVector(VECSXP, 4));
	PROTECT(binaryDataArrayNames = Rf_allocVector(STRSXP, 4));

	SEXP offset, arrayLength, encodedLength, dataType;

	PROTECT(offset = Rf_allocVector(REALSXP, n));
	PROTECT(arrayLength = Rf_allocVector(INTSXP, n));
	PROTECT(encodedLength = Rf_allocVector(INTSXP, n));
	PROTECT(dataType = Rf_allocVector(STRSXP, n));
	
	double * pOffset = REAL(offset);
	int * pArrayLength = INTEGER(arrayLength);
	int * pEncodedLength = INTEGER(encodedLength);

	pugi::xml_node spectrum = run.child("spectrumList").first_child();

	int i = 0;

	while ( i < n && spectrum )
	{
		pugi::xml_node binaryDataArray = get_intensityArray(spectrum);

		pOffset[i] = get_external_offset(binaryDataArray);
		pArrayLength[i] = get_external_array_length(binaryDataArray);
		pEncodedLength[i] = get_external_encoded_length(binaryDataArray);

		SET_STRING_ELT(dataType, i,
			Rf_mkChar(get_binary_data_type(binaryDataArray)));

		spectrum = spectrum.next_sibling();
		i++;
	}

	SET_STRING_ELT(binaryDataArrayNames, 0, Rf_mkChar(IMS_EXTERNAL_OFFSET_NAME));
	SET_VECTOR_ELT(binaryDataArrayList, 0, offset);
	SET_STRING_ELT(binaryDataArrayNames, 1, Rf_mkChar(IMS_EXTERNAL_ARRAY_LENGTH_NAME));
	SET_VECTOR_ELT(binaryDataArrayList, 1, arrayLength);
	SET_STRING_ELT(binaryDataArrayNames, 2, Rf_mkChar(IMS_EXTERNAL_ENCODED_LENGTH_NAME));
	SET_VECTOR_ELT(binaryDataArrayList, 2, encodedLength);
	SET_STRING_ELT(binaryDataArrayNames, 3, Rf_mkChar(MS_BINARY_DATA_TYPE_NAME));
	SET_VECTOR_ELT(binaryDataArrayList, 3, dataType);

	Rf_setAttrib(binaryDataArrayList, R_NamesSymbol, binaryDataArrayNames);
	UNPROTECT(6);

	return binaryDataArrayList;

}

// write all spectrum-level metadata

void write_spectra(pugi::xml_node run, int n)
{
	pugi::xml_node spectrumList = run.child("spectrumList");
	char buffer[100];
	for ( int i = 0; i < n; i++ )
	{
		pugi::xml_node spectrum;
		if ( i == 0 )
			spectrum = spectrumList.first_child();
		else
			spectrum = spectrumList.append_copy(spectrumList.first_child());
		// write spectrum id
		snprintf(buffer, 100, "Spectrum=%d", i+1);
		spectrum.attribute("id").set_value(buffer);
		// write spectrum index
		snprintf(buffer, 100, "%d", i+1);
		spectrum.attribute("index").set_value(buffer);
	}
}

void write_scan_metadata(pugi::xml_node run, SEXP scanMetadata)
{
	int n = run.child("spectrumList").attribute("count").as_int();

	pugi::xml_node spectrum = run.child("spectrumList").first_child();
	char buffer[100];

	SEXP x = get_listElement(scanMetadata, IMS_POSITION_X_NAME);
	SEXP y = get_listElement(scanMetadata, IMS_POSITION_Y_NAME);
	SEXP z = get_listElement(scanMetadata, IMS_POSITION_Z_NAME);
	SEXP x3d = get_listElement(scanMetadata, "3DPositionX");
	SEXP y3d = get_listElement(scanMetadata, "3DPositionY");
	SEXP z3d = get_listElement(scanMetadata, "3DPositionZ");

	int i = 0;

	while ( i < n && spectrum )
	{
		// write position x
		snprintf(buffer, 100, "%d", INTEGER(x)[i]);
		set_position_x(spectrum, buffer);
		
		// write position y
		snprintf(buffer, 100, "%d", INTEGER(y)[i]);
		set_position_y(spectrum, buffer);

		if ( !Rf_isNull(z) ) {
			snprintf(buffer, 100, "%d", INTEGER(z)[i]);
			set_position_z(spectrum, buffer);
		}

		if ( !Rf_isNull(x3d) ) {
			snprintf(buffer, 100, "%f", REAL(x3d)[i]);
			set_3D_position_x(spectrum, buffer);
		}

		if ( !Rf_isNull(y3d) ) {
			snprintf(buffer, 100, "%f", REAL(y3d)[i]);
			set_3D_position_y(spectrum, buffer);
		}

		if ( !Rf_isNull(z3d) ) {
			snprintf(buffer, 100, "%f", REAL(z3d)[i]);
			set_3D_position_z(spectrum, buffer);
		}

		spectrum = spectrum.next_sibling();
		i++;
	}
}

void write_mz_metadata(pugi::xml_node run, SEXP mzArrayList)
{
	int n = run.child("spectrumList").attribute("count").as_int();

	pugi::xml_node spectrum = run.child("spectrumList").first_child();
	char buffer[100];

	SEXP offset = get_listElement(mzArrayList, IMS_EXTERNAL_OFFSET_NAME);
	SEXP arrayLength = get_listElement(mzArrayList, IMS_EXTERNAL_ARRAY_LENGTH_NAME);
	SEXP encodedLength = get_listElement(mzArrayList, IMS_EXTERNAL_ENCODED_LENGTH_NAME);
	SEXP dataType = get_listElement(mzArrayList, MS_BINARY_DATA_TYPE_NAME);

	pugi::xml_node binaryDataArray = get_mzArray(spectrum);
	set_binary_data_type(binaryDataArray, CHAR(STRING_ELT(dataType, 0)));

	int i = 0;

	while ( i < n && spectrum )
	{
		binaryDataArray = get_mzArray(spectrum);

		// write offset
		snprintf(buffer, 100, "%ld", static_cast<long>(REAL(offset)[i]));
		set_external_offset(binaryDataArray, buffer);
		
		// write array length
		snprintf(buffer, 100, "%d", INTEGER(arrayLength)[i]);
		set_external_array_length(binaryDataArray, buffer);

		// write array length
		snprintf(buffer, 100, "%d", INTEGER(encodedLength)[i]);
		set_external_encoded_length(binaryDataArray, buffer);

		spectrum = spectrum.next_sibling();
		i++;
	}
}

void write_intensity_metadata(pugi::xml_node run, SEXP intensityArrayList)
{
	int n = run.child("spectrumList").attribute("count").as_int();

	pugi::xml_node spectrum = run.child("spectrumList").first_child();
	char buffer[100];

	SEXP offset = get_listElement(intensityArrayList, IMS_EXTERNAL_OFFSET_NAME);
	SEXP arrayLength = get_listElement(intensityArrayList, IMS_EXTERNAL_ARRAY_LENGTH_NAME);
	SEXP encodedLength = get_listElement(intensityArrayList, IMS_EXTERNAL_ENCODED_LENGTH_NAME);
	SEXP dataType = get_listElement(intensityArrayList, MS_BINARY_DATA_TYPE_NAME);

	pugi::xml_node binaryDataArray = get_intensityArray(spectrum);
	set_binary_data_type(binaryDataArray, CHAR(STRING_ELT(dataType, 0)));

	int i = 0;

	while ( i < n && spectrum )
	{
		binaryDataArray = get_intensityArray(spectrum);

		// write offset
		snprintf(buffer, 100, "%ld", static_cast<long>(REAL(offset)[i]));
		set_external_offset(binaryDataArray, buffer);
		
		// write array length
		snprintf(buffer, 100, "%d", INTEGER(arrayLength)[i]);
		set_external_array_length(binaryDataArray, buffer);

		// write array length
		snprintf(buffer, 100, "%d", INTEGER(encodedLength)[i]);
		set_external_encoded_length(binaryDataArray, buffer);

		spectrum = spectrum.next_sibling();
		i++;
	}
}

// begin extern 'C' (for calling from R)

extern "C"
{

	SEXP readImzML(SEXP filepath)
	{
		// read file
		const char * filename = CHAR(STRING_ELT(filepath, 0));
		FILE * pfile = fopen(filename, "rb");
		if ( pfile == NULL ) {
			return R_NilValue;
		} else {
			fclose(pfile);
		}
		pugi::xml_document doc;
		pugi::xml_parse_result result = doc.load_file(filename);
		if ( !result ) return R_NilValue;

		SEXP imzML, imzMLNames;

		PROTECT(imzML = Rf_allocVector(VECSXP, 4));
		PROTECT(imzMLNames = Rf_allocVector(STRSXP, 4));

		pugi::xml_node run = doc.child("mzML").child("run");

		SET_STRING_ELT(imzMLNames, 0, Rf_mkChar("experimentMetadata"));
		SET_VECTOR_ELT(imzML, 0, read_experiment_metadata(doc.root()));

		SET_STRING_ELT(imzMLNames, 1, Rf_mkChar("scanList"));
		SET_VECTOR_ELT(imzML, 1, read_scan_metadata(run));

		SET_STRING_ELT(imzMLNames, 2, Rf_mkChar("mzArrayList"));
		SET_VECTOR_ELT(imzML, 2, read_mz_metadata(run));

		SET_STRING_ELT(imzMLNames, 3, Rf_mkChar("intensityArrayList"));
		SET_VECTOR_ELT(imzML, 3, read_intensity_metadata(run));

		// SET_STRING_ELT(imzMLNames, 4, Rf_mkChar("spectrumList"));
		// SET_VECTOR_ELT(imzML, 4, read_spectrum_metadata(run));

		Rf_setAttrib(imzML, R_NamesSymbol, imzMLNames);
		UNPROTECT(2);

		return imzML;
	}

	SEXP writeImzML(SEXP metadata, SEXP tmpl, SEXP filepath)
	{
		// read buffer
		const char * tmp = CHAR(STRING_ELT(tmpl, 0));
		pugi::xml_document doc;
		pugi::xml_parse_result result = doc.load_string(tmp);
		if ( !result ) return Rf_ScalarLogical(false);

		SEXP experimentMetadata = get_listElement(metadata, "experimentMetadata");
		if ( !Rf_isNull(experimentMetadata) )
			write_experiment_metadata(doc.root(), experimentMetadata);

		pugi::xml_node run = doc.child("mzML").child("run");

		int n = run.child("spectrumList").attribute("count").as_int();
		if ( n > 0 )
			write_spectra(run, n);

		SEXP scanList = get_listElement(metadata, "scanList");
		if ( !Rf_isNull(scanList) )
			write_scan_metadata(run, scanList);

		SEXP mzArrayList = get_listElement(metadata, "mzArrayList");
		if ( !Rf_isNull(mzArrayList) )
			write_mz_metadata(run, mzArrayList);

		SEXP intensityArrayList = get_listElement(metadata, "intensityArrayList");
		if ( !Rf_isNull(intensityArrayList) )
			write_intensity_metadata(run, intensityArrayList);

		const char * filename = CHAR(STRING_ELT(filepath, 0));
		bool is_saved = doc.save_file(filename);

		return Rf_ScalarLogical(is_saved);
	}

} // end extern 'C' (for calling from R)


