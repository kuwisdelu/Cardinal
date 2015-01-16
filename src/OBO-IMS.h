
#define IMS_IMAGING_MASS_SPECTROMETRY_ONTOLOGY_ID "IMS:0000000"
#define IMS_IMAGING_MASS_SPECTROMETRY_ONTOLOGY_NAME "Imaging Mass Spectrometry Ontology"
/* namespace: imagingMS.ontology
def: "Imaging Mass Spectrometry Ontology." [COMPUTIS:IMS] */

#define IMS_IBD_OFFSET_HANDLE_ID "IMS:1000001"
#define IMS_IBD_OFFSET_HANDLE_NAME "ibd offset handle"
/* def: "Information for the access and checking of the binary data arrays." [COMPUTIS:IMS]
relationship: part_of IMS:0000000 ! Imaging Mass Spectrometry Ontology */

#define IMS_SAMPLE_STAGE_ID "IMS:1000002"
#define IMS_SAMPLE_STAGE_NAME "sample stage"
/* def: "Device that positions the imaging target." [COMPUTIS:IMS]
relationship: part_of IMS:0000000 ! Imaging Mass Spectrometry Ontology */

#define IMS_IBD_BINARY_TYPE_ID "IMS:1000003"
#define IMS_IBD_BINARY_TYPE_NAME "ibd binary type"
/* def: "Describes type of the binary (ibd) file ." [COMPUTIS:IMS]
relationship: part_of IMS:0000000 ! Imaging Mass Spectrometry Ontology */

#define IMS_IMAGE_ID "IMS:1000004"
#define IMS_IMAGE_NAME "image"
/* def: "Sample properties only concerning imaging samples." [COMPUTIS:IMS]
relationship: part_of IMS:0000000 ! Imaging Mass Spectrometry Ontology */

#define IMS_SPECTRUM_POSITION_ID "IMS:1000005"
#define IMS_SPECTRUM_POSITION_NAME "spectrum position"
/* def: "Attributes to describe the position of a spectrum in the image." [COMPUTIS:IMS]
relationship: part_of IMS:0000000 ! Imaging Mass Spectrometry Ontology */

#define IMS_IBD_FILE_ID "IMS:1000007"
#define IMS_IBD_FILE_NAME "ibd file"
/* def: "Attributes to describe the ibd file." [COMPUTIS:IMS]
relationship: part_of IMS:0000000 ! Imaging Mass Spectrometry Ontology */

#define IMS_IBD_IDENTIFICATION_ID "IMS:1000008"
#define IMS_IBD_IDENTIFICATION_NAME "ibd identification"
/* def: "Attributes to doubtlessly identify the ibd file." [COMPUTIS:IMS]
relationship: part_of IMS:0000000 ! Imaging Mass Spectrometry Ontology */

#define IMS_IBD_CHECKSUM_ID "IMS:1000009"
#define IMS_IBD_CHECKSUM_NAME "ibd checksum"
/* def: "Checksum is a form of redundancy check, a simple way to protect the integrity of data by detecting errors in data of the ibd file." [COMPUTIS:IMS]
relationship: part_of IMS:0000000 ! Imaging Mass Spectrometry Ontology */

#define IMS_SCAN_ID "IMS:1000010"
#define IMS_SCAN_NAME "scan"
/* def: "Describes the attributes of the generation of the image." [COMPUTIS:IMS]
relationship: part_of IMS:0000000 ! Imaging Mass Spectrometry Ontology */

#define IMS_LASER_SHOT_MODE_ID "IMS:1000011"
#define IMS_LASER_SHOT_MODE_NAME "laser shot mode"
/* def: "Describes the method how the laser was moved across the sample while firing." [COMPUTIS:IMS]
relationship: part_of IMS:0000000 ! Imaging Mass Spectrometry Ontology */

#define IMS_IMAGING_ION_SOURCE_ID "IMS:1000012"
#define IMS_IMAGING_ION_SOURCE_NAME "imaging ion source"
/* def: "Parameters describing the imaging source, which is used to create the ions measured by the MS." [COMPUTIS:IMS]
relationship: part_of IMS:0000000 ! Imaging Mass Spectrometry Ontology */

#define IMS_UNIT_ID "IMS:1000013"
#define IMS_UNIT_NAME "unit"
/* def: "Terms to describe units" [COMPUTIS:IMS]
relationship: part_of IMS:0000000 ! Imaging Mass Spectrometry Ontology */

#define IMS_IBD_DATA_TYPE_ID "IMS:1000014"
#define IMS_IBD_DATA_TYPE_NAME "ibd data type"
/* def: "Encoding type of binary data, e.g. 32-bit integer." [COMPUTIS:IMS]
relationship: part_of IMS:0000000 ! Imaging Mass Spectrometry Ontology */

#define IMS_CONTINUOUS_ID "IMS:1000030"
#define IMS_CONTINUOUS_NAME "continuous"
/* def: "Way of saving spectra in a imzML binary data file (ibd). The m/z values for all spectra are saved at the beginning of the ibd file. Then the spectral values follow." [COMPUTIS:IMS]
relationship: is_a IMS:1000003 ! IDB Binary Type */

#define IMS_PROCESSED_ID "IMS:1000031"
#define IMS_PROCESSED_NAME "processed"
/* def: "Way of saving spectra in a imzML binary data file (ibd). Every spectrum is saved with it's own m/z and intensity values." [COMPUTIS:IMS]
relationship: is_a IMS:1000003 ! IDB Binary Type */

#define IMS_LINESCAN_SEQUENCE_ID "IMS:1000040"
#define IMS_LINESCAN_SEQUENCE_NAME "linescan sequence"
/* def: "Description of the direction of the succession of the assembling of the linescans." [COMPUTIS:IMS]
relationship: part_of IMS:1000010 ! Scan */

#define IMS_SCAN_PATTERN_ID "IMS:1000041"
#define IMS_SCAN_PATTERN_NAME "scan pattern"
/* def: "Description of the pattern how the image was scanned." [COMPUTIS:IMS]
relationship: part_of IMS:1000010 ! Scan */

#define IMS_MAX_COUNT_OF_PIXELS_X_ID "IMS:1000042"
#define IMS_MAX_COUNT_OF_PIXELS_X_NAME "max count of pixels x"
/* def: "Maximum number of pixels of the x-axis of the image." [COMPUTIS:IMS]
xref: value-type:xsd\:nonNegativeInteger "The allowed value-type for this CV term."
relationship: is_a IMS:1000004 ! Image
relationship: has_units UO:0000189 ! Count Unit */

#define IMS_MAX_COUNT_OF_PIXELS_Y_ID "IMS:1000043"
#define IMS_MAX_COUNT_OF_PIXELS_Y_NAME "max count of pixels y"
/* def: "Maximum number of pixels of the y-axis of the image." [COMPUTIS:IMS]
xref: value-type:xsd\:nonNegativeInteger "The allowed value-type for this CV term."
relationship: is_a IMS:1000004 ! Image
relationship: has_units UO:0000189 ! Count Unit */

#define IMS_MAX_DIMENSION_X_ID "IMS:1000044"
#define IMS_MAX_DIMENSION_X_NAME "max dimension x"
/* def: "Maximum length of the image in x-axis." [COMPUTIS:IMS]
xref: value-type:xsd\:nonNegativeInteger "The allowed value-type for this CV term."
relationship: is_a IMS:1000004 ! Image
relationship: has_units UO:0000017 ! Micrometer */

#define IMS_MAX_DIMENSION_Y_ID "IMS:1000045"
#define IMS_MAX_DIMENSION_Y_NAME "max dimension y"
/* def: "Maximum length of the image in y-axis." [COMPUTIS:IMS]
xref: value-type:xsd\:nonNegativeInteger "The allowed value-type for this CV term."
relationship: is_a IMS:1000004 ! Image
relationship: has_units UO:0000017 ! Micrometer */

#define IMS_PIXEL_SIZE_ID "IMS:1000046"
#define IMS_PIXEL_SIZE_NAME "pixel size"
/* def: "Describes the area of the sample presented by one pixel." [COMPUTIS:IMS]
xref: value-type:xsd\:float "The allowed value-type for this CV term."
relationship: is_a IMS:1000004 ! Image
relationship: has_units UO:0000047 ! Area Unit */

#define IMS_IMAGE_SHAPE_ID "IMS:1000047"
#define IMS_IMAGE_SHAPE_NAME "image shape"
/* def: "Describes the shape of the image." [COMPUTIS:IMS]
relationship: is_a IMS:1000004 ! Image */

#define IMS_SCAN_TYPE_ID "IMS:1000048"
#define IMS_SCAN_TYPE_NAME "scan type"
/* def: "Shows the direction in which the lines were scanned." [COMPUTIS:IMS]
relationship: part_of IMS:1000010 ! Scan */

#define IMS_LINE_SCAN_DIRECTION_ID "IMS:1000049"
#define IMS_LINE_SCAN_DIRECTION_NAME "line scan direction"
/* def: "Description in wich direction the lines of the sample were scanned." [COMPUTIS:IMS]
relationship: part_of IMS:1000010 ! Scan */

#define IMS_POSITION_X_ID "IMS:1000050"
#define IMS_POSITION_X_NAME "position x"
/* def: "Attribute to describe the position of a spectrum in the direction of the x-axis in the image." [COMPUTIS:IMS]
xref: value-type:xsd\:nonNegativeInteger "The allowed value-type for this CV term."
relationship: is_a IMS:1000005 ! Spectrum Position
relationship: has_units UO:0000189 ! Count Unit */

#define IMS_POSITION_Y_ID "IMS:1000051"
#define IMS_POSITION_Y_NAME "position y"
/* def: "Attribute to describe the position of a spectrum in the direction of the y-axis in the image." [COMPUTIS:IMS]
xref: value-type:xsd\:nonNegativeInteger "The allowed value-type for this CV term."
relationship: is_a IMS:1000005 ! Spectrum Position
relationship: has_units UO:0000189 ! Count Unit */

#define IMS_POSITION_Z_ID "IMS:1000052"
#define IMS_POSITION_Z_NAME "position z"
/* def: "Attribute to describe the position of a spectrum in the direction of the z-axis in the image." [COMPUTIS:IMS]
xref: value-type:xsd\:nonNegativeInteger "The allowed value-type for this CV term."
relationship: is_a IMS:1000005 ! Spectrum Position
relationship: has_units UO:0000189 ! Count Unit */

#define IMS_ABSOLUTE_POSITION_OFFSET_X_ID "IMS:1000053"
#define IMS_ABSOLUTE_POSITION_OFFSET_X_NAME "absolute position offset x"
/* def: "Describes the position at the x-axis of the upper left point of the image on the target." [COMPUTIS:IMS]
xref: value-type:xsd\:nonNegativeFloat "The allowed value-type for this CV term."
relationship: is_a IMS:1000004 ! Image
relationship: has_units UO:0000017 ! Micrometer */

#define IMS_ABSOLUTE_POSITION_OFFSET_Y_ID "IMS:1000054"
#define IMS_ABSOLUTE_POSITION_OFFSET_Y_NAME "absolute position offset y"
/* def: "Describes the position at the y-axis of the upper left point of the image on the target." [COMPUTIS:IMS]
xref: value-type:xsd\:nonNegativeFloat "The allowed value-type for this CV term."
relationship: is_a IMS:1000004 ! Image
relationship: has_units UO:0000017 ! Micrometer */

#define IMS_SUBIMAGE_POSITION_X_ID "IMS:1000055"
#define IMS_SUBIMAGE_POSITION_X_NAME "subimage position x"
/* def: "Describes the position of a subimage in the direction of the x-axis of the complete image." [COMPUTIS:IMS]
xref: value-type:xsd\:nonNegativeInteger "The allowed value-type for this CV term."
relationship: is_a IMS:1000005 ! Spectrum Position
relationship: has_units UO:0000189 ! Count Unit */

#define IMS_SUBIMAGE_POSITION_Y_ID "IMS:1000056"
#define IMS_SUBIMAGE_POSITION_Y_NAME "subimage position y"
/* def: "Describes the position of a subimage in the direction of the y-axis of the complete image." [COMPUTIS:IMS]
xref: value-type:xsd\:nonNegativeInteger "The allowed value-type for this CV term."
relationship: is_a IMS:1000005 ! Spectrum Position
relationship: has_units UO:0000189 ! Count Unit */

#define IMS_SUBIMAGE_POSITION_Z_ID "IMS:1000057"
#define IMS_SUBIMAGE_POSITION_Z_NAME "subimage position z"
/* def: "Describes the position of a subimage in the direction of the z-axis of the complete image." [COMPUTIS:IMS]
xref: value-type:xsd\:nonNegativeInteger "The allowed value-type for this CV term."
relationship: is_a IMS:1000005 ! Spectrum Position
relationship: has_units UO:0000189 ! Count Unit */

#define IMS_EXTERNAL_BINARY_URI_ID "IMS:1000070"
#define IMS_EXTERNAL_BINARY_URI_NAME "external binary uri"
/* def: "Location as an URI where to find the ibd file." [COMPUTIS:IMS]
xref: value-type:xsd\:string "The allowed value-type for this CV term."
relationship: is_a IMS:1000007 ! Ibd File */

#define IMS_UNIVERSALLY_UNIQUE_IDENTIFIER_ID "IMS:1000080"
#define IMS_UNIVERSALLY_UNIQUE_IDENTIFIER_NAME "universally unique identifier"
/* def: "universally unique identifier is unique throughout the world and allows to doubtlessly identify the ibd file." [COMPUTIS:IMS]
xref: value-type:xsd\:string "The allowed value-type for this CV term."
relationship: is_a IMS:1000008 ! Ibd Indentification */

#define IMS_IBD_MD5_ID "IMS:1000090"
#define IMS_IBD_MD5_NAME "ibd MD5"
/* def: "MD5 (Message-Digest algorithm 5) is a cryptographic hash function with a 128-bit hash value used to check the integrity of files." [COMPUTIS:IMS]
xref: value-type:xsd\:string "The allowed value-type for this CV term."
relationship: is_a IMS:1000009 ! Ibd Checksum */

#define IMS_IBD_SHA_1_ID "IMS:1000091"
#define IMS_IBD_SHA_1_NAME "ibd SHA_1"
/* def: "SHA-1 (Secure Hash Algorithm-1) is a cryptographic hash function designed by the National Security Agency (NSA) and published by the NIST as a U. S. government standard. It is also used to verify file integrity." [COMPUTIS:IMS]
xref: value-type:xsd\:string "The allowed value-type for this CV term."
relationship: is_a IMS:1000009 ! Ibd Checksum */

#define IMS_EXTERNAL_DATA_ID "IMS:1000101"
#define IMS_EXTERNAL_DATA_NAME "external data"
/* def: "Shows that there is no data in the <binary> section of the file." [COMPUTIS:IMS]
xref: value-type:xsd\:boolean "The allowed value-type for this CV term."
relationship: is_a IMS:1000001 ! Ibd Offest Handle */


#define IMS_EXTERNAL_OFFSET_ID "IMS:1000102"
#define IMS_EXTERNAL_OFFSET_NAME "external offset"
/* def: "The position where the data of an array of a mass spectrum begins." [COMPUTIS:IMS]
xref: value-type:xsd\:nonNegativeInteger "The allowed value-type for this CV term."
relationship: is_a IMS:1000001 ! Ibd Offest Handle
relationship: has_units UO:0000233 ! Byte */

#define IMS_EXTERNAL_ARRAY_LENGTH_ID "IMS:1000103"
#define IMS_EXTERNAL_ARRAY_LENGTH_NAME "external array length"
/* def: "Describes how many fields an array contains." [COMPUTIS:IMS]
xref: value-type:xsd\:nonNegativeInteger "The allowed value-type for this CV term."
relationship: is_a IMS:1000001 ! Ibd Offest Handle
relationship: has_units UO:0000189 ! Count */

#define IMS_EXTERNAL_ENCODED_LENGTH_ID "IMS:1000104"
#define IMS_EXTERNAL_ENCODED_LENGTH_NAME "external encoded length"
/* def: "Describes the length of the written data." [COMPUTIS:IMS]
xref: value-type:xsd\:nonNegativeInteger "The allowed value-type for this CV term."
relationship: is_a IMS:1000001 ! Ibd Offest Handle
relationship: has_units UO:0000233 ! Byte */

#define IMS_PIXEL_MODE_ID "IMS:1000110"
#define IMS_PIXEL_MODE_NAME "pixel mode"
/* def: "laser keeps the position while firing at the same same spot one or several times." [COMPUTIS:IMS]
relationship: is_a IMS:1000011 ! laser shot mode */

#define IMS_RASTER_MODE_ID "IMS:1000111"
#define IMS_RASTER_MODE_NAME "raster mode"
/* def: "laser is moved while continuously firing at the sample." [COMPUTIS:IMS]
relationship: is_a IMS:1000011 ! laser shot mode */

#define IMS_STIGMATIC_MODE_ID "IMS:1000112"
#define IMS_STIGMATIC_MODE_NAME "stigmatic mode"
/* def: "laser is moved around one point firing until moved to the next position (pixel)." [COMPUTIS:IMS]
relationship: is_a IMS:1000011 ! laser shot mode */

#define IMS_SIMS_ID "IMS:1000120"
#define IMS_SIMS_NAME "SIMS"
/* def: "Parameters describing SIMS sources." [COMPUTIS:IMS]
relationship: part_of IMS:1000012 ! Imaging Ion Source */

#define IMS_DESI_ID "IMS:1000121"
#define IMS_DESI_NAME "DESI"
/* def: "Parameters describing DESI sources." [COMPUTIS:IMS]
relationship: part_of IMS:1000012 ! Imaging Ion Source */

#define IMS_IONS_PER_SQUARE_CENTIMETER_ID "IMS:1000130"
#define IMS_IONS_PER_SQUARE_CENTIMETER_NAME "ions per square centimeter"
/* def: "An area density unit which is equal to the count of ions divided by the area in centimeters squared." [COMPUTIS:IMS]
relationship: is_a IMS:1000013 ! unit */

#define IMS_MILLILITER_PER_MINUTE_ID "IMS:1000131"
#define IMS_MILLILITER_PER_MINUTE_NAME "milliliter per minute"
/* def: "A flow rate describes the throughput per time." [COMPUTIS:IMS]
relationship: is_a IMS:1000013 ! unit */

#define IMS_32_BIT_INTEGER_ID "IMS:1000141"
#define IMS_32_BIT_INTEGER_NAME "32-bit integer"
/* def: "Signed 32 bit integer." [COMPUTIS:IMS]
relationship: is_a IMS:1000014 ! ibd data type */

#define IMS_64_BIT_INTEGER_ID "IMS:1000142"
#define IMS_64_BIT_INTEGER_NAME "64-bit integer"
/* def: "Signed 64 bit integer." [COMPUTIS:IMS]
relationship: is_a IMS:1000014 ! ibd data type */

#define IMS_POSITION_ACCURACY_ID "IMS:1000200"
#define IMS_POSITION_ACCURACY_NAME "position accuracy"
/* def: "Accuracy is the degree of conformity of a measured position to its actual value." [COMPUTIS:IMS]
xref: value-type:xsd\:float "The allowed value-type for this CV term."
relationship: is_a IMS:1000002 ! Sample Stage
relationship: has_units UO:0000187 ! Percent */

#define IMS_STEP_SIZE_ID "IMS:1000201"
#define IMS_STEP_SIZE_NAME "step size"
/* def: "Specify the range between two different messuring points on the sample." [COMPUTIS:IMS]
xref: value-type:xsd\:float "The allowed value-type for this CV term."
relationship: is_a IMS:1000002 ! Sample Stage
relationship: has_units UO:0000017 ! Micrometer */

#define IMS_TARGET_MATERIAL_ID "IMS:1000202"
#define IMS_TARGET_MATERIAL_NAME "target material"
/* def: "Describes the material the target is made of." [COMPUTIS:IMS]
xref: value-type:xsd\:string "The allowed value-type for this CV term."
relationship: is_a IMS:1000002 ! Sample Stage */

#define IMS_BOTTOM_UP_ID "IMS:1000400"
#define IMS_BOTTOM_UP_NAME "bottom up"
/* def: "The starting point is at the bottom of the sample and the scanning happens in up direction (parallel to the y-axis)." [COMPUTIS:IMS]
relationship: is_a IMS:1000040 ! Scan Direction */

#define IMS_TOP_DOWN_ID "IMS:1000401"
#define IMS_TOP_DOWN_NAME "top down"
/* def: "The starting point is at the top of the sample and the scanning happens in bottom direction (parallel to the y-axis)." [COMPUTIS:IMS]
relationship: is_a IMS:1000040 ! Scan Direction */

#define IMS_LEFT_RIGHT_ID "IMS:1000402"
#define IMS_LEFT_RIGHT_NAME "left right"
/* def: "The starting point is at the left of the sample and the scanning happens in right direction (parallel to the x-axis)." [COMPUTIS:IMS]
relationship: is_a IMS:1000040 ! Scan Direction */

#define IMS_RIGHT_LEFT_ID "IMS:1000403"
#define IMS_RIGHT_LEFT_NAME "right left"
/* def: "The starting point is at the right of the sample and the scanning happens in left direction. (parallel to the x-axis)." [COMPUTIS:IMS]
relationship: is_a IMS:1000040 ! Scan Direction */

#define IMS_NO_DIRECTION_ID "IMS:1000404"
#define IMS_NO_DIRECTION_NAME "no direction"
/* def: "The scanning points are randomly distributed on the sample." [COMPUTIS:IMS]
relationship: is_a IMS:1000040 ! Scan Direction */

#define IMS_MEANDERING_ID "IMS:1000410"
#define IMS_MEANDERING_NAME "meandering"
/* def: "The scanning happens in nonstop way. As soon as the end of the sample is reached, the scanning direction will be switched and the scanning is continued. There is no new positioning neccessary." [COMPUTIS:IMS]
relationship: is_a IMS:1000041 ! Scan Pattern */

#define IMS_ONE_WAY_ID "IMS:1000411"
#define IMS_ONE_WAY_NAME "one way"
/* def: "OBSOLETE The scanning always happens in the same direction. As soon as the end of the sample is reached, the stage is positioned at the starting edge to begin the next run." [COMPUTIS:IMS]
comment: Was made obsolete because the generally used term is "flyback" (IMS:1000413).
is_obsolete: true */

#define IMS_RANDOM_ACCESS_ID "IMS:1000412"
#define IMS_RANDOM_ACCESS_NAME "random access"
/* def: "The scanning points are randomly chosen and do not follow a pattern." [COMPUTIS:IMS]
relationship: is_a IMS:1000041 ! Scan Pattern */

#define IMS_FLYBACK_ID "IMS:1000413"
#define IMS_FLYBACK_NAME "flyback"
/* def: "The scanning always happens in the same direction. As soon as the end of the sample is reached, the stage is positioned at the starting edge to begin the next run." [COMPUTIS:IMS]
relationship: is_a IMS:1000041 ! Scan Pattern */

#define IMS_HORIZONTAL_LINE_SCAN_ID "IMS:1000480"
#define IMS_HORIZONTAL_LINE_SCAN_NAME "horizontal line scan"
/* def: "The scanning line is a horizontal one." [COMPUTIS:IMS]
relationship: is_a IMS:1000048 ! Scan Type */

#define IMS_VERTICAL_LINE_SCAN_ID "IMS:1000481"
#define IMS_VERTICAL_LINE_SCAN_NAME "vertical line scan"
/* def: "The scanning line is a vertical one." [COMPUTIS:IMS]
relationship: is_a IMS:1000048 ! Scan Type */

#define IMS_LINESCAN_RIGHT_LEFT_ID "IMS:1000490"
#define IMS_LINESCAN_RIGHT_LEFT_NAME "linescan right left"
/* def: "The starting point is at the right of the sample and the scanning happens in left direction. (parallel to the x-axis)." [COMPUTIS:IMS]
relationship: is_a IMS:1000049 ! Line Scan Direction */

#define IMS_LINESCAN_LEFT_RIGHT_ID "IMS:1000491"
#define IMS_LINESCAN_LEFT_RIGHT_NAME "linescan left right"
/* def: "The starting point is at the left of the sample and the scanning happens in right direction (parallel to the x-axis)." [COMPUTIS:IMS]
relationship: is_a IMS:1000049 ! Line Scan Direction */

#define IMS_LINESCAN_BOTTOM_UP_ID "IMS:1000492"
#define IMS_LINESCAN_BOTTOM_UP_NAME "linescan bottom up"
/* def: "The starting point is at the bottom of the sample and the scanning happens in up direction (parallel to the y-axis)." [COMPUTIS:IMS]
relationship: is_a IMS:1000049 ! Line Scan Direction */

#define IMS_LINESCAN_TOP_DOWN_ID "IMS:1000493"
#define IMS_LINESCAN_TOP_DOWN_NAME "linescan top down"
/* def: "The starting point is at the top of the sample and the scanning happens in bottom direction (parallel to the y-axis)." [COMPUTIS:IMS]
relationship: is_a IMS:1000049 ! Line Scan Direction */

#define IMS__8_BIT_INTEGER_ID "IMS:1100000"
#define IMS__8_BIT_INTEGER_NAME "8-bit integer"
/* def: "Signed 8-bit integer." [COMPUTIS:IMS]
relationship: is_a MS:1000518 ! binary data type */

#define IMS__16_BIT_INTEGER_ID "IMS:1100001"
#define IMS__16_BIT_INTEGER_NAME "16-bit integer"
/* def: "Signed 16-bit integer." [COMPUTIS:IMS]
relationship: is_a MS:1000518 ! binary data type */

#define IMS_PRIMARY_ION_GUN_SPECIES_ID "IMS:1001201"
#define IMS_PRIMARY_ION_GUN_SPECIES_NAME "primary ion gun species"
/* def: "Describes the species of ions used for ionizing the sample." [COMPUTIS:IMS]
relationship: is_a IMS:1000120 ! SIMS */

#define IMS_BEAM_ENERGY_ID "IMS:1001202"
#define IMS_BEAM_ENERGY_NAME "beam energy"
/* def: "Energy of the shot of the primary ion gun species in electronvolt." [COMPUTIS:IMS]
xref: value-type:xsd\:float "The allowed value-type for this CV term."
relationship: is_a IMS:1000120 ! SIMS
relationship: has_units UO:0000226 ! electronvolt */

#define IMS_BEAM_CURRENT_ID "IMS:1001203"
#define IMS_BEAM_CURRENT_NAME "beam current"
/* def: "Number of charges per second shot at the surface of the analyte." [COMPUTIS:IMS]
xref: value-type:xsd\:float "The allowed value-type for this CV term."
relationship: is_a IMS:1000120 ! SIMS
relationship: has_units UO:0000038 ! microampere */

#define IMS_CYCLE_TIME_ID "IMS:1001204"
#define IMS_CYCLE_TIME_NAME "cycle time"
/* def: "Inversion of the number of ions shot at the surface in one second. The maximum depends on the maximum time of flight in the mass spectrometer." [COMPUTIS:IMS]
xref: value-type:xsd\:float "The allowed value-type for this CV term."
relationship: is_a IMS:1000120 ! SIMS
relationship: has_units UO:0000029 ! microsecond */

#define IMS_TIME_RESOLUTION_ID "IMS:1001205"
#define IMS_TIME_RESOLUTION_NAME "time resolution"
/* def: "Size of the bin of the time recorded by the time to digital converter measured in pico seconds" [COMPUTIS:IMS]
xref: value-type:xsd\:float "The allowed value-type for this CV term."
relationship: is_a IMS:1000120 ! SIMS
relationship: has_units UO:0000030 ! picosecond */

#define IMS_POLARITY_ID "IMS:1001206"
#define IMS_POLARITY_NAME "polarity"
/* def: "Polarity of the measured secondary ions (?)" [COMPUTIS:IMS]
relationship: is_a IMS:1000120 ! SIMS */

#define IMS_PRIMARY_ION_DOSE_DENSITY_ID "IMS:1001207"
#define IMS_PRIMARY_ION_DOSE_DENSITY_NAME "primary ion dose density"
/* def: "Density of the primary ions." [COMPUTIS:IMS]
xref: value-type:xsd\:float "The allowed value-type for this CV term."
relationship: is_a IMS:1000120 ! SIMS
relationship: has_units UO:0000030 ! ions per square centimeter */

#define IMS_SOLVENT_ID "IMS:1001211"
#define IMS_SOLVENT_NAME "solvent"
/* def: "The solvent which was used to pick up the analyte from the surface of the imaging object." [COMPUTIS:IMS]
relationship: is_a IMS:1000121 ! DESI */

#define IMS_SPRAY_VOLTAGE_ID "IMS:1001212"
#define IMS_SPRAY_VOLTAGE_NAME "spray voltage"
/* def: "Voltage applied to the DESI source to get ions moving into the direction of the inlet of the mass spectrometer." [COMPUTIS:IMS]
xref: value-type:xsd\:float "The allowed value-type for this CV term."
relationship: is_a IMS:1000121 ! DESI
relationship: has_units UO:0000248 ! kilovolt */

#define IMS_SOLVENT_FLOWRATE_ID "IMS:1001213"
#define IMS_SOLVENT_FLOWRATE_NAME "solvent flowrate"
/* def: "Rate with which the solvent is flowing on the surface of the imaging object" [COMPUTIS:IMS]
xref: value-type:xsd\:float "The allowed value-type for this CV term."
relationship: is_a IMS:1000121 ! DESI
relationship: has_units IMS:1000131 ! milliliter per minute */
