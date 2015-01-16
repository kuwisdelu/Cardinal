
#define MS_PROTEOMICS_STANDARDS_INITIATIVE_MASS_SPECTROMETRY_VOCABULARIES_ID "MS:0000000"
#define MS_PROTEOMICS_STANDARDS_INITIATIVE_MASS_SPECTROMETRY_VOCABULARIES_NAME "Proteomics Standards Initiative Mass Spectrometry Vocabularies"
/* def: "Proteomics Standards Initiative Mass Spectrometry Vocabularies." [PSI:MS] */

#define MS_SAMPLE_NUMBER_ID "MS:1000001"
#define MS_SAMPLE_NUMBER_NAME "sample number"
/* def: "A reference number relevant to the sample under study." [PSI:MS]
xref: value-type:xsd\:string "The allowed value-type for this CV term."
is_a: MS:1000548 ! sample attribute */

// #define MS_SAMPLE_NAME_ID "MS:1000002"
// #define MS_SAMPLE_NAME_NAME "sample name"
/* def: "OBSOLETE A reference string relevant to the sample under study." [PSI:MS]
comment: This former purgatory term was made obsolete.
is_obsolete: true */

#define MS_SAMPLE_STATE_ID "MS:1000003"
#define MS_SAMPLE_STATE_NAME "sample state"
/* def: "The chemical phase of a pure sample, or the state of a mixed sample." [PSI:MS]
is_a: MS:1000548 ! sample attribute */

#define MS_SAMPLE_MASS_ID "MS:1000004"
#define MS_SAMPLE_MASS_NAME "sample mass"
/* def: "Total mass of sample used." [PSI:MS]
xref: value-type:xsd\:float "The allowed value-type for this CV term."
is_a: MS:1000548 ! sample attribute
relationship: has_units UO:0000021 ! gram */

#define MS_SAMPLE_VOLUME_ID "MS:1000005"
#define MS_SAMPLE_VOLUME_NAME "sample volume"
/* def: "Total volume of solution used." [PSI:MS]
xref: value-type:xsd\:float "The allowed value-type for this CV term."
is_a: MS:1000548 ! sample attribute
relationship: has_units UO:0000098 ! milliliter */

#define MS_SAMPLE_CONCENTRATION_ID "MS:1000006"
#define MS_SAMPLE_CONCENTRATION_NAME "sample concentration"
/* def: "Concentration of sample in picomol/ul, femtomol/ul or attomol/ul solution used." [PSI:MS]
xref: value-type:xsd\:float "The allowed value-type for this CV term."
is_a: MS:1000548 ! sample attribute
relationship: has_units UO:0000175 ! gram per liter */

#define MS_INLET_TYPE_ID "MS:1000007"
#define MS_INLET_TYPE_NAME "inlet type"
/* def: "The nature of the sample inlet." [PSI:MS]
relationship: part_of MS:1000458 ! source */

#define MS_IONIZATION_TYPE_ID "MS:1000008"
#define MS_IONIZATION_TYPE_NAME "ionization type"
/* def: "The method by which gas phase ions are generated from the sample." [PSI:MS]
relationship: part_of MS:1000458 ! source */

// #define MS_IONIZATION_MODE_ID "MS:1000009"
// #define MS_IONIZATION_MODE_NAME "ionization mode"
/* def: "OBSOLETE Whether positive or negative ions are selected for analysis by the spectrometer." [PSI:MS]
comment: This term was made obsolete because it was replaced by scan polarity (MS:1000465).
is_obsolete: true */

// #define MS_ANALYZER_TYPE_ID "MS:1000010"
// #define MS_ANALYZER_TYPE_NAME "analyzer type"
/* def: "OBSOLETE The common name of the particular analyzer stage being described. Synonym of mass analyzer, should be obsoleted." [PSI:MS]
comment: This former purgatory term was made obsolete.
is_obsolete: true */

#define MS_MASS_RESOLUTION_ID "MS:1000011"
#define MS_MASS_RESOLUTION_NAME "mass resolution"
/* def: "Smallest mass difference between two equal magnitude peaks so that the valley between them is a specified fraction of the peak height." [PSI:MS]
xref: value-type:xsd\:string "The allowed value-type for this CV term."
is_a: MS:1000503 ! scan attribute */

#define MS_RESOLUTION_MEASUREMENT_METHOD_ID "MS:1000012"
#define MS_RESOLUTION_MEASUREMENT_METHOD_NAME "resolution measurement method"
/* def: "Which of the available standard measures is used to define whether two peaks are separate." [PSI:MS]
is_a: MS:1000596 ! measurement method */

#define MS_RESOLUTION_TYPE_ID "MS:1000013"
#define MS_RESOLUTION_TYPE_NAME "resolution type"
/* def: "Specify the nature of resolution for the mass analyzer. Resolution is usually either constant with respect to m/z or proportional to m/z." [PSI:MS]
relationship: part_of MS:1000479 ! purgatory */

#define MS_ACCURACY_ID "MS:1000014"
#define MS_ACCURACY_NAME "accuracy"
/* def: "Accuracy is the degree of conformity of a measured mass to its actual value." [PSI:MS]
xref: value-type:xsd\:float "The allowed value-type for this CV term."
is_a: MS:1000480 ! mass analyzer attribute
relationship: has_units MS:1000040 ! m/z
relationship: has_units UO:0000169 ! parts per million */

#define MS_SCAN_RATE_ID "MS:1000015"
#define MS_SCAN_RATE_NAME "scan rate"
/* def: "Rate in Th/sec for scanning analyzers." [PSI:MS]
xref: value-type:xsd\:float "The allowed value-type for this CV term."
is_a: MS:1000503 ! scan attribute
relationship: has_units MS:1000807 ! Th/s */

#define MS_SCAN_START_TIME_ID "MS:1000016"
#define MS_SCAN_START_TIME_NAME "scan start time"
/* def: "The time that an analyzer started a scan, relative to the start of the MS run." [PSI:MS]
xref: value-type:xsd\:float "The allowed value-type for this CV term."
is_a: MS:1000503 ! scan attribute
is_a: MS:1001105 ! peptide result details
is_a: MS:1001405 ! spectrum identification result details
relationship: has_units UO:0000010 ! second
relationship: has_units UO:0000031 ! minute */

#define MS_SCAN_FUNCTION_ID "MS:1000017"
#define MS_SCAN_FUNCTION_NAME "Scan Function"
/* def: "Describes the type of mass analysis being performed. Two primary modes are: typical acquisition over a range of masses (Mass Scan), and Selected Ion Detection. The primary difference is that Selected Ion Detection produces a single value for the signal at the selected mass rather than producing a mass spectrum." [PSI:MS]
relationship: part_of MS:1000479 ! purgatory */

#define MS_SCAN_DIRECTION_ID "MS:1000018"
#define MS_SCAN_DIRECTION_NAME "scan direction"
/* def: "Direction in terms of m/z of the scan for scanning analyzers (low to high, or high to low)." [PSI:MS]
relationship: part_of MS:1000441 ! scan */

#define MS_SCAN_LAW_ID "MS:1000019"
#define MS_SCAN_LAW_NAME "scan law"
/* def: "Describes the function in control of the m/z scan (for scanning instruments). Commonly the scan function is linear, but in principle any function can be used." [PSI:MS]
relationship: part_of MS:1000441 ! scan */

#define MS_SCANNING_METHOD_ID "MS:1000020"
#define MS_SCANNING_METHOD_NAME "scanning method"
/* def: "Describes the acquisition data type produced by a tandem mass spectrometry experiment." [PSI:MS]
synonym: "Tandem Scanning Method" RELATED []
relationship: part_of MS:1000479 ! purgatory */

#define MS_REFLECTRON_STATE_ID "MS:1000021"
#define MS_REFLECTRON_STATE_NAME "reflectron state"
/* def: "Status of the reflectron, turned on or off." [PSI:MS]
is_a: MS:1000480 ! mass analyzer attribute */

#define MS_TOF_TOTAL_PATH_LENGTH_ID "MS:1000022"
#define MS_TOF_TOTAL_PATH_LENGTH_NAME "TOF Total Path Length"
/* def: "The length of the field free drift space in a time of flight mass spectrometer." [PSI:MS]
xref: value-type:xsd\:float "The allowed value-type for this CV term."
is_a: MS:1000480 ! mass analyzer attribute
relationship: has_units UO:0000008 ! meter */

// #define MS_ISOLATION_WIDTH_ID "MS:1000023"
// #define MS_ISOLATION_WIDTH_NAME "isolation width"
/* def: "OBSOLETE The total width (i.e. not half for plus-or-minus) of the gate applied around a selected precursor ion." [PSI:MS]
comment: This former purgatory term was made obsolete.
xref: value-type:xsd\:float "The allowed value-type for this CV term."
relationship: has_units MS:1000040 ! m/z
is_obsolete: true */

#define MS_FINAL_MS_EXPONENT_ID "MS:1000024"
#define MS_FINAL_MS_EXPONENT_NAME "final MS exponent"
/* def: "Final MS level achieved when performing PFF with the ion trap (e.g. MS E10)." [PSI:MS]
xref: value-type:xsd\:int "The allowed value-type for this CV term."
is_a: MS:1000480 ! mass analyzer attribute */

#define MS_MAGNETIC_FIELD_STRENGTH_ID "MS:1000025"
#define MS_MAGNETIC_FIELD_STRENGTH_NAME "magnetic field strength"
/* def: "A property of space that produces a force on a charged particle equal to qv x B where q is the particle charge and v its velocity." [PSI:MS]
synonym: "B" EXACT []
synonym: "Magnetic Field" RELATED []
xref: value-type:xsd\:float "The allowed value-type for this CV term."
is_a: MS:1000480 ! mass analyzer attribute
relationship: has_units UO:0000228 ! tesla */

#define MS_DETECTOR_TYPE_ID "MS:1000026"
#define MS_DETECTOR_TYPE_NAME "detector type"
/* def: "Type of detector used in the mass spectrometer." [PSI:MS]
relationship: part_of MS:1000453 ! detector */

#define MS_DETECTOR_ACQUISITION_MODE_ID "MS:1000027"
#define MS_DETECTOR_ACQUISITION_MODE_NAME "detector acquisition mode"
/* def: "Method by which detector signal is acquired by the data system." [PSI:MS]
relationship: part_of MS:1000453 ! detector */

#define MS_DETECTOR_RESOLUTION_ID "MS:1000028"
#define MS_DETECTOR_RESOLUTION_NAME "detector resolution"
/* def: "The resolving power of the detector to detect the smallest difference between two ions so that the valley between them is a specified fraction of the peak height." [PSI:MS]
xref: value-type:xsd\:float "The allowed value-type for this CV term."
is_a: MS:1000481 ! detector attribute */

#define MS_SAMPLING_FREQUENCY_ID "MS:1000029"
#define MS_SAMPLING_FREQUENCY_NAME "sampling frequency"
/* def: "The rate of signal sampling (measurement) with respect to time." [PSI:MS]
synonym: "ADC Sampling Frequency" NARROW []
xref: value-type:xsd\:float "The allowed value-type for this CV term."
is_a: MS:1000481 ! detector attribute
relationship: has_units UO:0000106 ! hertz */

// #define MS_VENDOR_ID "MS:1000030"
// #define MS_VENDOR_NAME "vendor"
/* def: "OBSOLETE Name of instrument vendor." [PSI:MS]
comment: This term was made obsolete because it was replaced by instrument model (MS:1000031).
is_obsolete: true */

#define MS_INSTRUMENT_MODEL_ID "MS:1000031"
#define MS_INSTRUMENT_MODEL_NAME "instrument model"
/* def: "Instrument model name not including the vendor's name." [PSI:MS]
relationship: part_of MS:1000463 ! instrument */

#define MS_CUSTOMIZATION_ID "MS:1000032"
#define MS_CUSTOMIZATION_NAME "customization"
/* def: "Free text description of a single customization made to the instrument; for several modifications, use several entries." [PSI:MS]
xref: value-type:xsd\:string "The allowed value-type for this CV term."
is_a: MS:1000496 ! instrument attribute */

#define MS_DEISOTOPING_ID "MS:1000033"
#define MS_DEISOTOPING_NAME "deisotoping"
/* def: "The removal of isotope peaks to represent the fragment ion as one data point and is commonly done to reduce complexity. It is done in conjunction with the charge state deconvolution." [PSI:MS]
is_a: MS:1000543 ! data processing action */

#define MS_CHARGE_DECONVOLUTION_ID "MS:1000034"
#define MS_CHARGE_DECONVOLUTION_NAME "charge deconvolution"
/* def: "The determination of the mass of an ion based on the mass spectral peaks that represent multiple-charge ions." [PSI:MS]
is_a: MS:1000543 ! data processing action */

#define MS_PEAK_PICKING_ID "MS:1000035"
#define MS_PEAK_PICKING_NAME "peak picking"
/* def: "Spectral peak processing conducted on the acquired data to convert profile data to centroided data." [PSI:MS]
is_a: MS:1000543 ! data processing action */

// #define MS_SCAN_MODE_ID "MS:1000036"
// #define MS_SCAN_MODE_NAME "scan mode"
/* def: "OBSOLETE." [PSI:MS]
comment: This term was made obsolete because .
is_obsolete: true */

// #define MS_POLARITY_ID "MS:1000037"
// #define MS_POLARITY_NAME "polarity"
/* def: "OBSOLETE Terms to describe the polarity setting of the instrument." [PSI:MS]
comment: This term was made obsolete because it was redundant with the Pato Ontology term polarity (UO:0002186).
relationship: part_of MS:1000459 ! spectrum instrument description
is_obsolete: true */

// #define MS_MINUTE_ID "MS:1000038"
// #define MS_MINUTE_NAME "minute"
/* def: "OBSOLETE Acquisition time in minutes." [PSI:MS]
comment: This term was made obsolete because it was redundant with Unit Ontology minute (UO:0000031).
is_obsolete: true */

// #define MS_SECOND_ID "MS:1000039"
// #define MS_SECOND_NAME "second"
/* def: "OBSOLETE Acquisition time in seconds." [PSI:MS]
comment: This term was made obsolete because it was redundant with Unit Ontology second (UO:0000010).
is_obsolete: true */

#define MS_M_Z_ID "MS:1000040"
#define MS_M_Z_NAME "m/z"
/* def: "Three-character symbol m/z is used to denote the quantity formed by dividing the mass of an ion in unified atomic mass units by its charge number (regardless of sign). The symbol is written in italicized lower case letters with no spaces. Note 1: The term mass-to-charge-ratio is deprecated. Mass-to-charge ratio has been used for the abscissa of a mass spectrum, although the quantity measured is not the quotient of the ion's mass to its electric charge. The three-character symbol m/z is recommended for the quantity that is the independent variable in a mass spectrum Note 2: The proposed unit thomson (Th) is deprecated." [PSI:MS]
synonym: "mass-to-charge ratio" EXACT []
synonym: "Th" EXACT []
synonym: "thomson" EXACT []
is_a: UO:0000000 ! unit */

#define MS_CHARGE_STATE_ID "MS:1000041"
#define MS_CHARGE_STATE_NAME "charge state"
/* def: "The charge state of the ion, single or multiple and positive or negatively charged." [PSI:MS]
synonym: "z" EXACT []
xref: value-type:xsd\:int "The allowed value-type for this CV term."
is_a: MS:1000455 ! ion selection attribute */

#define MS_PEAK_INTENSITY_ID "MS:1000042"
#define MS_PEAK_INTENSITY_NAME "peak intensity"
/* def: "Intensity of ions as measured by the height or area of a peak in a mass spectrum." [PSI:MS]
xref: value-type:xsd\:float "The allowed value-type for this CV term."
is_a: MS:1000455 ! ion selection attribute
relationship: has_units MS:1000131 ! number of detector counts
relationship: has_units MS:1000132 ! percent of base peak
relationship: has_units MS:1000814 ! counts per second
relationship: has_units MS:1000905 ! percent of base peak times 100
relationship: has_units UO:0000269 ! absorbance unit
relationship: part_of MS:1000231 ! peak */

#define MS_INTENSITY_UNIT_ID "MS:1000043"
#define MS_INTENSITY_UNIT_NAME "intensity unit"
/* def: "Intensity units are commonly arbitrary. Detected in counts per second (cps) when using counting detectors, but measured in volts when using analog detectors." [PSI:MS]
is_a: UO:0000000 ! unit */

#define MS_DISSOCIATION_METHOD_ID "MS:1000044"
#define MS_DISSOCIATION_METHOD_NAME "dissociation method"
/* def: "Fragmentation method used for dissociation or fragmentation." [PSI:MS]
synonym: "Activation Method" RELATED []
relationship: part_of MS:1000456 ! precursor activation */

#define MS_COLLISION_ENERGY_ID "MS:1000045"
#define MS_COLLISION_ENERGY_NAME "collision energy"
/* def: "Energy for an ion experiencing collision with a stationary gas particle resulting in dissociation of the ion." [PSI:MS]
xref: value-type:xsd\:float "The allowed value-type for this CV term."
is_a: MS:1000510 ! precursor activation attribute
relationship: has_units UO:0000266 ! electronvolt */

// #define MS_ENERGY_UNIT_ID "MS:1000046"
// #define MS_ENERGY_UNIT_NAME "energy unit"
/* def: "OBSOLETE Energy units are represented in either eV or Joules." [PSI:MS]
comment: This term was made obsolete because it was redundant with the Unit Ontology term energy unit (UO:0000111).
is_a: UO:0000000 ! unit
is_obsolete: true */

#define MS_EMULSION_ID "MS:1000047"
#define MS_EMULSION_NAME "emulsion"
/* def: "State if the sample is in emulsion form." [PSI:MS]
is_a: MS:1000003 ! sample state */

#define MS_GASEOUS_SAMPLE_STATE_ID "MS:1000048"
#define MS_GASEOUS_SAMPLE_STATE_NAME "gaseous sample state"
/* def: "State if the sample is in gaseous form." [PSI:MS]
is_a: MS:1000003 ! sample state */

#define MS_LIQUID_SAMPLE_STATE_ID "MS:1000049"
#define MS_LIQUID_SAMPLE_STATE_NAME "liquid sample state"
/* def: "State if the sample is in liquid form." [PSI:MS]
is_a: MS:1000003 ! sample state */

#define MS_SOLID_SAMPLE_STATE_ID "MS:1000050"
#define MS_SOLID_SAMPLE_STATE_NAME "solid sample state"
/* def: "State if the sample is in solid form." [PSI:MS]
is_a: MS:1000003 ! sample state */

#define MS_SOLUTION_ID "MS:1000051"
#define MS_SOLUTION_NAME "solution"
/* def: "State if the sample is in solution form." [PSI:MS]
is_a: MS:1000003 ! sample state */

#define MS_SUSPENSION_ID "MS:1000052"
#define MS_SUSPENSION_NAME "suspension"
/* def: "State if the sample is in suspension form." [PSI:MS]
is_a: MS:1000003 ! sample state */

#define MS_SAMPLE_BATCH_ID "MS:1000053"
#define MS_SAMPLE_BATCH_NAME "sample batch"
/* def: "Sample batch lot identifier." [PSI:MS]
xref: value-type:xsd\:string "The allowed value-type for this CV term."
is_a: MS:1000548 ! sample attribute */

// #define MS_CHROMATOGRAPHY_ID "MS:1000054"
// #define MS_CHROMATOGRAPHY_NAME "chromatography"
/* def: "OBSOLETE Chromatographic conditions used to obtain the sample." [PSI:MS]
comment: This former purgatory term was made obsolete.
is_obsolete: true */

#define MS_CONTINUOUS_FLOW_FAST_ATOM_BOMBARDMENT_ID "MS:1000055"
#define MS_CONTINUOUS_FLOW_FAST_ATOM_BOMBARDMENT_NAME "continuous flow fast atom bombardment"
/* def: "Fast atom bombardment ionization in which the analyte in solution is entrained in a flowing liquid matrix." [PSI:MS]
synonym: "CF-FAB" EXACT []
is_a: MS:1000007 ! inlet type */

#define MS_DIRECT_INLET_ID "MS:1000056"
#define MS_DIRECT_INLET_NAME "direct inlet"
/* def: "The sample is directly inserted into the ion source, usually on the end of a heatable probe." [PSI:MS]
is_a: MS:1000007 ! inlet type */

#define MS_ELECTROSPRAY_INLET_ID "MS:1000057"
#define MS_ELECTROSPRAY_INLET_NAME "electrospray inlet"
/* def: "Inlet used for introducing the liquid sample into an electrospray ionization source." [PSI:MS]
is_a: MS:1000007 ! inlet type */

#define MS_FLOW_INJECTION_ANALYSIS_ID "MS:1000058"
#define MS_FLOW_INJECTION_ANALYSIS_NAME "flow injection analysis"
/* def: "Sample is directly injected or infused into the ionization source." [PSI:MS]
is_a: MS:1000007 ! inlet type */

#define MS_INDUCTIVELY_COUPLED_PLASMA_ID "MS:1000059"
#define MS_INDUCTIVELY_COUPLED_PLASMA_NAME "inductively coupled plasma"
/* def: "A gas discharge ion source in which the energy to the plasma is supplied by electromagnetic induction." [PSI:MS]
is_a: MS:1000007 ! inlet type */

#define MS_INFUSION_ID "MS:1000060"
#define MS_INFUSION_NAME "infusion"
/* def: "The continuous flow of solution of a sample into the ionization source." [PSI:MS]
is_a: MS:1000007 ! inlet type */

#define MS_JET_SEPARATOR_ID "MS:1000061"
#define MS_JET_SEPARATOR_NAME "jet separator"
/* def: "A device that separates carrier gas from gaseous analyte molecules on the basis of diffusivity." [PSI:MS]
is_a: MS:1000007 ! inlet type */

#define MS_MEMBRANE_SEPARATOR_ID "MS:1000062"
#define MS_MEMBRANE_SEPARATOR_NAME "membrane separator"
/* def: "A device to separate carrier molecules from analyte molecules on the basis of ease of diffusion across a semipermeable membrane." [PSI:MS]
is_a: MS:1000007 ! inlet type */

#define MS_MOVING_BELT_ID "MS:1000063"
#define MS_MOVING_BELT_NAME "moving belt"
/* def: "Continuous moving surface in the form of a belt which passes through an ionsource carrying analyte molecules." [PSI:MS]
is_a: MS:1000007 ! inlet type */

#define MS_MOVING_WIRE_ID "MS:1000064"
#define MS_MOVING_WIRE_NAME "moving wire"
/* def: "Continuous moving surface in the form of a wire which passes through an ionsource carrying analyte molecules." [PSI:MS]
is_a: MS:1000007 ! inlet type */

#define MS_OPEN_SPLIT_ID "MS:1000065"
#define MS_OPEN_SPLIT_NAME "open split"
/* def: "A division of flowing stream of liquid into two streams." [PSI:MS]
is_a: MS:1000007 ! inlet type */

#define MS_PARTICLE_BEAM_ID "MS:1000066"
#define MS_PARTICLE_BEAM_NAME "particle beam"
/* def: "Method for generating ions from a solution of an analyte." [PSI:MS]
is_a: MS:1000007 ! inlet type */

#define MS_RESERVOIR_ID "MS:1000067"
#define MS_RESERVOIR_NAME "reservoir"
/* def: "A sample inlet method involving a reservoir." [PSI:MS]
is_a: MS:1000007 ! inlet type */

#define MS_SEPTUM_ID "MS:1000068"
#define MS_SEPTUM_NAME "septum"
/* def: "A disc composed of a flexible material that seals the entrance to the reservoir. Can also be enterance to the vaccum chamber." [PSI:MS]
is_a: MS:1000007 ! inlet type */

#define MS_THERMOSPRAY_INLET_ID "MS:1000069"
#define MS_THERMOSPRAY_INLET_NAME "thermospray inlet"
/* def: "A method for generating gas phase ions from a solution of an analyte by rapid heating of the sample." [PSI:MS]
is_a: MS:1000007 ! inlet type */

#define MS_ATMOSPHERIC_PRESSURE_CHEMICAL_IONIZATION_ID "MS:1000070"
#define MS_ATMOSPHERIC_PRESSURE_CHEMICAL_IONIZATION_NAME "atmospheric pressure chemical ionization"
/* def: "Chemical ionization that takes place at atmospheric pressure as opposed to the reduced pressure is normally used for chemical ionization." [PSI:MS]
synonym: "APCI" EXACT []
is_a: MS:1000240 ! atmospheric pressure ionization */

#define MS_CHEMICAL_IONIZATION_ID "MS:1000071"
#define MS_CHEMICAL_IONIZATION_NAME "chemical ionization"
/* def: "The formation of a new ion by the reaction of a neutral species with an ion. The process may involve transfer of an electron, a proton or other charged species between the reactants. When a positive ion results from chemical ionization the term may be used without qualification. When a negative ion results the term negative ion chemical ionization should be used. Note that this term is not synonymous with chemi-ionization." [PSI:MS]
synonym: "CI" EXACT []
is_a: MS:1000008 ! ionization type */

// #define MS_ELECTRONIC_IONIZATION_ID "MS:1000072"
// #define MS_ELECTRONIC_IONIZATION_NAME "Electronic Ionization"
/* def: "OBSOLETE The ionization of an atom or molecule by electrons that are typically accelerated to energies between 50 and 150 eV. Usually 70 eV electrons are used to produce positive ions. The term 'electron impact' is not recommended." [PSI:MS]
comment: This term was made obsolete because it was replaced by electron ionization (MS:1000389).
synonym: "EI" EXACT []
is_obsolete: true */

#define MS_ELECTROSPRAY_IONIZATION_ID "MS:1000073"
#define MS_ELECTROSPRAY_IONIZATION_NAME "electrospray ionization"
/* def: "A process in which ionized species in the gas phase are produced from an analyte-containing solution via highly charged fine droplets, by means of spraying the solution from a narrow-bore needle tip at atmospheric pressure in the presence of a high electric field. When a pressurized gas is used to aid in the formation of a stable spray, the term pneumatically assisted electrospray ionization is used. The term ion spray is not recommended." [PSI:MS]
synonym: "ESI" EXACT []
is_a: MS:1000008 ! ionization type */

#define MS_FAST_ATOM_BOMBARDMENT_IONIZATION_ID "MS:1000074"
#define MS_FAST_ATOM_BOMBARDMENT_IONIZATION_NAME "fast atom bombardment ionization"
/* def: "The ionization of any species by the interaction of a focused beam of neutral atoms having a translational energy of several thousand eV with a sample that is typically dissolved in a solvent matrix. See also secondary ionization." [PSI:MS]
synonym: "FAB" EXACT []
is_a: MS:1000008 ! ionization type */

#define MS_MATRIX_ASSISTED_LASER_DESORPTION_IONIZATION_ID "MS:1000075"
#define MS_MATRIX_ASSISTED_LASER_DESORPTION_IONIZATION_NAME "matrix-assisted laser desorption ionization"
/* def: "The formation of gas-phase ions from molecules that are present in a solid or solvent matrix that is irradiated with a pulsed laser. See also laser desorption/ionization." [PSI:MS]
synonym: "MALDI" EXACT []
is_a: MS:1000247 ! desorption ionization */

// #define MS_NEGATIVE_ION_MODE_ID "MS:1000076"
// #define MS_NEGATIVE_ION_MODE_NAME "negative ion mode"
/* def: "OBSOLETE." [PSI:MS]
comment: This term was made obsolete because it was replaced by negative scan (MS:1000129).
is_obsolete: true */

// #define MS_POSITIVE_ION_MODE_ID "MS:1000077"
// #define MS_POSITIVE_ION_MODE_NAME "positive ion mode"
/* def: "OBSOLETE." [PSI:MS]
comment: This term was made obsolete because it was replaced by positive scan (MS:1000130).
is_obsolete: true */

#define MS_AXIAL_EJECTION_LINEAR_ION_TRAP_ID "MS:1000078"
#define MS_AXIAL_EJECTION_LINEAR_ION_TRAP_NAME "axial ejection linear ion trap"
/* def: "A linear ion trap mass spectrometer where ions are ejected along the axis of the analyzer." [PSI:MS]
is_a: MS:1000291 ! linear ion trap */

#define MS_FOURIER_TRANSFORM_ION_CYCLOTRON_RESONANCE_MASS_SPECTROMETER_ID "MS:1000079"
#define MS_FOURIER_TRANSFORM_ION_CYCLOTRON_RESONANCE_MASS_SPECTROMETER_NAME "fourier transform ion cyclotron resonance mass spectrometer"
/* def: "A mass spectrometer based on the principle of ion cyclotron resonance in which an ion in a magnetic field moves in a circular orbit at a frequency characteristic of its m/z value. Ions are coherently excited to a larger radius orbit using a pulse of radio frequency energy and their image charge is detected on receiver plates as a time domain signal. Fourier transformation of the time domain signal results in a frequency domain signal which is converted to a mass spectrum based in the inverse relationship between frequency and m/z." [PSI:MS]
synonym: "FT_ICR" EXACT []
is_a: MS:1000443 ! mass analyzer type */

#define MS_MAGNETIC_SECTOR_ID "MS:1000080"
#define MS_MAGNETIC_SECTOR_NAME "magnetic sector"
/* def: "A device that produces a magnetic field perpendicular to a charged particle beam that deflects the beam to an extent that is proportional to the particle momentum per unit charge. For a monoenergetic beam, the deflection is proportional to m/z." [PSI:MS]
is_a: MS:1000443 ! mass analyzer type */

#define MS_QUADRUPOLE_ID "MS:1000081"
#define MS_QUADRUPOLE_NAME "quadrupole"
/* def: "A mass spectrometer that consists of four parallel rods whose centers form the corners of a square and whose opposing poles are connected. The voltage applied to the rods is a superposition of a static potential and a sinusoidal radio frequency potential. The motion of an ion in the x and y dimensions is described by the Matthieu equation whose solutions show that ions in a particular m/z range can be transmitted along the z axis." [PSI:MS]
is_a: MS:1000443 ! mass analyzer type */

#define MS_QUADRUPOLE_ION_TRAP_ID "MS:1000082"
#define MS_QUADRUPOLE_ION_TRAP_NAME "quadrupole ion trap"
/* def: "Quadrupole Ion Trap mass analyzer captures the ions in a three dimensional ion trap and then selectively ejects them by varying the RF and DC potentials." [PSI:MS]
synonym: "Paul Ion trap" EXACT []
synonym: "QIT" EXACT []
synonym: "Quistor" EXACT []
is_a: MS:1000264 ! ion trap */

#define MS_RADIAL_EJECTION_LINEAR_ION_TRAP_ID "MS:1000083"
#define MS_RADIAL_EJECTION_LINEAR_ION_TRAP_NAME "radial ejection linear ion trap"
/* def: "A linear ion trap mass spectrometer where ions are ejected along the radius of the analyzer." [PSI:MS]
is_a: MS:1000291 ! linear ion trap */

#define MS_TIME_OF_FLIGHT_ID "MS:1000084"
#define MS_TIME_OF_FLIGHT_NAME "time-of-flight"
/* def: "Instrument that separates ions by m/z in a field-free region after acceleration to a fixed acceleration energy." [PSI:MS]
synonym: "TOF" EXACT []
is_a: MS:1000443 ! mass analyzer type */

#define MS_BASELINE_ID "MS:1000085"
#define MS_BASELINE_NAME "baseline"
/* def: "An attribute of resolution when recording the detector response in absence of the analyte." [PSI:MS]
is_a: MS:1000012 ! resolution measurement method */

#define MS_FULL_WIDTH_AT_HALF_MAXIMUM_ID "MS:1000086"
#define MS_FULL_WIDTH_AT_HALF_MAXIMUM_NAME "full width at half-maximum"
/* def: "A measure of resolution represented as width of the peak at half peak height." [PSI:MS]
synonym: "FWHM" EXACT []
is_a: MS:1000012 ! resolution measurement method */

#define MS_TEN_PERCENT_VALLEY_ID "MS:1000087"
#define MS_TEN_PERCENT_VALLEY_NAME "ten percent valley"
/* def: "An attribute of resolution when the ratio between adjacent signals is 10% of the signal height." [PSI:MS]
is_a: MS:1000012 ! resolution measurement method */

#define MS_CONSTANT_ID "MS:1000088"
#define MS_CONSTANT_NAME "constant"
/* def: "When resolution is constant with respect to m/z." [PSI:MS]
is_a: MS:1000013 ! resolution type */

#define MS_PROPORTIONAL_ID "MS:1000089"
#define MS_PROPORTIONAL_NAME "proportional"
/* def: "When resolution is proportional with respect to m/z." [PSI:MS]
is_a: MS:1000013 ! resolution type */

#define MS_MASS_SCAN_ID "MS:1000090"
#define MS_MASS_SCAN_NAME "mass scan"
/* def: "A variation of instrument where a selected mass is scanned." [PSI:MS]
is_a: MS:1000017 ! Scan Function */

#define MS_SELECTED_ION_DETECTION_ID "MS:1000091"
#define MS_SELECTED_ION_DETECTION_NAME "selected ion detection"
/* def: "Please see Single Ion Monitoring." [PSI:MS]
is_a: MS:1000017 ! Scan Function */

#define MS_DECREASING_M_Z_SCAN_ID "MS:1000092"
#define MS_DECREASING_M_Z_SCAN_NAME "decreasing m/z scan"
/* def: "High to low direction in terms of m/z of the scan for scanning analyzers." [PSI:MS]
is_a: MS:1000018 ! scan direction */

#define MS_INCREASING_M_Z_SCAN_ID "MS:1000093"
#define MS_INCREASING_M_Z_SCAN_NAME "increasing m/z scan"
/* def: "Low to high direction in terms of m/z of the scan for scanning analyzers." [PSI:MS]
is_a: MS:1000018 ! scan direction */

#define MS_EXPONENTIAL_ID "MS:1000094"
#define MS_EXPONENTIAL_NAME "exponential"
/* def: "The mass scan is done in exponential mode." [PSI:MS]
is_a: MS:1000019 ! scan law */

// #define MS_LINEAR_ID "MS:1000095"
// #define MS_LINEAR_NAME "linear"
/* def: "OBSOLETE The mass scan is done in linear mode." [PSI:MS]
comment: This term was made obsolete because it was redundant with the Pato Ontology term linear (UO:0001199).
is_a: MS:1000019 ! scan law
is_obsolete: true */

#define MS_QUADRATIC_ID "MS:1000096"
#define MS_QUADRATIC_NAME "quadratic"
/* def: "The mass scan is done in quadratic mode." [PSI:MS]
is_a: MS:1000019 ! scan law */

// #define MS_CONSTANT_NEUTRAL_MASS_LOSS_ID "MS:1000097"
// #define MS_CONSTANT_NEUTRAL_MASS_LOSS_NAME "constant neutral mass loss"
/* def: "OBSOLETE A spectrum formed of all product ions that have been produced with a selected m/z decrement from any precursor ions. The spectrum shown correlates to the precursor ion spectrum. See also neutral loss spectrum." [PSI:MS]
comment: This former purgatory term was made obsolete.
is_obsolete: true */

// #define MS_MULTIPLE_ION_MONITORING_ID "MS:1000098"
// #define MS_MULTIPLE_ION_MONITORING_NAME "multiple ion monitoring"
/* def: "OBSOLETE Data acquired when monitoring the ion current of a few specific m/z values. Remap to MS:1000205 -Selected Ion Monitoring." [PSI:MS]
comment: This former purgatory term was made obsolete.
is_obsolete: true */

// #define MS_MULTIPLE_REACTION_MONITORING_ID "MS:1000099"
// #define MS_MULTIPLE_REACTION_MONITORING_NAME "multiple reaction monitoring"
/* def: "OBSOLETE This term is not recommended. See Selected Reaction Monitoring." [PSI:MS]
comment: This term was made obsolete because it was merged with selected reaction monitoring (MS:1000206).
synonym: "MRM" EXACT []
is_obsolete: true */

// #define MS_PRECURSOR_ION_SCAN_ID "MS:1000100"
// #define MS_PRECURSOR_ION_SCAN_NAME "precursor ion scan"
/* def: "OBSOLETE The specific scan function or process that will record a precursor ion spectrum." [PSI:MS]
comment: This former purgatory term was made obsolete.
is_obsolete: true */

// #define MS_PRODUCT_ION_SCAN_ID "MS:1000101"
// #define MS_PRODUCT_ION_SCAN_NAME "product ion scan"
/* def: "OBSOLETE The specific scan function or process that records product ion spectrum." [PSI:MS]
comment: This former purgatory term was made obsolete.
is_obsolete: true */

// #define MS_SINGLE_ION_MONITORING_ID "MS:1000102"
// #define MS_SINGLE_ION_MONITORING_NAME "single ion monitoring"
/* def: "OBSOLETE The operation of a mass spectrometer to monitor a single ion rather than scanning entire mass spectrum." [PSI:MS]
comment: This term was made obsolete because it was replaced by single ion monitoring (MS:100205).
is_obsolete: true */

// #define MS_SINGLE_REACTION_MONITORING_ID "MS:1000103"
// #define MS_SINGLE_REACTION_MONITORING_NAME "single reaction monitoring"
/* def: "OBSOLETE This term is not recommended. See Selected Reaction Monitoring." [PSI:MS]
comment: This term was made obsolete because it was replaced by selected reaction monitoring (MS:1000206).
is_obsolete: true */

// #define MS_NONE__ID "MS:1000104"
// #define MS_NONE__NAME "None ??"
/* def: "OBSOLETE None." [PSI:MS]
comment: This term was made obsolete because .
is_obsolete: true */

#define MS_REFLECTRON_OFF_ID "MS:1000105"
#define MS_REFLECTRON_OFF_NAME "reflectron off"
/* def: "Reflectron is off." [PSI:MS]
is_a: MS:1000021 ! reflectron state */

#define MS_REFLECTRON_ON_ID "MS:1000106"
#define MS_REFLECTRON_ON_NAME "reflectron on"
/* def: "Reflectron is on." [PSI:MS]
is_a: MS:1000021 ! reflectron state */

#define MS_CHANNELTRON_ID "MS:1000107"
#define MS_CHANNELTRON_NAME "channeltron"
/* def: "A horn-shaped (or cone-shaped) continuous dynode particle multiplier. The ion strikes the inner surface of the device and induces the production of secondary electrons that in turn impinge on the inner surfaces to produce more secondary electrons. This avalanche effect produces an increase in signal in the final measured current pulse." [PSI:MS]
synonym: "Channeltron Detector" EXACT []
is_a: MS:1000026 ! detector type */

#define MS_CONVERSION_DYNODE_ELECTRON_MULTIPLIER_ID "MS:1000108"
#define MS_CONVERSION_DYNODE_ELECTRON_MULTIPLIER_NAME "conversion dynode electron multiplier"
/* def: "A surface that is held at high potential so that ions striking the surface produce electrons that are subsequently detected." [PSI:MS]
is_a: MS:1000346 ! conversion dynode */

#define MS_CONVERSION_DYNODE_PHOTOMULTIPLIER_ID "MS:1000109"
#define MS_CONVERSION_DYNODE_PHOTOMULTIPLIER_NAME "conversion dynode photomultiplier"
/* def: "A detector in which ions strike a conversion dynode to produce electrons that in turn generate photons through a phosphorescent screen that are detected by a photomultiplier." [PSI:MS]
synonym: "ion-to-photon detector" RELATED []
is_a: MS:1000346 ! conversion dynode */

#define MS_DALY_DETECTOR_ID "MS:1000110"
#define MS_DALY_DETECTOR_NAME "daly detector"
/* def: "Detector consisting of a conversion dynode, scintillator and photomultiplier. The metal knob at high potential emits secondary electrons when ions impinge on the surface. The secondary electrons are accelerated onto the scintillator that produces light that is then detected by the photomultiplier detector." [PSI:MS]
synonym: "Daly" EXACT []
is_a: MS:1000026 ! detector type */

#define MS_ELECTRON_MULTIPLIER_TUBE_ID "MS:1000111"
#define MS_ELECTRON_MULTIPLIER_TUBE_NAME "electron multiplier tube"
/* def: "A device to amplify the current of a beam or packet of charged particles or photons by incidence upon the surface of an electrode to produce secondary electrons." [PSI:MS]
synonym: "EMT" EXACT []
is_a: MS:1000253 ! electron multiplier */

#define MS_FARADAY_CUP_ID "MS:1000112"
#define MS_FARADAY_CUP_NAME "faraday cup"
/* def: "A conducting cup or chamber that intercepts a charged particle beam and is electrically connected to a current measuring device." [PSI:MS]
is_a: MS:1000026 ! detector type */

#define MS_FOCAL_PLANE_ARRAY_ID "MS:1000113"
#define MS_FOCAL_PLANE_ARRAY_NAME "focal plane array"
/* def: "An array of detectors for spatially disperse ion beams in which all ions simultaneously impinge on the detector plane." [PSI:MS]
is_a: MS:1000348 ! focal plane collector */

#define MS_MICROCHANNEL_PLATE_DETECTOR_ID "MS:1000114"
#define MS_MICROCHANNEL_PLATE_DETECTOR_NAME "microchannel plate detector"
/* def: "A thin plate that contains a closely spaced array of channels that each act as a continuous dynode particle multiplier. A charged particle, fast neutral particle, or photon striking the plate causes a cascade of secondary electrons that ultimately exits the opposite side of the plate." [PSI:MS]
synonym: "multichannel plate" EXACT []
is_a: MS:1000345 ! array detector */

#define MS_MULTI_COLLECTOR_ID "MS:1000115"
#define MS_MULTI_COLLECTOR_NAME "multi-collector"
/* def: "A detector system commonly used in inductively coupled plasma mass spectrometers." [PSI:MS]
is_a: MS:1000026 ! detector type */

#define MS_PHOTOMULTIPLIER_ID "MS:1000116"
#define MS_PHOTOMULTIPLIER_NAME "photomultiplier"
/* def: "A detector for conversion of the ion/electron signal into photon(s) which are then amplified and detected." [PSI:MS]
synonym: "PMT" EXACT []
is_a: MS:1000026 ! detector type */

#define MS_ANALOG_DIGITAL_CONVERTER_ID "MS:1000117"
#define MS_ANALOG_DIGITAL_CONVERTER_NAME "analog-digital converter"
/* def: "Analog-to-digital converter (abbreviated ADC, A/D or A to D) is an electronic integrated circuit (i/c) that converts continuous signals to discrete digital numbers." [PSI:MS]
synonym: "ADC" EXACT []
is_a: MS:1000027 ! detector acquisition mode */

#define MS_PULSE_COUNTING_ID "MS:1000118"
#define MS_PULSE_COUNTING_NAME "pulse counting"
/* def: "definition to do." [PSI:MS]
is_a: MS:1000027 ! detector acquisition mode */

#define MS_TIME_DIGITAL_CONVERTER_ID "MS:1000119"
#define MS_TIME_DIGITAL_CONVERTER_NAME "time-digital converter"
/* def: "A device for converting a signal of sporadic pluses into a digital representation of their time indices." [PSI:MS]
synonym: "TDC" EXACT []
is_a: MS:1000027 ! detector acquisition mode */

#define MS_TRANSIENT_RECORDER_ID "MS:1000120"
#define MS_TRANSIENT_RECORDER_NAME "transient recorder"
/* def: "A detector acquisition mode used for detecting transient signals." [PSI:MS]
is_a: MS:1000027 ! detector acquisition mode */

#define MS_AB_SCIEX_INSTRUMENT_MODEL_ID "MS:1000121"
#define MS_AB_SCIEX_INSTRUMENT_MODEL_NAME "AB SCIEX instrument model"
/* def: "The brand of instruments from the joint venture between Applied Biosystems and MDS Analytical Technologies (formerly MDS SCIEX). Previously branded as \"Applied Biosystems|MDS SCIEX\"." [PSI:MS]
synonym: "Applied Biosystems|MDS SCIEX" RELATED []
is_a: MS:1000031 ! instrument model */

#define MS_BRUKER_DALTONICS_INSTRUMENT_MODEL_ID "MS:1000122"
#define MS_BRUKER_DALTONICS_INSTRUMENT_MODEL_NAME "Bruker Daltonics instrument model"
/* def: "Bruker Daltonics' instrument model." [PSI:MS]
is_a: MS:1000031 ! instrument model */

#define MS_IONSPEC_INSTRUMENT_MODEL_ID "MS:1000123"
#define MS_IONSPEC_INSTRUMENT_MODEL_NAME "IonSpec instrument model"
/* def: "IonSpec corporation instrument model." [PSI:MS]
is_a: MS:1000489 ! Varian instrument model */

#define MS_SHIMADZU_INSTRUMENT_MODEL_ID "MS:1000124"
#define MS_SHIMADZU_INSTRUMENT_MODEL_NAME "Shimadzu instrument model"
/* def: "Shimadzu corporation instrument model." [PSI:MS]
is_a: MS:1000031 ! instrument model */

#define MS_THERMO_FINNIGAN_INSTRUMENT_MODEL_ID "MS:1000125"
#define MS_THERMO_FINNIGAN_INSTRUMENT_MODEL_NAME "Thermo Finnigan instrument model"
/* def: "ThermoFinnigan from Thermo Electron Corporation instrument model." [PSI:MS]
is_a: MS:1000483 ! Thermo Fisher Scientific instrument model */

#define MS_WATERS_INSTRUMENT_MODEL_ID "MS:1000126"
#define MS_WATERS_INSTRUMENT_MODEL_NAME "Waters instrument model"
/* def: "Waters Corporation instrument model." [PSI:MS]
is_a: MS:1000031 ! instrument model */

#define MS_CENTROID_SPECTRUM_ID "MS:1000127"
#define MS_CENTROID_SPECTRUM_NAME "centroid spectrum"
/* def: "Processing of profile data to produce spectra that contains discrete peaks of zero width. Often used to reduce the size of dataset." [PSI:MS]
synonym: "Discrete Mass Spectrum" EXACT []
is_a: MS:1000525 ! spectrum representation */

#define MS_PROFILE_SPECTRUM_ID "MS:1000128"
#define MS_PROFILE_SPECTRUM_NAME "profile spectrum"
/* def: "A profile mass spectrum is created when data is recorded with ion current (counts per second) on one axis and mass/charge ratio on another axis." [PSI:MS]
synonym: "continuous mass spectrum" EXACT []
synonym: "Continuum Mass Spectrum" EXACT []
is_a: MS:1000525 ! spectrum representation */

#define MS_NEGATIVE_SCAN_ID "MS:1000129"
#define MS_NEGATIVE_SCAN_NAME "negative scan"
/* def: "Polarity of the scan is negative." [PSI:MS]
is_a: PATO:0002186 ! polarity
is_a: MS:1000465 ! scan polarity */

#define MS_POSITIVE_SCAN_ID "MS:1000130"
#define MS_POSITIVE_SCAN_NAME "positive scan"
/* def: "Polarity of the scan is positive." [PSI:MS]
is_a: PATO:0002186 ! polarity
is_a: MS:1000465 ! scan polarity */

#define MS_NUMBER_OF_DETECTOR_COUNTS_ID "MS:1000131"
#define MS_NUMBER_OF_DETECTOR_COUNTS_NAME "number of detector counts"
/* def: "The number of counted events observed in one or a group of elements of a detector." [PSI:MS]
is_a: MS:1000043 ! intensity unit */

#define MS_PERCENT_OF_BASE_PEAK_ID "MS:1000132"
#define MS_PERCENT_OF_BASE_PEAK_NAME "percent of base peak"
/* def: "The magnitude of a peak or measurement element expressed in terms of the percentage of the magnitude of the base peak intensity." [PSI:MS]
xref: value-type:xsd\:float "The allowed value-type for this CV term."
is_a: MS:1000043 ! intensity unit */

#define MS_COLLISION_INDUCED_DISSOCIATION_ID "MS:1000133"
#define MS_COLLISION_INDUCED_DISSOCIATION_NAME "collision-induced dissociation"
/* def: "The dissociation of an ion after collisional excitation. The term collisional-activated dissociation is not recommended." [PSI:MS]
synonym: "CID" EXACT []
synonym: "CAD" EXACT []
synonym: "collision activated dissociation" EXACT []
is_a: MS:1000044 ! dissociation method */

#define MS_PLASMA_DESORPTION_ID "MS:1000134"
#define MS_PLASMA_DESORPTION_NAME "plasma desorption"
/* def: "The ionization of material in a solid sample by bombarding it with ionic or neutral atoms formed as a result of the fission of a suitable nuclide, typically 252Cf. Synonymous with fission fragment ionization." [PSI:MS]
synonym: "PD" EXACT []
is_a: MS:1000044 ! dissociation method */

#define MS_POST_SOURCE_DECAY_ID "MS:1000135"
#define MS_POST_SOURCE_DECAY_NAME "post-source decay"
/* def: "A technique specific to reflectron time-of-flight mass spectrometers where product ions of metastable transitions or collision-induced dissociations generated in the drift tube prior to entering the reflectron are m/z separated to yield product ion spectra." [PSI:MS]
synonym: "PSD" EXACT []
is_a: MS:1000044 ! dissociation method */

#define MS_SURFACE_INDUCED_DISSOCIATION_ID "MS:1000136"
#define MS_SURFACE_INDUCED_DISSOCIATION_NAME "surface-induced dissociation"
/* def: "Fragmentation that results from the collision of an ion with a surface." [PSI:MS]
synonym: "SID" EXACT []
is_a: MS:1000044 ! dissociation method */

// #define MS_ELECTRON_VOLT_ID "MS:1000137"
// #define MS_ELECTRON_VOLT_NAME "electron volt"
/* def: "OBSOLETE A non-SI unit of energy (eV) defined as the energy acquired by a particle containing one unit of charge through a potential difference of one volt. An electron-volt is equal to 1.602 176 53(14) x 10^-19 J." [PSI:MS]
comment: This term was made obsolete because it was redundant with the Unit Ontology term electron volt (UO:0000266).
synonym: "eV" EXACT []
is_obsolete: true */

#define MS_NORMALIZED_COLLISION_ENERGY_ID "MS:1000138"
#define MS_NORMALIZED_COLLISION_ENERGY_NAME "normalized collision energy"
/* def: "Instrument setting, expressed in percent, for adjusting collisional energies of ions in an effort to provide equivalent excitation of all ions." [PSI:PI]
xref: value-type:xsd\:float "The allowed value-type for this CV term."
is_a: MS:1000510 ! precursor activation attribute
relationship: has_units UO:0000187 ! percent */

#define MS__4000_QTRAP_ID "MS:1000139"
#define MS__4000_QTRAP_NAME "4000 QTRAP"
/* def: "Applied Biosystems/MDS SCIEX Q 4000 TRAP MS." [PSI:MS]
is_a: MS:1000121 ! AB SCIEX instrument model */

#define MS__4700_PROTEOMICS_ANALYZER_ID "MS:1000140"
#define MS__4700_PROTEOMICS_ANALYZER_NAME "4700 Proteomics Analyzer"
/* def: "Applied Biosystems/MDS SCIEX 4700 Proteomics Analyzer MS." [PSI:MS]
is_a: MS:1000495 ! Applied Biosystems instrument model */

#define MS_APEX_IV_ID "MS:1000141"
#define MS_APEX_IV_NAME "apex IV"
/* def: "Bruker Daltonics' apex IV: ESI, MALDI, Nanospray, APCI, APPI, Qh-FT_ICR." [PSI:MS]
is_a: MS:1001556 ! Bruker Daltonics apex series */

#define MS_APEX_Q_ID "MS:1000142"
#define MS_APEX_Q_NAME "apex Q"
/* def: "Bruker Daltonics' apex Q: ESI, MALDI, Nanospray, APCI, APPI, Qh-FT_ICR." [PSI:MS]
is_a: MS:1001556 ! Bruker Daltonics apex series */

#define MS_API_150EX_ID "MS:1000143"
#define MS_API_150EX_NAME "API 150EX"
/* def: "Applied Biosystems/MDS SCIEX API 150EX MS." [PSI:MS]
is_a: MS:1000121 ! AB SCIEX instrument model */

#define MS_API_150EX_PREP_ID "MS:1000144"
#define MS_API_150EX_PREP_NAME "API 150EX Prep"
/* def: "Applied Biosystems/MDS SCIEX API 150EX Prep MS." [PSI:MS]
is_a: MS:1000121 ! AB SCIEX instrument model */

#define MS_API_2000_ID "MS:1000145"
#define MS_API_2000_NAME "API 2000"
/* def: "Applied Biosystems/MDS SCIEX API 2000 MS." [PSI:MS]
is_a: MS:1000121 ! AB SCIEX instrument model */

#define MS_API_3000_ID "MS:1000146"
#define MS_API_3000_NAME "API 3000"
/* def: "Applied Biosystems/MDS SCIEX API 3000 MS." [PSI:MS]
is_a: MS:1000121 ! AB SCIEX instrument model */

#define MS_API_4000_ID "MS:1000147"
#define MS_API_4000_NAME "API 4000"
/* def: "Applied Biosystems/MDS SCIEX API 4000 MS." [PSI:MS]
is_a: MS:1000121 ! AB SCIEX instrument model */

#define MS_AUTOFLEX_II_ID "MS:1000148"
#define MS_AUTOFLEX_II_NAME "autoflex II"
/* def: "Bruker Daltonics' autoflex II: MALDI TOF." [PSI:MS]
is_a: MS:1001534 ! Bruker Daltonics flex series */

#define MS_AUTOFLEX_TOF_TOF_ID "MS:1000149"
#define MS_AUTOFLEX_TOF_TOF_NAME "autoflex TOF_TOF"
/* def: "Bruker Daltonics' autoflex TOF/TOF MS: MALDI TOF." [PSI:MS]
is_a: MS:1001534 ! Bruker Daltonics flex series */

#define MS_AUTO_SPEC_ULTIMA_NT_ID "MS:1000150"
#define MS_AUTO_SPEC_ULTIMA_NT_NAME "Auto Spec Ultima NT"
/* def: "Waters AutoSpec Ultima NT MS." [PSI:MS]
is_a: MS:1000126 ! Waters instrument model */

#define MS_BIOTOF_II_ID "MS:1000151"
#define MS_BIOTOF_II_NAME "BioTOF II"
/* def: "Bruker Daltonics' BioTOF II: ESI TOF." [PSI:MS]
is_a: MS:1001535 ! Bruker Daltonics BioTOF series */

#define MS_BIOTOF_Q_ID "MS:1000152"
#define MS_BIOTOF_Q_NAME "BioTOF-Q"
/* def: "Bruker Daltonics' BioTOF-Q: ESI Q-TOF." [PSI:MS]
is_a: MS:1001535 ! Bruker Daltonics BioTOF series */

#define MS_DELTA_PLUSADVANTAGE_ID "MS:1000153"
#define MS_DELTA_PLUSADVANTAGE_NAME "DELTA plusAdvantage"
/* def: "ThermoFinnigan DELTA plusAdvantage MS." [PSI:MS]
is_a: MS:1000125 ! Thermo Finnigan instrument model */

#define MS_DELTAPLUSXP_ID "MS:1000154"
#define MS_DELTAPLUSXP_NAME "DELTAplusXP"
/* def: "ThermoFinnigan DELTAplusXP MS." [PSI:MS]
is_a: MS:1000125 ! Thermo Finnigan instrument model */

// #define MS_ELEMENT2_ID "MS:1000155"
// #define MS_ELEMENT2_NAME "ELEMENT2"
/* def: "OBSOLETE ThermoFinnigan ELEMENT2 MS." [PSI:MS]
comment: This former purgatory term was made obsolete.
is_obsolete: true */

#define MS_ESQUIRE_4000_ID "MS:1000156"
#define MS_ESQUIRE_4000_NAME "esquire 4000"
/* def: "Bruker Daltonics' esquire 4000: linear ion trap, ESI, MALDI, Nanospray, APCI, APPI." [PSI:MS]
is_a: MS:1001533 ! Bruker Daltonics esquire series */

#define MS_ESQUIRE_6000_ID "MS:1000157"
#define MS_ESQUIRE_6000_NAME "esquire 6000"
/* def: "Bruker Daltonics' esquire 6000: linear ion trap, ESI, MALDI, Nanospray, APCI, APPI." [PSI:MS]
is_a: MS:1001533 ! Bruker Daltonics esquire series */

#define MS_EXPLORER_ID "MS:1000158"
#define MS_EXPLORER_NAME "explorer"
/* def: "IonSpec Explorer MS." [PSI:MS]
is_a: MS:1000123 ! IonSpec instrument model */

#define MS_GCT_ID "MS:1000159"
#define MS_GCT_NAME "GCT"
/* def: "Waters oa-ToF based GCT." [PSI:MS]
is_a: MS:1000126 ! Waters instrument model */

#define MS_HCT_ID "MS:1000160"
#define MS_HCT_NAME "HCT"
/* def: "Bruker Daltonics' HCT: ESI Q-TOF, Nanospray, APCI, APPI." [PSI:MS]
is_a: MS:1000697 ! Bruker Daltonics HCT Series */

#define MS_HCTPLUS_ID "MS:1000161"
#define MS_HCTPLUS_NAME "HCTplus"
/* def: "Bruker Daltonics' HCTplus: ESI Q-TOF, Nanospray, APCI, APPI." [PSI:MS]
is_a: MS:1000697 ! Bruker Daltonics HCT Series */

#define MS_HIRES_ESI_ID "MS:1000162"
#define MS_HIRES_ESI_NAME "HiRes ESI"
/* def: "IonSpec HiResESI MS." [PSI:MS]
is_a: MS:1000123 ! IonSpec instrument model */

#define MS_HIRES_MALDI_ID "MS:1000163"
#define MS_HIRES_MALDI_NAME "HiRes MALDI"
/* def: "IonSpec HiResMALDI MS." [PSI:MS]
is_a: MS:1000123 ! IonSpec instrument model */

#define MS_ISOPRIME_ID "MS:1000164"
#define MS_ISOPRIME_NAME "IsoPrime"
/* def: "Waters IsoPrime MS." [PSI:MS]
is_a: MS:1000126 ! Waters instrument model */

#define MS_ISOPROBE_ID "MS:1000165"
#define MS_ISOPROBE_NAME "IsoProbe"
/* def: "Waters IsoProbe MS." [PSI:MS]
is_a: MS:1000126 ! Waters instrument model */

#define MS_ISOPROBE_T_ID "MS:1000166"
#define MS_ISOPROBE_T_NAME "IsoProbe T"
/* def: "Waters IsoProbe T MS." [PSI:MS]
is_a: MS:1000126 ! Waters instrument model */

#define MS_LCQ_ADVANTAGE_ID "MS:1000167"
#define MS_LCQ_ADVANTAGE_NAME "LCQ Advantage"
/* def: "ThermoFinnigan LCQ Advantage MS." [PSI:MS]
is_a: MS:1000125 ! Thermo Finnigan instrument model */

#define MS_LCQ_CLASSIC_ID "MS:1000168"
#define MS_LCQ_CLASSIC_NAME "LCQ Classic"
/* def: "ThermoFinnigan LCQ Classic MS." [PSI:MS]
is_a: MS:1000125 ! Thermo Finnigan instrument model */

#define MS_LCQ_DECA_XP_PLUS_ID "MS:1000169"
#define MS_LCQ_DECA_XP_PLUS_NAME "LCQ Deca XP Plus"
/* def: "ThermoFinnigan LCQ Deca XP Plus MS." [PSI:MS]
is_a: MS:1000125 ! Thermo Finnigan instrument model */

#define MS_MALDI_L_ID "MS:1000170"
#define MS_MALDI_L_NAME "M@LDI L"
/* def: "Waters oa-ToF based MALDI L." [PSI:MS]
is_a: MS:1000126 ! Waters instrument model */

#define MS_MALDI_LR_ID "MS:1000171"
#define MS_MALDI_LR_NAME "M@LDI LR"
/* def: "Waters oa-ToF based MALDI LR." [PSI:MS]
is_a: MS:1000126 ! Waters instrument model */

#define MS_MAT253_ID "MS:1000172"
#define MS_MAT253_NAME "MAT253"
/* def: "ThermoFinnigan MAT253 MS." [PSI:MS]
is_a: MS:1000493 ! Finnigan MAT instrument model */

#define MS_MAT900XP_ID "MS:1000173"
#define MS_MAT900XP_NAME "MAT900XP"
/* def: "ThermoFinnigan MAT900XP MS." [PSI:MS]
is_a: MS:1000493 ! Finnigan MAT instrument model */

#define MS_MAT900XP_TRAP_ID "MS:1000174"
#define MS_MAT900XP_TRAP_NAME "MAT900XP Trap"
/* def: "ThermoFinnigan MAT900XP Trap MS." [PSI:MS]
is_a: MS:1000493 ! Finnigan MAT instrument model */

#define MS_MAT95XP_ID "MS:1000175"
#define MS_MAT95XP_NAME "MAT95XP"
/* def: "ThermoFinnigan MAT95XP MS." [PSI:MS]
is_a: MS:1000493 ! Finnigan MAT instrument model */

#define MS_MAT95XP_TRAP_ID "MS:1000176"
#define MS_MAT95XP_TRAP_NAME "MAT95XP Trap"
/* def: "ThermoFinnigan MAT95XP Trap MS." [PSI:MS]
is_a: MS:1000493 ! Finnigan MAT instrument model */

#define MS_MICROFLEX_ID "MS:1000177"
#define MS_MICROFLEX_NAME "microflex"
/* def: "Bruker Daltonics' microflex: MALDI TOF." [PSI:MS]
is_a: MS:1001534 ! Bruker Daltonics flex series */

#define MS_MICROTOF_LC_ID "MS:1000178"
#define MS_MICROTOF_LC_NAME "microTOF LC"
/* def: "Bruker Daltonics' microTOF LC: ESI TOF, Nanospray, APCI, APPI." [PSI:MS]
is_a: MS:1001536 ! Bruker Daltonics micrOTOF series */

#define MS_NEPTUNE_ID "MS:1000179"
#define MS_NEPTUNE_NAME "neptune"
/* def: "ThermoFinnigan NEPTUNE MS." [PSI:MS]
is_a: MS:1000125 ! Thermo Finnigan instrument model */

#define MS_NG_5400_ID "MS:1000180"
#define MS_NG_5400_NAME "NG-5400"
/* def: "Waters NG-5400 MS." [PSI:MS]
is_a: MS:1000126 ! Waters instrument model */

#define MS_OMEGA_ID "MS:1000181"
#define MS_OMEGA_NAME "OMEGA"
/* def: "IonSpec OMEGA MS." [PSI:MS]
is_a: MS:1000123 ! IonSpec instrument model */

#define MS_OMEGA_2001_ID "MS:1000182"
#define MS_OMEGA_2001_NAME "OMEGA-2001"
/* def: "IonSpec OMEGA-2001 MS." [PSI:MS]
is_a: MS:1000123 ! IonSpec instrument model */

#define MS_OMNIFLEX_ID "MS:1000183"
#define MS_OMNIFLEX_NAME "OmniFlex"
/* def: "Bruker Daltonics' OmniFlex: MALDI TOF." [PSI:MS]
is_a: MS:1001534 ! Bruker Daltonics flex series */

#define MS_PLATFORM_ICP_ID "MS:1000184"
#define MS_PLATFORM_ICP_NAME "Platform ICP"
/* def: "Waters Platform ICP MS." [PSI:MS]
is_a: MS:1000126 ! Waters instrument model */

#define MS_POLARISQ_ID "MS:1000185"
#define MS_POLARISQ_NAME "PolarisQ"
/* def: "ThermoFinnigan PolarisQ MS." [PSI:MS]
is_a: MS:1000125 ! Thermo Finnigan instrument model */

#define MS_PROTEOMICS_SOLUTION_1_ID "MS:1000186"
#define MS_PROTEOMICS_SOLUTION_1_NAME "proteomics solution 1"
/* def: "Applied Biosystems/MDS SCIEX Proteomics Solution 1 MS." [PSI:MS]
is_a: MS:1000121 ! AB SCIEX instrument model */

#define MS_Q_TRAP_ID "MS:1000187"
#define MS_Q_TRAP_NAME "Q TRAP"
/* def: "Applied Biosystems/MDS SCIEX Q TRAP MS." [PSI:MS]
is_a: MS:1000121 ! AB SCIEX instrument model */

#define MS_Q_TOF_MICRO_ID "MS:1000188"
#define MS_Q_TOF_MICRO_NAME "Q-Tof micro"
/* def: "Waters oa-ToF based Q-Tof micro." [PSI:MS]
is_a: MS:1000126 ! Waters instrument model */

#define MS_Q_TOF_ULTIMA_ID "MS:1000189"
#define MS_Q_TOF_ULTIMA_NAME "Q-Tof ultima"
/* def: "Waters oa-ToF based Q-Tof Ultima." [PSI:MS]
is_a: MS:1000126 ! Waters instrument model */

#define MS_QSTAR_ID "MS:1000190"
#define MS_QSTAR_NAME "QSTAR"
/* def: "Applied Biosystems/MDS SCIEX QSTAR MS." [PSI:MS]
is_a: MS:1000121 ! AB SCIEX instrument model */

#define MS_QUATTRO_MICRO_ID "MS:1000191"
#define MS_QUATTRO_MICRO_NAME "quattro micro"
/* def: "Waters oa-ToF based micro." [PSI:MS]
is_a: MS:1000126 ! Waters instrument model */

#define MS_QUATTRO_UITIMA_ID "MS:1000192"
#define MS_QUATTRO_UITIMA_NAME "Quattro UItima"
/* def: "Waters oa-ToF based Ultima." [PSI:MS]
is_a: MS:1000126 ! Waters instrument model */

#define MS_SURVEYOR_MSQ_ID "MS:1000193"
#define MS_SURVEYOR_MSQ_NAME "Surveyor MSQ"
/* def: "ThermoFinnigan Surveyor MSQ MS." [PSI:MS]
is_a: MS:1000125 ! Thermo Finnigan instrument model */

#define MS_SYMBIOT_I_ID "MS:1000194"
#define MS_SYMBIOT_I_NAME "SymBiot I"
/* def: "Applied Biosystems/MDS SCIEX SymBiot I MS." [PSI:MS]
is_a: MS:1000121 ! AB SCIEX instrument model */

#define MS_SYMBIOT_XVI_ID "MS:1000195"
#define MS_SYMBIOT_XVI_NAME "SymBiot XVI"
/* def: "Applied Biosystems/MDS SCIEX SymBiot XVI MS." [PSI:MS]
is_a: MS:1000121 ! AB SCIEX instrument model */

#define MS_TEMPUS_TOF_ID "MS:1000196"
#define MS_TEMPUS_TOF_NAME "TEMPUS TOF"
/* def: "ThermoFinnigan TEMPUS TOF MS." [PSI:MS]
is_a: MS:1000125 ! Thermo Finnigan instrument model */

#define MS_TRACE_DSQ_ID "MS:1000197"
#define MS_TRACE_DSQ_NAME "TRACE DSQ"
/* def: "ThermoFinnigan TRACE DSQ MS." [PSI:MS]
is_a: MS:1000125 ! Thermo Finnigan instrument model */

#define MS_TRITON_ID "MS:1000198"
#define MS_TRITON_NAME "TRITON"
/* def: "ThermoFinnigan TRITON MS." [PSI:MS]
is_a: MS:1000125 ! Thermo Finnigan instrument model */

#define MS_TSQ_QUANTUM_ID "MS:1000199"
#define MS_TSQ_QUANTUM_NAME "TSQ Quantum"
/* def: "ThermoFinnigan TSQ Quantum MS." [PSI:MS]
is_a: MS:1000125 ! Thermo Finnigan instrument model */

#define MS_ULTIMA_ID "MS:1000200"
#define MS_ULTIMA_NAME "ultima"
/* def: "IonSpec Ultima MS." [PSI:MS]
is_a: MS:1000123 ! IonSpec instrument model */

#define MS_ULTRAFLEX_ID "MS:1000201"
#define MS_ULTRAFLEX_NAME "ultraflex"
/* def: "Bruker Daltonics' ultraflex: MALDI TOF." [PSI:MS]
is_a: MS:1001534 ! Bruker Daltonics flex series */

#define MS_ULTRAFLEX_TOF_TOF_ID "MS:1000202"
#define MS_ULTRAFLEX_TOF_TOF_NAME "ultraflex TOF_TOF"
/* def: "Bruker Daltonics' ultraflex TOF/TOF: MALDI TOF." [PSI:MS]
is_a: MS:1001534 ! Bruker Daltonics flex series */

#define MS_VOYAGER_DE_PRO_ID "MS:1000203"
#define MS_VOYAGER_DE_PRO_NAME "Voyager-DE PRO"
/* def: "Applied Biosystems/MDS SCIEX Voyager-DE PRO MS." [PSI:MS]
is_a: MS:1000495 ! Applied Biosystems instrument model */

#define MS_VOYAGER_DE_STR_ID "MS:1000204"
#define MS_VOYAGER_DE_STR_NAME "Voyager-DE STR"
/* def: "Applied Biosystems/MDS SCIEX Voyager-DE STR MS." [PSI:MS]
is_a: MS:1000495 ! Applied Biosystems instrument model */

#define MS_SELECTED_ION_MONITORING_ID "MS:1000205"
#define MS_SELECTED_ION_MONITORING_NAME "selected ion monitoring"
/* def: "The operation of a mass spectrometer in which the intensities of several specific m/z values are recorded rather than the entire mass spectrum." [PSI:MS]
synonym: "MIM" RELATED []
synonym: "Multiple Ion Monitoring" EXACT []
synonym: "SIM" EXACT []
is_a: MS:1000596 ! measurement method */

#define MS_SELECTED_REACTION_MONITORING_ID "MS:1000206"
#define MS_SELECTED_REACTION_MONITORING_NAME "selected reaction monitoring"
/* def: "Data acquired from specific product ions corresponding to m/z selected precursor ions recorded via multiple stages of mass spectrometry. Selected reaction monitoring can be performed in time or in space." [PSI:MS]
synonym: "MRM" RELATED []
synonym: "Multiple Reaction Monitoring" RELATED []
synonym: "SRM" EXACT []
is_a: MS:1000596 ! measurement method */

#define MS_ACCURATE_MASS_ID "MS:1000207"
#define MS_ACCURATE_MASS_NAME "accurate mass"
/* def: "An experimentally determined mass that is can be to determine a unique elemental formula. For ions less than 200 u, a measurement with 5 ppm accuracy is sufficient to determine the elemental composition." [PSI:MS]
xref: value-type:xsd\:float "The allowed value-type for this CV term."
is_a: MS:1000507 ! ion attribute
relationship: has_units UO:0000002 ! mass unit */

#define MS_AVERAGE_MASS_ID "MS:1000208"
#define MS_AVERAGE_MASS_NAME "average mass"
/* def: "The mass of an ion or molecule calculated using the average mass of each element weighted for its natural isotopic abundance." [PSI:MS]
xref: value-type:xsd\:float "The allowed value-type for this CV term."
is_a: MS:1000507 ! ion attribute
relationship: has_units UO:0000002 ! mass unit */

#define MS_APPEARANCE_ENERGY_ID "MS:1000209"
#define MS_APPEARANCE_ENERGY_NAME "appearance energy"
/* def: "The minimum energy that must be imparted to an atom or molecule to produce a specified ion. The term appearance potential is not recommended." [PSI:MS]
synonym: "AE" EXACT []
xref: value-type:xsd\:float "The allowed value-type for this CV term."
is_a: MS:1000507 ! ion attribute
relationship: has_units UO:0000266 ! electronvolt */

#define MS_BASE_PEAK_ID "MS:1000210"
#define MS_BASE_PEAK_NAME "base peak"
/* def: "The peak in a mass spectrum that has the greatest intensity. This term may be applied to the spectra of pure substances or mixtures." [PSI:MS]
synonym: "BP" EXACT []
is_a: MS:1000231 ! peak */
 
// #define MS_OBSOLETE_CHARGE_NUMBER_ID "MS:1000211"
// #define MS_OBSOLETE_CHARGE_NUMBER_NAME "OBSOLETE charge number"
/* def: "OBSOLETE. The total charge on an ion divided by the electron charge e. OBSOLETED 2009-10-27 since this was viewed as a duplication of 00041 charge state." [PSI:MS]
synonym: "z" EXACT []
xref: value-type:xsd\:int "The allowed value-type for this CV term."
is_a: MS:1000507 ! ion attribute
is_obsolete: true */

// #define MS_DALTON_ID "MS:1000212"
// #define MS_DALTON_NAME "dalton"
/* def: "OBSOLETE A non-SI unit of mass (symbol Da) that is equal to the unified atomic mass unit: 1.660 538 86(28) x 10^-27 kg." [PSI:MS]
comment: This term was made obsolete because it was redundant with the Unit Ontology term dalton (UO:0000221).
synonym: "Da" EXACT []
is_obsolete: true */

#define MS_ELECTRON_AFFINITY_ID "MS:1000213"
#define MS_ELECTRON_AFFINITY_NAME "electron affinity"
/* def: "The electron affinity of M is the minimum energy required for the process M- ? M + e where M- and M are in their ground rotational, vibrational and electronic states and the electron has zero kinetic energy." [PSI:MS]
synonym: "EA" EXACT []
xref: value-type:xsd\:float "The allowed value-type for this CV term."
is_a: MS:1000507 ! ion attribute */

// #define MS_ELECTRON_ENERGY_OBSOLETE_ID "MS:1000214"
// #define MS_ELECTRON_ENERGY_OBSOLETE_NAME "electron energy obsolete"
/* def: "OBSOLETE The potential difference through which electrons are accelerated before they are used to bring about electron ionization." [PSI:MS]
comment: This former purgatory term was made obsolete.
is_obsolete: true */

#define MS_EXACT_MASS_ID "MS:1000215"
#define MS_EXACT_MASS_NAME "exact mass"
/* def: "The calculated mass of an ion or molecule containing a single isotope of each atom." [PSI:MS]
xref: value-type:xsd\:float "The allowed value-type for this CV term."
is_a: MS:1000507 ! ion attribute
relationship: has_units UO:0000002 ! mass unit */

#define MS_FIELD_FREE_REGION_ID "MS:1000216"
#define MS_FIELD_FREE_REGION_NAME "field-free region"
/* def: "A section of a mass spectrometer in which there are no electric or magnetic fields." [PSI:MS]
synonym: "FFR" EXACT []
is_a: MS:1000487 ! ion optics attribute */

#define MS_IONIZATION_CROSS_SECTION_ID "MS:1000217"
#define MS_IONIZATION_CROSS_SECTION_NAME "ionization cross section"
/* def: "A measure of the probability that a given ionization process will occur when an atom or molecule interacts with a photon, electron, atom or molecule." [PSI:MS]
is_a: MS:1000437 ! ion reaction */

// #define MS_IONIZATION_EFFICIENCY_ID "MS:1000218"
// #define MS_IONIZATION_EFFICIENCY_NAME "ionization efficiency"
/* def: "OBSOLETE The ratio of the number of ions formed to the number of electrons, molecules or photons used." [PSI:MS]
comment: This term was made obsolete because it was replaced by ionization efficiency (MS:1000392).
xref: value-type:xsd\:float "The allowed value-type for this CV term."
is_obsolete: true */

#define MS_IONIZATION_ENERGY_ID "MS:1000219"
#define MS_IONIZATION_ENERGY_NAME "ionization energy"
/* def: "The minimum energy required to remove an electron from an atom or molecule to produce a positive ion." [PSI:MS]
synonym: "IE" EXACT []
xref: value-type:xsd\:float "The allowed value-type for this CV term."
is_a: MS:1000507 ! ion attribute
relationship: has_units UO:0000266 ! electronvolt */

#define MS_ISOTOPE_DILUTION_MASS_SPECTROMETRY_ID "MS:1000220"
#define MS_ISOTOPE_DILUTION_MASS_SPECTROMETRY_NAME "isotope dilution mass spectrometry"
/* def: "A quantitative mass spectrometry technique in which an isotopically enriched compound is used as an internal standard." [PSI:MS]
synonym: "IDMS" EXACT []
is_a: MS:1000268 ! mass spectrometry */

#define MS_MAGNETIC_DEFLECTION_ID "MS:1000221"
#define MS_MAGNETIC_DEFLECTION_NAME "magnetic deflection"
/* def: "The deflection of charged particles in a magnetic field due to a force equal to qv B where q is the particle charge, v its velocity and B the magnetic field. Magnetic deflection of an ion beam is used for m/z separation in a magnetic sector mass spectrometer." [PSI:MS]
is_a: MS:1000597 ! ion optics type */

#define MS_MASS_DEFECT_ID "MS:1000222"
#define MS_MASS_DEFECT_NAME "mass defect"
/* def: "The difference between the monoisotipic and nominal mass of a molecule or atom." [PSI:MS]
xref: value-type:xsd\:float "The allowed value-type for this CV term."
is_a: MS:1000507 ! ion attribute
relationship: has_units UO:0000002 ! mass unit */

#define MS_MASS_NUMBER_ID "MS:1000223"
#define MS_MASS_NUMBER_NAME "mass number"
/* def: "The sum of the protons and neutrons in an atom, molecule or ion." [PSI:MS]
xref: value-type:xsd\:int "The allowed value-type for this CV term."
is_a: MS:1000507 ! ion attribute */

#define MS_MOLECULAR_MASS_ID "MS:1000224"
#define MS_MOLECULAR_MASS_NAME "molecular mass"
/* def: "The mass of one mole of a molecular substance (6.022 1415(10) x 10^23 molecules)." [PSI:MS]
xref: value-type:xsd\:float "The allowed value-type for this CV term."
is_a: MS:1000507 ! ion attribute
relationship: has_units UO:0000002 ! mass unit */

#define MS_MONOISOTOPIC_MASS_ID "MS:1000225"
#define MS_MONOISOTOPIC_MASS_NAME "monoisotopic mass"
/* def: "The mass of an ion or molecule calculated using the mass of the most abundant isotope of each element." [PSI:MS]
xref: value-type:xsd\:float "The allowed value-type for this CV term."
is_a: MS:1000507 ! ion attribute
relationship: has_units UO:0000002 ! mass unit */

#define MS_MOLECULAR_BEAM_MASS_SPECTROMETRY_ID "MS:1000226"
#define MS_MOLECULAR_BEAM_MASS_SPECTROMETRY_NAME "molecular beam mass spectrometry"
/* def: "A mass spectrometry technique in which the sample is introduced into the mass spectrometer as a molecular beam." [PSI:MS]
synonym: "MBMS" EXACT []
is_a: MS:1000293 ! mass spectrometer */

#define MS_MULTIPHOTON_IONIZATION_ID "MS:1000227"
#define MS_MULTIPHOTON_IONIZATION_NAME "multiphoton ionization"
/* def: "Photoionization of an atom or molecule in which in two or more photons are absorbed." [PSI:MS]
synonym: "MPI" EXACT []
is_a: MS:1000008 ! ionization type */

#define MS_NITROGEN_RULE_ID "MS:1000228"
#define MS_NITROGEN_RULE_NAME "nitrogen rule"
/* def: "An organic molecule containing the elements C, H, O, S, P, or halogen has an odd nominal mass if it contains an odd number of nitrogen atoms." [PSI:MS]
is_a: MS:1000437 ! ion reaction */

#define MS_NOMINAL_MASS_ID "MS:1000229"
#define MS_NOMINAL_MASS_NAME "nominal mass"
/* def: "The mass of an ion or molecule calculated using the mass of the most abundant isotope of each element rounded to the nearest integer value." [PSI:MS]
xref: value-type:xsd\:float "The allowed value-type for this CV term."
is_a: MS:1000507 ! ion attribute
relationship: has_units UO:0000002 ! mass unit */

#define MS_ODD_ELECTRON_RULE_ID "MS:1000230"
#define MS_ODD_ELECTRON_RULE_NAME "odd-electron rule"
/* def: "Odd-electron ions may dissociate to form either odd or even-electron ions, whereas even-electron ions generally form even-electron fragment ions." [PSI:MS]
is_a: MS:1000437 ! ion reaction */

#define MS_PEAK_ID "MS:1000231"
#define MS_PEAK_NAME "peak"
/* def: "A localized region of relatively large ion signal in a mass spectrum. Although peaks are often associated with particular ions, the terms peak and ion should not be used interchangeably." [PSI:MS]
relationship: part_of MS:1000442 ! spectrum */

// #define MS_PEAK_INTENSITY_ID "MS:1000232"
// #define MS_PEAK_INTENSITY_NAME "peak intensity"
/* def: "OBSOLETE The height or area of a peak in a mass spectrum." [PSI:MS]
comment: This term was made obsolete because it was replaced by base peak intensity (MS:1000505).
is_obsolete: true */

#define MS_PROTON_AFFINITY_ID "MS:1000233"
#define MS_PROTON_AFFINITY_NAME "proton affinity"
/* def: "The proton affinity of a species M is defined as the negative of the enthalpy change for the reaction M + H+ ->[M+H]+, where all species are in their ground rotational, vibrational and electronic states." [PSI:MS]
synonym: "PA" EXACT [PSI:MS]
xref: value-type:xsd\:float "The allowed value-type for this CV term."
is_a: MS:1000507 ! ion attribute */

// #define MS_MASS_RESOLVING_POWER_ID "MS:1000234"
// #define MS_MASS_RESOLVING_POWER_NAME "mass resolving power"
/* def: "OBSOLETE In a mass spectrum, the observed mass divided by the difference between two masses that can be separated. The method by which delta m was obtained and the mass at which the measurement was made should be reported." [PSI:MS]
comment: This term was made obsolete because it was replaced by mass resolving power (MS:1000800).
xref: value-type:xsd\:float "The allowed value-type for this CV term."
is_a: MS:1000444 ! m/z Separation Method
is_obsolete: true */

#define MS_TOTAL_ION_CURRENT_CHROMATOGRAM_ID "MS:1000235"
#define MS_TOTAL_ION_CURRENT_CHROMATOGRAM_NAME "total ion current chromatogram"
/* def: "Chromatogram obtained by plotting the total ion current detected in each of a series of mass spectra recorded as a function of retention time." [PSI:MS]
synonym: "TIC chromatogram" EXACT []
is_a: MS:1000524 ! data file content
is_a: MS:1000810 ! mass chromatogram */

#define MS_TRANSMISSION_ID "MS:1000236"
#define MS_TRANSMISSION_NAME "transmission"
/* def: "The ratio of the number of ions leaving a region of a mass spectrometer to the number entering that region." [PSI:MS]
xref: value-type:xsd\:float "The allowed value-type for this CV term."
is_a: MS:1000496 ! instrument attribute */

// #define MS_UNIFIED_ATOMIC_MASS_UNIT_ID "MS:1000237"
// #define MS_UNIFIED_ATOMIC_MASS_UNIT_NAME "unified atomic mass unit"
/* def: "OBSOLETE A non-SI unit of mass (u) defined as one twelfth of ^12 C in its ground state and equal to 1.660 538 86(28) x 10^-27 kg." [PSI:MS]
comment: This term was made obsolete because it was redundant with Unit Ontology dalton (UO:0000221).
synonym: "u" EXACT []
is_obsolete: true */

#define MS_ACCELERATOR_MASS_SPECTROMETRY_ID "MS:1000238"
#define MS_ACCELERATOR_MASS_SPECTROMETRY_NAME "accelerator mass spectrometry"
/* def: "A mass spectrometry technique in which atoms extracted from a sample are ionized, accelerated to MeV energies and separated according to their momentum, charge and energy." [PSI:MS]
synonym: "AMS" EXACT []
is_a: MS:1000293 ! mass spectrometer */

#define MS_ATMOSPHERIC_PRESSURE_MATRIX_ASSISTED_LASER_DESORPTION_IONIZATION_ID "MS:1000239"
#define MS_ATMOSPHERIC_PRESSURE_MATRIX_ASSISTED_LASER_DESORPTION_IONIZATION_NAME "atmospheric pressure matrix-assisted laser desorption ionization"
/* def: "Matrix-assisted laser desorption ionization in which the sample target is at atmospheric pressure and the ions formed by the pulsed laser are sampled through a small aperture into the mass spectrometer." [PSI:MS]
synonym: "AP MALDI" EXACT []
is_a: MS:1000240 ! atmospheric pressure ionization */

#define MS_ATMOSPHERIC_PRESSURE_IONIZATION_ID "MS:1000240"
#define MS_ATMOSPHERIC_PRESSURE_IONIZATION_NAME "atmospheric pressure ionization"
/* def: "Any ionization process in which ions are formed in the gas phase at atmospheric pressure." [PSI:MS]
synonym: "API" EXACT []
is_a: MS:1000008 ! ionization type */

// #define MS_ATMOSTPHERIC_PRESSURE_PHOTOIONIZATION_ID "MS:1000241"
// #define MS_ATMOSTPHERIC_PRESSURE_PHOTOIONIZATION_NAME "Atmostpheric Pressure Photoionization"
/* def: "OBSOLETE Atmospheric pressure chemical ionization in which the reactant ions are generated by photo-ionization." [PSI:MS]
comment: This term was made obsolete because it was replaced by atmospheric pressure photoionization (MS:1000382).
synonym: "APPI" EXACT []
is_obsolete: true */

#define MS_BLACKBODY_INFRARED_RADIATIVE_DISSOCIATION_ID "MS:1000242"
#define MS_BLACKBODY_INFRARED_RADIATIVE_DISSOCIATION_NAME "blackbody infrared radiative dissociation"
/* def: "A special case of infrared multiphoton dissociation wherein excitation of the reactant ion is caused by absorption of infrared photons radiating from heated blackbody surroundings, which are usually the walls of a vacuum chamber. See also infrared multiphoton dissociation." [PSI:MS]
synonym: "BIRD" EXACT []
is_a: MS:1000044 ! dissociation method
is_a: MS:1000437 ! ion reaction */

#define MS_CHARGE_REMOTE_FRAGMENTATION_ID "MS:1000243"
#define MS_CHARGE_REMOTE_FRAGMENTATION_NAME "charge-remote fragmentation"
/* def: "A fragmentation of an even-electron ion in which the cleaved bond is not adjacent to the apparent charge site." [PSI:MS]
synonym: "CRF" EXACT []
is_a: MS:1000437 ! ion reaction */

// #define MS_CONSECUTIVE_REACTION_MONITORING_ID "MS:1000244"
// #define MS_CONSECUTIVE_REACTION_MONITORING_NAME "consecutive reaction monitoring"
/* def: "OBSOLETE MSn experiment with three or more stages of m/z separation and in which a particular multi-step reaction path is monitored." [PSI:MS]
comment: This former purgatory term was made obsolete.
synonym: "CRM" EXACT []
is_obsolete: true */

#define MS_CHARGE_STRIPPING_ID "MS:1000245"
#define MS_CHARGE_STRIPPING_NAME "charge stripping"
/* def: "The reaction of a positive ion with an atom or molecule that results in the removal of one or more electrons from the ion." [PSI:MS]
synonym: "CS" EXACT []
is_a: MS:1000510 ! precursor activation attribute */

#define MS_DELAYED_EXTRACTION_ID "MS:1000246"
#define MS_DELAYED_EXTRACTION_NAME "delayed extraction"
/* def: "The application of the accelerating voltage pulse after a time delay in desorption ionization from a surface. The extraction delay can produce energy focusing in a time-of-flight mass spectrometer." [PSI:MS]
synonym: "DE" EXACT []
is_a: MS:1000597 ! ion optics type */

#define MS_DESORPTION_IONIZATION_ID "MS:1000247"
#define MS_DESORPTION_IONIZATION_NAME "desorption ionization"
/* def: "The formation of ions from a solid or liquid material after the rapid vaporization of that sample." [PSI:MS]
synonym: "DI" EXACT []
is_a: MS:1000008 ! ionization type */

#define MS_DIRECT_INSERTION_PROBE_ID "MS:1000248"
#define MS_DIRECT_INSERTION_PROBE_NAME "direct insertion probe"
/* def: "A device for introducing a solid or liquid sample into a mass spectrometer ion source for desorption ionization." [PSI:MS]
synonym: "DIP" EXACT []
is_a: MS:1000007 ! inlet type */

#define MS_DIRECT_LIQUID_INTRODUCTION_ID "MS:1000249"
#define MS_DIRECT_LIQUID_INTRODUCTION_NAME "direct liquid introduction"
/* def: "The delivery of a liquid sample into a mass spectrometer for spray or desorption ionization." [PSI:MS]
synonym: "DLI" EXACT []
is_a: MS:1000007 ! inlet type */

#define MS_ELECTRON_CAPTURE_DISSOCIATION_ID "MS:1000250"
#define MS_ELECTRON_CAPTURE_DISSOCIATION_NAME "electron capture dissociation"
/* def: "A process in which a multiply protonated molecules interacts with a low energy electrons. Capture of the electron leads the liberation of energy and a reduction in charge state of the ion with the production of the (M + nH) (n-1)+ odd electron ion, which readily fragments." [PSI:MS]
synonym: "ECD" EXACT []
is_a: MS:1000044 ! dissociation method
is_a: MS:1000437 ! ion reaction */

#define MS_EVEN_ELECTRON_ION_ID "MS:1000251"
#define MS_EVEN_ELECTRON_ION_NAME "even-electron ion"
/* def: "An ion containing no unpaired electrons in its ground electronic state, e.g. CH3+ in its ground state." [PSI:MS]
synonym: "EE" EXACT []
is_a: MS:1000508 ! ion chemical type */

#define MS_ELECTRON_INDUCED_EXCITATION_IN_ORGANICS_ID "MS:1000252"
#define MS_ELECTRON_INDUCED_EXCITATION_IN_ORGANICS_NAME "electron-induced excitation in organics"
/* def: "The reaction of an ion with an electron in which the translational energy of the collision is converted into internal energy of the ion." [PSI:MS]
synonym: "EIEIO" EXACT []
is_a: MS:1000437 ! ion reaction */

#define MS_ELECTRON_MULTIPLIER_ID "MS:1000253"
#define MS_ELECTRON_MULTIPLIER_NAME "electron multiplier"
/* def: "A device to amplify the current of a beam or packet of charged particles or photons by incidence upon the surface of an electrode to produce secondary electrons. The secondary electrons are then accelerated to other electrodes or parts of a continuous electrode to produce further secondary electrons." [PSI:MS]
synonym: "EM" EXACT []
is_a: MS:1000026 ! detector type */

#define MS_ELECTROSTATIC_ENERGY_ANALYZER_ID "MS:1000254"
#define MS_ELECTROSTATIC_ENERGY_ANALYZER_NAME "electrostatic energy analyzer"
/* def: "A device consisting of conducting parallel plates, concentric cylinders or concentric spheres that separates charged particles according to their kinetic energy by means of an electric field that is constant in time." [PSI:MS]
synonym: "ESA" EXACT []
is_a: MS:1000443 ! mass analyzer type */

#define MS_FLOWING_AFTERGLOW_ID "MS:1000255"
#define MS_FLOWING_AFTERGLOW_NAME "flowing afterglow"
/* def: "An ion source immersed in a flow of helium or other inert buffer gas that carries the ions through a meter-long reactor at pressures around 100 Pa." [PSI:MS]
synonym: "FA" EXACT []
is_a: MS:1000008 ! ionization type */

#define MS_HIGH_FIELD_ASYMMETRIC_WAVEFORM_ION_MOBILITY_SPECTROMETRY_ID "MS:1000256"
#define MS_HIGH_FIELD_ASYMMETRIC_WAVEFORM_ION_MOBILITY_SPECTROMETRY_NAME "high-field asymmetric waveform ion mobility spectrometry"
/* def: "The separation of ions between two concentric cylindrical electrodes due to application of a high voltage asymmetric waveform whereby ions migrate towards one of the two electrodes depending on the ratio of the high- to low-field mobility of the ion." [PSI:MS]
synonym: "FAIMS" EXACT []
is_a: MS:1000293 ! mass spectrometer */

#define MS_FIELD_DESORPTION_ID "MS:1000257"
#define MS_FIELD_DESORPTION_NAME "field desorption"
/* def: "The formation of gas-phase ions from a material deposited on a solid surface in the presence of a high electric field. Because this process may encompass ionization by field ionization or other mechanisms, it is not recommended as a synonym for field desorption ionization." [PSI:MS]
synonym: "FD" EXACT []
is_a: MS:1000247 ! desorption ionization */

#define MS_FIELD_IONIZATION_ID "MS:1000258"
#define MS_FIELD_IONIZATION_NAME "field ionization"
/* def: "The removal of electrons from any species by interaction with a high electric field." [PSI:MS]
synonym: "FI" EXACT []
is_a: MS:1000008 ! ionization type */

#define MS_GLOW_DISCHARGE_IONIZATION_ID "MS:1000259"
#define MS_GLOW_DISCHARGE_IONIZATION_NAME "glow discharge ionization"
/* def: "The formation of ions in the gas phase and from solid samples at the cathode by application of a voltage to a low pressure gas." [PSI:MS]
synonym: "GD-MS" EXACT []
is_a: MS:1000008 ! ionization type */

#define MS_ION_KINETIC_ENERGY_SPECTROMETRY_ID "MS:1000260"
#define MS_ION_KINETIC_ENERGY_SPECTROMETRY_NAME "ion kinetic energy spectrometry"
/* def: "A method of analysis in which a beam of ions is separated according to the ratio of its translational energy to charge." [PSI:MS]
synonym: "IKES" EXACT []
is_a: MS:1000293 ! mass spectrometer */

#define MS_ION_MOBILITY_SPECTROMETRY_ID "MS:1000261"
#define MS_ION_MOBILITY_SPECTROMETRY_NAME "ion mobility spectrometry"
/* def: "The separation of ions according to their velocity through a buffer gas under the influence of an electric field." [PSI:MS]
synonym: "IMS" EXACT []
is_a: MS:1000293 ! mass spectrometer */

#define MS_INFRARED_MULTIPHOTON_DISSOCIATION_ID "MS:1000262"
#define MS_INFRARED_MULTIPHOTON_DISSOCIATION_NAME "infrared multiphoton dissociation"
/* def: "Multiphoton ionization where the reactant ion dissociates as a result of the absorption of multiple infrared photons." [PSI:MS]
synonym: "IRMPD" EXACT []
is_a: MS:1000044 ! dissociation method */

#define MS_ISOTOPE_RATIO_MASS_SPECTROMETRY_ID "MS:1000263"
#define MS_ISOTOPE_RATIO_MASS_SPECTROMETRY_NAME "isotope ratio mass spectrometry"
/* def: "The measurement of the relative quantity of the different isotopes of an element in a material with a mass spectrometer." [PSI:MS]
synonym: "IRMS" EXACT []
is_a: MS:1000268 ! mass spectrometry */

#define MS_ION_TRAP_ID "MS:1000264"
#define MS_ION_TRAP_NAME "ion trap"
/* def: "A device for spatially confining ions using electric and magnetic fields alone or in combination." [PSI:MS]
synonym: "IT" EXACT []
is_a: MS:1000443 ! mass analyzer type */

#define MS_KINETIC_ENERGY_RELEASE_DISTRIBUTION_ID "MS:1000265"
#define MS_KINETIC_ENERGY_RELEASE_DISTRIBUTION_NAME "kinetic energy release distribution"
/* def: "Distribution of values of translational kinetic energy release for an ensemble of metastable ions undergoing a specific dissociation reaction." [PSI:MS]
synonym: "KERD" EXACT []
is_a: MS:1000437 ! ion reaction */

// #define MS_LASER_DESORPTION_ID "MS:1000266"
// #define MS_LASER_DESORPTION_NAME "Laser Desorption"
/* def: "OBSOLETE The formation of ions through the interaction of a laser with a material or with gas-phase ions or molecules." [PSI:MS]
comment: This term was made obsolete because it was replaced by laser desorption ionization (MS:1000393).
synonym: "Laser Ionization MERGE" EXACT []
synonym: "LD" EXACT []
is_obsolete: true */

#define MS_MASS_ANALYZED_ION_KINETIC_ENERGY_SPECTROMETRY_ID "MS:1000267"
#define MS_MASS_ANALYZED_ION_KINETIC_ENERGY_SPECTROMETRY_NAME "mass analyzed ion kinetic energy spectrometry"
/* def: "Spectra that are obtained from a sector mass spectrometer that incorporates at least one magnetic sector plus one electric sector in reverse geometry. The accelerating voltage, V, and the magnetic sector field, B, are set at fixed values to select the precursor ions, which are then allowed to dissociate or to react in a field free region between the two sectors. The kinetic energy product ions of m/z selected precursor ions is analyzed by scanning the electric sector field, E. The width of the product ion spectrum peaks is related to the kinetic energy release distribution (KERD) for the dissociation process." [PSI:MS]
synonym: "MIKES" EXACT []
is_a: MS:1000293 ! mass spectrometer */

#define MS_MASS_SPECTROMETRY_ID "MS:1000268"
#define MS_MASS_SPECTROMETRY_NAME "mass spectrometry"
/* def: "The branch of science that deals with all aspects of mass spectrometers and the results obtained with these instruments." [PSI:MS]
synonym: "MS" EXACT []
relationship: part_of MS:1000479 ! purgatory */

#define MS_MASS_SPECTROMETRY_MASS_SPECTROMETRY_ID "MS:1000269"
#define MS_MASS_SPECTROMETRY_MASS_SPECTROMETRY_NAME "mass spectrometry_mass spectrometry"
/* def: "The acquisition, study and spectra of the electrically charged products or precursors of a m/z selected ion or ions." [PSI:MS]
synonym: "MS/MS" EXACT []
is_a: MS:1000293 ! mass spectrometer */

#define MS_MULTIPLE_STAGE_MASS_SPECTROMETRY_ID "MS:1000270"
#define MS_MULTIPLE_STAGE_MASS_SPECTROMETRY_NAME "multiple stage mass spectrometry"
/* def: "Multiple stages of precursor ion m/z selection followed by product ion detection for successive progeny ions." [PSI:MS]
synonym: "MSn" EXACT []
is_a: MS:1000445 ! sequential m/z separation method */

#define MS_NEGATIVE_ION_CHEMICAL_IONIZATION_ID "MS:1000271"
#define MS_NEGATIVE_ION_CHEMICAL_IONIZATION_NAME "Negative Ion chemical ionization"
/* def: "Chemical ionization that results in the formation of negative ions." [PSI:MS]
synonym: "NICI" EXACT []
is_a: MS:1000008 ! ionization type */

#define MS_NEUTRALIZATION_REIONIZATION_MASS_SPECTROMETRY_ID "MS:1000272"
#define MS_NEUTRALIZATION_REIONIZATION_MASS_SPECTROMETRY_NAME "neutralization reionization mass spectrometry"
/* def: "With this technique, m/z selected ions form neutrals by charge transfer to a collision gas or by dissociation. The neutrals are separated from the remaining ions and ionized in collisions with a second gas. This method is used to investigate reaction intermediates and other unstable species." [PSI:MS]
synonym: "NRMS" EXACT []
is_a: MS:1000008 ! ionization type */

#define MS_PHOTOIONIZATION_ID "MS:1000273"
#define MS_PHOTOIONIZATION_NAME "photoionization"
/* def: "The ionization of an atom or molecule by a photon, written M + h? ? M^+ + e. The term photon impact is not recommended." [PSI:MS]
synonym: "PI" EXACT []
is_a: MS:1000008 ! ionization type */

#define MS_PYROLYSIS_MASS_SPECTROMETRY_ID "MS:1000274"
#define MS_PYROLYSIS_MASS_SPECTROMETRY_NAME "pyrolysis mass spectrometry"
/* def: "A mass spectrometry technique in which the sample is heated to the point of decomposition and the gaseous decomposition products are introduced into the ion source." [PSI:MS]
synonym: "PyMS" EXACT []
is_a: MS:1000008 ! ionization type */

#define MS_COLLISION_QUADRUPOLE_ID "MS:1000275"
#define MS_COLLISION_QUADRUPOLE_NAME "collision quadrupole"
/* def: "A transmission quadrupole to which an oscillating potential is applied so as to focus a beam of ions through a collision gas with no m/z separation." [PSI:MS]
synonym: "q" EXACT []
is_a: MS:1000597 ! ion optics type */

#define MS_RESONANCE_ENHANCED_MULTIPHOTON_IONIZATION_ID "MS:1000276"
#define MS_RESONANCE_ENHANCED_MULTIPHOTON_IONIZATION_NAME "resonance enhanced multiphoton ionization"
/* def: "Multiphoton ionization in which the ionization cross section is significantly enhanced because the energy of the incident photons is resonant with an intermediate excited state of the neutral species." [PSI:MS]
synonym: "REMPI" EXACT []
is_a: MS:1000008 ! ionization type */

#define MS_RESIDUAL_GAS_ANALYZER_ID "MS:1000277"
#define MS_RESIDUAL_GAS_ANALYZER_NAME "residual gas analyzer"
/* def: "A mass spectrometer used to measure the composition and pressure of gasses in an evacuated chamber." [PSI:MS]
synonym: "RGA" EXACT []
is_a: MS:1000293 ! mass spectrometer */

#define MS_SURFACE_ENHANCED_LASER_DESORPTION_IONIZATION_ID "MS:1000278"
#define MS_SURFACE_ENHANCED_LASER_DESORPTION_IONIZATION_NAME "surface enhanced laser desorption ionization"
/* def: "The formation of ionized species in the gas phase from analytes deposited on a particular surface substrate which is irradiated with a laser beam of which wavelength is absorbed by the surface. See also desorption/ionization on silicon and laser desorption/ionization." [PSI:MS]
synonym: "SELDI" EXACT []
is_a: MS:1000406 ! surface ionization */

#define MS_SURFACE_ENHANCED_NEAT_DESORPTION_ID "MS:1000279"
#define MS_SURFACE_ENHANCED_NEAT_DESORPTION_NAME "surface enhanced neat desorption"
/* def: "Matrix-assisted laser desorption ionization in which the matrix is covalently linked to the target surface." [PSI:MS]
synonym: "SEND" EXACT []
is_a: MS:1000406 ! surface ionization */

// #define MS_SUFACE_IONIZATION_ID "MS:1000280"
// #define MS_SUFACE_IONIZATION_NAME "suface ionization"
/* def: "OBSOLETE The ionization of a neutral species when it interacts with a solid surface with an appropriate work function and temperature." [PSI:MS]
comment: This term was made obsolete because it was replaced by surface ionization (MS:1000406).
synonym: "SI" EXACT []
is_obsolete: true */

#define MS_SELECTED_ION_FLOW_TUBE_ID "MS:1000281"
#define MS_SELECTED_ION_FLOW_TUBE_NAME "selected ion flow tube"
/* def: "A device in which m/z selected ions are entrained in an inert carrier gas and undergo ion-molecule reactions." [PSI:MS]
synonym: "SIFT" EXACT []
is_a: MS:1000597 ! ion optics type */

#define MS_SUSTAINED_OFF_RESONANCE_IRRADIATION_ID "MS:1000282"
#define MS_SUSTAINED_OFF_RESONANCE_IRRADIATION_NAME "sustained off-resonance irradiation"
/* def: "A technique associated with Fourier transform ion cyclotron resonance (FT-ICR) mass spectrometry to carry out ion/neutral reactions such as low-energy collision-induced dissociation. A radio-frequency electric field of slightly off-resonance to the cyclotron frequency of the reactant ion cyclically accelerates and decelerates the reactant ion that is confined in the Penning ion trap. The ion's orbit does not exceed the dimensions of ion trap while the ion undergoes an ion/neutral species process that produces a high average translational energy for an extended time." [PSI:MS]
synonym: "SORI" EXACT []
is_a: MS:1000044 ! dissociation method */

// #define MS_SPARK_SOURCE_MASS_SPECTROMETRY_ID "MS:1000283"
// #define MS_SPARK_SOURCE_MASS_SPECTROMETRY_NAME "Spark Source Mass Spectrometry"
/* def: "OBSOLETE Mass spectrometry using spark ionization." [PSI:MS]
comment: This term was made obsolete because it was replaced by spark ionization (MS:1000404).
synonym: "SSMS" EXACT []
is_obsolete: true */

#define MS_STORED_WAVEFORM_INVERSE_FOURIER_TRANSFORM_ID "MS:1000284"
#define MS_STORED_WAVEFORM_INVERSE_FOURIER_TRANSFORM_NAME "stored waveform inverse fourier transform"
/* def: "A technique to create excitation waveforms for ions in FT-ICR mass spectrometer or Paul ion trap. An excitation waveform in the time-domain is generated by taking the inverse Fourier transform of an appropriate frequency-domain programmed excitation spectrum, in which the resonance frequencies of ions to be excited are included. This technique may be used for selection of precursor ions in MS/MS experiments." [PSI:MS]
synonym: "SWIFT" EXACT []
is_a: MS:1000443 ! mass analyzer type */

#define MS_TOTAL_ION_CURRENT_ID "MS:1000285"
#define MS_TOTAL_ION_CURRENT_NAME "total ion current"
/* def: "The sum of all the separate ion currents carried by the ions of different m/z contributing to a complete mass spectrum or in a specified m/z range of a mass spectrum." [PSI:MS]
synonym: "TIC" EXACT []
xref: value-type:xsd\:float "The allowed value-type for this CV term."
is_a: MS:1000499 ! spectrum attribute */

#define MS_TIME_LAG_FOCUSING_ID "MS:1000286"
#define MS_TIME_LAG_FOCUSING_NAME "time lag focusing"
/* def: "Energy focusing in a time-of-flight mass spectrometer that is accomplished by introducing a time delay between the formation of the ions and the application of the accelerating voltage pulse." [PSI:MS]
synonym: "TLF" EXACT []
is_a: MS:1000597 ! ion optics type */

#define MS_TIME_OF_FLIGHT_MASS_SPECTROMETER_ID "MS:1000287"
#define MS_TIME_OF_FLIGHT_MASS_SPECTROMETER_NAME "time-of-flight mass spectrometer"
/* def: "An instrument that separates ions by m/z in a field-free region after acceleration to a fixed kinetic energy." [PSI:MS]
synonym: "TOF-MS" EXACT []
is_a: MS:1000293 ! mass spectrometer */

#define MS_CYCLOTRON_ID "MS:1000288"
#define MS_CYCLOTRON_NAME "cyclotron"
/* def: "A device that uses an oscillating electric field and magnetic field to accelerate charged particles." [PSI:MS]
is_a: MS:1000443 ! mass analyzer type */

#define MS_DOUBLE_FOCUSING_MASS_SPECTROMETER_ID "MS:1000289"
#define MS_DOUBLE_FOCUSING_MASS_SPECTROMETER_NAME "double-focusing mass spectrometer"
/* def: "A mass spectrometer that uses a magnetic sector for m/z focusing and an electric sector for energy focusing of an ion beam." [PSI:MS]
is_a: MS:1000293 ! mass spectrometer */

#define MS_HYBRID_MASS_SPECTROMETER_ID "MS:1000290"
#define MS_HYBRID_MASS_SPECTROMETER_NAME "hybrid mass spectrometer"
/* def: "A mass spectrometer that combines m/z analyzers of different types to perform tandem mass spectrometry." [PSI:MS]
is_a: MS:1000293 ! mass spectrometer */

#define MS_LINEAR_ION_TRAP_ID "MS:1000291"
#define MS_LINEAR_ION_TRAP_NAME "linear ion trap"
/* def: "A two dimensional Paul ion trap in which ions are confined in the axial dimension by means of an electric field at the ends of the trap." [PSI:MS]
is_a: MS:1000264 ! ion trap */
 
// #define MS_MASS_SPECTROGRAPH_OBSOLETE_ID "MS:1000292"
// #define MS_MASS_SPECTROGRAPH_OBSOLETE_NAME "mass spectrograph obsolete"
/* def: "OBSOLETE An instrument that separates a beam of ions according to their mass-to-charge ratio in which the ions are directed onto a focal plane detector such as a photographic plate." [PSI:MS]
comment: This former purgatory term was made obsolete.
is_obsolete: true */

#define MS_MASS_SPECTROMETER_ID "MS:1000293"
#define MS_MASS_SPECTROMETER_NAME "mass spectrometer"
/* def: "An instrument that measures the mass-to-charge ratio and relative abundances of ions." [PSI:MS]
is_a: MS:1000479 ! purgatory */

#define MS_MASS_SPECTRUM_ID "MS:1000294"
#define MS_MASS_SPECTRUM_NAME "mass spectrum"
/* def: "A plot of the relative abundance of a beam or other collection of ions as a function of the mass-to-charge ratio (m/z)." [PSI:MS]
is_a: MS:1000524 ! data file content
is_a: MS:1000559 ! spectrum type */

// #define MS_MATTAUCH_HERZOG_GEOMETRY_ID "MS:1000295"
// #define MS_MATTAUCH_HERZOG_GEOMETRY_NAME "mattauch-herzog geometry"
/* def: "OBSOLETE An arrangement for a double-focusing mass spectrometer in which a deflection of ?/(4 ?(2)) radians in a radial electric field is followed by a magnetic deflection of ?/2 radians." [PSI:MS]
comment: This former purgatory term was made obsolete.
is_obsolete: true */

// #define MS_NIER_JOHNSON_GEOMETRY_ID "MS:1000296"
// #define MS_NIER_JOHNSON_GEOMETRY_NAME "nier-johnson geometry"
/* def: "OBSOLETE An arrangement for a double-focusing mass spectrometer in which a deflection of ?/2 radians in a radial electric field analyzer is followed by a magnetic deflection of ?/3 radians." [PSI:MS]
comment: This former purgatory term was made obsolete.
is_obsolete: true */

// #define MS_PAUL_ION_TRAP_ID "MS:1000297"
// #define MS_PAUL_ION_TRAP_NAME "paul ion trap"
/* def: "OBSOLETE A device that permits the trapping of ions by means of an alternating current voltage. The ejection of ions with a m/z less than a prescribed value and retention of those with higher mass depends on the application of radio frequency voltages between a ring electrode and two end-cap electrodes to confine the ions in a circular path. The choice of these voltages determines the m/z below which ions are ejected." [PSI:MS]
comment: This term was made obsolete because it is redundant to quadrupole ion trap.
synonym: "quadrupole ion trap" RELATED []
is_obsolete: true */

#define MS_PROLATE_TRAOCHOIDAL_MASS_SPECTROMETER_ID "MS:1000298"
#define MS_PROLATE_TRAOCHOIDAL_MASS_SPECTROMETER_NAME "prolate traochoidal mass spectrometer"
/* def: "A mass spectrometer in which the ions of different m/z are separated by means of crossed electric and magnetic fields in such a way that the selected ions follow a prolate trochoidal path." [PSI:MS]
is_a: MS:1000293 ! mass spectrometer */

// #define MS_QUISTOR_ID "MS:1000299"
// #define MS_QUISTOR_NAME "quistor"
/* def: "OBSOLETE An abbreviation of quadrupole ion storage trap. This term is synonymous with Paul Ion Trap. If so then add a synonym to paul and obsolete this term." [PSI:MS]
comment: This former purgatory term was made obsolete.
is_obsolete: true */

#define MS_REFLECTRON_ID "MS:1000300"
#define MS_REFLECTRON_NAME "reflectron"
/* def: "A time-of-flight mass spectrometer that uses a static electric field to reverse the direction of travel of the ions entering it. A reflectron improves mass resolution by assuring that ions of the same m/z but different kinetic energy arrive at the detector at the same time." [PSI:MS]
is_a: MS:1000597 ! ion optics type */

#define MS_SECTOR_MASS_SPECTROMETER_ID "MS:1000301"
#define MS_SECTOR_MASS_SPECTROMETER_NAME "sector mass spectrometer"
/* def: "A mass spectrometer consisting of one or more magnetic sectors for m/z selection in a beam of ions. Such instruments may also have one or more electric sectors for energy selection." [PSI:MS]
is_a: MS:1000293 ! mass spectrometer */

#define MS_TANDEM_MASS_SPECTROMETER_ID "MS:1000302"
#define MS_TANDEM_MASS_SPECTROMETER_NAME "tandem mass spectrometer"
/* def: "A mass spectrometer designed for mass spectrometry/mass spectrometry." [PSI:MS]
is_a: MS:1000293 ! mass spectrometer */

#define MS_TRANSMISSION_QUADRUPOLE_MASS_SPECTROMETER_ID "MS:1000303"
#define MS_TRANSMISSION_QUADRUPOLE_MASS_SPECTROMETER_NAME "transmission quadrupole mass spectrometer"
/* def: "A mass spectrometer that consists of four parallel rods whose centers form the corners of a square and whose opposing poles are connected. The voltage applied to the rods is a superposition of a static potential and a sinusoidal radio frequency potential. The motion of an ion in the x and y dimensions is described by the Matthieu equation whose solutions show that ions in a particular m/z range can be transmitted along the z axis." [PSI:MS]
is_a: MS:1000293 ! mass spectrometer */

#define MS_ACCELERATING_VOLTAGE_ID "MS:1000304"
#define MS_ACCELERATING_VOLTAGE_NAME "accelerating voltage"
/* def: "The electrical potential used to impart kinetic energy to ions in a mass spectrometer." [PSI:MS]
xref: value-type:xsd\:float "The allowed value-type for this CV term."
is_a: MS:1000487 ! ion optics attribute
relationship: has_units UO:0000218 ! volt */

#define MS_CYCLOTRON_MOTION_ID "MS:1000305"
#define MS_CYCLOTRON_MOTION_NAME "cyclotron motion"
/* def: "The circular motion of a charged particle moving at velocity v in a magnetic field B that results from the force qvB." [PSI:MS]
is_a: MS:1000444 ! m/z Separation Method */

#define MS_DYNAMIC_MASS_SPECTROMETRY_ID "MS:1000306"
#define MS_DYNAMIC_MASS_SPECTROMETRY_NAME "dynamic mass spectrometry"
/* def: "A mass spectrometer in which m/z separation using one or more electric fields that vary with time." [PSI:MS]
is_a: MS:1000293 ! mass spectrometer */

#define MS_EINZEL_LENS_ID "MS:1000307"
#define MS_EINZEL_LENS_NAME "einzel lens"
/* def: "Three element charged particle lens in which the first and third elements are held at the same voltage. Such a lens produces focusing without changing the translational energy of the particle." [PSI:MS]
is_a: MS:1000597 ! ion optics type */

#define MS_ELECTRIC_FIELD_STRENGTH_ID "MS:1000308"
#define MS_ELECTRIC_FIELD_STRENGTH_NAME "electric field strength"
/* def: "The magnitude of the force per unit charge at a given point in space." [PSI:MS]
xref: value-type:xsd\:float "The allowed value-type for this CV term."
is_a: MS:1000487 ! ion optics attribute
relationship: has_units UO:0000268 ! volt per meter */

#define MS_FIRST_STABILITY_REGION_ID "MS:1000309"
#define MS_FIRST_STABILITY_REGION_NAME "first stability region"
/* def: "The region of a Mathieu stability diagram closest to the origin. Ions within this region can traverse the full length of a transmission quadrupole." [PSI:MS]
is_a: MS:1000597 ! ion optics type */

#define MS_FRINGING_FIELD_ID "MS:1000310"
#define MS_FRINGING_FIELD_NAME "fringing field"
/* def: "The electric or magnetic field that extends from the edge of a sector, lens or other ion optics element." [PSI:MS]
is_a: MS:1000597 ! ion optics type */

#define MS_KINETIC_ENERGY_ANALYZER_ID "MS:1000311"
#define MS_KINETIC_ENERGY_ANALYZER_NAME "kinetic energy analyzer"
/* def: "A device for measuring the kinetic energy of charged particles using a retarding field, time-of-flight, or the extent of deflection in an electric or magnetic field." [PSI:MS]
is_a: MS:1000597 ! ion optics type */

// #define MS_MASS_LIMIT_ID "MS:1000312"
// #define MS_MASS_LIMIT_NAME "mass limit"
/* def: "OBSOLETE The m/z value above which ions cannot be detected in a mass spectrometer." [PSI:MS]
comment: This former purgatory term was made obsolete.
is_obsolete: true */

// #define MS_SCAN_M_Z_RANGE__ID "MS:1000313"
// #define MS_SCAN_M_Z_RANGE__NAME "scan m/z range?"
/* def: "OBSOLETE The limit of m/z over which a mass spectrometer can detect ions." [PSI:MS]
comment: This former purgatory term was made obsolete.
is_obsolete: true */

#define MS_MASS_SELECTIVE_AXIAL_EJECTION_ID "MS:1000314"
#define MS_MASS_SELECTIVE_AXIAL_EJECTION_NAME "mass selective axial ejection"
/* def: "The use of mass selective instability to eject ions of selected m/z values from an ion trap." [PSI:MS]
is_a: MS:1000444 ! m/z Separation Method */

#define MS_MASS_SELECTIVE_INSTABILITY_ID "MS:1000315"
#define MS_MASS_SELECTIVE_INSTABILITY_NAME "mass selective instability"
/* def: "A method for selective ejection of ions according to their m/z value in an ion trap." [PSI:MS]
is_a: MS:1000444 ! m/z Separation Method */

#define MS_MATHIEU_STABILITY_DIAGRAM_ID "MS:1000316"
#define MS_MATHIEU_STABILITY_DIAGRAM_NAME "mathieu stability diagram"
/* def: "A graphical representation expressed in terms of reduced coordinates that describes charged particle motion in a quadrupole mass filter or quadrupole ion trap mass spectrometer." [PSI:MS]
is_a: MS:1000444 ! m/z Separation Method */

#define MS_ORTHOGONAL_EXTRACTION_ID "MS:1000317"
#define MS_ORTHOGONAL_EXTRACTION_NAME "orthogonal extraction"
/* def: "The pulsed acceleration of ions perpendicular to their direction of travel into a time-of-flight mass spectrometer. Ions may be extracted from a directional ion source, drift tube or m/z separation stage." [PSI:MS]
is_a: MS:1000444 ! m/z Separation Method */

#define MS_RESONANCE_ION_EJECTION_ID "MS:1000318"
#define MS_RESONANCE_ION_EJECTION_NAME "resonance ion ejection"
/* def: "A mode of ion ejection in a quadrupole ion trap that relies on a auxiliary radio frequency voltage that is applied to the end-cap electrodes. The voltage is tuned to the secular frequency of a particular ion to eject it." [PSI:MS]
is_a: MS:1000444 ! m/z Separation Method */

#define MS_SPACE_CHARGE_EFFECT_ID "MS:1000319"
#define MS_SPACE_CHARGE_EFFECT_NAME "space charge effect"
/* def: "The mutual repulsion of particles of like charge that limits the current in a charged-particle beam and causes beams or packets of charged particles to expand radially over time." [PSI:MS]
is_a: MS:1000487 ! ion optics attribute */

#define MS_STATIC_FIELD_ID "MS:1000320"
#define MS_STATIC_FIELD_NAME "static field"
/* def: "An electric or magnetic field that does not change in time." [PSI:MS]
is_a: MS:1000597 ! ion optics type */

#define MS__2E_MASS_SPECTRUM_ID "MS:1000321"
#define MS__2E_MASS_SPECTRUM_NAME "2E Mass Spectrum"
/* def: "A mass spectrum obtained by setting the electric sector field E to twice the value required to transmit the main ion-beam thereby allowing ions with a kinetic energy-to-charge ratio twice that of the main ion-beam to be transmitted. Product ions resulting from partial charge transfer reactions such as m^2+ + N ? m^+ + N^+ that occur in a collision cell (containing a gas, N) located in a field-free region preceding a magnetic and electric sector combination are detected. When the magnetic sector field B is scanned, a mass spectrum of singly charged product ions of doubly charged precursor ions is obtained." [PSI:MS]
is_a: MS:1000445 ! sequential m/z separation method */

#define MS_CHARGE_INVERSION_MASS_SPECTRUM_ID "MS:1000322"
#define MS_CHARGE_INVERSION_MASS_SPECTRUM_NAME "charge inversion mass spectrum"
/* def: "The measurement of the relative abundance of ions that result from a charge inversion reaction as a function of m/z." [PSI:MS]
is_a: MS:1000294 ! mass spectrum
is_a: MS:1000524 ! data file content */

// #define MS_CONSTANT_NEUTRAL_LOSS_SCAN_ID "MS:1000323"
// #define MS_CONSTANT_NEUTRAL_LOSS_SCAN_NAME "constant neutral loss scan"
/* def: "OBSOLETE Spectrum of all precursor ions that undergo a selected m/z decrement." [PSI:MS]
comment: This former purgatory term was made obsolete.
synonym: "constant neutral mass loss scan" RELATED []
synonym: "fixed neutral fragment scan" RELATED []
is_obsolete: true */

// #define MS_CONSTANT_NEUTRAL_GAIN_SCAN_ID "MS:1000324"
// #define MS_CONSTANT_NEUTRAL_GAIN_SCAN_NAME "constant neutral gain scan"
/* def: "OBSOLETE Spectrum of all precursor ions that undergo a selected m/z increment." [PSI:MS]
comment: This former purgatory term was made obsolete.
synonym: "Constant Neutral Mass Gain Scan" EXACT []
is_obsolete: true */

#define MS_CONSTANT_NEUTRAL_GAIN_SPECTRUM_ID "MS:1000325"
#define MS_CONSTANT_NEUTRAL_GAIN_SPECTRUM_NAME "constant neutral gain spectrum"
/* def: "A spectrum formed of all product ions that have been produced by gain of a pre-selected neutral mass following the reaction with and addition of the gas in a collision cell." [PSI:MS]
synonym: "constant neutral mass gain spectrum" EXACT []
is_a: MS:1000294 ! mass spectrum
is_a: MS:1000524 ! data file content */

#define MS_CONSTANT_NEUTRAL_LOSS_SPECTRUM_ID "MS:1000326"
#define MS_CONSTANT_NEUTRAL_LOSS_SPECTRUM_NAME "constant neutral loss spectrum"
/* def: "A spectrum formed of all product ions that have been produced with a selected m/z decrement from any precursor ions. The spectrum shown correlates to the precursor ion spectrum. See also neutral loss spectrum." [PSI:MS]
synonym: "constant neutral mass loss spectrum" EXACT []
is_a: MS:1000294 ! mass spectrum
is_a: MS:1000524 ! data file content */

// #define MS_CONSECUTIVE_REACTION_MONITORING_ID "MS:1000327"
// #define MS_CONSECUTIVE_REACTION_MONITORING_NAME "consecutive reaction monitoring"
/* def: "OBSOLETE A type of MS/MS experiments with three or more stages of m/z separation and in which a particular multi-step reaction path is monitored." [PSI:MS]
comment: This term was made obsolete because it was replaced by consecutive reaction monitoring (MS:1000244).
is_obsolete: true */

#define MS_E_2_MASS_SPECTRUM_ID "MS:1000328"
#define MS_E_2_MASS_SPECTRUM_NAME "e_2 mass spectrum"
/* def: "A mass spectrum obtained using a sector mass spectrometer in which the electric sector field E is set to half the value required to transmit the main ion-beam. This spectrum records the signal from doubly charged product ions of charge-stripping reactions." [PSI:MS]
is_a: MS:1000294 ! mass spectrum */

#define MS_LINKED_SCAN_ID "MS:1000329"
#define MS_LINKED_SCAN_NAME "linked scan"
/* def: "A scan in an instrument with two or more m/z analysers or in a sector mass spectrometer that incorporates at least one magnetic sector and one electric sector. Two or more of the analyzers are scanned simultaneously so as to preserve a predetermined relationship between scan parameters to produce a product ion, precursor ion or constant neutral loss spectrum." [PSI:MS]
is_a: MS:1000479 ! purgatory */

#define MS_LINKED_SCAN_AT_CONSTANT_B_E_ID "MS:1000330"
#define MS_LINKED_SCAN_AT_CONSTANT_B_E_NAME "linked scan at constant b_e"
/* def: "A linked scan at constant B/E may be performed on a sector mass spectrometer that incorporates at least one magnetic sector plus one electric sector. The magnetic field B and the electric field E are scanned simultaneously while the accelerating voltage V is held constant, so as to maintain the ratio of the two fields constant. This linked scan may record a product ion spectrum of dissociation or other reactions occurring in a field free region preceding the two sectors." [PSI:MS]
is_a: MS:1000329 ! linked scan */

#define MS_LINKED_SCAN_AT_CONSTANT_E2_V_ID "MS:1000331"
#define MS_LINKED_SCAN_AT_CONSTANT_E2_V_NAME "Linked Scan at Constant E2_V"
/* def: "A linked scan performed on a sector instrument that incorporates at least one electric sector plus one magnetic sector. The electric sector field, E, and the accelerating voltage, V, are scanned simultaneously, so as to maintain the ratio E2/V at a constant value. This linked scan recordss a product ion spectrum of dissociation or other reactions occurring in a field free region (FFR) preceding the two sectors." [PSI:MS]
is_a: MS:1000329 ! linked scan */

#define MS_LINKED_SCAN_AT_CONSTANT_B2_E_ID "MS:1000332"
#define MS_LINKED_SCAN_AT_CONSTANT_B2_E_NAME "Linked Scan at Constant B2_E"
/* def: "A linked scan performed on a sector mass spectrometer that incorporates at least one electric sector plus one magnetic sector in either order. The accelerating voltage is fixed and the magnetic field, B, and the electric field, E, are scanned simultaneously so as to maintain the ratio B2/E at a constant value. This linked scan records a precursor ion spectrum of dissociation or other reactions occurring in the field free region preceding the two sectors. The term B2/E linked scan is not recommended." [PSI:MS]
is_a: MS:1000329 ! linked scan */

#define MS_MS_MS_IN_TIME_ID "MS:1000334"
#define MS_MS_MS_IN_TIME_NAME "MS_MS in Time"
/* def: "A tandem mass spectrometry method in which product ion spectra are recorded in a single m/z analyzer (such as a Paul Ion Trap or FTMS) in discreet steps over time. Ions in a specific m/z range are selected, dissociated, and the product ions analyzed sequentially in time." [PSI:MS]
is_a: MS:1000445 ! sequential m/z separation method */

#define MS_MS_MS_IN_SPACE_ID "MS:1000335"
#define MS_MS_MS_IN_SPACE_NAME "MS_MS in Space"
/* def: "A tandem mass spectrometry method in which product ion spectra are recorded in m/z analyzers separated in space. Specific m/z separation functions are designed such that in one section of the instrument ions are selected, dissociated in an intermediate region, and the product ions are then transmitted to another analyser for m/z separation and data acquisition." [PSI:MS]
is_a: MS:1000445 ! sequential m/z separation method */

#define MS_NEUTRAL_LOSS_ID "MS:1000336"
#define MS_NEUTRAL_LOSS_NAME "neutral loss"
/* def: "The loss of an uncharged species during a rearrangement process." [PSI:MS]
is_a: MS:1000445 ! sequential m/z separation method
is_a: MS:1001055 ! modification parameters */

#define MS_NTH_GENERATION_PRODUCT_ION_ID "MS:1000337"
#define MS_NTH_GENERATION_PRODUCT_ION_NAME "nth generation product ion"
/* def: "Serial product ions from dissociation of selected precursor ions where n refers to the number of stages of dissociation. The term granddaughter ion is deprecated." [PSI:MS]
synonym: "granddaughter ion" RELATED []
is_a: MS:1000342 ! product ion */

// #define MS_NTH_GENERATION_PRODUCT_ION_SCAN_ID "MS:1000338"
// #define MS_NTH_GENERATION_PRODUCT_ION_SCAN_NAME "nth generation product ion scan"
/* def: "OBSOLETE The specific scan functions or processes that record the appropriate generation of product ion or ions of any m/z selected precursor ions." [PSI:MS]
comment: This former purgatory term was made obsolete.
is_obsolete: true */

// #define MS_NTH_GENERATION_PRODUCT_ION_SPECTRUM_ID "MS:1000339"
// #define MS_NTH_GENERATION_PRODUCT_ION_SPECTRUM_NAME "nth generation product ion spectrum"
/* def: "OBSOLETE The mass spectrum recorded from any mass spectrometer in which the appropriate scan function can be set to record the appropriate generation product ion or ions of m/z selected precursor ions." [PSI:MS]
comment: This term was made obsolete because it was merged with MSn spectrum (MS:1000580).
is_obsolete: true */

#define MS_PRECURSOR_ION_ID "MS:1000340"
#define MS_PRECURSOR_ION_NAME "precursor ion"
/* def: "An ion that reacts to form particular product ions. The reaction can be unimolecular dissociation, ion/molecule reaction, isomerization, or change in charge state. The term parent ion is not recommended." [PSI:MS]
synonym: "parent ion" RELATED []
is_a: MS:1000506 ! ion role */

#define MS_PRECURSOR_ION_SPECTRUM_ID "MS:1000341"
#define MS_PRECURSOR_ION_SPECTRUM_NAME "precursor ion spectrum"
/* def: "Spectrum generated by scanning precursor m/z while monitoring a fixed product m/z." [PSI:MS]
is_a: MS:1000294 ! mass spectrum
is_a: MS:1000524 ! data file content */

#define MS_PRODUCT_ION_ID "MS:1000342"
#define MS_PRODUCT_ION_NAME "product ion"
/* def: "An ion formed as the product of a reaction involving a particular precursor ion. The reaction can be unimolecular dissociation to form fragment ions, an ion/molecule reaction, or simply involve a change in the number of charges. The term fragment ion is deprecated. The term daughter ion is deprecated." [PSI:MS]
synonym: "daughter ion" RELATED []
is_a: MS:1000506 ! ion role */

// #define MS_PRODUCT_ION_SPECTRUM_ID "MS:1000343"
// #define MS_PRODUCT_ION_SPECTRUM_NAME "product ion spectrum"
/* def: "OBSOLETE A mass spectrum recorded from any spectrometer in which the appropriate m/z separation scan function is set to record the product ion or ions of selected precursor ions." [PSI:MS]
comment: This term was made obsolete because it was merged with MSn spectrum (MS:1000580).
is_a: MS:1000294 ! mass spectrum
is_a: MS:1000524 ! data file content
is_obsolete: true */

#define MS_PROGENY_ION_ID "MS:1000344"
#define MS_PROGENY_ION_NAME "progeny ion"
/* def: "A charged product of a series of consecutive reactions that includes product ions, 1st generation product ions, 2nd generation product ions, etc. Given the sequential fragmentation scheme: M1+ -> M2+ -> M3+ -> M4+ -> M5+. M4+ is the precursor ion of M5+, a 1st generation product ion of M3+, a 2nd generation product ion of M2+ and a 3rd generation product ion of M1+." [PSI:MS]
synonym: "Progeny Fragment Ion" EXACT []
is_a: MS:1000508 ! ion chemical type */

#define MS_ARRAY_DETECTOR_ID "MS:1000345"
#define MS_ARRAY_DETECTOR_NAME "array detector"
/* def: "Detector comprising several ion collection elements, arranged in a line or grid where each element is an individual detector." [PSI:MS]
is_a: MS:1000026 ! detector type */

#define MS_CONVERSION_DYNODE_ID "MS:1000346"
#define MS_CONVERSION_DYNODE_NAME "conversion dynode"
/* def: "A surface that is held at high potential such that ions striking the surface produce electrons that are subsequently detected." [PSI:MS]
is_a: MS:1000026 ! detector type */

#define MS_DYNODE_ID "MS:1000347"
#define MS_DYNODE_NAME "dynode"
/* def: "One of a series of electrodes in a photomultiplier tube. Such an arrangement is able to amplify the current emitted by the photocathode." [PSI:MS]
is_a: MS:1000026 ! detector type */

#define MS_FOCAL_PLANE_COLLECTOR_ID "MS:1000348"
#define MS_FOCAL_PLANE_COLLECTOR_NAME "focal plane collector"
/* def: "A detector for spatially disperse ion beams in which all ions simultaneously impinge on the detector plane." [PSI:MS]
is_a: MS:1000026 ! detector type */

#define MS_ION_TO_PHOTON_DETECTOR_ID "MS:1000349"
#define MS_ION_TO_PHOTON_DETECTOR_NAME "ion-to-photon detector"
/* def: "A detector in which ions strike a conversion dynode to produce electrons that in turn strike a phosphor and the resulting photons are detected by a photomultiplier." [PSI:MS]
is_a: MS:1000026 ! detector type */

#define MS_POINT_COLLECTOR_ID "MS:1000350"
#define MS_POINT_COLLECTOR_NAME "point collector"
/* def: "A detector in which the ion beam is focused onto a point and the individual ions arrive sequentially." [PSI:MS]
is_a: MS:1000026 ! detector type */

#define MS_POSTACCELERATION_DETECTOR_ID "MS:1000351"
#define MS_POSTACCELERATION_DETECTOR_NAME "postacceleration detector"
/* def: "A detector in which the charged particles are accelerated to a high velocity and impinge on a conversion dynode, emitting secondary electrons. The electrons are accelerated onto a phosphor screen, which emits photons that are in turn detected using a photomultiplier or other photon detector." [PSI:MS]
is_a: MS:1000026 ! detector type */

// #define MS_SECONDARY_ELECTRON_ID "MS:1000352"
// #define MS_SECONDARY_ELECTRON_NAME "secondary electron"
/* def: "OBSOLETE Electrons that are ejected from a sample surface as a result of bombardment by a primary beam of atoms, ions or photons. WAS IN DETECTOR TYPE. Where should it go." [PSI:MS]
comment: This former purgatory term was made obsolete.
is_obsolete: true */

#define MS_ADDUCT_ION_ID "MS:1000353"
#define MS_ADDUCT_ION_NAME "adduct ion"
/* def: "Ion formed by the interaction of an ion with one or more atoms or molecules to form an ion containing all the constituent atoms of the precursor ion as well as the additional atoms from the associated atoms or molecules." [PSI:MS]
is_a: MS:1000508 ! ion chemical type */

#define MS_AROMATIC_ION_ID "MS:1000354"
#define MS_AROMATIC_ION_NAME "aromatic ion"
/* def: "A planar cyclic ion that obeys the Hckel (4n + 2) rule where n is a positive integer representing the number of conjugated Pi electrons. Charge delocalization leads to greater stability compared to a hypothetical localized structure." [PSI:MS]
is_a: MS:1000508 ! ion chemical type */

#define MS_ANALOG_ION_ID "MS:1000355"
#define MS_ANALOG_ION_NAME "analog ion"
/* def: "Ions that have similar chemical valence, for example the acetyl cation CH3-CO+ and the thioacetyl cation CH3-CS+." [PSI:MS]
is_a: MS:1000508 ! ion chemical type */

#define MS_ANTI_AROMATIC_ION_ID "MS:1000356"
#define MS_ANTI_AROMATIC_ION_NAME "anti-aromatic ion"
/* def: "A planar cyclic ion with 4n ? electrons and is therefore not aromatic." [PSI:MS]
is_a: MS:1000508 ! ion chemical type */

#define MS_CATIONIZED_MOLECULE_ID "MS:1000357"
#define MS_CATIONIZED_MOLECULE_NAME "cationized molecule"
/* def: "An ion formed by the association of a cation with a neutral molecule, M, for example [M+ Na]+ and [M + K]+. The terms quasi-molecular ion and pseudo-molecular ion should not be used." [PSI:MS]
is_a: MS:1000508 ! ion chemical type */

#define MS_CLUSTER_ION_ID "MS:1000358"
#define MS_CLUSTER_ION_NAME "cluster ion"
/* def: "An ion formed by a multi-component atomic or molecular assembly of one or more ions with atoms or molecules, such as [(H20)nH]+, [(NaCl)nNa]+ and [(H3PO3)nHPO3]-." [PSI:MS]
is_a: MS:1000508 ! ion chemical type */

#define MS_CONVENTIONAL_ION_ID "MS:1000359"
#define MS_CONVENTIONAL_ION_NAME "Conventional ion"
/* def: "A radical cation or anion in which the charge site and the unpaired electron spin are both formally located in the same atom or group of atoms, as opposed to the spatially separate electronic configuration of distonic ions. The radical cation of methanol, CH3OH+, in which the charge and spin sites are formally located at the O atom is an example of a conventional ion, whereas .CH2-OH2+ is a distonic ion." [PSI:MS]
is_a: MS:1000508 ! ion chemical type */

#define MS_DIAGNOSTIC_ION_ID "MS:1000360"
#define MS_DIAGNOSTIC_ION_NAME "diagnostic ion"
/* def: "A product ion whose formation reveals structural or compositional information of its precursor. For instance, the phenyl cation in an electron ionization mass spectrum is a diagnostic ion for benzene and derivatives." [PSI:MS]
is_a: MS:1000508 ! ion chemical type */

#define MS_DIMERIC_ION_ID "MS:1000361"
#define MS_DIMERIC_ION_NAME "dimeric ion"
/* def: "An ion formed by ionization of a dimer or by the association of an ion with its neutral counterpart such as [M2]+ or [M-H-M]+." [PSI:MS]
is_a: MS:1000508 ! ion chemical type */

#define MS_DISTONIC_ION_ID "MS:1000362"
#define MS_DISTONIC_ION_NAME "distonic ion"
/* def: "A radical cation or anion in which the charge site and the unpaired electron spin cannot be both formally located in the same atom or group of atoms as it can be with a conventional ion. For example, CH2-OH2+ is a distonic ion whereas the radical cation of methanol, CH3OH+ is a conventional ion." [PSI:MS]
is_a: MS:1000508 ! ion chemical type */

#define MS_ENIUM_ION_ID "MS:1000363"
#define MS_ENIUM_ION_NAME "enium ion"
/* def: "A positively charged lower-valency ion of the nonmetallic elements. The methenium ion is CH3+. Other examples are the oxenium, sulfenium, nitrenium, phosphenium, and halenium ions." [PSI:MS]
is_a: MS:1000508 ! ion chemical type */

// #define MS_FRAGMENT_ION_ID "MS:1000364"
// #define MS_FRAGMENT_ION_NAME "fragment ion"
/* def: "OBSOLETE A product ion that results from the dissociation of a precursor ion." [PSI:MS]
comment: This term was made obsolete because it was replaced by product ion (MS:1000342).
is_obsolete: true */

#define MS_ION__ID "MS:1000365"
#define MS_ION__NAME "ion?"
/* def: "An atomic or molecular species having a net positive or negative electric charge." [PSI:MS]
relationship: part_of MS:1000479 ! purgatory */

#define MS_ISOTOPOLOGUE_ION_ID "MS:1000366"
#define MS_ISOTOPOLOGUE_ION_NAME "Isotopologue ion"
/* def: "An ion that differs only in the isotopic composition of one or more of its constituent atoms. For example CH4+ and CH3D+ or 10BF3 and 11BF3. The term isotopologue is a contraction of isotopic homologue." [PSI:MS]
is_a: MS:1000508 ! ion chemical type */

#define MS_ISOTOPOMERIC_ION_ID "MS:1000367"
#define MS_ISOTOPOMERIC_ION_NAME "Isotopomeric ion"
/* def: "Isomeric ion having the same numbers of each isotopic atom but differing in their positions. Isotopomeric ions can be either configurational isomers in which two atomic isotopes exchange positions or isotopic stereoisomers. The term isotopomer is a shortening of isotopic isomer." [PSI:MS]
is_a: MS:1000508 ! ion chemical type */

#define MS_METASTABLE_ION_ID "MS:1000368"
#define MS_METASTABLE_ION_NAME "metastable ion"
/* def: "An ion that is formed with internal energy higher than the threshold for dissociation but with a lifetime great enough to allow it to exit the ion source and enter the mass spectrometer where it dissociates before detection." [PSI:MS]
is_a: MS:1002343 ! ion stability type */

#define MS_MOLECULAR_ION_ID "MS:1000369"
#define MS_MOLECULAR_ION_NAME "molecular ion"
/* def: "An ion formed by the removal of one or more electrons to form a positive ion or the addition off one or more electrons to form a negative ion." [PSI:MS]
is_a: MS:1000508 ! ion chemical type */

#define MS_NEGATIVE_ION_ID "MS:1000370"
#define MS_NEGATIVE_ION_NAME "negative ion"
/* def: "An atomic or molecular species having a net negative electric charge." [PSI:MS]
is_a: MS:1000508 ! ion chemical type */

#define MS_NON_CLASSICAL_ION_ID "MS:1000371"
#define MS_NON_CLASSICAL_ION_NAME "non-classical ion"
/* def: "Hyper-coordinated carbonium ion such as the penta-coordinated norbornyl cation. Note: Tri-coordinated carbenium ions are termed classical ions." [PSI:MS]
is_a: MS:1000508 ! ion chemical type */

#define MS_ONIUM_ION_ID "MS:1000372"
#define MS_ONIUM_ION_NAME "onium ion"
/* def: "A positively charged hypervalent ion of the nonmetallic elements. Examples are the methonium ion CH5+, the hydrogenonium ion H3+ and the hydronium ion H3O+. Other examples are the carbonium, oxonium, sulfonium, nitronium, diazonium, phosphonium, and halonium ions. Onium ions are not limited to monopositive ions; multiply-charged onium ions exist such as the gitonic (proximal) oxonium dication H4O2+ and the distonic oxonium dication H2O+-CH2-CH2-OH2+." [PSI:MS]
is_a: MS:1000508 ! ion chemical type */

#define MS_PRINCIPAL_ION_ID "MS:1000373"
#define MS_PRINCIPAL_ION_NAME "principal ion"
/* def: "Most abundant ion of an isotope cluster, such as the 11B79Br2 81Br+ ion of m/z 250 of the cluster of isotopologue molecular ions of BBr3. The term principal ion has also been used to describe ions that have been artificially isotopically enriched in one or more positions such as CH3 13CH3+ or CH2D2 +, but those are best defined as isotopologue ions." [PSI:MS]
is_a: MS:1000508 ! ion chemical type */

#define MS_POSITIVE_ION_ID "MS:1000374"
#define MS_POSITIVE_ION_NAME "positive ion"
/* def: "An atomic or molecular species having a net positive electric charge." [PSI:MS]
is_a: MS:1000508 ! ion chemical type */

#define MS_PROTONATED_MOLECULE_ID "MS:1000375"
#define MS_PROTONATED_MOLECULE_NAME "protonated molecule"
/* def: "An ion formed by interaction of a neutral molecule with a proton and represented by the symbol [M + H]+, where M is the neutral molecule. The term 'protonated molecular ion,' 'quasi-molecular ion' and 'pseudo-molecular ion' are not recommended." [PSI:MS]
is_a: MS:1000508 ! ion chemical type */

#define MS_RADICAL_ION_ID "MS:1000376"
#define MS_RADICAL_ION_NAME "radical ion"
/* def: "An ion, either a cation or anion, containing unpaired electrons in its ground state. The unpaired electron is denoted by a superscript dot alongside the superscript symbol for charge, such as for the molecular ion of a molecule M, that is, M+. Radical ions with more than one charge and/or more than one unpaired electron are denoted such as M(2+)(2). Unless the positions of the unpaired electron and charge can be associated with specific atoms, superscript charge designation should be placed before the superscript dot designation." [PSI:MS]
is_a: MS:1000508 ! ion chemical type */

#define MS_REFERENCE_ION_ID "MS:1000377"
#define MS_REFERENCE_ION_NAME "reference ion"
/* def: "A stable ion whose structure is known with certainty. These ions are usually formed by direct ionization of a neutral molecule of known structure and are used to verify by comparison the structure of an unknown ion." [PSI:MS]
is_a: MS:1000508 ! ion chemical type */

#define MS_STABLE_ION_ID "MS:1000378"
#define MS_STABLE_ION_NAME "stable ion"
/* def: "An ion with internal energy sufficiently low that it does not rearrange or dissociate prior to detection in a mass spectrometer." [PSI:MS]
is_a: MS:1002343 ! ion stability type */

#define MS_UNSTABLE_ION_ID "MS:1000379"
#define MS_UNSTABLE_ION_NAME "unstable ion"
/* def: "An ion with sufficient energy to dissociate within the ion source." [PSI:MS]
is_a: MS:1002343 ! ion stability type */

#define MS_ADIABATIC_IONIZATION_ID "MS:1000380"
#define MS_ADIABATIC_IONIZATION_NAME "adiabatic ionization"
/* def: "A process whereby an electron is removed from an atom, ion, or molecule to produce an ion in its lowest energy state." [PSI:MS]
is_a: MS:1000008 ! ionization type */

#define MS_ASSOCIATIVE_IONIZATION_ID "MS:1000381"
#define MS_ASSOCIATIVE_IONIZATION_NAME "associative ionization"
/* def: "An ionization process in which two excited atoms or molecules react to form a single positive ion and an electron." [PSI:MS]
is_a: MS:1000008 ! ionization type */

#define MS_ATMOSPHERIC_PRESSURE_PHOTOIONIZATION_ID "MS:1000382"
#define MS_ATMOSPHERIC_PRESSURE_PHOTOIONIZATION_NAME "atmospheric pressure photoionization"
/* def: "Atmospheric pressure chemical ionization in which the reactant ions are generated by photo-ionization." [PSI:MS]
is_a: MS:1000240 ! atmospheric pressure ionization */

#define MS_AUTODETACHMENT_ID "MS:1000383"
#define MS_AUTODETACHMENT_NAME "autodetachment"
/* def: "The formation of a neutral when a negative ion in a disrtete state with an energy greater than the detachment threshold loses an electron spontaneously without further interaction with an energy source." [PSI:MS]
is_a: MS:1000008 ! ionization type */

#define MS_AUTOIONIZATION_ID "MS:1000384"
#define MS_AUTOIONIZATION_NAME "autoionization"
/* def: "The formation of an ion when an atom or molecule in a discrete state with an energy greater than the ionization threshold loses an electron spontaneously without further interaction with an energy source." [PSI:MS]
is_a: MS:1000008 ! ionization type */

#define MS_CHARGE_EXCHANGE_IONIZATION_ID "MS:1000385"
#define MS_CHARGE_EXCHANGE_IONIZATION_NAME "charge exchange ionization"
/* def: "The interaction of an ion with an atom or molecule in which the charge on the ion is transferred to the neutral without the dissociation of either. Synonymous with charge transfer ionization." [PSI:MS]
is_a: MS:1000008 ! ionization type */

#define MS_CHEMI_IONIZATION_ID "MS:1000386"
#define MS_CHEMI_IONIZATION_NAME "chemi-ionization"
/* def: "The reaction of a neutral molecule with an internally excited molecule to form an ion. Note that this term is not synonymous with chemical ionization." [PSI:MS]
is_a: MS:1000008 ! ionization type */

#define MS_DESORPTION_IONIZATION_ON_SILICON_ID "MS:1000387"
#define MS_DESORPTION_IONIZATION_ON_SILICON_NAME "desorption_ionization on silicon"
/* def: "The formation of ions by laser desorption ionization of a sample deposited on a porous silicon surface." [PSI:MS]
is_a: MS:1000247 ! desorption ionization */

#define MS_DISSOCIATIVE_IONIZATION_ID "MS:1000388"
#define MS_DISSOCIATIVE_IONIZATION_NAME "dissociative ionization"
/* def: "The reaction of a gas-phase molecule that results in its decomposition to form products, one of which is an ion." [PSI:MS]
is_a: MS:1000008 ! ionization type */

#define MS_ELECTRON_IONIZATION_ID "MS:1000389"
#define MS_ELECTRON_IONIZATION_NAME "electron ionization"
/* def: "The ionization of an atom or molecule by electrons that are typically accelerated to energies between 50 and 150 eV. Usually 70 eV electrons are used to produce positive ions. The term 'electron impact' is not recommended." [PSI:MS]
is_a: MS:1000008 ! ionization type */

#define MS_ION_DESOLVATION_ID "MS:1000390"
#define MS_ION_DESOLVATION_NAME "ion desolvation"
/* def: "The removal of solvent molecules clustered around a gas-phase ion by means of heating and/or collisions with gas molecules." [PSI:MS]
is_a: MS:1000437 ! ion reaction */

#define MS_ION_PAIR_FORMATION_ID "MS:1000391"
#define MS_ION_PAIR_FORMATION_NAME "ion-pair formation"
/* def: "The reaction of a molecule to form both a positive ion and negative ion fragment among the products." [PSI:MS]
is_a: MS:1000437 ! ion reaction */

#define MS_IONIZATION_EFFICIENCY_ID "MS:1000392"
#define MS_IONIZATION_EFFICIENCY_NAME "ionization efficiency"
/* def: "The ratio of the number of ions formed to the number of electrons, molecules or photons used." [PSI:MS]
xref: value-type:xsd\:float "The allowed value-type for this CV term."
is_a: MS:1000482 ! source attribute */

#define MS_LASER_DESORPTION_IONIZATION_ID "MS:1000393"
#define MS_LASER_DESORPTION_IONIZATION_NAME "laser desorption ionization"
/* def: "The formation of gas-phase ions by the interaction of a pulsed laser with a solid or liquid material." [PSI:MS]
is_a: MS:1000247 ! desorption ionization */

#define MS_LIQUID_SECONDARY_IONIZATION_ID "MS:1000395"
#define MS_LIQUID_SECONDARY_IONIZATION_NAME "liquid secondary ionization"
/* def: "The ionization of any species by the interaction of a focused beam of ions with a sample that is dissolved in a solvent matrix. See also fast atom bombardment and secondary ionization." [PSI:MS]
is_a: MS:1000008 ! ionization type */

#define MS_MEMBRANE_INLET_ID "MS:1000396"
#define MS_MEMBRANE_INLET_NAME "membrane inlet"
/* def: "A semi-permeable membrane separator that permits the passage of gas sample directly to the mass spectrometer ion source." [PSI:MS]
is_a: MS:1000007 ! inlet type */

#define MS_MICROELECTROSPRAY_ID "MS:1000397"
#define MS_MICROELECTROSPRAY_NAME "microelectrospray"
/* def: "Electrospray ionization at a solvent flow rate of 300-800 nL/min where the flow is a result of a mechanical pump. See nanaoelectrospray." [PSI:MS]
is_a: MS:1000073 ! electrospray ionization */

#define MS_NANOELECTROSPRAY_ID "MS:1000398"
#define MS_NANOELECTROSPRAY_NAME "nanoelectrospray"
/* def: "Electrospray ionization at a flow rate less than ~25 nL/min. Nanoelectrospray is synonymous with nanospray. The flow is dependent on the potenial on the tip of the electrospray needle and/or a gas presure to push the sample through the needle. See also electrospray ionization and microelectrospray." [PSI:MS]
synonym: "nanospray" EXACT []
is_a: MS:1000073 ! electrospray ionization */

#define MS_PENNING_IONIZATION_ID "MS:1000399"
#define MS_PENNING_IONIZATION_NAME "penning ionization"
/* def: "Ionization that occurs through the interaction of two or more neutral gaseous species, at least one of which is internally excited." [PSI:MS]
is_a: MS:1000008 ! ionization type */

#define MS_PLASMA_DESORPTION_IONIZATION_ID "MS:1000400"
#define MS_PLASMA_DESORPTION_IONIZATION_NAME "plasma desorption ionization"
/* def: "The ionization of material in a solid sample by bombarding it with ionic or neutral atoms formed as a result of the fission of a suitable nuclide, typically 252Cf. Synonymous with fission fragment ionization." [PSI:MS]
is_a: MS:1000008 ! ionization type */

#define MS_PRE_IONIZATION_STATE_ID "MS:1000401"
#define MS_PRE_IONIZATION_STATE_NAME "pre-ionization state"
/* def: "An electronic state capable of undergoing auto-Ionization." [PSI:MS]
is_a: MS:1000437 ! ion reaction */

#define MS_SECONDARY_IONIZATION_ID "MS:1000402"
#define MS_SECONDARY_IONIZATION_NAME "secondary ionization"
/* def: "The process in which ions are ejected from a sample surface as a result of bombardment by a primary beam of atoms or ions." [PSI:MS]
is_a: MS:1000008 ! ionization type */

#define MS_SOFT_IONIZATION_ID "MS:1000403"
#define MS_SOFT_IONIZATION_NAME "soft ionization"
/* def: "The formation of gas-phase ions without extensive fragmentation." [PSI:MS]
is_a: MS:1000008 ! ionization type */

#define MS_SPARK_IONIZATION_ID "MS:1000404"
#define MS_SPARK_IONIZATION_NAME "spark ionization"
/* def: "The formation of ions from a solid material by an intermittent electrical discharge." [PSI:MS]
is_a: MS:1000008 ! ionization type */

#define MS_SURFACE_ASSISTED_LASER_DESORPTION_IONIZATION_ID "MS:1000405"
#define MS_SURFACE_ASSISTED_LASER_DESORPTION_IONIZATION_NAME "surface-assisted laser desorption ionization"
/* def: "The formation of gas-phase ions from molecules that are deposited on a particular surface substrate that is irradiated with a pulsed laser. See also matrix-assisted laser desorption ionization." [PSI:MS]
is_a: MS:1000247 ! desorption ionization */

#define MS_SURFACE_IONIZATION_ID "MS:1000406"
#define MS_SURFACE_IONIZATION_NAME "surface ionization"
/* def: "The ionization of a neutral species when it interacts with a solid surface with an appropriate work function and temperature." [PSI:MS]
is_a: MS:1000008 ! ionization type */

#define MS_THERMAL_IONIZATION_ID "MS:1000407"
#define MS_THERMAL_IONIZATION_NAME "thermal ionization"
/* def: "The ionization of a neutral species through contact with a high temperature surface." [PSI:MS]
is_a: MS:1000008 ! ionization type */

#define MS_VERTICAL_IONIZATION_ID "MS:1000408"
#define MS_VERTICAL_IONIZATION_NAME "vertical ionization"
/* def: "A process in which an electron is removed from or added to a molecule without a change in the positions of the atoms. The resulting ion is typically in an excited vibrational state." [PSI:MS]
is_a: MS:1000008 ! ionization type */

#define MS_ASSOCIATION_REACTION_ID "MS:1000409"
#define MS_ASSOCIATION_REACTION_NAME "association reaction"
/* def: "The reaction of an ion with a neutral species in which the reactants combine to form a single ion." [PSI:MS]
is_a: MS:1000437 ! ion reaction */

#define MS_ALPHA_CLEAVAGE_ID "MS:1000410"
#define MS_ALPHA_CLEAVAGE_NAME "alpha-cleavage"
/* def: "A homolytic cleavage where the bond fission occurs between at the atom adjacent to the atom at the apparent charge site and an atom removed from the aparent charge site by two bonds." [PSI:MS]
is_a: MS:1000437 ! ion reaction */

#define MS_BETA_CLEAVAGE_ID "MS:1000411"
#define MS_BETA_CLEAVAGE_NAME "beta-cleavage"
/* def: "A homolytic cleavage where the bond fission occurs between at an atom removed from the apparent charge site atom by two bonds and an atom adjacent to that atom and removed from the aparent charge site by three bonds." [PSI:MS]
is_a: MS:1000437 ! ion reaction */

#define MS_BUFFER_GAS_ID "MS:1000412"
#define MS_BUFFER_GAS_NAME "buffer gas"
/* def: "An inert gas used for collisional deactivation of internally excited ions." [PSI:MS]
xref: value-type:xsd\:string "The allowed value-type for this CV term."
is_a: MS:1000510 ! precursor activation attribute */

#define MS_CHARGE_INDUCED_FRAGMENTATION_ID "MS:1000413"
#define MS_CHARGE_INDUCED_FRAGMENTATION_NAME "charge-induced fragmentation"
/* def: "Fragmentation of an odd electron ion in which the cleaved bond is adjacent to the apparent charge site. Synonymous with charge mediated fragmentation." [PSI:MS]
is_a: MS:1000437 ! ion reaction */

#define MS_CHARGE_INVERSION_REACTION_ID "MS:1000414"
#define MS_CHARGE_INVERSION_REACTION_NAME "charge inversion reaction"
/* def: "Reaction of an ion with a neutral species in which the charge on the product ion is reversed in sign with respect to the reactant ion." [PSI:MS]
is_a: MS:1000437 ! ion reaction */

#define MS_CHARGE_PERMUTATION_REACTION_ID "MS:1000415"
#define MS_CHARGE_PERMUTATION_REACTION_NAME "charge permutation reaction"
/* def: "The reaction of an ion with a neutral species with a resulting change in the magnitude or sign of the charge on the reactant ion." [PSI:MS]
is_a: MS:1000437 ! ion reaction */

#define MS_CHARGE_STRIPPING_REACTION_ID "MS:1000416"
#define MS_CHARGE_STRIPPING_REACTION_NAME "charge stripping reaction"
/* def: "Reaction of a positive ion with a neutral species in which the positive charge on the product ion is greater than that on the reactant ion." [PSI:MS]
is_a: MS:1000437 ! ion reaction */

#define MS_CHARGE_TRANSFER_REACTION_ID "MS:1000417"
#define MS_CHARGE_TRANSFER_REACTION_NAME "charge transfer reaction"
/* def: "The reaction of an ion with a neutral species in which some or all of the charge of the reactant ion is transferred to the neutral species." [PSI:MS]
is_a: MS:1000437 ! ion reaction */

#define MS_COLLISIONAL_EXCITATION_ID "MS:1000418"
#define MS_COLLISIONAL_EXCITATION_NAME "collisional excitation"
/* def: "The reaction of an ion with a neutral species in which the translational energy of the collision is converted into internal energy of the ion." [PSI:MS]
is_a: MS:1000437 ! ion reaction */

#define MS_COLLISION_GAS_ID "MS:1000419"
#define MS_COLLISION_GAS_NAME "collision gas"
/* def: "An inert gas used for collisional excitation. The term target gas is not recommended." [PSI:MS]
xref: value-type:xsd\:string "The allowed value-type for this CV term."
is_a: MS:1000510 ! precursor activation attribute */

#define MS_HETEROLYTIC_CLEAVAGE_ID "MS:1000420"
#define MS_HETEROLYTIC_CLEAVAGE_NAME "heterolytic cleavage"
/* def: "Fragmentation of a molecule or ion in which both electrons forming the single bond that is broken remain on one of the atoms that were originally bonded. This term is synonymous with heterolysis." [PSI:MS]
synonym: "heterolysis" RELATED []
is_a: MS:1000437 ! ion reaction */

#define MS_HIGH_ENERGY_COLLISION_ID "MS:1000421"
#define MS_HIGH_ENERGY_COLLISION_NAME "high energy collision"
/* def: "Collision-induced dissociation process wherein the projectile ion has laboratory-frame translational energy higher than 1 keV." [PSI:MS]
is_a: MS:1000437 ! ion reaction */

#define MS_HIGH_ENERGY_COLLISION_INDUCED_DISSOCIATION_ID "MS:1000422"
#define MS_HIGH_ENERGY_COLLISION_INDUCED_DISSOCIATION_NAME "high-energy collision-induced dissociation"
/* def: "A collision-induced dissociation process wherein the projectile ion has the translational energy higher than approximately 1000 eV." [PSI:MS]
synonym: "HCD" EXACT []
is_a: MS:1000044 ! dissociation method */

#define MS_HOMOLYTIC_CLEAVAGE_ID "MS:1000423"
#define MS_HOMOLYTIC_CLEAVAGE_NAME "homolytic cleavage"
/* def: "Fragmentation of an odd electron ion that results from one of a pair of electrons that form a bond between two atoms moving to form a pair with the odd electron on the atom at the apparent charge site. Fragmentation results in the formation of an even electron ion and a radical. This reaction involves the movement of a single electron and is symbolized by a single-barbed arrow. Synonymous with Homolysis." [PSI:MS]
synonym: "homolysis" RELATED []
is_a: MS:1000437 ! ion reaction */

#define MS_HYDROGEN_DEUTERIUM_EXCHANGE_ID "MS:1000424"
#define MS_HYDROGEN_DEUTERIUM_EXCHANGE_NAME "hydrogen_deuterium exchange"
/* def: "Exchange of hydrogen atoms with deuterium atoms in a molecule or pre-formed ion in solution prior to introduction into a mass spectrometer, or by reaction of an ion with a deuterated collision gas inside a mass spectrometer." [PSI:MS]
is_a: MS:1000437 ! ion reaction */

// #define MS_ION_ENERGY_LOSS_SPECTRUM_ID "MS:1000425"
// #define MS_ION_ENERGY_LOSS_SPECTRUM_NAME "ion energy loss spectrum"
/* def: "OBSOLETE A plot of the relative abundance of a beam or other collection of ions as a function their loss of translational energy in reactions with neutral species." [PSI:MS]
comment: This former purgatory term was made obsolete.
is_obsolete: true */

#define MS_IONIZING_COLLISION_ID "MS:1000426"
#define MS_IONIZING_COLLISION_NAME "ionizing collision"
/* def: "The reaction of an ion with a neutral species in which one or more electrons are removed from either the ion or neutral." [PSI:MS]
is_a: MS:1000437 ! ion reaction */

#define MS_ION_MOLECULE_REACTION_ID "MS:1000427"
#define MS_ION_MOLECULE_REACTION_NAME "ion_molecule reaction"
/* def: "The reaction of an ion with a neutral molecule. The term ion-molecule reaction is not recommended because the hyphen suggests a single species that is that is both an ion and a molecule." [PSI:MS]
is_a: MS:1000437 ! ion reaction */

#define MS_ION_NEUTRAL_COMPLEX_ID "MS:1000428"
#define MS_ION_NEUTRAL_COMPLEX_NAME "ion_neutral complex"
/* def: "A particular type of transition state that lies between precursor and product ions on the reaction coordinate of some ion reactions." [PSI:MS]
is_a: MS:1000437 ! ion reaction */

#define MS_ION_NEUTRAL_SPECIES_REACTION_ID "MS:1000429"
#define MS_ION_NEUTRAL_SPECIES_REACTION_NAME "ion_neutral species reaction"
/* def: "A process wherein a charged species interacts with a neutral reactant to produce either chemically different species or changes in the internal energy of one or both of the reactants." [PSI:MS]
is_a: MS:1000437 ! ion reaction */

#define MS_ION_NEUTRAL_SPECIES_EXCHANGE_REACTION_ID "MS:1000430"
#define MS_ION_NEUTRAL_SPECIES_EXCHANGE_REACTION_NAME "ion_neutral species exchange reaction"
/* def: "In this reaction an association reaction is accompanied by the subsequent or simultaneous liberation of a different neutral species as a product." [PSI:MS]
is_a: MS:1000437 ! ion reaction */

#define MS_KINETIC_METHOD_ID "MS:1000431"
#define MS_KINETIC_METHOD_NAME "kinetic method"
/* def: "An approach to determination of ion thermodynamic quantities by a bracketing procedure in which the relative probabilities of competing ion fragmentations are measured via the relative abundances of the reaction products. The extended kinetic method takes the associated entropy changes into account." [PSI:MS]
is_a: MS:1000437 ! ion reaction */

#define MS_LOW_ENERGY_COLLISIONS_ID "MS:1000432"
#define MS_LOW_ENERGY_COLLISIONS_NAME "low energy collisions"
/* def: "A collision between an ion and neutral species with translational energy approximately 1000 eV or lower." [PSI:MS]
is_a: MS:1000437 ! ion reaction */

#define MS_LOW_ENERGY_COLLISION_INDUCED_DISSOCIATION_ID "MS:1000433"
#define MS_LOW_ENERGY_COLLISION_INDUCED_DISSOCIATION_NAME "low-energy collision-induced dissociation"
/* def: "A collision-induced dissociation process wherein the precursor ion has the translational energy lower than approximately 1000 eV. This process typically requires multiple collisions and the collisional excitation is cumulative." [PSI:MS]
is_a: MS:1000044 ! dissociation method */

#define MS_MCLAFFERTY_REARRANGEMENT_ID "MS:1000434"
#define MS_MCLAFFERTY_REARRANGEMENT_NAME "McLafferty Rearrangement"
/* def: "A dissociation reaction triggered by transfer of a hydrogen atom via a 6-member transition state to the formal radical/charge site from a carbon atom four atoms removed from the charge/radical site (the gamma-carbon); subsequent rearrangement of electron density leads to expulsion of an olefin molecule. This term was originally applied to ketone ions where the charge/radical site is the carbonyl oxygen, but it is now more widely applied." [PSI:MS]
is_a: MS:1000437 ! ion reaction */

#define MS_PHOTODISSOCIATION_ID "MS:1000435"
#define MS_PHOTODISSOCIATION_NAME "photodissociation"
/* def: "A process wherein the reactant ion is dissociated as a result of absorption of one or more photons." [PSI:MS]
synonym: "multiphoton dissociation" EXACT []
synonym: "MPD" EXACT []
is_a: MS:1000044 ! dissociation method */

#define MS_PARTIAL_CHARGE_TRANSFER_REACTION_ID "MS:1000436"
#define MS_PARTIAL_CHARGE_TRANSFER_REACTION_NAME "partial charge transfer reaction"
/* def: "Reaction of an ion with a neutral species in which some but not all of the ion charge is transferred to the neutral." [PSI:MS]
is_a: MS:1000437 ! ion reaction */

#define MS_ION_REACTION_ID "MS:1000437"
#define MS_ION_REACTION_NAME "ion reaction"
/* def: "Chemical transformation involving an ion." [PSI:MS]
relationship: part_of MS:1000365 ! ion? */

#define MS_SUPERELASTIC_COLLISION_ID "MS:1000438"
#define MS_SUPERELASTIC_COLLISION_NAME "superelastic collision"
/* def: "Collision in which the translational energy of the fast-moving collision partner is increased at the expense of internal energy of one or both collision partners." [PSI:MS]
is_a: MS:1000437 ! ion reaction */

#define MS_SURFACE_INDUCED_REACTION_ID "MS:1000439"
#define MS_SURFACE_INDUCED_REACTION_NAME "surface-induced reaction"
/* def: "A process wherein a reactant ion interacts with a surface to produce either chemically different species or a change in the internal energy of the reactant ion." [PSI:MS]
is_a: MS:1000437 ! ion reaction */

#define MS_UNIMOLECULAR_DISSOCIATION_ID "MS:1000440"
#define MS_UNIMOLECULAR_DISSOCIATION_NAME "unimolecular dissociation"
/* def: "Fragmentation reaction in which the molecularity is treated as one, irrespective of whether the dissociative state is that of a metastable ion produced in the ion source or results from collisional excitation of a stable ion." [PSI:MS]
is_a: MS:1000437 ! ion reaction */

#define MS_SCAN_ID "MS:1000441"
#define MS_SCAN_NAME "scan"
/* def: "Function or process of the mass spectrometer where it records a spectrum." [PSI:MS]
relationship: part_of MS:1001458 ! spectrum generation information */

#define MS_SPECTRUM_ID "MS:1000442"
#define MS_SPECTRUM_NAME "spectrum"
/* def: "A mass spectrum is an intensity vs m/z (mass-to-charge ratio) plot representing a chemical analysis." [PSI:MS]
relationship: part_of MS:1001458 ! spectrum generation information */

#define MS_MASS_ANALYZER_TYPE_ID "MS:1000443"
#define MS_MASS_ANALYZER_TYPE_NAME "mass analyzer type"
/* def: "Mass analyzer separates the ions according to their mass-to-charge ratio." [PSI:MS]
relationship: part_of MS:1000451 ! mass analyzer */

#define MS_M_Z_SEPARATION_METHOD_ID "MS:1000444"
#define MS_M_Z_SEPARATION_METHOD_NAME "m/z Separation Method"
/* def: "Mass/charge separation Method." [PSI:MS]
relationship: part_of MS:1000479 ! purgatory */

#define MS_SEQUENTIAL_M_Z_SEPARATION_METHOD_ID "MS:1000445"
#define MS_SEQUENTIAL_M_Z_SEPARATION_METHOD_NAME "sequential m/z separation method"
/* def: "Sequential m/z separation method." [PSI:MS]
relationship: part_of MS:1000479 ! purgatory */

#define MS_FAST_ION_BOMBARDMENT_ID "MS:1000446"
#define MS_FAST_ION_BOMBARDMENT_NAME "fast ion bombardment"
/* def: "The ionization of any species by the interaction of a focused beam of ions having a translational energy of several thousand eV with a solid sample." [PSI:MS]
synonym: "FIB" EXACT []
is_a: MS:1000008 ! ionization type */

#define MS_LTQ_ID "MS:1000447"
#define MS_LTQ_NAME "LTQ"
/* def: "Finnigan LTQ MS." [PSI:MS]
is_a: MS:1000494 ! Thermo Scientific instrument model */

#define MS_LTQ_FT_ID "MS:1000448"
#define MS_LTQ_FT_NAME "LTQ FT"
/* def: "Finnigan LTQ FT MS." [PSI:MS]
is_a: MS:1000494 ! Thermo Scientific instrument model */

#define MS_LTQ_ORBITRAP_ID "MS:1000449"
#define MS_LTQ_ORBITRAP_NAME "LTQ Orbitrap"
/* def: "Finnigan LTQ Orbitrap MS." [PSI:MS]
is_a: MS:1000494 ! Thermo Scientific instrument model */

#define MS_LXQ_ID "MS:1000450"
#define MS_LXQ_NAME "LXQ"
/* def: "Finnigan LXQ MS." [PSI:MS]
is_a: MS:1000494 ! Thermo Scientific instrument model */

#define MS_MASS_ANALYZER_ID "MS:1000451"
#define MS_MASS_ANALYZER_NAME "mass analyzer"
/* def: "Terms used to describe the Analyzer." [PSI:MS]
synonym: "analyzer" EXACT []
relationship: part_of MS:1000463 ! instrument */

#define MS_DATA_TRANSFORMATION_ID "MS:1000452"
#define MS_DATA_TRANSFORMATION_NAME "data transformation"
/* def: "Terms used to describe types of data processing." [PSI:MS]
synonym: "data processing" EXACT []
relationship: part_of MS:1001458 ! spectrum generation information */

#define MS_DETECTOR_ID "MS:1000453"
#define MS_DETECTOR_NAME "detector"
/* def: "The device that detects ions." [PSI:MS]
relationship: part_of MS:1000463 ! instrument */

// #define MS_INSTRUMENT_ADDITIONAL_DESCRIPTION_ID "MS:1000454"
// #define MS_INSTRUMENT_ADDITIONAL_DESCRIPTION_NAME "instrument additional description"
/* def: "OBSOLETE Additional terms to describe the instrument as outlined in the mass spec doc, Appendix 1, section 1.5." [PSI:MS]
comment: This former purgatory term was made obsolete.
is_obsolete: true */

#define MS_ION_SELECTION_ATTRIBUTE_ID "MS:1000455"
#define MS_ION_SELECTION_ATTRIBUTE_NAME "ion selection attribute"
/* def: "Ion selection properties that are associated with a value." [PSI:MS]
is_a: MS:1000547 ! object attribute
relationship: part_of MS:1000442 ! spectrum */

#define MS_PRECURSOR_ACTIVATION_ID "MS:1000456"
#define MS_PRECURSOR_ACTIVATION_NAME "precursor activation"
/* def: "Terms to describe the precursor activation." [PSI:MS]
synonym: "activation" EXACT []
relationship: part_of MS:1000442 ! spectrum */

#define MS_SAMPLE_ID "MS:1000457"
#define MS_SAMPLE_NAME "sample"
/* def: "Terms to describe the sample." [PSI:MS]
relationship: part_of MS:1001458 ! spectrum generation information */

#define MS_SOURCE_ID "MS:1000458"
#define MS_SOURCE_NAME "source"
/* def: "Terms to describe the source." [PSI:MS]
relationship: part_of MS:1000463 ! instrument */

#define MS_SPECTRUM_INSTRUMENT_DESCRIPTION_ID "MS:1000459"
#define MS_SPECTRUM_INSTRUMENT_DESCRIPTION_NAME "spectrum instrument description"
/* def: "Terms used to describe the spectrum." [PSI:MS]
relationship: part_of MS:1000479 ! purgatory */

// #define MS_UNIT_ID "MS:1000460"
// #define MS_UNIT_NAME "unit"
/* def: "OBSOLETE Terms to describe units." [PSI:MS]
comment: This term was made obsolete because it was redundant with the Unit Ontology term unit (UO:0000000).
relationship: part_of MS:1001458 ! spectrum generation information
is_obsolete: true */

// #define MS_ADDITIONAL_DESCRIPTION_ID "MS:1000461"
// #define MS_ADDITIONAL_DESCRIPTION_NAME "additional description"
/* def: "OBSOLETE Terms to describe Additional." [PSI:MS]
comment: This former purgatory term was made obsolete.
is_obsolete: true */

#define MS_ION_OPTICS_ID "MS:1000462"
#define MS_ION_OPTICS_NAME "ion optics"
/* def: "Device used in the construction of a mass spectrometer to focus, contain or otherwise manipulate ions." [PSI:MS]
relationship: part_of MS:1000463 ! instrument */

#define MS_INSTRUMENT_ID "MS:1000463"
#define MS_INSTRUMENT_NAME "instrument"
/* def: "Description of the instrument or the mass spectrometer." [PSI:MS]
synonym: "instrument configuration" EXACT []
relationship: part_of MS:1001458 ! spectrum generation information */

// #define MS_MASS_UNIT_ID "MS:1000464"
// #define MS_MASS_UNIT_NAME "mass unit"
/* def: "OBSOLETE A unit of measurement for mass." [PSI:MS]
comment: This term was made obsolete because it was redundant with Unit Ontology mass unit (UO:0000002).
is_obsolete: true */

#define MS_SCAN_POLARITY_ID "MS:1000465"
#define MS_SCAN_POLARITY_NAME "scan polarity"
/* def: "An acquisition mode to which specifies weather polarity is negative, positive or alternating." [PSI:MS]
relationship: part_of MS:1000441 ! scan */

// #define MS_ALTERNATING_ID "MS:1000466"
// #define MS_ALTERNATING_NAME "alternating"
/* def: "OBSOLETE Alternating." [PSI:MS]
comment: This term was made obsolete because .
is_obsolete: true */

#define MS__1200_SERIES_LC_MSD_SL_ID "MS:1000467"
#define MS__1200_SERIES_LC_MSD_SL_NAME "1200 series LC_MSD SL"
/* def: "The 1200 Series LC/MSD SL ion trap belongs to the Agilent LC/MSD ion trap family. It provides fast polarity switching and multisignal data acquisition capabilities in a single run while also providing 5 stages of automated data dependent MS/MS and 11 stages of manual MS/MS." [PSI:MS]
is_a: MS:1000490 ! Agilent instrument model */

#define MS__6110_QUADRUPOLE_LC_MS_ID "MS:1000468"
#define MS__6110_QUADRUPOLE_LC_MS_NAME "6110 Quadrupole LC_MS"
/* def: "The 6110 Quadrupole LC/MS system is a Agilent liquid chromatography instrument combined with an entry level single quadrupole mass spectrometer from the 6100 Series of Agilent quadrupole mass spectrometers. 6110 Quadrupole mass spectrometer has m/z range of 10-1500 and 2500 u/s scan speed. It proves useful for wide range of SIM quantitative applications." [PSI:MS]
is_a: MS:1000490 ! Agilent instrument model */

#define MS__6120_QUADRUPOLE_LC_MS_ID "MS:1000469"
#define MS__6120_QUADRUPOLE_LC_MS_NAME "6120 Quadrupole LC_MS"
/* def: "The 6120 Quadrupole LC/MS system is a Agilent liquid chromatography instrument combined with a single quadrupole mass spectrometer from the 6100 Series of Agilent mass spectrometers. 6120 quadrupole mass spectrometer has m/z range of 10-1500, 2500 u/s scan speed and utilizes multiple signal acquisition." [PSI:MS]
is_a: MS:1000490 ! Agilent instrument model */

#define MS__6130_QUADRUPOLE_LC_MS_ID "MS:1000470"
#define MS__6130_QUADRUPOLE_LC_MS_NAME "6130 Quadrupole LC_MS"
/* def: "The 6130 Quadrupole LC/MS system is a Agilent liquid chromatography instrument combined with a single quadrupole mass spectrometer from the 6100 series of Agilent mass spectrometers. The 6130 quadrupole mass spectrometer has m/z range of 2-3000, 2500 u/s scan speed in standard mode and 5250 u/s speed in fast-scan mode. It also uses multiple signal acquisition." [PSI:MS]
is_a: MS:1000490 ! Agilent instrument model */

#define MS__6140_QUADRUPOLE_LC_MS_ID "MS:1000471"
#define MS__6140_QUADRUPOLE_LC_MS_NAME "6140 Quadrupole LC_MS"
/* def: "The 6140 Quadrupole LC/MS system is a Agilent liquid chromatography instrument combined with a single quadrupole mass spectrometer from the 6100 Series of Agilent quadrupole mass spectrometers. 6140 Quadrupole mass spectrometer has m/z range of 10-1350, 2500 u/s scan speed in standard mode and 10000 u/s speed in fast-scan mode. It also uses multiple signal acquisition." [PSI:MS]
is_a: MS:1000490 ! Agilent instrument model */

#define MS__6210_TIME_OF_FLIGHT_LC_MS_ID "MS:1000472"
#define MS__6210_TIME_OF_FLIGHT_LC_MS_NAME "6210 Time-of-Flight LC_MS"
/* def: "The 6210 Time-of-Flight LC/MS is a Agilent liquid chromatography instrument combined with a Agilent time of flight mass spectrometer. This time of flight mass spectrometer has a m/z range of 50-12000, mass accuracy of less than 2 ppm and resolution greater than 13,000 at m/z 2722. It has multiple ion sources and can be used with multimode ion sources." [PSI:MS]
is_a: MS:1000490 ! Agilent instrument model */

#define MS__6310_ION_TRAP_LC_MS_ID "MS:1000473"
#define MS__6310_ION_TRAP_LC_MS_NAME "6310 Ion Trap LC_MS"
/* def: "The 6310 Ion Trap LC/MS is a Agilent liquid chromatography instrument combined with a 6300 series Agilent ion trap. It has a mass range of 50-2200 between 0.6 to 0.35 resolution and mass range of 200-4000 with resolution of 3-4. The scan speed varies from 1650-27000 for the respective mass ranges." [PSI:MS]
is_a: MS:1000490 ! Agilent instrument model */

#define MS__6320_ION_TRAP_LC_MS_ID "MS:1000474"
#define MS__6320_ION_TRAP_LC_MS_NAME "6320 Ion Trap LC_MS"
/* def: "The 6320 Ion Trap LC/MS is a Agilent liquid chromatography instrument combined with a 6300 series Agilent ion trap. It has a mass range of 50-2200 between 0.6 to 0.25 resolution and mass range of 200-4000 with resolution of less than 3. The scan speed varies from 1650-27000 for the respective mass ranges." [PSI:MS]
is_a: MS:1000490 ! Agilent instrument model */

#define MS__6330_ION_TRAP_LC_MS_ID "MS:1000475"
#define MS__6330_ION_TRAP_LC_MS_NAME "6330 Ion Trap LC_MS"
/* def: "The 6330 Ion Trap LC/MS is a Agilent liquid chromatography instrument combined with a 6300 series Agilent ion trap. It has a mass range of 50-2200 between 0.6 to 0.25 resolution and mass range of 200-4000 with resolution of less than 3. The scan speed varies from 1650-27000 for the respective mass ranges." [PSI:MS]
is_a: MS:1000490 ! Agilent instrument model */

#define MS__6340_ION_TRAP_LC_MS_ID "MS:1000476"
#define MS__6340_ION_TRAP_LC_MS_NAME "6340 Ion Trap LC_MS"
/* def: "The 6340 Ion Trap LC/MS is a Agilent liquid chromatography instrument combined with a 6300 series Agilent ion trap. It has a mass range of 50-2200 between 0.6 to 0.25 resolution and mass range of 200-4000 with resolution of less than 3. The scan speed varies from 1650-27000 for the respective mass ranges." [PSI:MS]
is_a: MS:1000490 ! Agilent instrument model */

#define MS__6410_TRIPLE_QUADRUPOLE_LC_MS_ID "MS:1000477"
#define MS__6410_TRIPLE_QUADRUPOLE_LC_MS_NAME "6410 Triple Quadrupole LC_MS"
/* def: "The 6410 Quadrupole LC/MS system is a Agilent liquid chromatography instrument combined with a Agilent triple quadrupole mass spectrometer. Mass range of the mass spectrometer is 15-1650 m/z, resolution is at three settings of 0.7 u (unit), 1.2 u (wide) and 2.5 u (widest). The mass accuracy for 6410 mass spectrometer is 0.1 across the mass range. The collision cell is a hexapole with linear acceleration." [PSI:MS]
synonym: "6410 Triple Quad LC/MS" EXACT []
is_a: MS:1000490 ! Agilent instrument model */

#define MS__1200_SERIES_LC_MSD_VL_ID "MS:1000478"
#define MS__1200_SERIES_LC_MSD_VL_NAME "1200 series LC_MSD VL"
/* def: "The LC/MSD VL ion trap is part of the family of Agilent ion trap mass spectrometers. It has ESI, APCI and APPI ion sources and is a useful ion trap when the amount of sample is not the limiting factor." [PSI:MS]
is_a: MS:1000490 ! Agilent instrument model */

#define MS_PURGATORY_ID "MS:1000479"
#define MS_PURGATORY_NAME "purgatory"
/* def: "Terms that will likely become obsolete unless there are wails of dissent." [PSI:MS]
relationship: part_of MS:1001458 ! spectrum generation information */

#define MS_MASS_ANALYZER_ATTRIBUTE_ID "MS:1000480"
#define MS_MASS_ANALYZER_ATTRIBUTE_NAME "mass analyzer attribute"
/* def: "Analyzer properties that are associated with a value." [PSI:MS]
relationship: part_of MS:1000451 ! mass analyzer */

#define MS_DETECTOR_ATTRIBUTE_ID "MS:1000481"
#define MS_DETECTOR_ATTRIBUTE_NAME "detector attribute"
/* def: "Detector attribute recognized as a value." [PSI:MS]
relationship: part_of MS:1000453 ! detector */

#define MS_SOURCE_ATTRIBUTE_ID "MS:1000482"
#define MS_SOURCE_ATTRIBUTE_NAME "source attribute"
/* def: "Property of a source device that need a value." [PSI:MS]
relationship: part_of MS:1000458 ! source */

#define MS_THERMO_FISHER_SCIENTIFIC_INSTRUMENT_MODEL_ID "MS:1000483"
#define MS_THERMO_FISHER_SCIENTIFIC_INSTRUMENT_MODEL_NAME "Thermo Fisher Scientific instrument model"
/* def: "Thermo Fisher Scientific instrument model. The company has gone through several names including Thermo Finnigan, Thermo Scientific." [PSI:MS]
synonym: "Thermo Scientific" RELATED []
is_a: MS:1000031 ! instrument model */

#define MS_ORBITRAP_ID "MS:1000484"
#define MS_ORBITRAP_NAME "orbitrap"
/* def: "An ion trapping device that consists of an outer barrel-like electrode and a coaxial inner spindle-like electrode that form an electrostatic field with quadro-logarithmic potential distribution. The frequency of harmonic oscillations of the orbitally trapped ions along the axis of the electrostatic field is independent of the ion velocity and is inversely proportional to the square root of m/z so that the trap can be used as a mass analyzer." [PSI:MS]
is_a: MS:1000443 ! mass analyzer type */

#define MS_NANOSPRAY_INLET_ID "MS:1000485"
#define MS_NANOSPRAY_INLET_NAME "nanospray inlet"
/* def: "Nanospray Inlet." [PSI:MS]
is_a: MS:1000057 ! electrospray inlet */

#define MS_SOURCE_POTENTIAL_ID "MS:1000486"
#define MS_SOURCE_POTENTIAL_NAME "source potential"
/* def: "Potential difference at the MS source in volts." [PSI:MS]
xref: value-type:xsd\:float "The allowed value-type for this CV term."
is_a: MS:1000482 ! source attribute
relationship: has_units UO:0000218 ! volt */

#define MS_ION_OPTICS_ATTRIBUTE_ID "MS:1000487"
#define MS_ION_OPTICS_ATTRIBUTE_NAME "ion optics attribute"
/* def: "Ion optics involves components that help focus ion streams in mass spectrometry." [PSI:MS]
is_a: MS:1000462 ! ion optics */

#define MS_HITACHI_INSTRUMENT_MODEL_ID "MS:1000488"
#define MS_HITACHI_INSTRUMENT_MODEL_NAME "Hitachi instrument model"
/* def: "Hitachi instrument model." [PSI:MS]
is_a: MS:1000031 ! instrument model */

#define MS_VARIAN_INSTRUMENT_MODEL_ID "MS:1000489"
#define MS_VARIAN_INSTRUMENT_MODEL_NAME "Varian instrument model"
/* def: "Varian instrument model." [PSI:MS]
is_a: MS:1000031 ! instrument model */

#define MS_AGILENT_INSTRUMENT_MODEL_ID "MS:1000490"
#define MS_AGILENT_INSTRUMENT_MODEL_NAME "Agilent instrument model"
/* def: "Agilent instrument model." [PSI:MS]
is_a: MS:1000031 ! instrument model */

#define MS_DIONEX_INSTRUMENT_MODEL_ID "MS:1000491"
#define MS_DIONEX_INSTRUMENT_MODEL_NAME "Dionex instrument model"
/* def: "Dionex instrument model." [PSI:MS]
is_a: MS:1000031 ! instrument model */

#define MS_THERMO_ELECTRON_INSTRUMENT_MODEL_ID "MS:1000492"
#define MS_THERMO_ELECTRON_INSTRUMENT_MODEL_NAME "Thermo Electron instrument model"
/* def: "Thermo Electron Corporation instrument model." [PSI:MS]
is_a: MS:1000483 ! Thermo Fisher Scientific instrument model */

#define MS_FINNIGAN_MAT_INSTRUMENT_MODEL_ID "MS:1000493"
#define MS_FINNIGAN_MAT_INSTRUMENT_MODEL_NAME "Finnigan MAT instrument model"
/* def: "Finnigan MAT instrument model." [PSI:MS]
is_a: MS:1000483 ! Thermo Fisher Scientific instrument model */

#define MS_THERMO_SCIENTIFIC_INSTRUMENT_MODEL_ID "MS:1000494"
#define MS_THERMO_SCIENTIFIC_INSTRUMENT_MODEL_NAME "Thermo Scientific instrument model"
/* def: "Thermo Scientific instrument model." [PSI:MS]
is_a: MS:1000483 ! Thermo Fisher Scientific instrument model */

#define MS_APPLIED_BIOSYSTEMS_INSTRUMENT_MODEL_ID "MS:1000495"
#define MS_APPLIED_BIOSYSTEMS_INSTRUMENT_MODEL_NAME "Applied Biosystems instrument model"
/* def: "Applied Biosystems instrument model." [PSI:MS]
synonym: "ABI" EXACT []
is_a: MS:1000031 ! instrument model */

#define MS_INSTRUMENT_ATTRIBUTE_ID "MS:1000496"
#define MS_INSTRUMENT_ATTRIBUTE_NAME "instrument attribute"
/* def: "Instrument properties that are associated with a value." [PSI:MS]
is_a: MS:1000547 ! object attribute
relationship: part_of MS:1000463 ! instrument */

#define MS_ZOOM_SCAN_ID "MS:1000497"
#define MS_ZOOM_SCAN_NAME "zoom scan"
/* def: "Special scan mode, where data with improved resoltuion is acquired. This is typically achieved by scanning a more narrow m/z window or scanning with a lower scan rate." [PSI:MS]
synonym: "enhanced resolution scan" EXACT []
is_a: MS:1000499 ! spectrum attribute */

#define MS_FULL_SCAN_ID "MS:1000498"
#define MS_FULL_SCAN_NAME "full scan"
/* def: "Feature of the ion trap mass spectrometer where MS data is acquired over a mass range." [PSI:MS]
is_a: MS:1000020 ! scanning method */

#define MS_SPECTRUM_ATTRIBUTE_ID "MS:1000499"
#define MS_SPECTRUM_ATTRIBUTE_NAME "spectrum attribute"
/* def: "Spectrum properties that are associated with a value." [PSI:MS]
is_a: MS:1000547 ! object attribute
relationship: part_of MS:1000442 ! spectrum */

#define MS_SCAN_WINDOW_UPPER_LIMIT_ID "MS:1000500"
#define MS_SCAN_WINDOW_UPPER_LIMIT_NAME "scan window upper limit"
/* def: "The lower m/z bound of a mass spectrometer scan window." [PSI:MS]
synonym: "mzRangeStop" RELATED []
xref: value-type:xsd\:float "The allowed value-type for this CV term."
is_a: MS:1000549 ! selection window attribute
relationship: has_units MS:1000040 ! m/z */

#define MS_SCAN_WINDOW_LOWER_LIMIT_ID "MS:1000501"
#define MS_SCAN_WINDOW_LOWER_LIMIT_NAME "scan window lower limit"
/* def: "The upper m/z bound of a mass spectrometer scan window." [PSI:MS]
synonym: "mzRangeStart" RELATED []
xref: value-type:xsd\:float "The allowed value-type for this CV term."
is_a: MS:1000549 ! selection window attribute
relationship: has_units MS:1000040 ! m/z */

#define MS_DWELL_TIME_ID "MS:1000502"
#define MS_DWELL_TIME_NAME "dwell time"
/* def: "The time spent gathering data across a peak." [PSI:MS]
synonym: "Scan Duration" RELATED []
xref: value-type:xsd\:float "The allowed value-type for this CV term."
is_a: MS:1000503 ! scan attribute
relationship: has_units UO:0000010 ! second
relationship: has_units UO:0000031 ! minute */

#define MS_SCAN_ATTRIBUTE_ID "MS:1000503"
#define MS_SCAN_ATTRIBUTE_NAME "scan attribute"
/* def: "Scan properties that are associated with a value." [PSI:MS]
is_a: MS:1000547 ! object attribute
relationship: part_of MS:1000441 ! scan */

#define MS_BASE_PEAK_M_Z_ID "MS:1000504"
#define MS_BASE_PEAK_M_Z_NAME "base peak m/z"
/* def: "M/z value of the signal of highest intensity in the mass spectrum." [PSI:MS]
xref: value-type:xsd\:float "The allowed value-type for this CV term."
is_a: MS:1000499 ! spectrum attribute
relationship: has_units MS:1000040 ! m/z */

#define MS_BASE_PEAK_INTENSITY_ID "MS:1000505"
#define MS_BASE_PEAK_INTENSITY_NAME "base peak intensity"
/* def: "The intensity of the greatest peak in the mass spectrum." [PSI:MS]
xref: value-type:xsd\:float "The allowed value-type for this CV term."
is_a: MS:1000499 ! spectrum attribute
relationship: has_units MS:1000131 ! number of detector counts
relationship: has_units MS:1000132 ! percent of base peak
relationship: has_units MS:1000814 ! counts per second
relationship: has_units MS:1000905 ! percent of base peak times 100
relationship: has_units UO:0000269 ! absorbance unit */

#define MS_ION_ROLE_ID "MS:1000506"
#define MS_ION_ROLE_NAME "ion role"
/* def: "Ion Role." [PSI:MS]
relationship: part_of MS:1000365 ! ion? */

#define MS_ION_ATTRIBUTE_ID "MS:1000507"
#define MS_ION_ATTRIBUTE_NAME "ion attribute"
/* def: "Ion properties that are associated with a value." [PSI:MS]
relationship: part_of MS:1000365 ! ion? */

#define MS_ION_CHEMICAL_TYPE_ID "MS:1000508"
#define MS_ION_CHEMICAL_TYPE_NAME "ion chemical type"
/* def: "Ion Type." [PSI:MS]
relationship: part_of MS:1000365 ! ion? */

#define MS_ACTIVATION_ENERGY_ID "MS:1000509"
#define MS_ACTIVATION_ENERGY_NAME "activation energy"
/* def: "Activation Energy." [PSI:MS]
xref: value-type:xsd\:float "The allowed value-type for this CV term."
is_a: MS:1000510 ! precursor activation attribute
relationship: has_units UO:0000266 ! electronvolt */

#define MS_PRECURSOR_ACTIVATION_ATTRIBUTE_ID "MS:1000510"
#define MS_PRECURSOR_ACTIVATION_ATTRIBUTE_NAME "precursor activation attribute"
/* def: "Precursor Activation Attribute." [PSI:MS]
relationship: part_of MS:1000456 ! precursor activation */

#define MS_MS_LEVEL_ID "MS:1000511"
#define MS_MS_LEVEL_NAME "ms level"
/* def: "Stages of ms achieved in a multi stage mass spectrometry experiment." [PSI:MS]
xref: value-type:xsd\:int "The allowed value-type for this CV term."
is_a: MS:1000499 ! spectrum attribute */

#define MS_FILTER_STRING_ID "MS:1000512"
#define MS_FILTER_STRING_NAME "filter string"
/* def: "A string unique to Thermo instrument describing instrument settings for the scan." [PSI:MS]
xref: value-type:xsd\:string "The allowed value-type for this CV term."
is_a: MS:1000503 ! scan attribute */

#define MS_BINARY_DATA_ARRAY_ID "MS:1000513"
#define MS_BINARY_DATA_ARRAY_NAME "binary data array"
/* def: "A data array of values." [PSI:MS]
relationship: part_of MS:1000442 ! spectrum
relationship: part_of MS:1000625 ! chromatogram */

#define MS_M_Z_ARRAY_ID "MS:1000514"
#define MS_M_Z_ARRAY_NAME "m/z array"
/* def: "A data array of m/z values." [PSI:MS]
xref: binary-data-type:MS\:1000521 "32-bit float"
xref: binary-data-type:MS\:1000523 "64-bit float"
is_a: MS:1000513 ! binary data array
relationship: has_units MS:1000040 ! m/z */

#define MS_INTENSITY_ARRAY_ID "MS:1000515"
#define MS_INTENSITY_ARRAY_NAME "intensity array"
/* def: "A data array of intensity values." [PSI:MS]
xref: binary-data-type:MS\:1000521 "32-bit float"
xref: binary-data-type:MS\:1000523 "64-bit float"
is_a: MS:1000513 ! binary data array
relationship: has_units MS:1000131 ! number of detector counts
relationship: has_units MS:1000132 ! percent of base peak
relationship: has_units MS:1000814 ! counts per second
relationship: has_units MS:1000905 ! percent of base peak times 100
relationship: has_units UO:0000269 ! absorbance unit */

#define MS_CHARGE_ARRAY_ID "MS:1000516"
#define MS_CHARGE_ARRAY_NAME "charge array"
/* def: "A data array of charge values." [PSI:MS]
xref: binary-data-type:MS\:1000519 "32-bit integer"
is_a: MS:1000513 ! binary data array */

#define MS_SIGNAL_TO_NOISE_ARRAY_ID "MS:1000517"
#define MS_SIGNAL_TO_NOISE_ARRAY_NAME "signal to noise array"
/* def: "A data array of signal-to-noise values." [PSI:MS]
xref: binary-data-type:MS\:1000521 "32-bit float"
xref: binary-data-type:MS\:1000523 "64-bit float"
is_a: MS:1000513 ! binary data array */

#define MS_BINARY_DATA_TYPE_ID "MS:1000518"
#define MS_BINARY_DATA_TYPE_NAME "binary data type"
/* def: "Encoding type of binary data specifying the binary representation and precision, e.g. 64-bit float." [PSI:MS]
relationship: part_of MS:1000442 ! spectrum
relationship: part_of MS:1000625 ! chromatogram */

#define MS_32_BIT_INTEGER_ID "MS:1000519"
#define MS_32_BIT_INTEGER_NAME "32-bit integer"
/* def: "Signed 32-bit little-endian integer." [PSI:MS]
is_a: MS:1000518 ! binary data type */

// #define MS_16_BIT_FLOAT_ID "MS:1000520"
// #define MS_16_BIT_FLOAT_NAME "16-bit float"
/* def: "OBSOLETE Signed 16-bit float." [PSI:MS]
is_a: MS:1000518 ! binary data type
is_obsolete: true */

#define MS_32_BIT_FLOAT_ID "MS:1000521"
#define MS_32_BIT_FLOAT_NAME "32-bit float"
/* def: "32-bit precision little-endian floating point conforming to IEEE-754." [PSI:MS]
is_a: MS:1000518 ! binary data type */

#define MS_64_BIT_INTEGER_ID "MS:1000522"
#define MS_64_BIT_INTEGER_NAME "64-bit integer"
/* def: "Signed 64-bit little-endian integer." [PSI:MS]
is_a: MS:1000518 ! binary data type */

#define MS_64_BIT_FLOAT_ID "MS:1000523"
#define MS_64_BIT_FLOAT_NAME "64-bit float"
/* def: "64-bit precision little-endian floating point conforming to IEEE-754." [PSI:MS]
is_a: MS:1000518 ! binary data type */

#define MS_DATA_FILE_CONTENT_ID "MS:1000524"
#define MS_DATA_FILE_CONTENT_NAME "data file content"
/* def: "Describes the data content on the file." [PSI:MS]
relationship: part_of MS:1000577 ! raw data file */

#define MS_SPECTRUM_REPRESENTATION_ID "MS:1000525"
#define MS_SPECTRUM_REPRESENTATION_NAME "spectrum representation"
/* def: "Way in which the spectrum is represented, either with regularly spaced data points or with a list of centroided peaks." [PSI:MS]
relationship: part_of MS:1000442 ! spectrum */

#define MS_WATERS_RAW_FILE_ID "MS:1000526"
#define MS_WATERS_RAW_FILE_NAME "Waters raw file"
/* def: "Waters data file found in a Waters RAW directory, generated from an MS acquisition." [PSI:MS]
is_a: MS:1000560 ! mass spectrometer file format */

#define MS_HIGHEST_OBSERVED_M_Z_ID "MS:1000527"
#define MS_HIGHEST_OBSERVED_M_Z_NAME "highest observed m/z"
/* def: "Highest m/z value observed in the m/z array." [PSI:MS]
xref: value-type:xsd\:float "The allowed value-type for this CV term."
is_a: MS:1000499 ! spectrum attribute
is_a: MS:1000808 ! chromatogram attribute
relationship: has_units MS:1000040 ! m/z */

#define MS_LOWEST_OBSERVED_M_Z_ID "MS:1000528"
#define MS_LOWEST_OBSERVED_M_Z_NAME "lowest observed m/z"
/* def: "Lowest m/z value observed in the m/z array." [PSI:MS]
xref: value-type:xsd\:float "The allowed value-type for this CV term."
is_a: MS:1000499 ! spectrum attribute
is_a: MS:1000808 ! chromatogram attribute
relationship: has_units MS:1000040 ! m/z */

#define MS_INSTRUMENT_SERIAL_NUMBER_ID "MS:1000529"
#define MS_INSTRUMENT_SERIAL_NUMBER_NAME "instrument serial number"
/* def: "Serial Number of the instrument." [PSI:MS]
xref: value-type:xsd\:string "The allowed value-type for this CV term."
is_a: MS:1000496 ! instrument attribute */

#define MS_FILE_FORMAT_CONVERSION_ID "MS:1000530"
#define MS_FILE_FORMAT_CONVERSION_NAME "file format conversion"
/* def: "Conversion of one file format to another." [PSI:MS]
is_a: MS:1000452 ! data transformation */

#define MS_SOFTWARE_ID "MS:1000531"
#define MS_SOFTWARE_NAME "software"
/* def: "Software related to the recording or transformation of spectra." [PSI:MS]
relationship: part_of MS:0000000 ! Proteomics Standards Initiative Mass Spectrometry Vocabularies */

#define MS_XCALIBUR_ID "MS:1000532"
#define MS_XCALIBUR_NAME "Xcalibur"
/* def: "Thermo Finnigan software for data acquisition and analysis." [PSI:MS]
is_a: MS:1000693 ! Thermo Finnigan software
is_a: MS:1001455 ! acquisition software
is_a: MS:1001456 ! analysis software
is_a: MS:1001457 ! data processing software */

#define MS_BIOWORKS_ID "MS:1000533"
#define MS_BIOWORKS_NAME "Bioworks"
/* def: "Thermo Finnigan software for data analysis of peptides and proteins." [PSI:MS]
synonym: "Bioworks Browser" RELATED []
is_a: MS:1000693 ! Thermo Finnigan software
is_a: MS:1001456 ! analysis software
is_a: MS:1001457 ! data processing software */

#define MS_MASSLYNX_ID "MS:1000534"
#define MS_MASSLYNX_NAME "MassLynx"
/* def: "Micromass software for data acquisition and analysis." [PSI:MS]
is_a: MS:1000694 ! Waters software
is_a: MS:1001455 ! acquisition software
is_a: MS:1001456 ! analysis software
is_a: MS:1001457 ! data processing software */

#define MS_FLEXANALYSIS_ID "MS:1000535"
#define MS_FLEXANALYSIS_NAME "FlexAnalysis"
/* def: "Bruker software for data analysis." [PSI:MS]
is_a: MS:1000692 ! Bruker software
is_a: MS:1001456 ! analysis software
is_a: MS:1001457 ! data processing software */

#define MS_DATA_EXPLORER_ID "MS:1000536"
#define MS_DATA_EXPLORER_NAME "Data Explorer"
/* def: "Applied Biosystems software for data acquisition and analysis." [PSI:MS]
is_a: MS:1000691 ! Applied Biosystems software
is_a: MS:1001455 ! acquisition software
is_a: MS:1001456 ! analysis software
is_a: MS:1001457 ! data processing software */

#define MS__4700_EXPLORER_ID "MS:1000537"
#define MS__4700_EXPLORER_NAME "4700 Explorer"
/* def: "Applied Biosystems software for data acquisition and analysis." [PSI:MS]
is_a: MS:1000691 ! Applied Biosystems software
is_a: MS:1001455 ! acquisition software
is_a: MS:1001456 ! analysis software
is_a: MS:1001457 ! data processing software */

#define MS_MASSWOLF_ID "MS:1000538"
#define MS_MASSWOLF_NAME "massWolf"
/* def: "A software for converting Waters raw directory format to mzXML or mzML. MassWolf was originally developed at the Institute for Systems Biology." [PSI:MS]
synonym: "wolf" EXACT []
is_a: MS:1001457 ! data processing software */

#define MS_VOYAGER_BIOSPECTROMETRY_WORKSTATION_SYSTEM_ID "MS:1000539"
#define MS_VOYAGER_BIOSPECTROMETRY_WORKSTATION_SYSTEM_NAME "Voyager Biospectrometry Workstation System"
/* def: "Applied Biosystems MALDI-TOF data acquisition and analysis system." [PSI:MS]
is_a: MS:1000691 ! Applied Biosystems software
is_a: MS:1001455 ! acquisition software
is_a: MS:1001456 ! analysis software
is_a: MS:1001457 ! data processing software */

#define MS_FLEXCONTROL_ID "MS:1000540"
#define MS_FLEXCONTROL_NAME "FlexControl"
/* def: "Bruker software for data acquisition." [PSI:MS]
is_a: MS:1000692 ! Bruker software
is_a: MS:1001455 ! acquisition software */

#define MS_READW_ID "MS:1000541"
#define MS_READW_NAME "ReAdW"
/* def: "A software program for converting Thermo Finnigan RAW file format to mzXML or mzML. ReAdW was originally developed at the Institute for Systems Biology. Its whimsical interleaved spelling and capitalization is pronounced \"readraw\"." [PSI:MS]
is_a: MS:1001457 ! data processing software */

#define MS_MZSTAR_ID "MS:1000542"
#define MS_MZSTAR_NAME "MzStar"
/* def: "A software program for converting Applied Biosystems wiff file format to mzXML format. MzStar was originally developed at the Institute for Systems Biology. It is now obsoleted by the MzWiff program." [PSI:MS]
is_a: MS:1001457 ! data processing software */

#define MS_DATA_PROCESSING_ACTION_ID "MS:1000543"
#define MS_DATA_PROCESSING_ACTION_NAME "data processing action"
/* def: "Data processing attribute used to describe the type of data processing performed on the data file." [PSI:MS]
is_a: MS:1000452 ! data transformation */

#define MS_CONVERSION_TO_MZML_ID "MS:1000544"
#define MS_CONVERSION_TO_MZML_NAME "Conversion to mzML"
/* def: "Conversion of a file format to Proteomics Standards Initiative mzML file format." [PSI:MS]
is_a: MS:1000530 ! file format conversion */

#define MS_CONVERSION_TO_MZXML_ID "MS:1000545"
#define MS_CONVERSION_TO_MZXML_NAME "Conversion to mzXML"
/* def: "Conversion of a file format to Institute of Systems Biology mzXML file format." [PSI:MS]
is_a: MS:1000530 ! file format conversion */

#define MS_CONVERSION_TO_MZDATA_ID "MS:1000546"
#define MS_CONVERSION_TO_MZDATA_NAME "Conversion to mzData"
/* def: "Conversion of a file format to Proteomics Standards Initiative mzData file format." [PSI:MS]
is_a: MS:1000530 ! file format conversion */

#define MS_OBJECT_ATTRIBUTE_ID "MS:1000547"
#define MS_OBJECT_ATTRIBUTE_NAME "object attribute"
/* def: "Object Attribute." [PSI:MS]
relationship: part_of MS:1001458 ! spectrum generation information */

#define MS_SAMPLE_ATTRIBUTE_ID "MS:1000548"
#define MS_SAMPLE_ATTRIBUTE_NAME "sample attribute"
/* def: "Sample properties that are associated with a value." [PSI:MS]
is_a: MS:1000547 ! object attribute
relationship: part_of MS:1000457 ! sample */

#define MS_SELECTION_WINDOW_ATTRIBUTE_ID "MS:1000549"
#define MS_SELECTION_WINDOW_ATTRIBUTE_NAME "selection window attribute"
/* def: "Selection window properties that are associated with a value." [PSI:MS]
is_a: MS:1000547 ! object attribute
relationship: part_of MS:1000441 ! scan */

// #define MS_TIME_UNIT_ID "MS:1000550"
// #define MS_TIME_UNIT_NAME "time unit"
/* def: "OBSOLETE Time Unit." [PSI:MS]
comment: This term was made obsolete because it was redundant with the Unit Ontology term time unit (UO:0000003).
is_obsolete: true */

#define MS_ANALYST_ID "MS:1000551"
#define MS_ANALYST_NAME "Analyst"
/* def: "AB SCIEX or Applied Biosystems|MDS SCIEX software for data acquisition." [PSI:MS]
is_a: MS:1000690 ! AB SCIEX software
is_a: MS:1001455 ! acquisition software
is_a: MS:1001456 ! analysis software
is_a: MS:1001457 ! data processing software */

// #define MS_MALDI_SPOT_IDENTIFIER_ID "MS:1000552"
// #define MS_MALDI_SPOT_IDENTIFIER_NAME "maldi spot identifier"
/* def: "OBSOLETE Maldi Spot Identifier." [PSI:MS]
comment: This former purgatory term was made obsolete.
is_obsolete: true */

#define MS_TRAPPER_ID "MS:1000553"
#define MS_TRAPPER_NAME "Trapper"
/* def: "A software program for converting Agilent MassHunter format to mzXML or mzML. Trapper was originally developed at the Institute for Systems Biology." [PSI:MS]
is_a: MS:1001457 ! data processing software */

#define MS_LCQ_DECA_ID "MS:1000554"
#define MS_LCQ_DECA_NAME "LCQ Deca"
/* def: "ThermoFinnigan LCQ Deca." [PSI:MS]
is_a: MS:1000125 ! Thermo Finnigan instrument model */

#define MS_LTQ_ORBITRAP_DISCOVERY_ID "MS:1000555"
#define MS_LTQ_ORBITRAP_DISCOVERY_NAME "LTQ Orbitrap Discovery"
/* def: "LTQ Orbitrap Discovery." [PSI:MS]
is_a: MS:1000494 ! Thermo Scientific instrument model */

#define MS_LTQ_ORBITRAP_XL_ID "MS:1000556"
#define MS_LTQ_ORBITRAP_XL_NAME "LTQ Orbitrap XL"
/* def: "LTQ Orbitrap XL." [PSI:MS]
is_a: MS:1000494 ! Thermo Scientific instrument model */

#define MS_LTQ_FT_ULTRA_ID "MS:1000557"
#define MS_LTQ_FT_ULTRA_NAME "LTQ FT Ultra"
/* def: "LTQ FT Ultra." [PSI:MS]
is_a: MS:1000494 ! Thermo Scientific instrument model */

#define MS_GC_QUANTUM_ID "MS:1000558"
#define MS_GC_QUANTUM_NAME "GC Quantum"
/* def: "GC Quantum." [PSI:MS]
is_a: MS:1000125 ! Thermo Finnigan instrument model */

#define MS_SPECTRUM_TYPE_ID "MS:1000559"
#define MS_SPECTRUM_TYPE_NAME "spectrum type"
/* def: "Spectrum type." [PSI:MS]
relationship: part_of MS:1000442 ! spectrum */

#define MS_MASS_SPECTROMETER_FILE_FORMAT_ID "MS:1000560"
#define MS_MASS_SPECTROMETER_FILE_FORMAT_NAME "mass spectrometer file format"
/* def: "The format of the file being used. This could be a instrument or vendor specific proprietary file format or a converted open file format." [PSI:MS]
is_a: MS:1001459 ! file format */

#define MS_DATA_FILE_CHECKSUM_TYPE_ID "MS:1000561"
#define MS_DATA_FILE_CHECKSUM_TYPE_NAME "data file checksum type"
/* def: "Checksum is a form of redundancy check, a simple way to protect the integrity of data by detecting errors in data." [PSI:MS]
relationship: part_of MS:1000577 ! raw data file */

#define MS_ABI_WIFF_FILE_ID "MS:1000562"
#define MS_ABI_WIFF_FILE_NAME "ABI WIFF file"
/* def: "Applied Biosystems WIFF file format." [PSI:MS]
is_a: MS:1000560 ! mass spectrometer file format */

#define MS_THERMO_RAW_FILE_ID "MS:1000563"
#define MS_THERMO_RAW_FILE_NAME "Thermo RAW file"
/* def: "Thermo Scientific RAW file format." [PSI:MS]
is_a: MS:1000560 ! mass spectrometer file format */

#define MS_PSI_MZDATA_FILE_ID "MS:1000564"
#define MS_PSI_MZDATA_FILE_NAME "PSI mzData file"
/* def: "Proteomics Standards Inititative mzData file format." [PSI:MS]
is_a: MS:1000560 ! mass spectrometer file format */

#define MS_MICROMASS_PKL_FILE_ID "MS:1000565"
#define MS_MICROMASS_PKL_FILE_NAME "Micromass PKL file"
/* def: "Micromass PKL file format." [PSI:MS]
is_a: MS:1000560 ! mass spectrometer file format */

#define MS_ISB_MZXML_FILE_ID "MS:1000566"
#define MS_ISB_MZXML_FILE_NAME "ISB mzXML file"
/* def: "Institute of Systems Biology mzXML file format." [PSI:MS]
is_a: MS:1000560 ! mass spectrometer file format */

#define MS_BRUKER_AGILENT_YEP_FILE_ID "MS:1000567"
#define MS_BRUKER_AGILENT_YEP_FILE_NAME "Bruker_Agilent YEP file"
/* def: "Bruker/Agilent YEP file format." [PSI:MS]
is_a: MS:1000560 ! mass spectrometer file format */

#define MS_MD5_ID "MS:1000568"
#define MS_MD5_NAME "MD5"
/* def: "MD5 (Message-Digest algorithm 5) is a cryptographic hash function with a 128-bit hash value used to check the integrity of files." [PSI:MS]
xref: value-type:xsd\:string "The allowed value-type for this CV term."
is_a: MS:1000561 ! data file checksum type */

#define MS_SHA_1_ID "MS:1000569"
#define MS_SHA_1_NAME "SHA-1"
/* def: "SHA-1 (Secure Hash Algorithm-1) is a cryptographic hash function designed by the National Security Agency (NSA) and published by the NIST as a U. S. government standard. It is also used to verify file integrity." [PSI:MS]
xref: value-type:xsd\:string "The allowed value-type for this CV term."
is_a: MS:1000561 ! data file checksum type */

#define MS_SPECTRA_COMBINATION_ID "MS:1000570"
#define MS_SPECTRA_COMBINATION_NAME "spectra combination"
/* def: "Method used to combine the mass spectra." [PSI:MS]
relationship: part_of MS:1000442 ! spectrum */

#define MS_SUM_OF_SPECTRA_ID "MS:1000571"
#define MS_SUM_OF_SPECTRA_NAME "sum of spectra"
/* def: "Spectra Sum." [PSI:MS]
is_a: MS:1000570 ! spectra combination */

#define MS_BINARY_DATA_COMPRESSION_TYPE_ID "MS:1000572"
#define MS_BINARY_DATA_COMPRESSION_TYPE_NAME "binary data compression type"
/* def: "Compression Type." [PSI:MS]
relationship: part_of MS:1000442 ! spectrum
relationship: part_of MS:1000625 ! chromatogram */

#define MS_MEDIAN_OF_SPECTRA_ID "MS:1000573"
#define MS_MEDIAN_OF_SPECTRA_NAME "median of spectra"
/* def: "Spectra is combined by calculating the median of the spectra." [PSI:MS]
is_a: MS:1000570 ! spectra combination */

#define MS_ZLIB_COMPRESSION_ID "MS:1000574"
#define MS_ZLIB_COMPRESSION_NAME "zlib compression"
/* def: "Zlib." [PSI:MS]
is_a: MS:1000572 ! binary data compression type */

#define MS_MEAN_OF_SPECTRA_ID "MS:1000575"
#define MS_MEAN_OF_SPECTRA_NAME "mean of spectra"
/* def: "Spectra is combined by calculating the mean of the spectra." [PSI:MS]
is_a: MS:1000570 ! spectra combination */

#define MS_NO_COMPRESSION_ID "MS:1000576"
#define MS_NO_COMPRESSION_NAME "no compression"
/* def: "No Compression." [PSI:MS]
is_a: MS:1000572 ! binary data compression type */

#define MS_RAW_DATA_FILE_ID "MS:1000577"
#define MS_RAW_DATA_FILE_NAME "raw data file"
/* def: "Describes the type of file and its content." [PSI:MS]
synonym: "source file" EXACT []
relationship: part_of MS:1001458 ! spectrum generation information */

#define MS_LCQ_FLEET_ID "MS:1000578"
#define MS_LCQ_FLEET_NAME "LCQ Fleet"
/* def: "LCQ Fleet." [PSI:MS]
is_a: MS:1000125 ! Thermo Finnigan instrument model */

#define MS_MS1_SPECTRUM_ID "MS:1000579"
#define MS_MS1_SPECTRUM_NAME "MS1 spectrum"
/* def: "Mass spectrum created by a single-stage MS experiment or the first stage of a multi-stage experiment." [PSI:MS]
synonym: "full spectrum" EXACT []
synonym: "Q1 spectrum" EXACT []
synonym: "Q3 spectrum" EXACT []
synonym: "Single-Stage Mass Spectrometry" EXACT []
is_a: MS:1000294 ! mass spectrum
is_a: MS:1000524 ! data file content */

#define MS_MSN_SPECTRUM_ID "MS:1000580"
#define MS_MSN_SPECTRUM_NAME "MSn spectrum"
/* def: "MSn refers to multi-stage MS/MS experiments designed to record product ion spectra where n is the number of product ion stages (progeny ions). For ion traps, sequential MS/MS experiments can be undertaken where n > 2 whereas for a simple triple quadrupole system n= 2." [PSI:MS]
synonym: "multiple-stage mass spectrometry spectrum" EXACT []
synonym: "nth generation product ion spectrum" EXACT []
synonym: "product ion spectrum" EXACT []
xref: value-type:xsd\:positiveInteger "The allowed value-type for this CV term."
is_a: MS:1000294 ! mass spectrum
is_a: MS:1000524 ! data file content */

#define MS_CRM_SPECTRUM_ID "MS:1000581"
#define MS_CRM_SPECTRUM_NAME "CRM spectrum"
/* def: "Spectrum generated from MSn experiment with three or more stages of m/z separation and in which a particular multi-step reaction path is monitored." [PSI:MS]
is_a: MS:1000294 ! mass spectrum
is_a: MS:1000524 ! data file content */

#define MS_SIM_SPECTRUM_ID "MS:1000582"
#define MS_SIM_SPECTRUM_NAME "SIM spectrum"
/* def: "Spectrum obtained with the operation of a mass spectrometer in which the abundances of one ion or several ions of specific m/z values are recorded rather than the entire mass spectrum (Selected Ion Monitoring)." [PSI:MS]
synonym: "MIM spectrum" EXACT []
synonym: "multiple ion monitoring spectrum" EXACT []
synonym: "selected ion monitoring spectrum" EXACT []
is_a: MS:1000294 ! mass spectrum
is_a: MS:1000524 ! data file content */

#define MS_SRM_SPECTRUM_ID "MS:1000583"
#define MS_SRM_SPECTRUM_NAME "SRM spectrum"
/* def: "Spectrum obtained when data are acquired from specific product ions corresponding to m/z values of selected precursor ions a recorded via two or more stages of mass spectrometry. The precursor/product ion pair is called a transition pair. Data can be obtained for a single transition pair or multiple transition pairs. Multiple time segments of different transition pairs can exist in a single file. Single precursor ions can have multiple product ions consitituting multiple transition pairs. Selected reaction monitoring can be performed as tandem mass spectrometry in time or tandem mass spectrometry in space." [PSI:MS]
synonym: "MRM spectrum" EXACT []
synonym: "multiple reaction monitoring spectrum" EXACT []
synonym: "selected reaction monitoring spectrum" EXACT []
is_a: MS:1000294 ! mass spectrum
is_a: MS:1000524 ! data file content */

#define MS_MZML_FILE_ID "MS:1000584"
#define MS_MZML_FILE_NAME "mzML file"
/* def: "Proteomics Standards Inititative mzML file format." [PSI:MS]
is_a: MS:1000560 ! mass spectrometer file format */

#define MS_CONTACT_ATTRIBUTE_ID "MS:1000585"
#define MS_CONTACT_ATTRIBUTE_NAME "contact attribute"
/* def: "Details about a person or organization to contact in case of concern or discussion about the file." [PSI:MS]
is_a: MS:1000547 ! object attribute
relationship: part_of MS:0000000 ! Proteomics Standards Initiative Mass Spectrometry Vocabularies */

#define MS_CONTACT_NAME_ID "MS:1000586"
#define MS_CONTACT_NAME_NAME "contact name"
/* def: "Name of the contact person or organization." [PSI:MS]
xref: value-type:xsd\:string "The allowed value-type for this CV term."
is_a: MS:1000585 ! contact attribute */

#define MS_CONTACT_ADDRESS_ID "MS:1000587"
#define MS_CONTACT_ADDRESS_NAME "contact address"
/* def: "Postal address of the contact person or organization." [PSI:MS]
xref: value-type:xsd\:string "The allowed value-type for this CV term."
is_a: MS:1000585 ! contact attribute */

#define MS_CONTACT_URL_ID "MS:1000588"
#define MS_CONTACT_URL_NAME "contact URL"
/* def: "Uniform Resource Locator related to the contact person or organization." [PSI:MS]
xref: value-type:xsd\:string "The allowed value-type for this CV term."
is_a: MS:1000585 ! contact attribute */

#define MS_CONTACT_EMAIL_ID "MS:1000589"
#define MS_CONTACT_EMAIL_NAME "contact email"
/* def: "Email address of the contact person or organization." [PSI:MS]
xref: value-type:xsd\:string "The allowed value-type for this CV term."
is_a: MS:1000585 ! contact attribute */

#define MS_CONTACT_ORGANIZATION_ID "MS:1000590"
#define MS_CONTACT_ORGANIZATION_NAME "contact organization"
/* def: "Home institution of the contact person." [PSI:MS]
xref: value-type:xsd\:string "The allowed value-type for this CV term."
is_a: MS:1000585 ! contact attribute */

#define MS_MZWIFF_ID "MS:1000591"
#define MS_MZWIFF_NAME "MzWiff"
/* def: "A software program for converting Applied Biosystems wiff file format to the mzXML or mzML format. MzWiff is currently maintained at the Institute for Systems Biology. It replaces the slower mzStar program." [PSI:MS]
is_a: MS:1001457 ! data processing software */

#define MS_SMOOTHING_ID "MS:1000592"
#define MS_SMOOTHING_NAME "smoothing"
/* def: "A process of reducing spikes of intensity in order to reduce noise while preserving real peak signal. Many algorithms can be applied for this process." [PSI:MS]
is_a: MS:1000543 ! data processing action */

#define MS_BASELINE_REDUCTION_ID "MS:1000593"
#define MS_BASELINE_REDUCTION_NAME "baseline reduction"
/* def: "A process of removal of varying intensities generated due to variable energy absorption before further processing can take place. Baseline reduction facilitates meaningful comparision between intensities of m/z values." [PSI:MS]
is_a: MS:1000543 ! data processing action */

#define MS_LOW_INTENSITY_DATA_POINT_REMOVAL_ID "MS:1000594"
#define MS_LOW_INTENSITY_DATA_POINT_REMOVAL_NAME "low intensity data point removal"
/* def: "The removal of very low intensity data points that are likely to be spurious noise rather than real signal." [PSI:MS]
synonym: "thresholding" EXACT []
is_a: MS:1001486 ! data filtering */

#define MS_TIME_ARRAY_ID "MS:1000595"
#define MS_TIME_ARRAY_NAME "time array"
/* def: "A data array of relative time offset values from a reference time." [PSI:MS]
xref: binary-data-type:MS\:1000521 "32-bit float"
xref: binary-data-type:MS\:1000523 "64-bit float"
is_a: MS:1000513 ! binary data array
relationship: has_units UO:0000010 ! second
relationship: has_units UO:0000031 ! minute */

#define MS_MEASUREMENT_METHOD_ID "MS:1000596"
#define MS_MEASUREMENT_METHOD_NAME "measurement method"
/* def: "An attribute of resolution when recording the detector response in absence of the analyte." [PSI:MS]
relationship: part_of MS:1001458 ! spectrum generation information */

#define MS_ION_OPTICS_TYPE_ID "MS:1000597"
#define MS_ION_OPTICS_TYPE_NAME "ion optics type"
/* def: "The electrical potential used to impart kinetic energy to ions in a mass spectrometer." [PSI:MS]
is_a: MS:1000462 ! ion optics */

#define MS_ELECTRON_TRANSFER_DISSOCIATION_ID "MS:1000598"
#define MS_ELECTRON_TRANSFER_DISSOCIATION_NAME "electron transfer dissociation"
/* def: "A process to fragment ions in a mass spectrometer by inducing fragmentation of cations (e.g. peptides or proteins) by transferring electrons to them." [PSI:MS]
synonym: "ETD" EXACT []
is_a: MS:1000044 ! dissociation method
is_a: MS:1000437 ! ion reaction */

#define MS_PULSED_Q_DISSOCIATION_ID "MS:1000599"
#define MS_PULSED_Q_DISSOCIATION_NAME "pulsed q dissociation"
/* def: "A process that involves precursor ion activation at high Q, a time delay to allow the precursor to fragment, then a rapid pulse to low Q where all fragment ions are trapped. The product ions can then be scanned out of the ion trap and detected." [PSI:MS]
synonym: "PQD" EXACT []
is_a: MS:1000044 ! dissociation method
is_a: MS:1000437 ! ion reaction */

#define MS_PROTEIOS_ID "MS:1000600"
#define MS_PROTEIOS_NAME "Proteios"
/* def: "Database application and analysis platform for proteomics." [PSI:MS, source:http://www.proteios.org]
is_a: MS:1001456 ! analysis software
is_a: MS:1001457 ! data processing software */

#define MS_PROTEINLYNX_GLOBAL_SERVER_ID "MS:1000601"
#define MS_PROTEINLYNX_GLOBAL_SERVER_NAME "ProteinLynx Global Server"
/* def: "Waters software for data analysis." [PSI:MS]
is_a: MS:1000694 ! Waters software
is_a: MS:1001456 ! analysis software
is_a: MS:1001457 ! data processing software */

#define MS_SHIMADZU_MALDI_TOF_INSTRUMENT_MODEL_ID "MS:1000602"
#define MS_SHIMADZU_MALDI_TOF_INSTRUMENT_MODEL_NAME "Shimadzu MALDI-TOF instrument model"
/* def: "Shimadzu MALDI-TOF instrument model." [PSI:MS]
is_a: MS:1000124 ! Shimadzu instrument model */

#define MS_SHIMADZU_SCIENTIFIC_INSTRUMENTS_INSTRUMENT_MODEL_ID "MS:1000603"
#define MS_SHIMADZU_SCIENTIFIC_INSTRUMENTS_INSTRUMENT_MODEL_NAME "Shimadzu Scientific Instruments instrument model"
/* def: "Shimadzu Scientific Instruments instrument model." [PSI:MS]
is_a: MS:1000124 ! Shimadzu instrument model */

#define MS_LCMS_IT_TOF_ID "MS:1000604"
#define MS_LCMS_IT_TOF_NAME "LCMS-IT-TOF"
/* def: "Shimadzu Scientific Instruments LCMS-IT-TOF MS." [PSI:MS]
is_a: MS:1000603 ! Shimadzu Scientific Instruments instrument model */

#define MS_LCMS_2010EV_ID "MS:1000605"
#define MS_LCMS_2010EV_NAME "LCMS-2010EV"
/* def: "Shimadzu Scientific Instruments LCMS-2010EV MS." [PSI:MS]
is_a: MS:1000603 ! Shimadzu Scientific Instruments instrument model */

#define MS_LCMS_2010A_ID "MS:1000606"
#define MS_LCMS_2010A_NAME "LCMS-2010A"
/* def: "Shimadzu Scientific Instruments LCMS-2010A MS." [PSI:MS]
is_a: MS:1000603 ! Shimadzu Scientific Instruments instrument model */

#define MS_AXIMA_CFR_MALDI_TOF_ID "MS:1000607"
#define MS_AXIMA_CFR_MALDI_TOF_NAME "AXIMA CFR MALDI-TOF"
/* def: "Shimadzu Biotech AXIMA CFR MALDI-TOF MS." [PSI:MS]
is_a: MS:1000602 ! Shimadzu MALDI-TOF instrument model */

#define MS_AXIMA_QIT_ID "MS:1000608"
#define MS_AXIMA_QIT_NAME "AXIMA-QIT"
/* def: "Shimadzu Biotech AXIMA-QIT MS." [PSI:MS]
is_a: MS:1000602 ! Shimadzu MALDI-TOF instrument model */

#define MS_AXIMA_CFR_PLUS_ID "MS:1000609"
#define MS_AXIMA_CFR_PLUS_NAME "AXIMA-CFR plus"
/* def: "Shimadzu Biotech AXIMA-CFR plus MS." [PSI:MS]
is_a: MS:1000602 ! Shimadzu MALDI-TOF instrument model */

#define MS_AXIMA_PERFORMANCE_MALDI_TOF_TOF_ID "MS:1000610"
#define MS_AXIMA_PERFORMANCE_MALDI_TOF_TOF_NAME "AXIMA Performance MALDI-TOF_TOF"
/* def: "Shimadzu Biotech AXIMA Performance MALDI-TOF/TOF MS." [PSI:MS]
is_a: MS:1000602 ! Shimadzu MALDI-TOF instrument model */

#define MS_AXIMA_CONFIDENCE_MALDI_TOF_ID "MS:1000611"
#define MS_AXIMA_CONFIDENCE_MALDI_TOF_NAME "AXIMA Confidence MALDI-TOF"
/* def: "Shimadzu Biotech AXIMA Confidence MALDI-TOF (curved field reflectron) MS." [PSI:MS]
is_a: MS:1000602 ! Shimadzu MALDI-TOF instrument model */

#define MS_AXIMA_ASSURANCE_LINEAR_MALDI_TOF_ID "MS:1000612"
#define MS_AXIMA_ASSURANCE_LINEAR_MALDI_TOF_NAME "AXIMA Assurance Linear MALDI-TOF"
/* def: "Shimadzu Biotech AXIMA Assurance Linear MALDI-TOF MS." [PSI:MS]
is_a: MS:1000602 ! Shimadzu MALDI-TOF instrument model */

#define MS_DTA_FILE_ID "MS:1000613"
#define MS_DTA_FILE_NAME "DTA file"
/* def: "SEQUEST DTA file format." [PSI:MS]
is_a: MS:1000560 ! mass spectrometer file format */

#define MS_PROTEINLYNX_GLOBAL_SERVER_MASS_SPECTRUM_XML_FILE_ID "MS:1000614"
#define MS_PROTEINLYNX_GLOBAL_SERVER_MASS_SPECTRUM_XML_FILE_NAME "ProteinLynx Global Server mass spectrum XML file"
/* def: "Peak list file format used by ProteinLynx Global Server." [PSI:MS]
is_a: MS:1000560 ! mass spectrometer file format */

#define MS_PROTEOWIZARD_SOFTWARE_ID "MS:1000615"
#define MS_PROTEOWIZARD_SOFTWARE_NAME "ProteoWizard software"
/* def: "ProteoWizard software for data processing and analysis. Primarily developed by the labs of P. Malick and D. Tabb." [PSI:MS]
synonym: "pwiz" EXACT []
is_a: MS:1001456 ! analysis software
is_a: MS:1001457 ! data processing software */

#define MS_PRESET_SCAN_CONFIGURATION_ID "MS:1000616"
#define MS_PRESET_SCAN_CONFIGURATION_NAME "preset scan configuration"
/* def: "A user-defined scan configuration that specifies the instrumental settings in which a spectrum is acquired. An instrument may cycle through a list of preset scan configurations to acquire data. This is a more generic term for the Thermo \"scan event\", which is defined in the Thermo Xcalibur glossary as: a mass spectrometer scan that is defined by choosing the necessary scan parameter settings. Multiple scan events can be defined for each segment of time." [PSI:MS]
xref: value-type:xsd\:string "The allowed value-type for this CV term."
is_a: MS:1000503 ! scan attribute */

#define MS_WAVELENGTH_ARRAY_ID "MS:1000617"
#define MS_WAVELENGTH_ARRAY_NAME "wavelength array"
/* def: "A data array of electromagnetic radiation wavelength values." [PSI:MS]
xref: binary-data-type:MS\:1000521 "32-bit float"
xref: binary-data-type:MS\:1000523 "64-bit float"
is_a: MS:1000513 ! binary data array
relationship: has_units UO:0000018 ! nanometer */

#define MS_HIGHEST_OBSERVED_WAVELENGTH_ID "MS:1000618"
#define MS_HIGHEST_OBSERVED_WAVELENGTH_NAME "highest observed wavelength"
/* def: "Highest wavelength observed in an EMR spectrum." [PSI:MS]
xref: value-type:xsd\:float "The allowed value-type for this CV term."
is_a: MS:1000499 ! spectrum attribute
is_a: MS:1000808 ! chromatogram attribute
relationship: has_units UO:0000018 ! nanometer */

#define MS_LOWEST_OBSERVED_WAVELENGTH_ID "MS:1000619"
#define MS_LOWEST_OBSERVED_WAVELENGTH_NAME "lowest observed wavelength"
/* def: "Lowest wavelength observed in an EMR spectrum." [PSI:MS]
xref: value-type:xsd\:float "The allowed value-type for this CV term."
is_a: MS:1000499 ! spectrum attribute
is_a: MS:1000808 ! chromatogram attribute
relationship: has_units UO:0000018 ! nanometer */

// #define MS_PDA_SPECTRUM_ID "MS:1000620"
// #define MS_PDA_SPECTRUM_NAME "PDA spectrum"
/* def: "OBSOLETE Spectrum generated from a photodiode array detector (ultraviolet/visible spectrum)." [PSI:MS]
comment: This term was made obsolete because it was replaced by absorption spectrum (MS:1000806).
is_a: MS:1000524 ! data file content
is_a: MS:1000559 ! spectrum type
is_obsolete: true */

#define MS_PHOTODIODE_ARRAY_DETECTOR_ID "MS:1000621"
#define MS_PHOTODIODE_ARRAY_DETECTOR_NAME "photodiode array detector"
/* def: "An array detector used to record spectra in the ultraviolet and visible region of light." [PSI:MS]
synonym: "PDA" EXACT []
is_a: MS:1000345 ! array detector */

#define MS_SURVEYOR_PDA_ID "MS:1000622"
#define MS_SURVEYOR_PDA_NAME "Surveyor PDA"
/* def: "Surveyor PDA." [PSI:MS]
is_a: MS:1000494 ! Thermo Scientific instrument model */

#define MS_ACCELA_PDA_ID "MS:1000623"
#define MS_ACCELA_PDA_NAME "Accela PDA"
/* def: "Accela PDA." [PSI:MS]
is_a: MS:1000494 ! Thermo Scientific instrument model */

#define MS_INDUCTIVE_DETECTOR_ID "MS:1000624"
#define MS_INDUCTIVE_DETECTOR_NAME "inductive detector"
/* def: "Inductive detector." [PSI:MS]
synonym: "image current detector" EXACT []
is_a: MS:1000026 ! detector type */

#define MS_CHROMATOGRAM_ID "MS:1000625"
#define MS_CHROMATOGRAM_NAME "chromatogram"
/* def: "The representation of detector response versus time." [PSI:MS]
relationship: part_of MS:1001458 ! spectrum generation information */

#define MS_CHROMATOGRAM_TYPE_ID "MS:1000626"
#define MS_CHROMATOGRAM_TYPE_NAME "chromatogram type"
/* def: "Broad category or type of a chromatogram." [PSI:MS]
relationship: part_of MS:1000625 ! chromatogram */

#define MS_SELECTED_ION_CURRENT_CHROMATOGRAM_ID "MS:1000627"
#define MS_SELECTED_ION_CURRENT_CHROMATOGRAM_NAME "selected ion current chromatogram"
/* def: "Chromatogram created by creating an array of the measurements of a specific single ion current at each time point." [PSI:MS]
synonym: "SIC chromatogram" EXACT []
is_a: MS:1000524 ! data file content
is_a: MS:1000810 ! mass chromatogram */

#define MS_BASEPEAK_CHROMATOGRAM_ID "MS:1000628"
#define MS_BASEPEAK_CHROMATOGRAM_NAME "basepeak chromatogram"
/* def: "Chromatogram created by creating an array of the most intense peaks at each time point." [PSI:MS]
is_a: MS:1000524 ! data file content
is_a: MS:1000810 ! mass chromatogram */

#define MS_LOW_INTENSITY_THRESHOLD_ID "MS:1000629"
#define MS_LOW_INTENSITY_THRESHOLD_NAME "low intensity threshold"
/* def: "Threshold below which some action is taken." [PSI:MS]
xref: value-type:xsd\:float "The allowed value-type for this CV term."
is_a: MS:1000630 ! data processing parameter
relationship: has_units MS:1000131 ! number of detector counts
relationship: has_units MS:1000132 ! percent of base peak
relationship: has_units MS:1000814 ! counts per second
relationship: has_units MS:1000905 ! percent of base peak times 100
relationship: has_units UO:0000269 ! absorbance unit */

#define MS_DATA_PROCESSING_PARAMETER_ID "MS:1000630"
#define MS_DATA_PROCESSING_PARAMETER_NAME "data processing parameter"
/* def: "Data processing parameter used in the data processing performed on the data file." [PSI:MS]
relationship: part_of MS:1001458 ! spectrum generation information */

#define MS_HIGH_INTENSITY_THRESHOLD_ID "MS:1000631"
#define MS_HIGH_INTENSITY_THRESHOLD_NAME "high intensity threshold"
/* def: "Threshold above which some action is taken." [PSI:MS]
xref: value-type:xsd\:float "The allowed value-type for this CV term."
is_a: MS:1000630 ! data processing parameter
relationship: has_units MS:1000131 ! number of detector counts
relationship: has_units MS:1000132 ! percent of base peak
relationship: has_units MS:1000814 ! counts per second
relationship: has_units MS:1000905 ! percent of base peak times 100
relationship: has_units UO:0000269 ! absorbance unit */

#define MS_Q_TOF_PREMIER_ID "MS:1000632"
#define MS_Q_TOF_PREMIER_NAME "Q-Tof Premier"
/* def: "Waters oa-ToF based Q-Tof Premier." [PSI:MS]
is_a: MS:1000126 ! Waters instrument model */

#define MS_POSSIBLE_CHARGE_STATE_ID "MS:1000633"
#define MS_POSSIBLE_CHARGE_STATE_NAME "possible charge state"
/* def: "A possible charge state of the ion in a situation where the charge of an ion is known to be one of several possible values rather than a completely unknown value or determined to be a specific charge with reasonable certainty." [PSI:MS]
xref: value-type:xsd\:int "The allowed value-type for this CV term."
is_a: MS:1000455 ! ion selection attribute */

#define MS_DSQ_ID "MS:1000634"
#define MS_DSQ_NAME "DSQ"
/* def: "ThermoFinnigan DSQ GC-MS." [PSI:MS]
is_a: MS:1000125 ! Thermo Finnigan instrument model */

#define MS_ITQ_700_ID "MS:1000635"
#define MS_ITQ_700_NAME "ITQ 700"
/* def: "Thermo Scientific ITQ 700 GC-MS." [PSI:MS]
is_a: MS:1000494 ! Thermo Scientific instrument model */

#define MS_ITQ_900_ID "MS:1000636"
#define MS_ITQ_900_NAME "ITQ 900"
/* def: "Thermo Scientific ITQ 900 GC-MS." [PSI:MS]
is_a: MS:1000494 ! Thermo Scientific instrument model */

#define MS_ITQ_1100_ID "MS:1000637"
#define MS_ITQ_1100_NAME "ITQ 1100"
/* def: "Thermo Scientific ITQ 1100 GC-MS." [PSI:MS]
is_a: MS:1000494 ! Thermo Scientific instrument model */

#define MS_LTQ_XL_ETD_ID "MS:1000638"
#define MS_LTQ_XL_ETD_NAME "LTQ XL ETD"
/* def: "Thermo Scientific LTQ XL MS with ETD." [PSI:MS]
is_a: MS:1000494 ! Thermo Scientific instrument model */

#define MS_LTQ_ORBITRAP_XL_ETD_ID "MS:1000639"
#define MS_LTQ_ORBITRAP_XL_ETD_NAME "LTQ Orbitrap XL ETD"
/* def: "Thermo Scientific LTQ Orbitrap XL MS with ETD." [PSI:MS]
is_a: MS:1000494 ! Thermo Scientific instrument model */

#define MS_DFS_ID "MS:1000640"
#define MS_DFS_NAME "DFS"
/* def: "Thermo Scientific DFS HR GC-MS." [PSI:MS]
is_a: MS:1000494 ! Thermo Scientific instrument model */

#define MS_DSQ_II_ID "MS:1000641"
#define MS_DSQ_II_NAME "DSQ II"
/* def: "Thermo Scientific DSQ II GC-MS." [PSI:MS]
is_a: MS:1000494 ! Thermo Scientific instrument model */

#define MS_MALDI_LTQ_XL_ID "MS:1000642"
#define MS_MALDI_LTQ_XL_NAME "MALDI LTQ XL"
/* def: "Thermo Scientific MALDI LTQ XL MS." [PSI:MS]
is_a: MS:1000494 ! Thermo Scientific instrument model */

#define MS_MALDI_LTQ_ORBITRAP_ID "MS:1000643"
#define MS_MALDI_LTQ_ORBITRAP_NAME "MALDI LTQ Orbitrap"
/* def: "Thermo Scientific MALDI LTQ Orbitrap MS." [PSI:MS]
is_a: MS:1000494 ! Thermo Scientific instrument model */

#define MS_TSQ_QUANTUM_ACCESS_ID "MS:1000644"
#define MS_TSQ_QUANTUM_ACCESS_NAME "TSQ Quantum Access"
/* def: "Thermo Scientific TSQ Quantum Access MS." [PSI:MS]
is_a: MS:1000494 ! Thermo Scientific instrument model */

#define MS_ELEMENT_XR_ID "MS:1000645"
#define MS_ELEMENT_XR_NAME "Element XR"
/* def: "Thermo Scientific Element XR HR-ICP-MS." [PSI:MS]
is_a: MS:1000494 ! Thermo Scientific instrument model */

#define MS_ELEMENT_2_ID "MS:1000646"
#define MS_ELEMENT_2_NAME "Element 2"
/* def: "Thermo Scientific Element 2 HR-ICP-MS." [PSI:MS]
is_a: MS:1000494 ! Thermo Scientific instrument model */

#define MS_ELEMENT_GD_ID "MS:1000647"
#define MS_ELEMENT_GD_NAME "Element GD"
/* def: "Thermo Scientific Element GD Glow Discharge MS." [PSI:MS]
is_a: MS:1000494 ! Thermo Scientific instrument model */

#define MS_GC_ISOLINK_ID "MS:1000648"
#define MS_GC_ISOLINK_NAME "GC IsoLink"
/* def: "Thermo Scientific GC IsoLink Isotope Ratio MS." [PSI:MS]
is_a: MS:1000494 ! Thermo Scientific instrument model */

#define MS_EXACTIVE_ID "MS:1000649"
#define MS_EXACTIVE_NAME "Exactive"
/* def: "Thermo Scientific Exactive MS." [PSI:MS]
is_a: MS:1000494 ! Thermo Scientific instrument model */

#define MS_PROTEOME_DISCOVERER_ID "MS:1000650"
#define MS_PROTEOME_DISCOVERER_NAME "Proteome Discoverer"
/* def: "Thermo Scientific software for data analysis of peptides and proteins." [PSI:MS]
is_a: MS:1000693 ! Thermo Finnigan software
is_a: MS:1001456 ! analysis software
is_a: MS:1001457 ! data processing software */

#define MS__3200_QTRAP_ID "MS:1000651"
#define MS__3200_QTRAP_NAME "3200 QTRAP"
/* def: "AB SCIEX or Applied Biosystems|MDS SCIEX QTRAP 3200." [PSI:MS]
is_a: MS:1000121 ! AB SCIEX instrument model */

#define MS__4800_PLUS_MALDI_TOF_TOF_ID "MS:1000652"
#define MS__4800_PLUS_MALDI_TOF_TOF_NAME "4800 Plus MALDI TOF_TOF"
/* def: "AB SCIEX or Applied Biosystems|MDS SCIEX 4800 Plus MALDI TOF-TOF Analyzer." [PSI:MS]
is_a: MS:1000121 ! AB SCIEX instrument model */

#define MS_API_3200_ID "MS:1000653"
#define MS_API_3200_NAME "API 3200"
/* def: "AB SCIEX or Applied Biosystems|MDS SCIEX API 3200 MS." [PSI:MS]
is_a: MS:1000121 ! AB SCIEX instrument model */

#define MS_API_5000_ID "MS:1000654"
#define MS_API_5000_NAME "API 5000"
/* def: "AB SCIEX or Applied Biosystems|MDS SCIEX API 5000 MS." [PSI:MS]
is_a: MS:1000121 ! AB SCIEX instrument model */

#define MS_QSTAR_ELITE_ID "MS:1000655"
#define MS_QSTAR_ELITE_NAME "QSTAR Elite"
/* def: "AB SCIEX or Applied Biosystems|MDS SCIEX QSTAR Elite." [PSI:MS]
is_a: MS:1000121 ! AB SCIEX instrument model */

#define MS_QSTAR_PULSAR_ID "MS:1000656"
#define MS_QSTAR_PULSAR_NAME "QSTAR Pulsar"
/* def: "Applied Biosystems|MDS SCIEX QSTAR Pulsar." [PSI:MS]
is_a: MS:1000121 ! AB SCIEX instrument model */

#define MS_QSTAR_XL_ID "MS:1000657"
#define MS_QSTAR_XL_NAME "QSTAR XL"
/* def: "Applied Biosystems|MDS SCIEX QSTAR XL." [PSI:MS]
is_a: MS:1000121 ! AB SCIEX instrument model */

#define MS__4800_PROTEOMICS_ANALYZER_ID "MS:1000658"
#define MS__4800_PROTEOMICS_ANALYZER_NAME "4800 Proteomics Analyzer"
/* def: "Applied Biosystems|MDS SCIEX 4800 Proteomics Analyzer." [PSI:MS]
is_a: MS:1000495 ! Applied Biosystems instrument model */

#define MS__4000_SERIES_EXPLORER_SOFTWARE_ID "MS:1000659"
#define MS__4000_SERIES_EXPLORER_SOFTWARE_NAME "4000 Series Explorer Software"
/* def: "AB SCIEX or Applied Biosystems software for data acquisition and analysis." [PSI:MS]
is_a: MS:1000690 ! AB SCIEX software
is_a: MS:1001455 ! acquisition software
is_a: MS:1001456 ! analysis software
is_a: MS:1001457 ! data processing software */

#define MS_GPS_EXPLORER_ID "MS:1000661"
#define MS_GPS_EXPLORER_NAME "GPS Explorer"
/* def: "AB SCIEX or Applied Biosystems software for data acquisition and analysis." [PSI:MS]
is_a: MS:1000690 ! AB SCIEX software
is_a: MS:1001455 ! acquisition software
is_a: MS:1001456 ! analysis software
is_a: MS:1001457 ! data processing software */

#define MS_LIGHTSIGHT_SOFTWARE_ID "MS:1000662"
#define MS_LIGHTSIGHT_SOFTWARE_NAME "LightSight Software"
/* def: "AB SCIEX or Applied Biosystems|MDS SCIEX software metabolite identification." [PSI:MS]
is_a: MS:1000690 ! AB SCIEX software
is_a: MS:1001456 ! analysis software
is_a: MS:1001457 ! data processing software */

#define MS_PROTEINPILOT_SOFTWARE_ID "MS:1000663"
#define MS_PROTEINPILOT_SOFTWARE_NAME "ProteinPilot Software"
/* def: "AB SCIEX or Applied Biosystems|MDS SCIEX software for protein ID and quant." [PSI:MS]
is_a: MS:1000690 ! AB SCIEX software
is_a: MS:1001456 ! analysis software
is_a: MS:1001457 ! data processing software */

#define MS_TISSUEVIEW_SOFTWARE_ID "MS:1000664"
#define MS_TISSUEVIEW_SOFTWARE_NAME "TissueView Software"
/* def: "Applied Biosystems|MDS SCIEX software for tissue imaging." [PSI:MS]
is_a: MS:1000690 ! AB SCIEX software
is_a: MS:1001456 ! analysis software
is_a: MS:1001457 ! data processing software */

#define MS_MARKERVIEW_SOFTWARE_ID "MS:1000665"
#define MS_MARKERVIEW_SOFTWARE_NAME "MarkerView Software"
/* def: "Applied Biosystems|MDS SCIEX software for metabolomics and biomarker profiling." [PSI:MS]
is_a: MS:1000690 ! AB SCIEX software
is_a: MS:1001456 ! analysis software
is_a: MS:1001457 ! data processing software */

#define MS_MRMPILOT_SOFTWARE_ID "MS:1000666"
#define MS_MRMPILOT_SOFTWARE_NAME "MRMPilot Software"
/* def: "Applied Biosystems|MDS SCIEX software for MRM assay development." [PSI:MS]
is_a: MS:1000690 ! AB SCIEX software
is_a: MS:1001456 ! analysis software
is_a: MS:1001457 ! data processing software */

#define MS_BIOANALYST_ID "MS:1000667"
#define MS_BIOANALYST_NAME "BioAnalyst"
/* def: "Applied Biosystems|MDS SCIEX software for bio-related data exploration." [PSI:MS]
is_a: MS:1000690 ! AB SCIEX software
is_a: MS:1001456 ! analysis software
is_a: MS:1001457 ! data processing software */

#define MS_PRO_ID_ID "MS:1000668"
#define MS_PRO_ID_NAME "Pro ID"
/* def: "Applied Biosystems|MDS SCIEX software for protein identification." [PSI:MS]
is_a: MS:1000690 ! AB SCIEX software
is_a: MS:1001456 ! analysis software
is_a: MS:1001457 ! data processing software */

#define MS_PRO_ICAT_ID "MS:1000669"
#define MS_PRO_ICAT_NAME "Pro ICAT"
/* def: "Applied Biosystems|MDS SCIEX software for protein ID and quant by ICAT." [PSI:MS]
is_a: MS:1000690 ! AB SCIEX software
is_a: MS:1001456 ! analysis software
is_a: MS:1001457 ! data processing software */

#define MS_PRO_QUANT_ID "MS:1000670"
#define MS_PRO_QUANT_NAME "Pro Quant"
/* def: "Applied Biosystems|MDS SCIEX software for protein ID and quant by iTRAQ." [PSI:MS]
is_a: MS:1000690 ! AB SCIEX software
is_a: MS:1001456 ! analysis software
is_a: MS:1001457 ! data processing software */

#define MS_PRO_BLAST_ID "MS:1000671"
#define MS_PRO_BLAST_NAME "Pro BLAST"
/* def: "Applied Biosystems|MDS SCIEX software for MS-BLAST identification." [PSI:MS]
is_a: MS:1000690 ! AB SCIEX software
is_a: MS:1001456 ! analysis software
is_a: MS:1001457 ! data processing software */

#define MS_CLIQUID_ID "MS:1000672"
#define MS_CLIQUID_NAME "Cliquid"
/* def: "AB SCIEX Cliquid software for data analysis and quantitation." [PSI:MS]
is_a: MS:1000690 ! AB SCIEX software */

#define MS_MIDAS_WORKFLOW_DESIGNER_ID "MS:1000673"
#define MS_MIDAS_WORKFLOW_DESIGNER_NAME "MIDAS Workflow Designer"
/* def: "Applied Biosystems|MDS SCIEX software for MRM assay development." [PSI:MS]
is_a: MS:1000690 ! AB SCIEX software */

#define MS_MULTIQUANT_ID "MS:1000674"
#define MS_MULTIQUANT_NAME "MultiQuant"
/* def: "Applied Biosystems|MDS SCIEX software for MRM-based quantitation." [PSI:MS]
is_a: MS:1000690 ! AB SCIEX software
is_a: MS:1001456 ! analysis software
is_a: MS:1001457 ! data processing software */

#define MS__6220_TIME_OF_FLIGHT_LC_MS_ID "MS:1000675"
#define MS__6220_TIME_OF_FLIGHT_LC_MS_NAME "6220 Time-of-Flight LC_MS"
/* def: "The 6220 Time-of-Flight LC/MS is a Agilent liquid chromatography instrument combined with a Agilent time of flight mass spectrometer. This time of flight mass spectrometer has a m/z range of 50-12000, mass accuracy of less than 2 ppm and resolution greater than 13,000 at m/z 2722. It has multiple ion sources and can be used with multimode ion sources." [PSI:MS]
is_a: MS:1000490 ! Agilent instrument model */

#define MS__6510_QUADRUPOLE_TIME_OF_FLIGHT_LC_MS_ID "MS:1000676"
#define MS__6510_QUADRUPOLE_TIME_OF_FLIGHT_LC_MS_NAME "6510 Quadrupole Time-of-Flight LC_MS"
/* def: "The 6510 Quadrupole Time-of-Flight LC/MS is a Agilent liquid chromatography instrument combined with a Agilent time of flight mass spectrometer. This time of flight mass spectrometer has a m/z range of 50-12000, mass accuracy of less than 2 ppm and resolution greater than 13,000 at m/z 2722. It has multiple ion sources and can be used with multimode ion sources." [PSI:MS]
is_a: MS:1000490 ! Agilent instrument model */

#define MS__6520_QUADRUPOLE_TIME_OF_FLIGHT_LC_MS_ID "MS:1000677"
#define MS__6520_QUADRUPOLE_TIME_OF_FLIGHT_LC_MS_NAME "6520 Quadrupole Time-of-Flight LC_MS"
/* def: "The 6520 Quadrupole Time-of-Flight LC/MS is a Agilent liquid chromatography instrument combined with a Agilent time of flight mass spectrometer. This time of flight mass spectrometer has a m/z range of 50-12000, mass accuracy of less than 2 ppm and resolution greater than 26,000 at m/z 2722. It has multiple ion sources and can be used with multimode ion sources." [PSI:MS]
is_a: MS:1000490 ! Agilent instrument model */

#define MS_MASSHUNTER_DATA_ACQUISITION_ID "MS:1000678"
#define MS_MASSHUNTER_DATA_ACQUISITION_NAME "MassHunter Data Acquisition"
/* def: "Software for data acquisition of 6000 series instruments." [PSI:MS]
is_a: MS:1000689 ! Agilent software
is_a: MS:1001455 ! acquisition software */

#define MS_MASSHUNTER_EASY_ACCESS_ID "MS:1000679"
#define MS_MASSHUNTER_EASY_ACCESS_NAME "MassHunter Easy Access"
/* def: "Software for open access data acquisition." [PSI:MS]
is_a: MS:1000689 ! Agilent software
is_a: MS:1001455 ! acquisition software */

#define MS_MASSHUNTER_QUALITATIVE_ANALYSIS_ID "MS:1000680"
#define MS_MASSHUNTER_QUALITATIVE_ANALYSIS_NAME "MassHunter Qualitative Analysis"
/* def: "Software for data analysis of data from 6000 series instruments." [PSI:MS]
is_a: MS:1000689 ! Agilent software
is_a: MS:1001456 ! analysis software
is_a: MS:1001457 ! data processing software */

#define MS_MASSHUNTER_QUANTITATIVE_ANALYSIS_ID "MS:1000681"
#define MS_MASSHUNTER_QUANTITATIVE_ANALYSIS_NAME "MassHunter Quantitative Analysis"
/* def: "Software for quantitation of Triple Quadruople and Quadrupole Time-of-Flight data." [PSI:MS]
is_a: MS:1000689 ! Agilent software
is_a: MS:1001456 ! analysis software
is_a: MS:1001457 ! data processing software */

#define MS_MASSHUNTER_METABOLITE_ID_ID "MS:1000682"
#define MS_MASSHUNTER_METABOLITE_ID_NAME "MassHunter Metabolite ID"
/* def: "Software for identification of metabolites." [PSI:MS]
is_a: MS:1000689 ! Agilent software
is_a: MS:1001456 ! analysis software
is_a: MS:1001457 ! data processing software */

#define MS_MASSHUNTER_BIOCONFIRM_ID "MS:1000683"
#define MS_MASSHUNTER_BIOCONFIRM_NAME "MassHunter BioConfirm"
/* def: "Software for protein characterization." [PSI:MS]
is_a: MS:1000689 ! Agilent software
is_a: MS:1001456 ! analysis software
is_a: MS:1001457 ! data processing software */

#define MS_GENESPRING_MS_ID "MS:1000684"
#define MS_GENESPRING_MS_NAME "Genespring MS"
/* def: "Software for quantitation and statistical analysis of TOF and Q-TOF LC/MS data." [PSI:MS]
is_a: MS:1000689 ! Agilent software
is_a: MS:1001456 ! analysis software
is_a: MS:1001457 ! data processing software */

#define MS_MASSHUNTER_MASS_PROFILER_ID "MS:1000685"
#define MS_MASSHUNTER_MASS_PROFILER_NAME "MassHunter Mass Profiler"
/* def: "Software for quantitation and statistical analysis of TOF and Q-TOF LC/MS data." [PSI:MS]
is_a: MS:1000689 ! Agilent software
is_a: MS:1001456 ! analysis software
is_a: MS:1001457 ! data processing software */

#define MS_METLIN_ID "MS:1000686"
#define MS_METLIN_NAME "METLIN"
/* def: "Personal Metabolite Database for MassHunter Workstation. Software for identification of human metabolites." [PSI:MS]
is_a: MS:1000689 ! Agilent software
is_a: MS:1001456 ! analysis software
is_a: MS:1001457 ! data processing software */

#define MS_SPECTRUM_MILL_FOR_MASSHUNTER_WORKSTATION_ID "MS:1000687"
#define MS_SPECTRUM_MILL_FOR_MASSHUNTER_WORKSTATION_NAME "Spectrum Mill for MassHunter Workstation"
/* def: "Software for protein identification and characterization of complex protein digest mixtures." [PSI:MS]
is_a: MS:1000689 ! Agilent software
is_a: MS:1001456 ! analysis software
is_a: MS:1001457 ! data processing software */

#define MS__6300_SERIES_ION_TRAP_DATA_ANALYSIS_SOFTWARE_ID "MS:1000688"
#define MS__6300_SERIES_ION_TRAP_DATA_ANALYSIS_SOFTWARE_NAME "6300 Series Ion Trap Data Analysis Software"
/* def: "Software for data analysis of 6300 series ion trap mass spectrometers." [PSI:MS]
is_a: MS:1000689 ! Agilent software
is_a: MS:1001455 ! acquisition software
is_a: MS:1001456 ! analysis software
is_a: MS:1001457 ! data processing software */

#define MS_AGILENT_SOFTWARE_ID "MS:1000689"
#define MS_AGILENT_SOFTWARE_NAME "Agilent software"
/* def: "Agilent software for data acquisition and analysis." [PSI:MS]
is_a: MS:1000531 ! software */

#define MS_AB_SCIEX_SOFTWARE_ID "MS:1000690"
#define MS_AB_SCIEX_SOFTWARE_NAME "AB SCIEX software"
/* def: "AB SCIEX or Applied Biosystems software for data acquisition and analysis." [PSI:MS]
is_a: MS:1000531 ! software */

#define MS_APPLIED_BIOSYSTEMS_SOFTWARE_ID "MS:1000691"
#define MS_APPLIED_BIOSYSTEMS_SOFTWARE_NAME "Applied Biosystems software"
/* def: "Applied Biosystems|MDS SCIEX software for data acquisition and analysis." [PSI:MS]
is_a: MS:1000531 ! software */

#define MS_BRUKER_SOFTWARE_ID "MS:1000692"
#define MS_BRUKER_SOFTWARE_NAME "Bruker software"
/* def: "Bruker software for data acquisition and analysis." [PSI:MS]
is_a: MS:1000531 ! software */

#define MS_THERMO_FINNIGAN_SOFTWARE_ID "MS:1000693"
#define MS_THERMO_FINNIGAN_SOFTWARE_NAME "Thermo Finnigan software"
/* def: "Thermo Finnigan software for data acquisition and analysis." [PSI:MS]
synonym: "Bioworks Browser" RELATED []
is_a: MS:1000531 ! software */

#define MS_WATERS_SOFTWARE_ID "MS:1000694"
#define MS_WATERS_SOFTWARE_NAME "Waters software"
/* def: "Waters software for data acquisition and analysis." [PSI:MS]
is_a: MS:1000531 ! software */

#define MS_APEX_ULTRA_ID "MS:1000695"
#define MS_APEX_ULTRA_NAME "apex ultra"
/* def: "Bruker Daltonics' apex ultra: ESI, MALDI, Nanospray, APCI, APPI, Qh-FT_ICR." [PSI:MS]
is_a: MS:1001556 ! Bruker Daltonics apex series */

#define MS_AUTOFLEX_III_SMARTBEAM_ID "MS:1000696"
#define MS_AUTOFLEX_III_SMARTBEAM_NAME "autoflex III smartbeam"
/* def: "Bruker Daltonics' autoflex III smartbeam: MALDI TOF." [PSI:MS]
is_a: MS:1001534 ! Bruker Daltonics flex series */

#define MS_BRUKER_DALTONICS_HCT_SERIES_ID "MS:1000697"
#define MS_BRUKER_DALTONICS_HCT_SERIES_NAME "Bruker Daltonics HCT Series"
/* def: "Bruker Daltonics' HCT Series." [PSI:MS]
is_a: MS:1000122 ! Bruker Daltonics instrument model */

#define MS_HCTULTRA_ID "MS:1000698"
#define MS_HCTULTRA_NAME "HCTultra"
/* def: "Bruker Daltonics' HCTultra: ESI TOF, Nanospray, APCI, APPI." [PSI:MS]
is_a: MS:1000697 ! Bruker Daltonics HCT Series */

#define MS_HCTULTRA_PTM_ID "MS:1000699"
#define MS_HCTULTRA_PTM_NAME "HCTultra PTM"
/* def: "Bruker Daltonics' HCTultra PTM: ESI TOF, Nanospray, APCI, APPI, PTR." [PSI:MS]
is_a: MS:1000697 ! Bruker Daltonics HCT Series */

#define MS_HCTULTRA_ETD_II_ID "MS:1000700"
#define MS_HCTULTRA_ETD_II_NAME "HCTultra ETD II"
/* def: "Bruker Daltonics' HCTultra ETD II: ESI Q-TOF, Nanospray, APCI, APPI, ETD." [PSI:MS]
is_a: MS:1000697 ! Bruker Daltonics HCT Series */

#define MS_MICROFLEX_LT_ID "MS:1000701"
#define MS_MICROFLEX_LT_NAME "microflex LT"
/* def: "Bruker Daltonics' microflex LT: MALDI TOF." [PSI:MS]
is_a: MS:1001534 ! Bruker Daltonics flex series */

#define MS_MICROTOF_ID "MS:1000702"
#define MS_MICROTOF_NAME "micrOTOF"
/* def: "Bruker Daltonics' micrOTOF: ESI TOF, APCI, APPI." [PSI:MS]
is_a: MS:1001536 ! Bruker Daltonics micrOTOF series */

#define MS_MICROTOF_Q_ID "MS:1000703"
#define MS_MICROTOF_Q_NAME "micrOTOF-Q"
/* def: "Bruker Daltonics' micrOTOF-Q: ESI Q-TOF, Nanospray, APCI, APPI." [PSI:MS]
is_a: MS:1001536 ! Bruker Daltonics micrOTOF series */

#define MS_MICROTOF_Q_II_ID "MS:1000704"
#define MS_MICROTOF_Q_II_NAME "micrOTOF-Q II"
/* def: "Bruker Daltonics' micrOTOF-Q II: ESI Q-TOF, Nanospray, APCI, APPI." [PSI:MS]
is_a: MS:1001536 ! Bruker Daltonics micrOTOF series */

#define MS_ULTRAFLEX_III_TOF_TOF_ID "MS:1000705"
#define MS_ULTRAFLEX_III_TOF_TOF_NAME "ultraflex III TOF_TOF"
/* def: "Bruker Daltonics' ultraflex III TOF/TOF: MALDI TOF." [PSI:MS]
is_a: MS:1001534 ! Bruker Daltonics flex series */

#define MS_APEXCONTROL_ID "MS:1000706"
#define MS_APEXCONTROL_NAME "apexControl"
/* def: "Bruker software for data acquisition." [PSI:MS]
is_a: MS:1000692 ! Bruker software
is_a: MS:1001455 ! acquisition software */

#define MS_BIOTOOLS_ID "MS:1000707"
#define MS_BIOTOOLS_NAME "BioTools"
/* def: "Bruker software for data analysis." [PSI:MS]
is_a: MS:1000692 ! Bruker software
is_a: MS:1001456 ! analysis software
is_a: MS:1001457 ! data processing software */

#define MS_CLINPROT_ID "MS:1000708"
#define MS_CLINPROT_NAME "CLINPROT"
/* def: "Bruker CLINPROT software." [PSI:MS]
is_a: MS:1000692 ! Bruker software
is_a: MS:1001456 ! analysis software
is_a: MS:1001457 ! data processing software */

#define MS_CLINPROT_MICRO_ID "MS:1000709"
#define MS_CLINPROT_MICRO_NAME "CLINPROT micro"
/* def: "Bruker CLINPROT micro software." [PSI:MS]
is_a: MS:1000692 ! Bruker software
is_a: MS:1001456 ! analysis software
is_a: MS:1001457 ! data processing software */

#define MS_CLINPROT_ROBOT_ID "MS:1000710"
#define MS_CLINPROT_ROBOT_NAME "CLINPROT robot"
/* def: "Bruker CLINPROT robot software." [PSI:MS]
is_a: MS:1000692 ! Bruker software */

#define MS_CLINPROTOOLS_ID "MS:1000711"
#define MS_CLINPROTOOLS_NAME "ClinProTools"
/* def: "Bruker ClinProTools software." [PSI:MS]
is_a: MS:1000692 ! Bruker software */

#define MS_COMPASS_ID "MS:1000712"
#define MS_COMPASS_NAME "Compass"
/* def: "Bruker Compass software." [PSI:MS]
is_a: MS:1000692 ! Bruker software
is_a: MS:1001456 ! analysis software
is_a: MS:1001457 ! data processing software */

#define MS_COMPASS_FOR_HCT_ESQUIRE_ID "MS:1000713"
#define MS_COMPASS_FOR_HCT_ESQUIRE_NAME "Compass for HCT_esquire"
/* def: "Bruker Compass for HCT/esquire software." [PSI:MS]
is_a: MS:1000692 ! Bruker software
is_a: MS:1001456 ! analysis software
is_a: MS:1001457 ! data processing software */

#define MS_COMPASS_FOR_MICROTOF_ID "MS:1000714"
#define MS_COMPASS_FOR_MICROTOF_NAME "Compass for micrOTOF"
/* def: "Bruker Compass for micrOTOF software." [PSI:MS]
is_a: MS:1000692 ! Bruker software
is_a: MS:1001456 ! analysis software
is_a: MS:1001457 ! data processing software */

#define MS_COMPASS_OPENACCESS_ID "MS:1000715"
#define MS_COMPASS_OPENACCESS_NAME "Compass OpenAccess"
/* def: "Bruker compass OpenAccess software." [PSI:MS]
is_a: MS:1000692 ! Bruker software
is_a: MS:1001456 ! analysis software
is_a: MS:1001457 ! data processing software */

#define MS_COMPASS_SECURITY_PACK_ID "MS:1000716"
#define MS_COMPASS_SECURITY_PACK_NAME "Compass Security Pack"
/* def: "Bruker compass Security Pack software." [PSI:MS]
is_a: MS:1000692 ! Bruker software */

#define MS_COMPASSXPORT_ID "MS:1000717"
#define MS_COMPASSXPORT_NAME "CompassXport"
/* def: "Bruker stand-alone software for data conversion." [PSI:MS]
is_a: MS:1000692 ! Bruker software
is_a: MS:1001457 ! data processing software */

#define MS_COMPASSXTRACT_ID "MS:1000718"
#define MS_COMPASSXTRACT_NAME "CompassXtract"
/* def: "Bruker software library for data access." [PSI:MS]
is_a: MS:1000692 ! Bruker software
is_a: MS:1001457 ! data processing software */

#define MS_DATAANALYSIS_ID "MS:1000719"
#define MS_DATAANALYSIS_NAME "DataAnalysis"
/* def: "Bruker software for data analysis." [PSI:MS]
is_a: MS:1000692 ! Bruker software
is_a: MS:1001456 ! analysis software
is_a: MS:1001457 ! data processing software */

#define MS_DPCONTROL_ID "MS:1000720"
#define MS_DPCONTROL_NAME "dpControl"
/* def: "Bruker software for data acquisition." [PSI:MS]
is_a: MS:1000692 ! Bruker software
is_a: MS:1001455 ! acquisition software */

#define MS_ESQUIRECONTROL_ID "MS:1000721"
#define MS_ESQUIRECONTROL_NAME "esquireControl"
/* def: "Bruker software for data acquisition." [PSI:MS]
is_a: MS:1000692 ! Bruker software
is_a: MS:1001455 ! acquisition software */

#define MS_FLEXIMAGING_ID "MS:1000722"
#define MS_FLEXIMAGING_NAME "flexImaging"
/* def: "Bruker software for data analysis." [PSI:MS]
is_a: MS:1000692 ! Bruker software
is_a: MS:1001456 ! analysis software
is_a: MS:1001457 ! data processing software */

#define MS_GENOLINK_ID "MS:1000723"
#define MS_GENOLINK_NAME "GENOLINK"
/* def: "Bruker GENOLINK software." [PSI:MS]
is_a: MS:1000692 ! Bruker software */

#define MS_GENOTOOLS_ID "MS:1000724"
#define MS_GENOTOOLS_NAME "GenoTools"
/* def: "Bruker GenoTools software." [PSI:MS]
is_a: MS:1000692 ! Bruker software */

#define MS_HCTCONTROL_ID "MS:1000725"
#define MS_HCTCONTROL_NAME "HCTcontrol"
/* def: "Bruker software for data acquisition." [PSI:MS]
is_a: MS:1000692 ! Bruker software
is_a: MS:1001455 ! acquisition software */

#define MS_MICROTOFCONTROL_ID "MS:1000726"
#define MS_MICROTOFCONTROL_NAME "micrOTOFcontrol"
/* def: "Bruker software for data acquisition." [PSI:MS]
is_a: MS:1000692 ! Bruker software
is_a: MS:1001455 ! acquisition software */

#define MS_POLYTOOLS_ID "MS:1000727"
#define MS_POLYTOOLS_NAME "PolyTools"
/* def: "Bruker PolyTools software." [PSI:MS]
is_a: MS:1000692 ! Bruker software */

#define MS_PROFILEANALYSIS_ID "MS:1000728"
#define MS_PROFILEANALYSIS_NAME "ProfileAnalysis"
/* def: "Bruker software for data analysis." [PSI:MS]
is_a: MS:1000692 ! Bruker software
is_a: MS:1001456 ! analysis software
is_a: MS:1001457 ! data processing software */

#define MS_PROTEINEER_ID "MS:1000729"
#define MS_PROTEINEER_NAME "PROTEINEER"
/* def: "Bruker PROTEINEER software." [PSI:PI]
is_a: MS:1000692 ! Bruker software */

#define MS_PROTEINEER_DP_ID "MS:1000730"
#define MS_PROTEINEER_DP_NAME "PROTEINEER dp"
/* def: "Bruker PROTEINEER dp software." [PSI:PI]
is_a: MS:1000692 ! Bruker software */

#define MS_PROTEINEER_FC_ID "MS:1000731"
#define MS_PROTEINEER_FC_NAME "PROTEINEER fc"
/* def: "Bruker PROTEINEER fc software." [PSI:PI]
is_a: MS:1000692 ! Bruker software */

#define MS_PROTEINEER_SPII_ID "MS:1000732"
#define MS_PROTEINEER_SPII_NAME "PROTEINEER spII"
/* def: "Bruker PROTEINEER spII software." [PSI:PI]
is_a: MS:1000692 ! Bruker software */

#define MS_PROTEINEER_LC_ID "MS:1000733"
#define MS_PROTEINEER_LC_NAME "PROTEINEER-LC"
/* def: "Bruker PROTEINEER-LC software." [PSI:PI]
is_a: MS:1000692 ! Bruker software */

#define MS_PROTEINSCAPE_ID "MS:1000734"
#define MS_PROTEINSCAPE_NAME "ProteinScape"
/* def: "Bruker ProteinScape software." [PSI:PI]
is_a: MS:1000692 ! Bruker software
is_a: MS:1001456 ! analysis software */

#define MS_PUREDISK_ID "MS:1000735"
#define MS_PUREDISK_NAME "PureDisk"
/* def: "BrukerPureDisk software." [PSI:MS]
is_a: MS:1000692 ! Bruker software */

#define MS_QUANTANALYSIS_ID "MS:1000736"
#define MS_QUANTANALYSIS_NAME "QuantAnalysis"
/* def: "Bruker software for data analysis." [PSI:MS]
is_a: MS:1000692 ! Bruker software
is_a: MS:1001456 ! analysis software
is_a: MS:1001457 ! data processing software */

#define MS_SPCONTROL_ID "MS:1000737"
#define MS_SPCONTROL_NAME "spControl"
/* def: "Bruker software for data acquisition." [PSI:MS]
is_a: MS:1000692 ! Bruker software
is_a: MS:1001455 ! acquisition software */

#define MS_TARGETANALYSIS_ID "MS:1000738"
#define MS_TARGETANALYSIS_NAME "TargetAnalysis"
/* def: "Bruker TargetAnalysis software." [PSI:MS]
is_a: MS:1000692 ! Bruker software */

#define MS_WARP_LC_ID "MS:1000739"
#define MS_WARP_LC_NAME "WARP-LC"
/* def: "Bruker WARP-LC software." [PSI:MS]
is_a: MS:1000692 ! Bruker software
is_a: MS:1001139 ! quantitation software name */

#define MS_PARAMETER_FILE_ID "MS:1000740"
#define MS_PARAMETER_FILE_NAME "parameter file"
/* def: "Parameter file used to configure the acquisition of raw data on the instrument." [PSI:MS]
is_a: MS:1000560 ! mass spectrometer file format */

#define MS_CONVERSION_TO_DTA_ID "MS:1000741"
#define MS_CONVERSION_TO_DTA_NAME "Conversion to dta"
/* def: "Conversion to dta format." [PSI:MS]
is_a: MS:1000530 ! file format conversion */

#define MS_BIOWORKS_SRF_FILE_ID "MS:1000742"
#define MS_BIOWORKS_SRF_FILE_NAME "Bioworks SRF file"
/* def: "Thermo Finnigan SRF file format." [PSI:MS]
is_a: MS:1000560 ! mass spectrometer file format
is_a: MS:1001040 ! intermediate analysis format */

#define MS_TSQ_QUANTUM_ULTRA_AM_ID "MS:1000743"
#define MS_TSQ_QUANTUM_ULTRA_AM_NAME "TSQ Quantum Ultra AM"
/* def: "Thermo Scientific TSQ Quantum Ultra AM." [PSI:MS]
is_a: MS:1000494 ! Thermo Scientific instrument model */

#define MS_SELECTED_ION_M_Z_ID "MS:1000744"
#define MS_SELECTED_ION_M_Z_NAME "selected ion m/z"
/* def: "Mass-to-charge ratio of an selected ion." [PSI:MS]
xref: value-type:xsd\:float "The allowed value-type for this CV term."
is_a: MS:1000455 ! ion selection attribute
relationship: has_units MS:1000040 ! m/z */

#define MS_RETENTION_TIME_ALIGNMENT_ID "MS:1000745"
#define MS_RETENTION_TIME_ALIGNMENT_NAME "retention time alignment"
/* def: "The correction of the spectrum scan times, as used e.g. in label-free proteomics." [PSI:MS]
is_a: MS:1000543 ! data processing action */

#define MS_HIGH_INTENSITY_DATA_POINT_REMOVAL_ID "MS:1000746"
#define MS_HIGH_INTENSITY_DATA_POINT_REMOVAL_NAME "high intensity data point removal"
/* def: "The removal of very high intensity data points." [PSI:MS]
is_a: MS:1001486 ! data filtering */

#define MS_COMPLETION_TIME_ID "MS:1000747"
#define MS_COMPLETION_TIME_NAME "completion time"
/* def: "The time that a data processing action was finished." [PSI:MS]
xref: value-type:xsd\:date "The allowed value-type for this CV term."
is_a: MS:1000630 ! data processing parameter */

#define MS_SSQ_7000_ID "MS:1000748"
#define MS_SSQ_7000_NAME "SSQ 7000"
/* def: "ThermoFinnigan SSQ 7000 MS." [PSI:MS]
is_a: MS:1000493 ! Finnigan MAT instrument model */

#define MS_TSQ_7000_ID "MS:1000749"
#define MS_TSQ_7000_NAME "TSQ 7000"
/* def: "ThermoFinnigan TSQ 7000 MS." [PSI:MS]
is_a: MS:1000493 ! Finnigan MAT instrument model */

#define MS_TSQ_ID "MS:1000750"
#define MS_TSQ_NAME "TSQ"
/* def: "ThermoFinnigan TSQ MS." [PSI:MS]
is_a: MS:1000493 ! Finnigan MAT instrument model */

#define MS_TSQ_QUANTUM_ULTRA_ID "MS:1000751"
#define MS_TSQ_QUANTUM_ULTRA_NAME "TSQ Quantum Ultra"
/* def: "Thermo Scientific TSQ Quantum Ultra." [PSI:MS]
is_a: MS:1000494 ! Thermo Scientific instrument model */

#define MS_TOPP_SOFTWARE_ID "MS:1000752"
#define MS_TOPP_SOFTWARE_NAME "TOPP software"
/* def: "TOPP (The OpenMS proteomics pipeline) software." [PSI:MS]
is_a: MS:1001456 ! analysis software
is_a: MS:1001457 ! data processing software */

#define MS_BASELINEFILTER_ID "MS:1000753"
#define MS_BASELINEFILTER_NAME "BaselineFilter"
/* def: "Removes the baseline from profile spectra using a top-hat filter." [PSI:MS]
is_a: MS:1000752 ! TOPP software */

#define MS_DBEXPORTER_ID "MS:1000754"
#define MS_DBEXPORTER_NAME "DBExporter"
/* def: "Exports data from an OpenMS database to a file." [PSI:MS]
is_a: MS:1000752 ! TOPP software */

#define MS_DBIMPORTER_ID "MS:1000755"
#define MS_DBIMPORTER_NAME "DBImporter"
/* def: "Imports data to an OpenMS database." [PSI:MS]
is_a: MS:1000752 ! TOPP software */

#define MS_FILECONVERTER_ID "MS:1000756"
#define MS_FILECONVERTER_NAME "FileConverter"
/* def: "Converts between different MS file formats." [PSI:MS]
is_a: MS:1000752 ! TOPP software */

#define MS_FILEFILTER_ID "MS:1000757"
#define MS_FILEFILTER_NAME "FileFilter"
/* def: "Extracts or manipulates portions of data from peak, feature or consensus feature files." [PSI:MS]
is_a: MS:1000752 ! TOPP software */

#define MS_FILEMERGER_ID "MS:1000758"
#define MS_FILEMERGER_NAME "FileMerger"
/* def: "Merges several MS files into one file." [PSI:MS]
is_a: MS:1000752 ! TOPP software */

#define MS_INTERNALCALIBRATION_ID "MS:1000759"
#define MS_INTERNALCALIBRATION_NAME "InternalCalibration"
/* def: "Applies an internal calibration." [PSI:MS]
is_a: MS:1000752 ! TOPP software */

// #define MS_MAPALIGNER_ID "MS:1000760"
// #define MS_MAPALIGNER_NAME "MapAligner"
/* def: "OBSOLETE Corrects retention time distortions between maps." [PSI:MS]
comment: This term was made obsolete, because it is replaced by the terms under the 'TOPP map aligner' (MS:1002147) branch.
is_a: MS:1000752 ! TOPP software
is_obsolete: true */

#define MS_MAPNORMALIZER_ID "MS:1000761"
#define MS_MAPNORMALIZER_NAME "MapNormalizer"
/* def: "Normalizes peak intensities in an MS run." [PSI:MS]
is_a: MS:1000752 ! TOPP software */

// #define MS_NOISEFILTER_ID "MS:1000762"
// #define MS_NOISEFILTER_NAME "NoiseFilter"
/* def: "OBSOLETE Removes noise from profile spectra by using different smoothing techniques." [PSI:MS]
comment: This term was made obsolete, because it is replaced by the terms under the 'TOPP noise filter' (MS:1002131) branch.
is_a: MS:1000752 ! TOPP software
is_obsolete: true */

// #define MS_PEAKPICKER_ID "MS:1000763"
// #define MS_PEAKPICKER_NAME "PeakPicker"
/* def: "OBSOLETE Finds mass spectrometric peaks in profile mass spectra." [PSI:MS]
comment: This term was made obsolete, because it is replaced by the terms under the 'TOPP peak picker' (MS:1002134) branch.
is_a: MS:1000752 ! TOPP software
is_obsolete: true */

#define MS_RESAMPLER_ID "MS:1000764"
#define MS_RESAMPLER_NAME "Resampler"
/* def: "Transforms an LC/MS map into a resampled map or a png image." [PSI:MS]
is_a: MS:1000752 ! TOPP software */

// #define MS_SPECTRAFILTER_ID "MS:1000765"
// #define MS_SPECTRAFILTER_NAME "SpectraFilter"
/* def: "OBSOLETE Applies a filter to peak spectra." [PSI:MS]
comment: This term was made obsolete, because it is replaced by the terms under the 'TOPP spectra filter' (MS:1002137) branch.
is_a: MS:1000752 ! TOPP software
is_obsolete: true */

#define MS_TOFCALIBRATION_ID "MS:1000766"
#define MS_TOFCALIBRATION_NAME "TOFCalibration"
/* def: "Applies time of flight calibration." [PSI:MS]
is_a: MS:1000752 ! TOPP software */

#define MS_NATIVE_SPECTRUM_IDENTIFIER_FORMAT_ID "MS:1000767"
#define MS_NATIVE_SPECTRUM_IDENTIFIER_FORMAT_NAME "native spectrum identifier format"
/* def: "Describes how the native spectrum identifiers are formated." [PSI:MS]
synonym: "nativeID format" EXACT []
relationship: part_of MS:1000577 ! raw data file */

#define MS_THERMO_NATIVEID_FORMAT_ID "MS:1000768"
#define MS_THERMO_NATIVEID_FORMAT_NAME "Thermo nativeID format"
/* def: "controllerType=xsd:nonNegativeInteger controllerNumber=xsd:positiveInteger scan=xsd:positiveInteger." [PSI:MS]
is_a: MS:1000767 ! native spectrum identifier format */

#define MS_WATERS_NATIVEID_FORMAT_ID "MS:1000769"
#define MS_WATERS_NATIVEID_FORMAT_NAME "Waters nativeID format"
/* def: "function=xsd:positiveInteger process=xsd:nonNegativeInteger scan=xsd:nonNegativeInteger." [PSI:MS]
is_a: MS:1000767 ! native spectrum identifier format */

#define MS_WIFF_NATIVEID_FORMAT_ID "MS:1000770"
#define MS_WIFF_NATIVEID_FORMAT_NAME "WIFF nativeID format"
/* def: "sample=xsd:nonNegativeInteger period=xsd:nonNegativeInteger cycle=xsd:nonNegativeInteger experiment=xsd:nonNegativeInteger." [PSI:MS]
is_a: MS:1000767 ! native spectrum identifier format */

#define MS_BRUKER_AGILENT_YEP_NATIVEID_FORMAT_ID "MS:1000771"
#define MS_BRUKER_AGILENT_YEP_NATIVEID_FORMAT_NAME "Bruker_Agilent YEP nativeID format"
/* def: "scan=xsd:nonNegativeInteger." [PSI:MS]
is_a: MS:1000767 ! native spectrum identifier format */

#define MS_BRUKER_BAF_NATIVEID_FORMAT_ID "MS:1000772"
#define MS_BRUKER_BAF_NATIVEID_FORMAT_NAME "Bruker BAF nativeID format"
/* def: "scan=xsd:nonNegativeInteger." [PSI:MS]
is_a: MS:1000767 ! native spectrum identifier format */

#define MS_BRUKER_FID_NATIVEID_FORMAT_ID "MS:1000773"
#define MS_BRUKER_FID_NATIVEID_FORMAT_NAME "Bruker FID nativeID format"
/* def: "file=xsd:IDREF." [PSI:MS]
comment: The nativeID must be the same as the source file ID.
is_a: MS:1000767 ! native spectrum identifier format */

#define MS_MULTIPLE_PEAK_LIST_NATIVEID_FORMAT_ID "MS:1000774"
#define MS_MULTIPLE_PEAK_LIST_NATIVEID_FORMAT_NAME "multiple peak list nativeID format"
/* def: "index=xsd:nonNegativeInteger." [PSI:MS]
comment: Used for conversion of peak list files with multiple spectra, i.e. MGF, PKL, merged DTA files. Index is the spectrum number in the file, starting from 0.
is_a: MS:1000767 ! native spectrum identifier format */

#define MS_SINGLE_PEAK_LIST_NATIVEID_FORMAT_ID "MS:1000775"
#define MS_SINGLE_PEAK_LIST_NATIVEID_FORMAT_NAME "single peak list nativeID format"
/* def: "file=xsd:IDREF." [PSI:MS]
comment: The nativeID must be the same as the source file ID. Used for conversion of peak list files with one spectrum per file, typically folder of PKL or DTAs, each sourceFileRef is different.
is_a: MS:1000767 ! native spectrum identifier format */

#define MS_SCAN_NUMBER_ONLY_NATIVEID_FORMAT_ID "MS:1000776"
#define MS_SCAN_NUMBER_ONLY_NATIVEID_FORMAT_NAME "scan number only nativeID format"
/* def: "scan=xsd:nonNegativeInteger." [PSI:MS]
comment: Used for conversion from mzXML, or DTA folder where native scan numbers can be derived.
is_a: MS:1000767 ! native spectrum identifier format */

#define MS_SPECTRUM_IDENTIFIER_NATIVEID_FORMAT_ID "MS:1000777"
#define MS_SPECTRUM_IDENTIFIER_NATIVEID_FORMAT_NAME "spectrum identifier nativeID format"
/* def: "spectrum=xsd:nonNegativeInteger." [PSI:MS]
comment: Used for conversion from mzData. The spectrum id attribute is referenced.
is_a: MS:1000767 ! native spectrum identifier format */

#define MS_CHARGE_STATE_CALCULATION_ID "MS:1000778"
#define MS_CHARGE_STATE_CALCULATION_NAME "charge state calculation"
/* def: "A process that infers the charge state of an MSn spectrum's precursor(s) by the application of some algorithm." [PSI:MS]
is_a: MS:1000543 ! data processing action */

#define MS_BELOW_PRECURSOR_INTENSITY_DOMINANCE_CHARGE_STATE_CALCULATION_ID "MS:1000779"
#define MS_BELOW_PRECURSOR_INTENSITY_DOMINANCE_CHARGE_STATE_CALCULATION_NAME "below precursor intensity dominance charge state calculation"
/* def: "Infers charge state as single or amibiguously multiple by determining the fraction of intensity below the precursor m/z." [PSI:MS]
is_a: MS:1000778 ! charge state calculation */

#define MS_PRECURSOR_RECALCULATION_ID "MS:1000780"
#define MS_PRECURSOR_RECALCULATION_NAME "precursor recalculation"
/* def: "A process that recalculates existing precursor selected ions with one or more algorithmically determined precursor selected ions." [PSI:MS]
is_a: MS:1000543 ! data processing action */

#define MS_MSPREFIX_PRECURSOR_RECALCULATION_ID "MS:1000781"
#define MS_MSPREFIX_PRECURSOR_RECALCULATION_NAME "msPrefix precursor recalculation"
/* def: "Recalculates one or more precursor selected ions by peak detection in the isolation windows of high accuracy MS precursor scans." [PSI:MS]
is_a: MS:1000780 ! precursor recalculation */

#define MS_SAVITZKY_GOLAY_SMOOTHING_ID "MS:1000782"
#define MS_SAVITZKY_GOLAY_SMOOTHING_NAME "Savitzky-Golay smoothing"
/* def: "Reduces intensity spikes by applying local polynomial regression (of degree k) on a distribution (of at least k+1 equally spaced points) to determine the smoothed value for each point. It tends to preserve features of the distribution such as relative maxima, minima and width, which are usually 'flattened' by other adjacent averaging techniques." [PSI:MS]
is_a: MS:1000592 ! smoothing */

#define MS_LOWESS_SMOOTHING_ID "MS:1000783"
#define MS_LOWESS_SMOOTHING_NAME "LOWESS smoothing"
/* def: "Reduces intensity spikes by applying a modeling method known as locally weighted polynomial regression. At each point in the data set a low-degree polynomial is fit to a subset of the data, with explanatory variable values near the point whose response is being estimated. The polynomial is fit using weighted least squares, giving more weight to points near the point whose response is being estimated and less weight to points further away. The value of the regression function for the point is then obtained by evaluating the local polynomial using the explanatory variable values for that data point. The LOESS fit is complete after regression function values have been computed for each of the n data points. Many of the details of this method, such as the degree of the polynomial model and the weights, are flexible." [PSI:MS]
is_a: MS:1000592 ! smoothing */

#define MS_GAUSSIAN_SMOOTHING_ID "MS:1000784"
#define MS_GAUSSIAN_SMOOTHING_NAME "Gaussian smoothing"
/* def: "Reduces intensity spikes by convolving the data with a one-dimensional Gaussian function." [PSI:MS]
synonym: "binomial smoothing" EXACT []
synonym: "Weierstrass transform" EXACT []
is_a: MS:1000592 ! smoothing */

#define MS_MOVING_AVERAGE_SMOOTHING_ID "MS:1000785"
#define MS_MOVING_AVERAGE_SMOOTHING_NAME "moving average smoothing"
/* def: "Reduces intensity spikes by averaging each point with two or more adjacent points. The more adjacent points that used, the stronger the smoothing effect." [PSI:MS]
synonym: "box smoothing" EXACT []
synonym: "boxcar smoothing" EXACT []
synonym: "sliding average smoothing" EXACT []
is_a: MS:1000592 ! smoothing */

#define MS_NON_STANDARD_DATA_ARRAY_ID "MS:1000786"
#define MS_NON_STANDARD_DATA_ARRAY_NAME "non-standard data array"
/* def: "A data array that contains data not covered by any other term in this group. Please do not use this term, if the binary data array type might be commonly used - contact the PSI-MS working group in order to have another CV term added." [PSI:MS]
xref: binary-data-type:MS\:1000519 "32-bit integer"
xref: binary-data-type:MS\:1000521 "32-bit float"
xref: binary-data-type:MS\:1000522 "64-bit integer"
xref: binary-data-type:MS\:1000523 "64-bit float"
xref: binary-data-type:MS\:1001479 "null-terminated ASCII string"
xref: value-type:xsd\:string "The allowed value-type for this CV term."
is_a: MS:1000513 ! binary data array */

#define MS_INCLUSIVE_LOW_INTENSITY_THRESHOLD_ID "MS:1000787"
#define MS_INCLUSIVE_LOW_INTENSITY_THRESHOLD_NAME "inclusive low intensity threshold"
/* def: "Threshold at or below which some action is taken." [PSI:MS]
xref: value-type:xsd\:float "The allowed value-type for this CV term."
is_a: MS:1000630 ! data processing parameter
relationship: has_units MS:1000131 ! number of detector counts
relationship: has_units MS:1000132 ! percent of base peak
relationship: has_units MS:1000814 ! counts per second
relationship: has_units MS:1000905 ! percent of base peak times 100
relationship: has_units UO:0000269 ! absorbance unit */

#define MS_INCLUSIVE_HIGH_INTENSITY_THRESHOLD_ID "MS:1000788"
#define MS_INCLUSIVE_HIGH_INTENSITY_THRESHOLD_NAME "inclusive high intensity threshold"
/* def: "Threshold at or above which some action is taken." [PSI:MS]
xref: value-type:xsd\:float "The allowed value-type for this CV term."
is_a: MS:1000630 ! data processing parameter
relationship: has_units MS:1000131 ! number of detector counts
relationship: has_units MS:1000132 ! percent of base peak
relationship: has_units MS:1000814 ! counts per second
relationship: has_units MS:1000905 ! percent of base peak times 100
relationship: has_units UO:0000269 ! absorbance unit */

#define MS_ENHANCED_MULTIPLY_CHARGED_SPECTRUM_ID "MS:1000789"
#define MS_ENHANCED_MULTIPLY_CHARGED_SPECTRUM_NAME "enhanced multiply charged spectrum"
/* def: "MS1 spectrum that is enriched in multiply-charged ions compared to singly-charged ions." [PSI:MS]
is_a: MS:1000524 ! data file content
is_a: MS:1000579 ! MS1 spectrum */

#define MS_TIME_DELAYED_FRAGMENTATION_SPECTRUM_ID "MS:1000790"
#define MS_TIME_DELAYED_FRAGMENTATION_SPECTRUM_NAME "time-delayed fragmentation spectrum"
/* def: "MSn spectrum in which the product ions are collected after a time delay, which allows the observation of lower energy fragmentation processes after precursor ion activation." [PSI:MS]
is_a: MS:1000524 ! data file content
is_a: MS:1000580 ! MSn spectrum */

// #define MS_ENHANCED_RESOLUTION_SCAN_ID "MS:1000791"
// #define MS_ENHANCED_RESOLUTION_SCAN_NAME "enhanced resolution scan"
/* def: "OBSOLETE Scan with enhanced resolution." [PSI:MS]
comment: This term was made obsolete because it was merged with zoom scan (MS:1000497).
is_a: MS:1000020 ! scanning method
is_obsolete: true */

#define MS_ISOLATION_WINDOW_ATTRIBUTE_ID "MS:1000792"
#define MS_ISOLATION_WINDOW_ATTRIBUTE_NAME "isolation window attribute"
/* def: "Isolation window parameter." [PSI:MS]
is_a: MS:1000547 ! object attribute
relationship: part_of MS:1000441 ! scan */

// #define MS_ISOLATION_WINDOW_UPPER_LIMIT_ID "MS:1000793"
// #define MS_ISOLATION_WINDOW_UPPER_LIMIT_NAME "isolation window upper limit"
/* def: "OBSOLETE The highest m/z being isolated in an isolation window." [PSI:MS]
comment: This term was obsoleted in favor of using a target, lower, upper offset scheme. See terms 1000827-1000829.
xref: value-type:xsd\:float "The allowed value-type for this CV term."
is_a: MS:1000792 ! isolation window attribute
relationship: has_units MS:1000040 ! m/z
is_obsolete: true */

// #define MS_ISOLATION_WINDOW_LOWER_LIMIT_ID "MS:1000794"
// #define MS_ISOLATION_WINDOW_LOWER_LIMIT_NAME "isolation window lower limit"
/* def: "OBSOLETE The lowest m/z being isolated in an isolation window." [PSI:MS]
comment: This term was obsoleted in favor of using a target, lower, upper offset scheme. See terms 1000827-1000829.
xref: value-type:xsd\:float "The allowed value-type for this CV term."
is_a: MS:1000792 ! isolation window attribute
relationship: has_units MS:1000040 ! m/z
is_obsolete: true */

#define MS_NO_COMBINATION_ID "MS:1000795"
#define MS_NO_COMBINATION_NAME "no combination"
/* def: "Use this term if only one scan was recorded or there is no information about scans available." [PSI:MS]
is_a: MS:1000570 ! spectra combination */

#define MS_SPECTRUM_TITLE_ID "MS:1000796"
#define MS_SPECTRUM_TITLE_NAME "spectrum title"
/* def: "A free-form text title describing a spectrum." [PSI:MS]
comment: This is the preferred storage place for the spectrum TITLE from an MGF peak list.
xref: value-type:xsd\:string "The allowed value-type for this CV term."
is_a: MS:1000499 ! spectrum attribute
is_a: MS:1001405 ! spectrum identification result details */

#define MS_PEAK_LIST_SCANS_ID "MS:1000797"
#define MS_PEAK_LIST_SCANS_NAME "peak list scans"
/* def: "A list of scan numbers and or scan ranges associated with a peak list. If possible the list of scans should be converted to native spectrum identifiers instead of using this term." [PSI:MS]
comment: This is the preferred storage place for the spectrum SCANS attribute from an MGF peak list.
xref: value-type:xsd\:string "The allowed value-type for this CV term."
is_a: MS:1000499 ! spectrum attribute
is_a: MS:1001405 ! spectrum identification result details */

#define MS_PEAK_LIST_RAW_SCANS_ID "MS:1000798"
#define MS_PEAK_LIST_RAW_SCANS_NAME "peak list raw scans"
/* def: "A list of raw scans and or scan ranges used to generate a peak list. If possible the list of scans should be converted to native spectrum identifiers instead of using this term." [PSI:MS]
comment: This is the preferred storage place for the spectrum RAWSCANS attribute from an MGF peak list.
xref: value-type:xsd\:string "The allowed value-type for this CV term."
is_a: MS:1000499 ! spectrum attribute
is_a: MS:1001405 ! spectrum identification result details */

#define MS_CUSTOM_UNRELEASED_SOFTWARE_TOOL_ID "MS:1000799"
#define MS_CUSTOM_UNRELEASED_SOFTWARE_TOOL_NAME "custom unreleased software tool"
/* def: "A software tool that has not yet been released. The value should describe the software. Please do not use this term for publicly available software - contact the PSI-MS working group in order to have another CV term added." [PSI:MS]
xref: value-type:xsd\:string "The allowed value-type for this CV term."
is_a: MS:1000531 ! software */

#define MS_MASS_RESOLVING_POWER_ID "MS:1000800"
#define MS_MASS_RESOLVING_POWER_NAME "mass resolving power"
/* def: "The observed mass divided by the difference between two masses that can be separated: m/dm. The procedure by which dm was obtained and the mass at which the measurement was made should be reported." [PSI:MS]
xref: value-type:xsd\:string "The allowed value-type for this CV term."
is_a: MS:1000503 ! scan attribute */

#define MS_AREA_PEAK_PICKING_ID "MS:1000801"
#define MS_AREA_PEAK_PICKING_NAME "area peak picking"
/* def: "Spectral peak processing conducted on the acquired data to convert profile data to centroided data. The area defined by all raw data points that belong to the peak is reported." [PSI:MS]
synonym: "sum peak picking" EXACT []
is_a: MS:1000035 ! peak picking */

#define MS_HEIGHT_PEAK_PICKING_ID "MS:1000802"
#define MS_HEIGHT_PEAK_PICKING_NAME "height peak picking"
/* def: "Spectral peak processing conducted on the acquired data to convert profile data to centroided data. The maximum intensity of all raw data points that belong to the peak is reported." [PSI:MS]
synonym: "max peak picking" EXACT []
is_a: MS:1000035 ! peak picking */

#define MS_ANALYZER_SCAN_OFFSET_ID "MS:1000803"
#define MS_ANALYZER_SCAN_OFFSET_NAME "analyzer scan offset"
/* def: "Offset between two analyzers in a constant neutral loss or neutral gain scan. The value corresponds to the neutral loss or neutral gain value." [PSI:MS]
xref: value-type:xsd\:float "The allowed value-type for this CV term."
is_a: MS:1000503 ! scan attribute
relationship: has_units MS:1000040 ! m/z */

#define MS_ELECTROMAGNETIC_RADIATION_SPECTRUM_ID "MS:1000804"
#define MS_ELECTROMAGNETIC_RADIATION_SPECTRUM_NAME "electromagnetic radiation spectrum"
/* def: "A plot of the relative intensity of electromagnetic radiation as a function of the wavelength." [PSI:MS]
synonym: "EMR spectrum" EXACT []
is_a: MS:1000524 ! data file content
is_a: MS:1000559 ! spectrum type */

#define MS_EMISSION_SPECTRUM_ID "MS:1000805"
#define MS_EMISSION_SPECTRUM_NAME "emission spectrum"
/* def: "A plot of the relative intensity of electromagnetic radiation emitted by atoms or molecules when excited." [PSI:MS]
is_a: MS:1000524 ! data file content
is_a: MS:1000559 ! spectrum type */

#define MS_ABSORPTION_SPECTRUM_ID "MS:1000806"
#define MS_ABSORPTION_SPECTRUM_NAME "absorption spectrum"
/* def: "A plot of the relative intensity of electromagnetic radiation absorbed by atoms or molecules when excited." [PSI:MS]
is_a: MS:1000524 ! data file content
is_a: MS:1000559 ! spectrum type */

#define MS_TH_S_ID "MS:1000807"
#define MS_TH_S_NAME "Th_s"
/* def: "Unit describing the scan rate of a spectrum in Thomson per second." [PSI:MS]
is_a: UO:0000000 ! unit */

#define MS_CHROMATOGRAM_ATTRIBUTE_ID "MS:1000808"
#define MS_CHROMATOGRAM_ATTRIBUTE_NAME "chromatogram attribute"
/* def: "Chromatogram properties that are associated with a value." [PSI:MS]
is_a: MS:1000547 ! object attribute
relationship: part_of MS:1000625 ! chromatogram */

#define MS_CHROMATOGRAM_TITLE_ID "MS:1000809"
#define MS_CHROMATOGRAM_TITLE_NAME "chromatogram title"
/* def: "A free-form text title describing a chromatogram." [PSI:MS]
comment: This is the preferred storage place for the spectrum title.
xref: value-type:xsd\:string "The allowed value-type for this CV term."
is_a: MS:1000808 ! chromatogram attribute */

#define MS_MASS_CHROMATOGRAM_ID "MS:1000810"
#define MS_MASS_CHROMATOGRAM_NAME "mass chromatogram"
/* def: "A plot of the relative abundance of a beam or other collection of ions as a function of the retention time." [PSI:MS]
is_a: MS:1000524 ! data file content
is_a: MS:1000626 ! chromatogram type */

#define MS_ELECTROMAGNETIC_RADIATION_CHROMATOGRAM_ID "MS:1000811"
#define MS_ELECTROMAGNETIC_RADIATION_CHROMATOGRAM_NAME "electromagnetic radiation chromatogram"
/* def: "The measurement of electromagnetic properties as a function of the retention time." [PSI:MS]
synonym: "EMR radiation chromatogram" EXACT []
is_a: MS:1000524 ! data file content
is_a: MS:1000626 ! chromatogram type */

#define MS_ABSORPTION_CHROMATOGRAM_ID "MS:1000812"
#define MS_ABSORPTION_CHROMATOGRAM_NAME "absorption chromatogram"
/* def: "The measurement of light absorbed by the sample as a function of the retention time." [PSI:MS]
is_a: MS:1000524 ! data file content
is_a: MS:1000811 ! electromagnetic radiation chromatogram */

#define MS_EMISSION_CHROMATOGRAM_ID "MS:1000813"
#define MS_EMISSION_CHROMATOGRAM_NAME "emission chromatogram"
/* def: "The measurement of light emitted by the sample as a function of the retention time." [PSI:MS]
is_a: MS:1000524 ! data file content
is_a: MS:1000811 ! electromagnetic radiation chromatogram */

#define MS_COUNTS_PER_SECOND_ID "MS:1000814"
#define MS_COUNTS_PER_SECOND_NAME "counts per second"
/* def: "The number of counted events observed per second in one or a group of elements of a detector." [PSI:MS]
is_a: MS:1000043 ! intensity unit */

#define MS_BRUKER_BAF_FILE_ID "MS:1000815"
#define MS_BRUKER_BAF_FILE_NAME "Bruker BAF file"
/* def: "Bruker BAF raw file format." [PSI:MS]
is_a: MS:1000560 ! mass spectrometer file format */

#define MS_BRUKER_U2_FILE_ID "MS:1000816"
#define MS_BRUKER_U2_FILE_NAME "Bruker U2 file"
/* def: "Bruker HyStar U2 file format." [PSI:MS]
is_a: MS:1000560 ! mass spectrometer file format */

#define MS_HYSTAR_ID "MS:1000817"
#define MS_HYSTAR_NAME "HyStar"
/* def: "Bruker software for hyphenated experiments." [PSI:MS]
is_a: MS:1000692 ! Bruker software */

#define MS_ACQUITY_UPLC_PDA_ID "MS:1000818"
#define MS_ACQUITY_UPLC_PDA_NAME "Acquity UPLC PDA"
/* def: "Acquity UPLC Photodiode Array Detector." [PSI:MS]
is_a: MS:1000126 ! Waters instrument model
is_a: MS:1000621 ! photodiode array detector */

#define MS_ACQUITY_UPLC_FLR_ID "MS:1000819"
#define MS_ACQUITY_UPLC_FLR_NAME "Acquity UPLC FLR"
/* def: "Acquity UPLC Fluorescence Detector." [PSI:MS]
is_a: MS:1000126 ! Waters instrument model
is_a: MS:1002308 ! fluorescence detector */

#define MS_FLOW_RATE_ARRAY_ID "MS:1000820"
#define MS_FLOW_RATE_ARRAY_NAME "flow rate array"
/* def: "A data array of flow rate measurements." [PSI:MS]
xref: binary-data-type:MS\:1000521 "32-bit float"
xref: binary-data-type:MS\:1000523 "64-bit float"
is_a: MS:1000513 ! binary data array */

#define MS_PRESSURE_ARRAY_ID "MS:1000821"
#define MS_PRESSURE_ARRAY_NAME "pressure array"
/* def: "A data array of pressure measurements." [PSI:MS]
xref: binary-data-type:MS\:1000521 "32-bit float"
xref: binary-data-type:MS\:1000523 "64-bit float"
is_a: MS:1000513 ! binary data array
relationship: has_units UO:0000110 ! pascal */

#define MS_TEMPERATURE_ARRAY_ID "MS:1000822"
#define MS_TEMPERATURE_ARRAY_NAME "temperature array"
/* def: "A data array of temperature measurements." [PSI:MS]
xref: binary-data-type:MS\:1000521 "32-bit float"
xref: binary-data-type:MS\:1000523 "64-bit float"
is_a: MS:1000513 ! binary data array
relationship: has_units UO:0000012 ! kelvin */

#define MS_BRUKER_U2_NATIVEID_FORMAT_ID "MS:1000823"
#define MS_BRUKER_U2_NATIVEID_FORMAT_NAME "Bruker U2 nativeID format"
/* def: "declaration=xsd:nonNegativeInteger collection=xsd:nonNegativeInteger scan=xsd:nonNegativeInteger." [PSI:MS]
is_a: MS:1000767 ! native spectrum identifier format */

#define MS_NO_NATIVEID_FORMAT_ID "MS:1000824"
#define MS_NO_NATIVEID_FORMAT_NAME "no nativeID format"
/* def: "No nativeID format indicates that the file tagged with this term does not contain spectra that can have a nativeID format." [PSI:MS]
is_a: MS:1000767 ! native spectrum identifier format */

#define MS_BRUKER_FID_FILE_ID "MS:1000825"
#define MS_BRUKER_FID_FILE_NAME "Bruker FID file"
/* def: "Bruker FID file format." [PSI:MS]
is_a: MS:1000560 ! mass spectrometer file format */

#define MS_ELUTION_TIME_ID "MS:1000826"
#define MS_ELUTION_TIME_NAME "elution time"
/* def: "The time of elution from all used chromatographic columns (one or more) in the chromatographic separation step, relative to the start of the chromatography." [PSI:MS]
xref: value-type:xsd\:float "The allowed value-type for this CV term."
is_a: MS:1000503 ! scan attribute
relationship: has_units UO:0000010 ! second
relationship: has_units UO:0000031 ! minute */

#define MS_ISOLATION_WINDOW_TARGET_M_Z_ID "MS:1000827"
#define MS_ISOLATION_WINDOW_TARGET_M_Z_NAME "isolation window target m/z"
/* def: "The primary or reference m/z about which the isolation window is defined." [PSI:MS]
xref: value-type:xsd\:float "The allowed value-type for this CV term."
is_a: MS:1000792 ! isolation window attribute
relationship: has_units MS:1000040 ! m/z */

#define MS_ISOLATION_WINDOW_LOWER_OFFSET_ID "MS:1000828"
#define MS_ISOLATION_WINDOW_LOWER_OFFSET_NAME "isolation window lower offset"
/* def: "The extent of the isolation window in m/z below the isolation window target m/z. The lower and upper offsets may be asymmetric about the target m/z." [PSI:MS]
xref: value-type:xsd\:float "The allowed value-type for this CV term."
is_a: MS:1000792 ! isolation window attribute
relationship: has_units MS:1000040 ! m/z */

#define MS_ISOLATION_WINDOW_UPPER_OFFSET_ID "MS:1000829"
#define MS_ISOLATION_WINDOW_UPPER_OFFSET_NAME "isolation window upper offset"
/* def: "The extent of the isolation window in m/z above the isolation window target m/z. The lower and upper offsets may be asymmetric about the target m/z." [PSI:MS]
xref: value-type:xsd\:float "The allowed value-type for this CV term."
is_a: MS:1000792 ! isolation window attribute
relationship: has_units MS:1000040 ! m/z */

#define MS_SAMPLE_PREPARATION_ID "MS:1000831"
#define MS_SAMPLE_PREPARATION_NAME "sample preparation"
/* def: "Properties of the preparation steps which took place before the measurement was performed." [PSI:MS]
is_a: MS:1000547 ! object attribute
relationship: part_of MS:1000548 ! sample attribute */

#define MS_MALDI_MATRIX_APPLICATION_ID "MS:1000832"
#define MS_MALDI_MATRIX_APPLICATION_NAME "MALDI matrix application"
/* def: "Attributes to describe the technique how the sample is prepared with the matrix solution." [PSI:MS]
relationship: part_of MS:1000831 ! sample preparation */

#define MS_MATRIX_APPLICATION_TYPE_ID "MS:1000833"
#define MS_MATRIX_APPLICATION_TYPE_NAME "matrix application type"
/* def: "Describes the technique how the matrix is put on the sample target." [PSI:MS]
relationship: part_of MS:1000832 ! MALDI matrix application */

#define MS_MATRIX_SOLUTION_ID "MS:1000834"
#define MS_MATRIX_SOLUTION_NAME "matrix solution"
/* def: "Describes the chemical solution used as matrix." [PSI:MS]
xref: value-type:xsd\:string "The allowed value-type for this CV term."
is_a: MS:1000832 ! MALDI matrix application */

#define MS_MATRIX_SOLUTION_CONCENTRATION_ID "MS:1000835"
#define MS_MATRIX_SOLUTION_CONCENTRATION_NAME "matrix solution concentration"
/* def: "Concentration of the chemical solution used as matrix." [PSI:MS]
xref: value-type:xsd\:float "The allowed value-type for this CV term."
is_a: MS:1000832 ! MALDI matrix application
relationship: has_units UO:0000175 ! gram per liter */

#define MS_DRIED_DROPLET_MALDI_MATRIX_PREPARATION_ID "MS:1000836"
#define MS_DRIED_DROPLET_MALDI_MATRIX_PREPARATION_NAME "dried droplet MALDI matrix preparation"
/* def: "Dried droplet in MALDI matrix preparation." [PSI:MS]
is_a: MS:1000833 ! matrix application type */

#define MS_PRINTED_MALDI_MATRIX_PREPARATION_ID "MS:1000837"
#define MS_PRINTED_MALDI_MATRIX_PREPARATION_NAME "printed MALDI matrix preparation"
/* def: "Printed MALDI matrix preparation." [PSI:MS]
is_a: MS:1000833 ! matrix application type */

#define MS_SPRAYED_MALDI_MATRIX_PREPARATION_ID "MS:1000838"
#define MS_SPRAYED_MALDI_MATRIX_PREPARATION_NAME "sprayed MALDI matrix preparation"
/* def: "Sprayed MALDI matrix preparation." [PSI:MS]
is_a: MS:1000833 ! matrix application type */

#define MS_PRECOATED_MALDI_SAMPLE_PLATE_ID "MS:1000839"
#define MS_PRECOATED_MALDI_SAMPLE_PLATE_NAME "precoated MALDI sample plate"
/* def: "Precoated MALDI sample plate." [PSI:MS]
is_a: MS:1000833 ! matrix application type
is_a: MS:1001938 ! sample plate type */

#define MS_LASER_ID "MS:1000840"
#define MS_LASER_NAME "laser"
/* def: "Device that emits light (electromagnetic radiation) through a process called stimulated emission. The term is an acronym for Light Amplification by Stimulated Emission of Radiation." [PSI:MS]
relationship: part_of MS:1000482 ! source attribute */

#define MS_LASER_ATTRIBUTE_ID "MS:1000841"
#define MS_LASER_ATTRIBUTE_NAME "laser attribute"
/* def: "Laser properties that are associated with a value." [PSI:MS]
relationship: part_of MS:1000840 ! laser */

#define MS_LASER_TYPE_ID "MS:1000842"
#define MS_LASER_TYPE_NAME "laser type"
/* def: "Type of laser used used for desorption purpose." [PSI:MS]
relationship: part_of MS:1000840 ! laser */

// #define MS_WAVELENGTH_ID "MS:1000843"
// #define MS_WAVELENGTH_NAME "wavelength"
/* def: "OBSOLETE The distance between two peaks of the emitted laser beam." [PSI:MS]
comment: This term was made obsolete because it was redundant with the Pato Ontology term wavelength (UO:0001242).
xref: value-type:xsd\:float "The allowed value-type for this CV term."
is_a: MS:1000841 ! laser attribute
relationship: has_units UO:0000018 ! nanometer
is_obsolete: true */

#define MS_FOCUS_DIAMETER_X_ID "MS:1000844"
#define MS_FOCUS_DIAMETER_X_NAME "focus diameter x"
/* def: "Describes the diameter of the laser beam in x direction." [PSI:MS]
xref: value-type:xsd\:float "The allowed value-type for this CV term."
is_a: MS:1000841 ! laser attribute
relationship: has_units UO:0000017 ! micrometer */

#define MS_FOCUS_DIAMETER_Y_ID "MS:1000845"
#define MS_FOCUS_DIAMETER_Y_NAME "focus diameter y"
/* def: "Describes the diameter of the laser beam in y direction." [PSI:MS]
xref: value-type:xsd\:float "The allowed value-type for this CV term."
is_a: MS:1000841 ! laser attribute
relationship: has_units UO:0000017 ! micrometer */

#define MS_PULSE_ENERGY_ID "MS:1000846"
#define MS_PULSE_ENERGY_NAME "pulse energy"
/* def: "Describes output energy of the laser system. May be attenuated by filters or other means." [PSI:MS]
xref: value-type:xsd\:float "The allowed value-type for this CV term."
is_a: MS:1000841 ! laser attribute
relationship: has_units UO:0000112 ! joule */

#define MS_PULSE_DURATION_ID "MS:1000847"
#define MS_PULSE_DURATION_NAME "pulse duration"
/* def: "Describes how long the laser beam was emitted from the laser device." [PSI:MS]
xref: value-type:xsd\:float "The allowed value-type for this CV term."
is_a: MS:1000841 ! laser attribute
relationship: has_units UO:0000150 ! nanosecond */

#define MS_ATTENUATION_ID "MS:1000848"
#define MS_ATTENUATION_NAME "attenuation"
/* def: "Describes the reduction of the intensity of the laser beam energy." [PSI:MS]
xref: value-type:xsd\:float "The allowed value-type for this CV term."
is_a: MS:1000841 ! laser attribute
relationship: has_units UO:0000187 ! percent */

#define MS_IMPACT_ANGLE_ID "MS:1000849"
#define MS_IMPACT_ANGLE_NAME "impact angle"
/* def: "Describes the angle between the laser beam and the sample target." [PSI:MS]
xref: value-type:xsd\:float "The allowed value-type for this CV term."
is_a: MS:1000841 ! laser attribute
relationship: has_units UO:0000185 ! degree */

#define MS_GAS_LASER_ID "MS:1000850"
#define MS_GAS_LASER_NAME "gas laser"
/* def: "Laser which is powered by a gaseous medium." [PSI:MS]
is_a: MS:1000842 ! laser type */

#define MS_SOLID_STATE_LASER_ID "MS:1000851"
#define MS_SOLID_STATE_LASER_NAME "solid-state laser"
/* def: "Solid state laser materials are commonly made by doping a crystalline solid host with ions that provide the required energy states." [PSI:MS]
is_a: MS:1000842 ! laser type */

#define MS_DYE_LASER_ID "MS:1000852"
#define MS_DYE_LASER_NAME "dye-laser"
/* def: "Dye lasers use an organic dye as the gain medium." [PSI:MS]
is_a: MS:1000842 ! laser type */

#define MS_FREE_ELECTRON_LASER_ID "MS:1000853"
#define MS_FREE_ELECTRON_LASER_NAME "free electron laser"
/* def: "Free electron laser uses a relativistic electron beam as the lasing medium which move freely through a magnetic structure, hence the term." [PSI:MS]
is_a: MS:1000842 ! laser type */

#define MS_LTQ_XL_ID "MS:1000854"
#define MS_LTQ_XL_NAME "LTQ XL"
/* def: "Thermo Scientific LTQ XL MS." [PSI:MS]
is_a: MS:1000494 ! Thermo Scientific instrument model */

#define MS_LTQ_VELOS_ID "MS:1000855"
#define MS_LTQ_VELOS_NAME "LTQ Velos"
/* def: "Thermo Scientific LTQ Velos MS." [PSI:MS]
is_a: MS:1000494 ! Thermo Scientific instrument model */

#define MS_LTQ_VELOS_ETD_ID "MS:1000856"
#define MS_LTQ_VELOS_ETD_NAME "LTQ Velos ETD"
/* def: "Thermo Scientific LTQ Velos MS with ETD." [PSI:MS]
is_a: MS:1000494 ! Thermo Scientific instrument model */

#define MS_RUN_ATTRIBUTE_ID "MS:1000857"
#define MS_RUN_ATTRIBUTE_NAME "run attribute"
/* def: "Properties of the described run." [PSI:MS]
is_a: MS:1000547 ! object attribute */

#define MS_FRACTION_IDENTIFIER_ID "MS:1000858"
#define MS_FRACTION_IDENTIFIER_NAME "fraction identifier"
/* def: "Identier string that describes the sample fraction. This identifer should contain the fraction number(s) or similar information." [PSI:MS]
xref: value-type:xsd\:string "The allowed value-type for this CV term."
is_a: MS:1000857 ! run attribute */

#define MS_MOLECULE_ID "MS:1000859"
#define MS_MOLECULE_NAME "molecule"
/* def: "A molecules is a fundamental component of a chemical compound that is the smallest part of the compound that can participate in a chemical reaction." [PSI:MS]
relationship: part_of MS:1000881 ! chemical compound */

#define MS_PEPTIDE_ID "MS:1000860"
#define MS_PEPTIDE_NAME "peptide"
/* def: "A compound of low molecular weight that is composed of two or more amino acids." [PSI:MS]
is_a: MS:1000881 ! chemical compound */

#define MS_CHEMICAL_COMPOUND_ATTRIBUTE_ID "MS:1000861"
#define MS_CHEMICAL_COMPOUND_ATTRIBUTE_NAME "chemical compound attribute"
/* def: "A describable property of a chemical compound." [PSI:MS]
relationship: part_of MS:1000881 ! chemical compound */

#define MS_ISOELECTRIC_POINT_ID "MS:1000862"
#define MS_ISOELECTRIC_POINT_NAME "isoelectric point"
/* def: "The pH of a solution at which a charged molecule does not migrate in an electric field." [PSI:MS]
synonym: "pI" EXACT []
xref: value-type:xsd\:float "The allowed value-type for this CV term."
is_a: MS:1000861 ! chemical compound attribute */

#define MS_PREDICTED_ISOELECTRIC_POINT_ID "MS:1000863"
#define MS_PREDICTED_ISOELECTRIC_POINT_NAME "predicted isoelectric point"
/* def: "The pH of a solution at which a charged molecule would not migrate in an electric field, as predicted by a software algorithm." [PSI:MS]
synonym: "predicted pI" EXACT []
xref: value-type:xsd\:float "The allowed value-type for this CV term."
is_a: MS:1000862 ! isoelectric point */

#define MS_CHEMICAL_COMPOUND_FORMULA_ID "MS:1000864"
#define MS_CHEMICAL_COMPOUND_FORMULA_NAME "chemical compound formula"
/* def: "A combination of symbols used to express the chemical composition of a compound." [PSI:MS]
relationship: part_of MS:1000881 ! chemical compound */

#define MS_EMPIRICAL_FORMULA_ID "MS:1000865"
#define MS_EMPIRICAL_FORMULA_NAME "empirical formula"
/* def: "A chemical formula which expresses the proportions of the elements present in a substance." [PSI:MS]
xref: value-type:xsd\:string "The allowed value-type for this CV term."
is_a: MS:1000864 ! chemical compound formula */

#define MS_MOLECULAR_FORMULA_ID "MS:1000866"
#define MS_MOLECULAR_FORMULA_NAME "molecular formula"
/* def: "A chemical compound formula expressing the number of atoms of each element present in a compound, without indicating how they are linked." [PSI:MS]
xref: value-type:xsd\:string "The allowed value-type for this CV term."
is_a: MS:1000864 ! chemical compound formula */

#define MS_STRUCTURAL_FORMULA_ID "MS:1000867"
#define MS_STRUCTURAL_FORMULA_NAME "structural formula"
/* def: "A chemical formula showing the number of atoms of each element in a molecule, their spatial arrangement, and their linkage to each other." [PSI:MS]
xref: value-type:xsd\:string "The allowed value-type for this CV term."
is_a: MS:1000864 ! chemical compound formula */

#define MS_SMILES_STRING_ID "MS:1000868"
#define MS_SMILES_STRING_NAME "SMILES string"
/* def: "The simplified molecular input line entry specification or SMILES is a specification for unambiguously describing the structure of a chemical compound using a short ASCII string." [PSI:MS]
xref: value-type:xsd\:string "The allowed value-type for this CV term."
is_a: MS:1000864 ! chemical compound formula */

#define MS_COLLISION_GAS_PRESSURE_ID "MS:1000869"
#define MS_COLLISION_GAS_PRESSURE_NAME "collision gas pressure"
/* def: "The gas pressure of the collision gas used for collisional excitation." [PSI:MS]
xref: value-type:xsd\:float "The allowed value-type for this CV term."
is_a: MS:1000510 ! precursor activation attribute
relationship: has_units UO:0000110 ! pascal */

// #define MS__4000_QTRAP_ID "MS:1000870"
// #define MS__4000_QTRAP_NAME "4000 QTRAP"
/* def: "OBSOLETE AB SCIEX or Applied Biosystems|MDS SCIEX QTRAP 4000." [PSI:MS]
comment: This term was obsoleted because was redundant to MS:1000139.
is_a: MS:1000121 ! AB SCIEX instrument model
is_obsolete: true */

#define MS_SRM_SOFTWARE_ID "MS:1000871"
#define MS_SRM_SOFTWARE_NAME "SRM software"
/* def: "Software used to predict, select, or optimize transitions or analyze the results of selected reaction monitoring runs." [PSI:MS]
is_a: MS:1000531 ! software */

#define MS_MARIMBA_ID "MS:1000872"
#define MS_MARIMBA_NAME "MaRiMba"
/* def: "Software used to predict transitions for selected reaction monitoring experiments based on observed spectrum libraries developed and distributed by the Institute for Systems Biology." [http://tools.proteomecenter.org/wiki/index.php?title=Software:TPP-MaRiMba]
is_a: MS:1000871 ! SRM software */

#define MS_PEPTIDE_ATTRIBUTE_CALCULATION_SOFTWARE_ID "MS:1000873"
#define MS_PEPTIDE_ATTRIBUTE_CALCULATION_SOFTWARE_NAME "peptide attribute calculation software"
/* def: "Software used to predict or calculate numerical attributes of peptides." [PSI:MS]
is_a: MS:1000531 ! software */

#define MS_SSRCALC_ID "MS:1000874"
#define MS_SSRCALC_NAME "SSRCalc"
/* def: "Sequence Specific Retention Calculator esimates the retention time of peptides based on their sequence." [http://hs2.proteome.ca/SSRCalc/SSRCalc.html]
is_a: MS:1000873 ! peptide attribute calculation software */

#define MS_DECLUSTERING_POTENTIAL_ID "MS:1000875"
#define MS_DECLUSTERING_POTENTIAL_NAME "declustering potential"
/* def: "Potential difference between the orifice and the skimmer in volts." [PSI:MS]
xref: value-type:xsd\:float "The allowed value-type for this CV term."
is_a: MS:1000482 ! source attribute
relationship: has_units UO:0000218 ! volt */

#define MS_CONE_VOLTAGE_ID "MS:1000876"
#define MS_CONE_VOLTAGE_NAME "cone voltage"
/* def: "Potential difference between the sampling cone/orifice in volts." [PSI:MS]
xref: value-type:xsd\:float "The allowed value-type for this CV term."
is_a: MS:1000482 ! source attribute
relationship: has_units UO:0000218 ! volt */

#define MS_TUBE_LENS_VOLTAGE_ID "MS:1000877"
#define MS_TUBE_LENS_VOLTAGE_NAME "tube lens voltage"
/* def: "Potential difference setting of the tube lens in volts." [PSI:MS]
xref: value-type:xsd\:float "The allowed value-type for this CV term."
is_a: MS:1000482 ! source attribute
relationship: has_units UO:0000218 ! volt */

#define MS_EXTERNAL_REFERENCE_IDENTIFIER_ID "MS:1000878"
#define MS_EXTERNAL_REFERENCE_IDENTIFIER_NAME "external reference identifier"
/* def: "An identifier/accession number to an external reference database." [PSI:MS]
relationship: part_of MS:0000000 ! Proteomics Standards Initiative Mass Spectrometry Vocabularies */

#define MS_PUBMED_IDENTIFIER_ID "MS:1000879"
#define MS_PUBMED_IDENTIFIER_NAME "PubMed identifier"
/* def: "A unique identifier for a publication in the PubMed database (MIR:00000015)." [PSI:MS]
xref: value-type:xsd\:integer "The allowed value-type for this CV term."
is_a: MS:1000878 ! external reference identifier */

#define MS_INTERCHANNEL_DELAY_ID "MS:1000880"
#define MS_INTERCHANNEL_DELAY_NAME "interchannel delay"
/* def: "The duration of intervals between scanning, during which the instrument configuration is switched." [PSI:MS]
xref: value-type:xsd\:float "The allowed value-type for this CV term."
is_a: MS:1000503 ! scan attribute
relationship: has_units UO:0000010 ! second */

#define MS_CHEMICAL_COMPOUND_ID "MS:1000881"
#define MS_CHEMICAL_COMPOUND_NAME "chemical compound"
/* def: "A substance formed by chemical union of two or more elements or ingredients in definite proportion by weight." [PSI:MS]
relationship: part_of MS:0000000 ! Proteomics Standards Initiative Mass Spectrometry Vocabularies */

#define MS_PROTEIN_ID "MS:1000882"
#define MS_PROTEIN_NAME "protein"
/* def: "A compound composed of one or more chains of amino acids in a specific order determined by the base sequence of nucleotides in the DNA coding for the protein." [PSI:MS]
is_a: MS:1000881 ! chemical compound */

#define MS_PROTEIN_SHORT_NAME_ID "MS:1000883"
#define MS_PROTEIN_SHORT_NAME_NAME "protein short name"
/* def: "A short name or symbol of a protein (e.g., HSF 1 or HSF1_HUMAN)." [PSI:MS]
xref: value-type:xsd\:string "The allowed value-type for this CV term."
is_a: MS:1000884 ! protein attribute */

#define MS_PROTEIN_ATTRIBUTE_ID "MS:1000884"
#define MS_PROTEIN_ATTRIBUTE_NAME "protein attribute"
/* def: "An nonphysical attribute describing a specific protein." [PSI:MS]
relationship: part_of MS:1000882 ! protein
is_a: MS:1001806 ! quantification object attribute */

#define MS_PROTEIN_ACCESSION_ID "MS:1000885"
#define MS_PROTEIN_ACCESSION_NAME "protein accession"
/* def: "Accession number for a specific protein in a database." [PSI:MS]
xref: value-type:xsd\:string "The allowed value-type for this CV term."
is_a: MS:1000884 ! protein attribute */

#define MS_PROTEIN_NAME_ID "MS:1000886"
#define MS_PROTEIN_NAME_NAME "protein name"
/* def: "A long name describing the function of the protein." [PSI:MS]
xref: value-type:xsd\:string "The allowed value-type for this CV term."
is_a: MS:1000884 ! protein attribute */

#define MS_PEPTIDE_ATTRIBUTE_ID "MS:1000887"
#define MS_PEPTIDE_ATTRIBUTE_NAME "peptide attribute"
/* def: "An nonphysical attribute that can be used to describe a peptide." [PSI:MS]
relationship: part_of MS:1000860 ! peptide */

#define MS_UNMODIFIED_PEPTIDE_SEQUENCE_ID "MS:1000888"
#define MS_UNMODIFIED_PEPTIDE_SEQUENCE_NAME "unmodified peptide sequence"
/* def: "A sequence of letter symbols denoting the order of amino acids that compose the peptide, without encoding any amino acid mass modifications that might be present." [PSI:MS]
xref: value-type:xsd\:string "The allowed value-type for this CV term."
is_a: MS:1000887 ! peptide attribute */

#define MS_MODIFIED_PEPTIDE_SEQUENCE_ID "MS:1000889"
#define MS_MODIFIED_PEPTIDE_SEQUENCE_NAME "modified peptide sequence"
/* def: "A sequence of letter symbols denoting the order of amino acids that compose the peptide plus the encoding any amino acid modifications that are present." [PSI:MS]
comment: Make it more general as there are actually many other ways to display a modified peptide sequence.
xref: value-type:xsd\:string "The allowed value-type for this CV term."
is_a: MS:1000887 ! peptide attribute */

#define MS_PEPTIDE_LABELING_STATE_ID "MS:1000890"
#define MS_PEPTIDE_LABELING_STATE_NAME "peptide labeling state"
/* def: "A state description of how a peptide might be isotopically or isobarically labelled." [PSI:MS]
is_a: MS:1000887 ! peptide attribute */

#define MS_HEAVY_LABELED_PEPTIDE_ID "MS:1000891"
#define MS_HEAVY_LABELED_PEPTIDE_NAME "heavy labeled peptide"
/* def: "A peptide that has been created or labeled with some heavier-than-usual isotopes." [PSI:MS]
is_a: MS:1000890 ! peptide labeling state */

#define MS_UNLABELED_PEPTIDE_ID "MS:1000892"
#define MS_UNLABELED_PEPTIDE_NAME "unlabeled peptide"
/* def: "A peptide that has not been labeled with heavier-than-usual isotopes. This is often referred to as \"light\" to distinguish from \"heavy\"." [PSI:MS]
synonym: "light labeled peptide" EXACT []
is_a: MS:1000890 ! peptide labeling state */

#define MS_PEPTIDE_GROUP_LABEL_ID "MS:1000893"
#define MS_PEPTIDE_GROUP_LABEL_NAME "peptide group label"
/* def: "An arbitrary string label used to mark a set of peptides that belong together in a set, whereby the members are differentiated by different isotopic labels. For example, the heavy and light forms of the same peptide will both be assigned the same peptide group label." [PSI:MS]
xref: value-type:xsd\:string "The allowed value-type for this CV term."
is_a: MS:1000887 ! peptide attribute */

#define MS_RETENTION_TIME_ID "MS:1000894"
#define MS_RETENTION_TIME_NAME "retention time"
/* def: "A time interval from the start of chromatography when an analyte exits a chromatographic column." [PSI:MS]
xref: value-type:xsd\:float "The allowed value-type for this CV term."
is_a: MS:1000887 ! peptide attribute
relationship: has_units UO:0000010 ! second
relationship: has_units UO:0000031 ! minute */

#define MS_LOCAL_RETENTION_TIME_ID "MS:1000895"
#define MS_LOCAL_RETENTION_TIME_NAME "local retention time"
/* def: "A time interval from the start of chromatography when an analyte exits an unspecified local chromatographic column and instrumental setup." [PSI:MS]
xref: value-type:xsd\:float "The allowed value-type for this CV term."
is_a: MS:1000894 ! retention time
relationship: has_units UO:0000010 ! second
relationship: has_units UO:0000031 ! minute */

#define MS_NORMALIZED_RETENTION_TIME_ID "MS:1000896"
#define MS_NORMALIZED_RETENTION_TIME_NAME "normalized retention time"
/* def: "A time interval from the start of chromatography when an analyte exits a standardized reference chromatographic column and instrumental setup." [PSI:MS]
xref: value-type:xsd\:float "The allowed value-type for this CV term."
is_a: MS:1000894 ! retention time
relationship: has_units UO:0000010 ! second
relationship: has_units UO:0000031 ! minute */

#define MS_PREDICTED_RETENTION_TIME_ID "MS:1000897"
#define MS_PREDICTED_RETENTION_TIME_NAME "predicted retention time"
/* def: "A time interval from the start of chromatography when an analyte exits a chromatographic column as predicted by a referenced software application." [PSI:MS]
xref: value-type:xsd\:float "The allowed value-type for this CV term."
is_a: MS:1000894 ! retention time
relationship: has_units UO:0000010 ! second
relationship: has_units UO:0000031 ! minute */

#define MS_STANDARD_ID "MS:1000898"
#define MS_STANDARD_NAME "standard"
/* def: "Something, such as a practice or a product, that is widely recognized or employed, especially because of its excellence." [PSI:MS]
relationship: part_of MS:0000000 ! Proteomics Standards Initiative Mass Spectrometry Vocabularies */

#define MS_DE_FACTO_STANDARD_ID "MS:1000899"
#define MS_DE_FACTO_STANDARD_NAME "de facto standard"
/* def: "A practice or product that has become a standard not because it has been approved by a standards organization but because it is widely used and recognized by the industry as being standard." [PSI:MS]
is_a: MS:1000898 ! standard */

#define MS_MINIMUM_INFORMATION_STANDARD_ID "MS:1000900"
#define MS_MINIMUM_INFORMATION_STANDARD_NAME "minimum information standard"
/* def: "A specification of a minimum amount of information needed to reproduce or fully interpret a scientific result." [PSI:MS]
is_a: MS:1000898 ! standard */

#define MS_RETENTION_TIME_NORMALIZATION_STANDARD_ID "MS:1000901"
#define MS_RETENTION_TIME_NORMALIZATION_STANDARD_NAME "retention time normalization standard"
/* def: "A standard providing the retention times at which a set of reference compounds exit the reference chromatographic column." [PSI:MS]
is_a: MS:1000898 ! standard */

#define MS_H_PINS_RETENTION_TIME_NORMALIZATION_STANDARD_ID "MS:1000902"
#define MS_H_PINS_RETENTION_TIME_NORMALIZATION_STANDARD_NAME "H-PINS retention time normalization standard"
/* def: "The de facto standard providing the retention times at which a set of halogenated reference peptides exit the reference chromatographic column." [DOI:10.1074/mcp.M800569-MCP200, PMID:19411281]
is_a: MS:1000901 ! retention time normalization standard */

#define MS_PRODUCT_ION_SERIES_ORDINAL_ID "MS:1000903"
#define MS_PRODUCT_ION_SERIES_ORDINAL_NAME "product ion series ordinal"
/* def: "The ordinal of the fragment within a specified ion series. (e.g. 8 for a y8 ion)." [PSI:PI]
xref: value-type:xsd\:positiveInteger "The allowed value-type for this CV term."
is_a: MS:1001221 ! fragmentation information */

#define MS_PRODUCT_ION_M_Z_DELTA_ID "MS:1000904"
#define MS_PRODUCT_ION_M_Z_DELTA_NAME "product ion m/z delta"
/* def: "The difference in m/z of the predicted m/z based on the assigned product ion minus the actual observed peak m/z." [PSI:PI]
xref: value-type:xsd\:float "The allowed value-type for this CV term."
is_a: MS:1001221 ! fragmentation information
relationship: has_units MS:1000040 ! m/z */

#define MS_PERCENT_OF_BASE_PEAK_TIMES_100_ID "MS:1000905"
#define MS_PERCENT_OF_BASE_PEAK_TIMES_100_NAME "percent of base peak times 100"
/* def: "The magnitude of a peak expressed in terms of the percentage of the magnitude of the base peak intensity multiplied by 100. The base peak is therefore 10000. This unit is common in normalized spectrum libraries." [PSI:MS]
xref: value-type:xsd\:float "The allowed value-type for this CV term."
is_a: MS:1000043 ! intensity unit */

#define MS_PEAK_INTENSITY_RANK_ID "MS:1000906"
#define MS_PEAK_INTENSITY_RANK_NAME "peak intensity rank"
/* def: "Ordinal specifying the rank in intensity of a peak in a spectrum. Base peak is 1. The next most intense peak is 2, etc." [PSI:MS]
xref: value-type:xsd\:positiveInteger "The allowed value-type for this CV term."
is_a: MS:1000455 ! ion selection attribute
relationship: part_of MS:1000231 ! peak */

#define MS_PEAK_TARGETING_SUITABILITY_RANK_ID "MS:1000907"
#define MS_PEAK_TARGETING_SUITABILITY_RANK_NAME "peak targeting suitability rank"
/* def: "Ordinal specifying the rank of a peak in a spectrum in terms of suitability for targeting. The most suitable peak is 1. The next most suitability peak is 2, etc. Suitability is algorithm and context dependant." [PSI:MS]
xref: value-type:xsd\:positiveInteger "The allowed value-type for this CV term."
is_a: MS:1000455 ! ion selection attribute
relationship: part_of MS:1000231 ! peak */

#define MS_TRANSITION_ID "MS:1000908"
#define MS_TRANSITION_NAME "transition"
/* def: "A set of two m/z values corresponding to the precursor m/z and a fragment m/z that in combination can be used to identify or quantify a specific ion, although not necessarily uniquely." [PSI:MS]
synonym: "reaction" EXACT []
relationship: part_of MS:1001458 ! spectrum generation information */

#define MS_TRANSITION_VALIDATION_METHOD_ID "MS:1000909"
#define MS_TRANSITION_VALIDATION_METHOD_NAME "transition validation method"
/* def: "The strategy used to validate that a transition is effective." [PSI:MS]
relationship: part_of MS:1000908 ! transition */

#define MS_TRANSITION_OPTIMIZED_ON_SPECIFIED_INSTRUMENT_ID "MS:1000910"
#define MS_TRANSITION_OPTIMIZED_ON_SPECIFIED_INSTRUMENT_NAME "transition optimized on specified instrument"
/* def: "The transition has been optimized by direct injection of the peptide into an instrument specified in a separate term, and the optimum voltages and fragmentation energies have been determined." [PSI:MS]
is_a: MS:1000909 ! transition validation method */

#define MS_TRANSITION_VALIDATED_WITH_AN_MS_MS_SPECTRUM_ON_SPECIFIED_INSTRUMENT_ID "MS:1000911"
#define MS_TRANSITION_VALIDATED_WITH_AN_MS_MS_SPECTRUM_ON_SPECIFIED_INSTRUMENT_NAME "transition validated with an MS_MS spectrum on specified instrument"
/* def: "The transition has been validated by obtaining an MS/MS spectrum and demonstrating that the peak is detectable on the instrument specified with a separate term." [PSI:MS]
is_a: MS:1000909 ! transition validation method */

#define MS_TRANSITION_PURPORTED_FROM_AN_MS_MS_SPECTRUM_ON_A_DIFFERENT__SPECIFIED_INSTRUMENT_ID "MS:1000912"
#define MS_TRANSITION_PURPORTED_FROM_AN_MS_MS_SPECTRUM_ON_A_DIFFERENT__SPECIFIED_INSTRUMENT_NAME "transition purported from an MS_MS spectrum on a different, specified instrument"
/* def: "The transition has been purported by obtaining an MS/MS spectrum and demonstrating that the peak is detectable on the instrument specified with a separate term. However, the detecting instrument is of a different type (e.g. ion trap) than the instrument that the transition will eventually be used on (e.g. triple quad)." [PSI:MS]
is_a: MS:1000909 ! transition validation method */

#define MS_TRANSITION_PREDICTED_BY_INFORMATIC_ANALYSIS_ID "MS:1000913"
#define MS_TRANSITION_PREDICTED_BY_INFORMATIC_ANALYSIS_NAME "transition predicted by informatic analysis"
/* def: "The transition has been predicted by informatics software without any direct spectral evidence." [PSI:MS]
is_a: MS:1000909 ! transition validation method */

#define MS_TAB_DELIMITED_TEXT_FILE_ID "MS:1000914"
#define MS_TAB_DELIMITED_TEXT_FILE_NAME "tab delimited text file"
/* def: "A file that has two or more columns of tabular data where each column is separated by a TAB character." [PSI:MS]
is_a: MS:1001459 ! file format
is_a: MS:1001040 ! intermediate analysis format */

#define MS_RETENTION_TIME_WINDOW_ATTRIBUTE_ID "MS:1000915"
#define MS_RETENTION_TIME_WINDOW_ATTRIBUTE_NAME "retention time window attribute"
/* def: "An attribute of a window in time about which a peptide might elute from the column." [PSI:MS]
relationship: part_of MS:1000894 ! retention time */

#define MS_RETENTION_TIME_WINDOW_LOWER_OFFSET_ID "MS:1000916"
#define MS_RETENTION_TIME_WINDOW_LOWER_OFFSET_NAME "retention time window lower offset"
/* def: "The extent of the retention time window in time units below the target retention time. The lower and upper offsets may be asymetric about the target time." [PSI:MS]
xref: value-type:xsd\:float "The allowed value-type for this CV term."
is_a: MS:1000915 ! retention time window attribute
relationship: has_units UO:0000010 ! second
relationship: has_units UO:0000031 ! minute */

#define MS_RETENTION_TIME_WINDOW_UPPER_OFFSET_ID "MS:1000917"
#define MS_RETENTION_TIME_WINDOW_UPPER_OFFSET_NAME "retention time window upper offset"
/* def: "The extent of the retention time window in time units above the target retention time. The lower and upper offsets may be asymetric about the target time." [PSI:MS]
xref: value-type:xsd\:float "The allowed value-type for this CV term."
is_a: MS:1000915 ! retention time window attribute
relationship: has_units UO:0000010 ! second
relationship: has_units UO:0000031 ! minute */

#define MS_TARGET_LIST_ID "MS:1000918"
#define MS_TARGET_LIST_NAME "target list"
/* def: "A list of peptides or compounds and their expected m/z coordinates that can be used to cause a mass spectrometry to obtain spectra of those molecules specifically." [PSI:MS]
relationship: part_of MS:1001458 ! spectrum generation information */

#define MS_TARGET_INCLUSION_EXCLUSION_PRIORITY_ID "MS:1000919"
#define MS_TARGET_INCLUSION_EXCLUSION_PRIORITY_NAME "target inclusion exclusion priority"
/* def: "A priority setting specifying whether included or excluded targets have priority over the other." [PSI:MS]
relationship: part_of MS:1000918 ! target list */

#define MS_INCLUDES_SUPERSEDE_EXCLUDES_ID "MS:1000920"
#define MS_INCLUDES_SUPERSEDE_EXCLUDES_NAME "includes supersede excludes"
/* def: "A priority setting specifying that included targets have priority over the excluded targets if there is a conflict." [PSI:MS]
is_a: MS:1000919 ! target inclusion exclusion priority */

#define MS_EXCLUDES_SUPERSEDE_INCLUDES_ID "MS:1000921"
#define MS_EXCLUDES_SUPERSEDE_INCLUDES_NAME "excludes supersede includes"
/* def: "A priority setting specifying that excluded targets have priority over the included targets if there is a conflict." [PSI:MS]
is_a: MS:1000919 ! target inclusion exclusion priority */

#define MS_SKYLINE_ID "MS:1000922"
#define MS_SKYLINE_NAME "Skyline"
/* def: "Software used to predict, select, and optimize transitions as well as analyze the results of selected reaction monitoring runs developed and distributed by the MacCoss lab at the University of Washington." [https://brendanx-uw1.gs.washington.edu/labkey/wiki/home/software/Skyline/page.view?name=default ""]
is_a: MS:1000871 ! SRM software */

#define MS_TIQAM_ID "MS:1000923"
#define MS_TIQAM_NAME "TIQAM"
/* def: "Software used to predict, select, and optimize transitions for selected reaction monitoring experiments developed and distributed by the Institute for Systems Biology." [http://tools.proteomecenter.org/TIQAM/TIQAM.html]
is_a: MS:1000871 ! SRM software */

// #define MS_MARIMBA_ID "MS:1000924"
// #define MS_MARIMBA_NAME "MaRiMba"
/* def: "OBSOLETE Software used to predict transitions for selected reaction monitoring experiments based on observed spectrum libraries developed and distributed by the Institute for Systems Biology." [http://tools.proteomecenter.org/wiki/index.php?title=Software:TPP-MaRiMba]
comment: This term was made obsolete because it was redundant with an existing term (MS:1000872).
is_obsolete: true
replaced_by: MS:1000872 */

#define MS_ATAQS_ID "MS:1000925"
#define MS_ATAQS_NAME "ATAQS"
/* def: "Software suite used to predict, select, and optimize transitions as well as analyze the results of selected reaction monitoring runs developed and distributed by the Institute for Systems Biology." [PSI:MS]
is_a: MS:1000871 ! SRM software */

#define MS_PRODUCT_INTERPRETATION_RANK_ID "MS:1000926"
#define MS_PRODUCT_INTERPRETATION_RANK_NAME "product interpretation rank"
/* def: "The integer rank given an interpretation of an observed product ion. For example, if y8 is selected as the most likely interpretation of a peak, then it is assigned a rank of 1." [PSI:MS]
xref: value-type:xsd\:positiveInteger "The allowed value-type for this CV term."
is_a: MS:1001221 ! fragmentation information */

#define MS_ION_INJECTION_TIME_ID "MS:1000927"
#define MS_ION_INJECTION_TIME_NAME "ion injection time"
/* def: "The length of time spent filling an ion trapping device." [PSI:MS]
xref: value-type:xsd\:float "The allowed value-type for this CV term."
is_a: MS:1000503 ! scan attribute
relationship: has_units UO:0000028 ! millisecond */

#define MS_CALIBRATION_SPECTRUM_ID "MS:1000928"
#define MS_CALIBRATION_SPECTRUM_NAME "calibration spectrum"
/* def: "A spectrum derived from a special calibration source, rather than from the primary injected sample. A calibration spectrum is typically derived from a substance that can be used to correct systematic shift in m/z for spectra of the primary inject sample." [PSI:MS]
is_a: MS:1000559 ! spectrum type */

#define MS_SHIMADZU_BIOTECH_NATIVEID_FORMAT_ID "MS:1000929"
#define MS_SHIMADZU_BIOTECH_NATIVEID_FORMAT_NAME "Shimadzu Biotech nativeID format"
/* def: "source=xsd:string start=xsd:nonNegativeInteger end=xsd:nonNegativeInteger." [PSI:MS]
is_a: MS:1000767 ! native spectrum identifier format */

#define MS_SHIMADZU_BIOTECH_DATABASE_ENTITY_ID "MS:1000930"
#define MS_SHIMADZU_BIOTECH_DATABASE_ENTITY_NAME "Shimadzu Biotech database entity"
/* def: "Shimadzu Biotech format." [PSI:MS]
is_a: MS:1000560 ! mass spectrometer file format */

#define MS_QTRAP_5500_ID "MS:1000931"
#define MS_QTRAP_5500_NAME "QTRAP 5500"
/* def: "Applied Biosystems|MDS SCIEX QTRAP 5500." [PSI:MS]
is_a: MS:1000121 ! AB SCIEX instrument model */

#define MS_TRIPLETOF_5600_ID "MS:1000932"
#define MS_TRIPLETOF_5600_NAME "TripleTOF 5600"
/* def: "AB SCIEX TripleTOF 5600, a quadrupole - quadrupole - time-of-flight mass spectrometer." [PSI:MS]
is_a: MS:1000121 ! AB SCIEX instrument model */

#define MS_PROTEIN_MODIFICATIONS_ID "MS:1000933"
#define MS_PROTEIN_MODIFICATIONS_NAME "protein modifications"
/* def: "Encoding of modifications of the protein sequence from the specified accession, written in PEFF notation." [PSI:MS]
xref: value-type:xsd\:string "The allowed value-type for this CV term."
is_a: MS:1000884 ! protein attribute */

#define MS_GENE_NAME_ID "MS:1000934"
#define MS_GENE_NAME_NAME "gene name"
/* def: "Name of the gene from which the protein is translated." [PSI:MS]
xref: value-type:xsd\:string "The allowed value-type for this CV term."
is_a: MS:1000884 ! protein attribute */

#define MS_SPECTRUM_INTERPRETATION_ID "MS:1001000"
#define MS_SPECTRUM_INTERPRETATION_NAME "spectrum interpretation"
/* def: "Collection of terms from the PSI Proteome Informatics standards describing the interpretation of spectra." [PSI:PI]
relationship: part_of MS:0000000 ! Proteomics Standards Initiative Mass Spectrometry Vocabularies */

#define MS_SEQUEST_CLEAVESAT_ID "MS:1001005"
#define MS_SEQUEST_CLEAVESAT_NAME "SEQUEST:CleavesAt"
/* xref: value-type:xsd\:string "The allowed value-type for this CV term."
is_a: MS:1002096 ! SEQUEST input parameter */

#define MS_SEQUEST_VIEWCV_ID "MS:1001006"
#define MS_SEQUEST_VIEWCV_NAME "SEQUEST:ViewCV"
/* def: "SEQUEST View Input Parameters." [PSI:PI]
is_a: MS:1002096 ! SEQUEST input parameter */

#define MS_SEQUEST_OUTPUTLINES_ID "MS:1001007"
#define MS_SEQUEST_OUTPUTLINES_NAME "SEQUEST:OutputLines"
/* def: "Number of peptide results to show." [PSI:MS]
xref: value-type:xsd\:nonNegativeInteger "The allowed value-type for this CV term."
is_a: MS:1002096 ! SEQUEST input parameter */

#define MS_SEQUEST_DESCRIPTIONLINES_ID "MS:1001009"
#define MS_SEQUEST_DESCRIPTIONLINES_NAME "SEQUEST:DescriptionLines"
/* def: "Number of full protein descriptions to show for top N peptides." [PSI:MS]
xref: value-type:xsd\:string "The allowed value-type for this CV term."
is_a: MS:1002096 ! SEQUEST input parameter */

#define MS_DE_NOVO_SEARCH_ID "MS:1001010"
#define MS_DE_NOVO_SEARCH_NAME "de novo search"
/* def: "A de novo sequencing search (without database)." [PSI:PI]
is_a: MS:1001080 ! search type */

#define MS_SEARCH_DATABASE_DETAILS_ID "MS:1001011"
#define MS_SEARCH_DATABASE_DETAILS_NAME "search database details"
/* def: "Details about the database searched." [PSI:PI]
is_a: MS:1001249 ! search input details */

#define MS_DATABASE_SOURCE_ID "MS:1001012"
#define MS_DATABASE_SOURCE_NAME "database source"
/* def: "The organisation, project or laboratory from where the database is obtained (uniprot, ncbi, ebi, other)." [PSI:PI]
xref: value-type:xsd\:string "The allowed value-type for this CV term."
is_a: MS:1001011 ! search database details */

#define MS_DATABASE_NAME_ID "MS:1001013"
#define MS_DATABASE_NAME_NAME "database name"
/* def: "The name of the search database (nr, SwissProt or est_human)." [PSI:PI]
is_a: MS:1001011 ! search database details */

// #define MS_DATABASE_LOCAL_FILE_PATH_ID "MS:1001014"
// #define MS_DATABASE_LOCAL_FILE_PATH_NAME "database local file path"
/* def: "OBSOLETE: Use attribute in mzIdentML instead. Local file path of the search database from the search engine's point of view." [PSI:PI]
is_a: MS:1001011 ! search database details
is_obsolete: true */

#define MS_DATABASE_ORIGINAL_URI_ID "MS:1001015"
#define MS_DATABASE_ORIGINAL_URI_NAME "database original uri"
/* def: "URI, from where the search database was originally downloaded." [PSI:PI]
xref: value-type:xsd\:anyURI "The allowed value-type for this CV term."
is_a: MS:1001011 ! search database details */

// #define MS_DATABASE_VERSION_ID "MS:1001016"
// #define MS_DATABASE_VERSION_NAME "database version"
/* def: "OBSOLETE: Use attribute in mzIdentML instead. Version of the search database." [PSI:PI]
is_a: MS:1001011 ! search database details
is_obsolete: true */

// #define MS_DATABASE_RELEASE_DATE_ID "MS:1001017"
// #define MS_DATABASE_RELEASE_DATE_NAME "database release date"
/* def: "OBSOLETE: Use attribute in mzIdentML instead. Release date of the search database." [PSI:PI]
is_a: MS:1001011 ! search database details
is_obsolete: true */

#define MS_DATABASE_TYPE_ID "MS:1001018"
#define MS_DATABASE_TYPE_NAME "database type"
/* def: "Database containing amino acid or nucleic acid sequences." [PSI:PI]
is_a: MS:1001011 ! search database details */

#define MS_DATABASE_FILTERING_ID "MS:1001019"
#define MS_DATABASE_FILTERING_NAME "database filtering"
/* def: "Was there filtering used on the database." [PSI:PI]
is_a: MS:1001011 ! search database details */

#define MS_DB_FILTER_TAXONOMY_ID "MS:1001020"
#define MS_DB_FILTER_TAXONOMY_NAME "DB filter taxonomy"
/* def: "A taxonomy filter was to the database search." [PSI:PI]
is_a: MS:1001511 ! Sequence database filter types */

#define MS_DB_FILTER_ON_ACCESSION_NUMBERS_ID "MS:1001021"
#define MS_DB_FILTER_ON_ACCESSION_NUMBERS_NAME "DB filter on accession numbers"
/* def: "Filtering applied specifically by accession number pattern." [PSI:PI]
is_a: MS:1001511 ! Sequence database filter types */

#define MS_DB_MW_FILTER_ID "MS:1001022"
#define MS_DB_MW_FILTER_NAME "DB MW filter"
/* def: "Filtering applied specifically by protein molecular weight, specified as either a range or above/below a threshold value." [PSI:PI]
is_a: MS:1001511 ! Sequence database filter types */

#define MS_DB_PI_FILTER_ID "MS:1001023"
#define MS_DB_PI_FILTER_NAME "DB PI filter"
/* def: "Filtering applied specifically by predicted protein isoelectric focussing point (pI), specified as either a range or above/below a threshold value." [PSI:PI]
is_a: MS:1001511 ! Sequence database filter types */

#define MS_TRANSLATION_FRAME_ID "MS:1001024"
#define MS_TRANSLATION_FRAME_NAME "translation frame"
/* def: "The translated open reading frames from a nucleotide database considered in the search (range: 1-6)." [PSI:PI]
is_a: MS:1001011 ! search database details */

#define MS_TRANSLATION_TABLE_ID "MS:1001025"
#define MS_TRANSLATION_TABLE_NAME "translation table"
/* def: "The translation table used to translate the nucleotides to amino acids." [PSI:PI]
xref: value-type:xsd\:string "The allowed value-type for this CV term."
is_a: MS:1001011 ! search database details */

#define MS_SEQUEST_NORMALIZEXCORRVALUES_ID "MS:1001026"
#define MS_SEQUEST_NORMALIZEXCORRVALUES_NAME "SEQUEST:NormalizeXCorrValues"
/* xref: value-type:xsd\:boolean "The allowed value-type for this CV term."
is_a: MS:1002096 ! SEQUEST input parameter */

#define MS_DB_FILTER_ON_SEQUENCE_PATTERN_ID "MS:1001027"
#define MS_DB_FILTER_ON_SEQUENCE_PATTERN_NAME "DB filter on sequence pattern"
/* def: "Filtering applied specifically by amino acid sequence pattern." [PSI:PI]
is_a: MS:1001511 ! Sequence database filter types */

#define MS_SEQUEST_SEQUENCEHEADERFILTER_ID "MS:1001028"
#define MS_SEQUEST_SEQUENCEHEADERFILTER_NAME "SEQUEST:SequenceHeaderFilter"
/* def: "String in the header of a sequence entry for that entry to be searched." [PSI:MS]
xref: value-type:xsd\:string "The allowed value-type for this CV term."
is_a: MS:1002096 ! SEQUEST input parameter */

#define MS_NUMBER_OF_SEQUENCES_SEARCHED_ID "MS:1001029"
#define MS_NUMBER_OF_SEQUENCES_SEARCHED_NAME "number of sequences searched"
/* def: "The number of sequences (proteins / nucleotides) from the database search after filtering." [PSI:PI]
xref: value-type:xsd\:positiveInteger "The allowed value-type for this CV term."
is_a: MS:1001011 ! search database details */

#define MS_NUMBER_OF_PEPTIDE_SEQS_COMPARED_TO_EACH_SPECTRUM_ID "MS:1001030"
#define MS_NUMBER_OF_PEPTIDE_SEQS_COMPARED_TO_EACH_SPECTRUM_NAME "number of peptide seqs compared to each spectrum"
/* def: "Number of peptide seqs compared to each spectrum." [PSI:PI]
xref: value-type:xsd\:nonNegativeInteger "The allowed value-type for this CV term."
is_a: MS:1001011 ! search database details
is_a: MS:1001405 ! spectrum identification result details */

#define MS_SPECTRAL_LIBRARY_SEARCH_ID "MS:1001031"
#define MS_SPECTRAL_LIBRARY_SEARCH_NAME "spectral library search"
/* def: "A search using a library of spectra." [PSI:PI]
is_a: MS:1001080 ! search type */

#define MS_SEQUEST_SEQUENCEPARTIALFILTER_ID "MS:1001032"
#define MS_SEQUEST_SEQUENCEPARTIALFILTER_NAME "SEQUEST:SequencePartialFilter"
/*xref: value-type:xsd\:string "The allowed value-type for this CV term."
is_a: MS:1002096 ! SEQUEST input parameter */

// #define MS_DATE___TIME_SEARCH_PERFORMED_ID "MS:1001035"
// #define MS_DATE___TIME_SEARCH_PERFORMED_NAME "date _ time search performed"
/* def: "OBSOLETE: use attribute in mzIdentML instead. Date and time of the actual search run." [PSI:PI]
is_a: MS:1001184 ! search statistics
is_obsolete: true */

#define MS_SEARCH_TIME_TAKEN_ID "MS:1001036"
#define MS_SEARCH_TIME_TAKEN_NAME "search time taken"
/* def: "The time taken to complete the search in seconds." [PSI:PI]
xref: value-type:xsd\:positiveInteger "The allowed value-type for this CV term."
is_a: MS:1001184 ! search statistics */

#define MS_SEQUEST_SHOWFRAGMENTIONS_ID "MS:1001037"
#define MS_SEQUEST_SHOWFRAGMENTIONS_NAME "SEQUEST:ShowFragmentIons"
/* def: "Flag indicating that fragment ions should be shown." [PSI:MS]
xref: value-type:xsd\:boolean "The allowed value-type for this CV term."
is_a: MS:1002096 ! SEQUEST input parameter */

#define MS_SEQUEST_CONSENSUS_ID "MS:1001038"
#define MS_SEQUEST_CONSENSUS_NAME "SEQUEST:Consensus"
/* def: "Specify depth as value of the CVParam." [PSI:PI]
xref: value-type:xsd\:positiveInteger "The allowed value-type for this CV term."
is_a: MS:1001006 ! SEQUEST:ViewCV */

#define MS_INTERMEDIATE_ANALYSIS_FORMAT_ID "MS:1001040"
#define MS_INTERMEDIATE_ANALYSIS_FORMAT_NAME "intermediate analysis format"
/* def: "Type of the source file, the mzIdentML was created from." [PSI:PI]
is_a: MS:1001459 ! file format */

#define MS_SEQUEST_SORTCV_ID "MS:1001041"
#define MS_SEQUEST_SORTCV_NAME "SEQUEST:sortCV"
/* def: "SEQUEST View / Sort Input Parameters." [PSI:PI]
is_a: MS:1001006 ! SEQUEST:ViewCV */

#define MS_SEQUEST_LIMITTO_ID "MS:1001042"
#define MS_SEQUEST_LIMITTO_NAME "SEQUEST:LimitTo"
/* def: "Specify \"number of dtas shown\" as value of the CVParam." [PSI:PI]
xref: value-type:xsd\:positiveInteger "The allowed value-type for this CV term."
is_a: MS:1002096 ! SEQUEST input parameter */

#define MS_CLEAVAGE_AGENT_DETAILS_ID "MS:1001044"
#define MS_CLEAVAGE_AGENT_DETAILS_NAME "cleavage agent details"
/* def: "Details of cleavage agent (enzyme)." [PSI:PI]
is_a: MS:1001249 ! search input details */

#define MS_CLEAVAGE_AGENT_NAME_ID "MS:1001045"
#define MS_CLEAVAGE_AGENT_NAME_NAME "cleavage agent name"
/* def: "The name of the cleavage agent." [PSI:PI]
is_a: MS:1001044 ! cleavage agent details */

#define MS_SEQUEST_SORT_BY_DCN_ID "MS:1001046"
#define MS_SEQUEST_SORT_BY_DCN_NAME "SEQUEST:sort by dCn"
/* def: "Sort order of SEQUEST search results by the delta of the normalized correlation score." [PSI:PI]
xref: value-type:xsd\:boolean "The allowed value-type for this CV term."
is_a: MS:1001041 ! SEQUEST:sortCV */

#define MS_SEQUEST_SORT_BY_DM_ID "MS:1001047"
#define MS_SEQUEST_SORT_BY_DM_NAME "SEQUEST:sort by dM"
/* def: "Sort order of SEQUEST search results by the difference between a theoretically calculated and the corresponding experimentally measured molecular mass M." [PSI:PI]
xref: value-type:xsd\:boolean "The allowed value-type for this CV term."
is_a: MS:1001041 ! SEQUEST:sortCV */

#define MS_SEQUEST_SORT_BY_IONS_ID "MS:1001048"
#define MS_SEQUEST_SORT_BY_IONS_NAME "SEQUEST:sort by Ions"
/* xref: value-type:xsd\:boolean "The allowed value-type for this CV term."
is_a: MS:1001041 ! SEQUEST:sortCV */

#define MS_SEQUEST_SORT_BY_MH__ID "MS:1001049"
#define MS_SEQUEST_SORT_BY_MH__NAME "SEQUEST:sort by MH+"
/* xref: value-type:xsd\:boolean "The allowed value-type for this CV term."
is_a: MS:1001041 ! SEQUEST:sortCV */

#define MS_SEQUEST_SORT_BY_P_ID "MS:1001050"
#define MS_SEQUEST_SORT_BY_P_NAME "SEQUEST:sort by P"
/* xref: value-type:xsd\:boolean "The allowed value-type for this CV term."
is_a: MS:1001041 ! SEQUEST:sortCV */

// #define MS_MULTIPLE_ENZYME_COMBINATION_RULES_ID "MS:1001051"
// #define MS_MULTIPLE_ENZYME_COMBINATION_RULES_NAME "multiple enzyme combination rules"
/* def: "OBSOLETE: use attribute independent in mzIdentML instead. Description of multiple enzyme digestion protocol, if any." [PSI:PI]
xref: value-type:xsd\:string "The allowed value-type for this CV term."
is_obsolete: true */

#define MS_SEQUEST_SORT_BY_PREVIOUSAMINOACID_ID "MS:1001052"
#define MS_SEQUEST_SORT_BY_PREVIOUSAMINOACID_NAME "SEQUEST:sort by PreviousAminoAcid"
/* xref: value-type:xsd\:boolean "The allowed value-type for this CV term."
is_a: MS:1001041 ! SEQUEST:sortCV */

#define MS_SEQUEST_SORT_BY_REF_ID "MS:1001053"
#define MS_SEQUEST_SORT_BY_REF_NAME "SEQUEST:sort by Ref"
/* xref: value-type:xsd\:boolean "The allowed value-type for this CV term."
is_a: MS:1001041 ! SEQUEST:sortCV */

#define MS_MODIFICATION_PARAMETERS_ID "MS:1001055"
#define MS_MODIFICATION_PARAMETERS_NAME "modification parameters"
/* def: "Modification parameters for the search engine run." [PSI:PI]
relationship: part_of MS:1001000 ! spectrum interpretation */

#define MS_MODIFICATION_SPECIFICITY_RULE_ID "MS:1001056"
#define MS_MODIFICATION_SPECIFICITY_RULE_NAME "modification specificity rule"
/* def: "The specificity rules for the modifications applied by the search engine." [PSI:PI]
is_a: MS:1001055 ! modification parameters */

// #define MS_TOLERANCE_ON_TYPES_ID "MS:1001057"
// #define MS_TOLERANCE_ON_TYPES_NAME "tolerance on types"
/* def: "OBSOLETE: Tolerance on types." [PSI:PI]
is_a: MS:1001055 ! modification parameters
is_obsolete: true */

#define MS_QUALITY_ESTIMATION_BY_MANUAL_VALIDATION_ID "MS:1001058"
#define MS_QUALITY_ESTIMATION_BY_MANUAL_VALIDATION_NAME "quality estimation by manual validation"
/* def: "The quality estimation was done manually." [PSI:PI]
is_a: MS:1001060 ! quality estimation method details */

#define MS_SEQUEST_SORT_BY_RSP_ID "MS:1001059"
#define MS_SEQUEST_SORT_BY_RSP_NAME "SEQUEST:sort by RSp"
/* xref: value-type:xsd\:boolean "The allowed value-type for this CV term."
is_a: MS:1001041 ! SEQUEST:sortCV */

#define MS_QUALITY_ESTIMATION_METHOD_DETAILS_ID "MS:1001060"
#define MS_QUALITY_ESTIMATION_METHOD_DETAILS_NAME "quality estimation method details"
/* def: "Method for quality estimation (manually or with decoy database)." [PSI:PI]
is_a: MS:1001249 ! search input details */

// #define MS_NEUTRAL_LOSS_ID "MS:1001061"
// #define MS_NEUTRAL_LOSS_NAME "neutral loss"
/* def: "OBSOLETE: replaced by MS:1000336 (neutral loss): Leave this to PSI-MOD." [PSI:PI]
is_a: MS:1001055 ! modification parameters
is_obsolete: true
replaced_by: MS:1000336 */

#define MS_MASCOT_MGF_FILE_ID "MS:1001062"
#define MS_MASCOT_MGF_FILE_NAME "Mascot MGF file"
/* def: "Mascot MGF file." [PSI:MS]
is_a: MS:1000560 ! mass spectrometer file format */

// #define MS_TODOSCORING_MODEL_ID "MS:1001065"
// #define MS_TODOSCORING_MODEL_NAME "TODOscoring model"
/* def: "OBSOLETE: There is Phenyx:ScoringModel for Phenyx! Scoring model (more detailed granularity). TODO: add some child terms." [PSI:PI]
comment: This term was made obsolete and is replaced by the term (MS:1001961).
is_a: MS:1001249 ! search input details
is_obsolete: true
replaced_by: MS:1001961 */

#define MS_IONS_SERIES_CONSIDERED_IN_SEARCH_ID "MS:1001066"
#define MS_IONS_SERIES_CONSIDERED_IN_SEARCH_NAME "ions series considered in search"
/* def: "The description of the ion fragment series (including charges and neutral losses) that are considered by the search engine." [PSI:PI]
is_a: MS:1001249 ! search input details */

#define MS_SEQUEST_SORT_BY_SP_ID "MS:1001068"
#define MS_SEQUEST_SORT_BY_SP_NAME "SEQUEST:sort by Sp"
/* def: "Sort order of SEQUEST search results by the Sp score." [PSI:PI]
xref: value-type:xsd\:boolean "The allowed value-type for this CV term."
is_a: MS:1001041 ! SEQUEST:sortCV */

#define MS_SEQUEST_SORT_BY_TIC_ID "MS:1001069"
#define MS_SEQUEST_SORT_BY_TIC_NAME "SEQUEST:sort by TIC"
/* xref: value-type:xsd\:boolean "The allowed value-type for this CV term."
is_a: MS:1001041 ! SEQUEST:sortCV */

#define MS_SEQUEST_SORT_BY_SCAN_ID "MS:1001070"
#define MS_SEQUEST_SORT_BY_SCAN_NAME "SEQUEST:sort by Scan"
/* xref: value-type:xsd\:boolean "The allowed value-type for this CV term."
is_a: MS:1001041 ! SEQUEST:sortCV */

#define MS_SEQUEST_SORT_BY_SEQUENCE_ID "MS:1001071"
#define MS_SEQUEST_SORT_BY_SEQUENCE_NAME "SEQUEST:sort by Sequence"
/* xref: value-type:xsd\:boolean "The allowed value-type for this CV term."
is_a: MS:1001041 ! SEQUEST:sortCV */

#define MS_SEQUEST_SORT_BY_SF_ID "MS:1001072"
#define MS_SEQUEST_SORT_BY_SF_NAME "SEQUEST:sort by Sf"
/* xref: value-type:xsd\:boolean "The allowed value-type for this CV term."
is_a: MS:1001041 ! SEQUEST:sortCV */

#define MS_DATABASE_TYPE_AMINO_ACID_ID "MS:1001073"
#define MS_DATABASE_TYPE_AMINO_ACID_NAME "database type amino acid"
/* def: "Database contains amino acid sequences." [PSI:PI]
is_a: MS:1001018 ! database type */

#define MS_DATABASE_TYPE_NUCLEOTIDE_ID "MS:1001079"
#define MS_DATABASE_TYPE_NUCLEOTIDE_NAME "database type nucleotide"
/* def: "Database contains nucleid acid sequences." [PSI:PI]
is_a: MS:1001018 ! database type */

#define MS_SEARCH_TYPE_ID "MS:1001080"
#define MS_SEARCH_TYPE_NAME "search type"
/* def: "Enumeration of type of search value (i.e. from PMF, sequence tag, MS-MS)." [PSI:PI]
is_a: MS:1001249 ! search input details */

#define MS_PMF_SEARCH_ID "MS:1001081"
#define MS_PMF_SEARCH_NAME "pmf search"
/* def: "A peptide mass fingerprint search." [PSI:PI]
is_a: MS:1001080 ! search type */

#define MS_TAG_SEARCH_ID "MS:1001082"
#define MS_TAG_SEARCH_NAME "tag search"
/* def: "A sequence tag search." [PSI:PI]
is_a: MS:1001080 ! search type */

#define MS_MS_MS_SEARCH_ID "MS:1001083"
#define MS_MS_MS_SEARCH_NAME "ms-ms search"
/* def: "An ms/ms search (with fragment ions)." [PSI:PI]
is_a: MS:1001080 ! search type */

#define MS_DATABASE_NR_ID "MS:1001084"
#define MS_DATABASE_NR_NAME "database nr"
/* def: "Non-redundant GenBank sequence database." [PSI:PI]
is_a: MS:1001013 ! database name */

#define MS_PROTEIN_RESULT_DETAILS_ID "MS:1001085"
#define MS_PROTEIN_RESULT_DETAILS_NAME "protein result details"
/* def: "Protein level information." [PSI:PI]
is_a: MS:1001405 ! spectrum identification result details */

#define MS_SEQUEST_SORT_BY_XCORR_ID "MS:1001086"
#define MS_SEQUEST_SORT_BY_XCORR_NAME "SEQUEST:sort by XCorr"
/* def: "Sort order of SEQUEST search results by the correlation score." [PSI:PI]
xref: value-type:xsd\:boolean "The allowed value-type for this CV term."
is_a: MS:1001041 ! SEQUEST:sortCV */

#define MS_SEQUEST_PROCESSCV_ID "MS:1001087"
#define MS_SEQUEST_PROCESSCV_NAME "SEQUEST:ProcessCV"
/* def: "SEQUEST View / Process Input Parameters." [PSI:PI]
is_a: MS:1002096 ! SEQUEST input parameter */

#define MS_PROTEIN_DESCRIPTION_ID "MS:1001088"
#define MS_PROTEIN_DESCRIPTION_NAME "protein description"
/* def: "The protein description line from the sequence entry in the source database FASTA file." [PSI:PI]
xref: value-type:xsd\:string "The allowed value-type for this CV term."
is_a: MS:1001116 ! single protein result details
is_a: MS:1001342 ! database sequence details */

#define MS_MOLECULE_TAXONOMY_ID "MS:1001089"
#define MS_MOLECULE_TAXONOMY_NAME "molecule taxonomy"
/* def: "The taxonomy of the resultant molecule from the search." [PSI:PI]
xref: value-type:xsd\:string "The allowed value-type for this CV term."
is_a: MS:1001085 ! protein result details
is_a: MS:1001342 ! database sequence details
is_a: MS:1001512 ! Sequence database filters */

// #define MS_TAXONOMY_NOMENCLATURE_ID "MS:1001090"
// #define MS_TAXONOMY_NOMENCLATURE_NAME "taxonomy nomenclature"
/* def: "OBSOLETE: The system used to indicate taxonomy. There should be an enumerated list of options: latin name, NCBI TaxID, common name, Swiss-Prot species ID (ex. RABIT from the full protein ID ALBU_RABIT)." [PSI:PI]
is_a: MS:1001089 ! molecule taxonomy
is_obsolete: true
replaced_by: MS:1001467
replaced_by: MS:1001468
replaced_by: MS:1001469
replaced_by: MS:1001470 */

#define MS_NOENZYME_ID "MS:1001091"
#define MS_NOENZYME_NAME "NoEnzyme"
/* is_a: MS:1001045 ! cleavage agent name
comment: This term was made obsolete because it is ambiguous and is replaced by NoCleavage (MS:1001955) and unspecific cleavage (MS:1001956).
is_obsolete: true */

#define MS_PEPTIDE_IDENTIFICATION_CONFIDENCE_METRIC_ID "MS:1001092"
#define MS_PEPTIDE_IDENTIFICATION_CONFIDENCE_METRIC_NAME "peptide identification confidence metric"
/* def: "Identification confidence metric for a peptide." [PSI:PI]
is_a: MS:1001105 ! peptide result details */

#define MS_SEQUENCE_COVERAGE_ID "MS:1001093"
#define MS_SEQUENCE_COVERAGE_NAME "sequence coverage"
/* def: "The percent coverage for the protein based upon the matched peptide sequences (can be calculated)." [PSI:PI]
xref: value-type:xsd\:decimal "The allowed value-type for this CV term."
is_a: MS:1001116 ! single protein result details */

#define MS_SEQUEST_SORT_BY_Z_ID "MS:1001094"
#define MS_SEQUEST_SORT_BY_Z_NAME "SEQUEST:sort by z"
/* xref: value-type:xsd\:boolean "The allowed value-type for this CV term."
is_a: MS:1001041 ! SEQUEST:sortCV */

#define MS_SEQUEST_PROCESSALL_ID "MS:1001095"
#define MS_SEQUEST_PROCESSALL_NAME "SEQUEST:ProcessAll"
/* xref: value-type:xsd\:boolean "The allowed value-type for this CV term."
is_a: MS:1001087 ! SEQUEST:ProcessCV */

#define MS_SEQUEST_TOPPERCENTMOSTINTENSE_ID "MS:1001096"
#define MS_SEQUEST_TOPPERCENTMOSTINTENSE_NAME "SEQUEST:TopPercentMostIntense"
/* def: "Specify \"percentage\" as value of the CVParam." [PSI:PI]
xref: value-type:xsd\:double "The allowed value-type for this CV term."
is_a: MS:1001087 ! SEQUEST:ProcessCV */

#define MS_DISTINCT_PEPTIDE_SEQUENCES_ID "MS:1001097"
#define MS_DISTINCT_PEPTIDE_SEQUENCES_NAME "distinct peptide sequences"
/* def: "This counts distinct sequences hitting the protein without regard to a minimal confidence threshold." [PSI:PI]
xref: value-type:xsd\:nonNegativeInteger "The allowed value-type for this CV term."
is_a: MS:1001116 ! single protein result details */

#define MS_CONFIDENT_DISTINCT_PEPTIDE_SEQUENCES_ID "MS:1001098"
#define MS_CONFIDENT_DISTINCT_PEPTIDE_SEQUENCES_NAME "confident distinct peptide sequences"
/* def: "This counts the number of distinct peptide sequences. Multiple charge states and multiple modification states do NOT count as multiple sequences. The definition of 'confident' must be qualified elsewhere." [PSI:PI]
xref: value-type:xsd\:nonNegativeInteger "The allowed value-type for this CV term."
is_a: MS:1001116 ! single protein result details */

#define MS_CONFIDENT_PEPTIDE_QUALIFICATION_ID "MS:1001099"
#define MS_CONFIDENT_PEPTIDE_QUALIFICATION_NAME "confident peptide qualification"
/* def: "The point of this entry is to define what is meant by confident for the term Confident distinct peptide sequence and/or Confident peptides. Example 1 - metric=Paragon:Confidence value=95 sense=greater than Example 2 - metric=Mascot:Eval value=0.05 sense=less than." [PSI:PI]
xref: value-type:xsd\:nonNegativeInteger "The allowed value-type for this CV term."
is_a: MS:1001116 ! single protein result details */

#define MS_CONFIDENT_PEPTIDE_SEQUENCE_NUMBER_ID "MS:1001100"
#define MS_CONFIDENT_PEPTIDE_SEQUENCE_NUMBER_NAME "confident peptide sequence number"
/* def: "This counts the number of peptide sequences without regard to whether they are distinct. Multiple charges states and multiple modification states DO count as multiple peptides. The definition of 'confident' must be qualified elsewhere." [PSI:PI]
xref: value-type:xsd\:nonNegativeInteger "The allowed value-type for this CV term."
is_a: MS:1001116 ! single protein result details */

#define MS_PROTEIN_GROUP_OR_SUBSET_RELATIONSHIP_ID "MS:1001101"
#define MS_PROTEIN_GROUP_OR_SUBSET_RELATIONSHIP_NAME "protein group or subset relationship"
/* def: "Protein group or subset relationships." [PSI:PI]
is_a: MS:1001085 ! protein result details */

#define MS_SEQUEST_CHROMATOGRAM_ID "MS:1001102"
#define MS_SEQUEST_CHROMATOGRAM_NAME "SEQUEST:Chromatogram"
/* xref: value-type:xsd\:boolean "The allowed value-type for this CV term."
is_a: MS:1001006 ! SEQUEST:ViewCV */

#define MS_SEQUEST_INFOANDLOG_ID "MS:1001103"
#define MS_SEQUEST_INFOANDLOG_NAME "SEQUEST:InfoAndLog"
/* xref: value-type:xsd\:boolean "The allowed value-type for this CV term."
is_a: MS:1001006 ! SEQUEST:ViewCV */

#define MS_DATABASE_UNIPROTKB_SWISS_PROT_ID "MS:1001104"
#define MS_DATABASE_UNIPROTKB_SWISS_PROT_NAME "database UniProtKB_Swiss-Prot"
/* def: "The name of the UniProtKB/Swiss-Prot knowledgebase." [PSI:PI]
is_a: MS:1002126 ! database UniProtKB */

#define MS_PEPTIDE_RESULT_DETAILS_ID "MS:1001105"
#define MS_PEPTIDE_RESULT_DETAILS_NAME "peptide result details"
/* def: "Peptide level information." [PSI:PI]
is_a: MS:1001405 ! spectrum identification result details */

#define MS_SEQUEST_TOPNUMBER_ID "MS:1001106"
#define MS_SEQUEST_TOPNUMBER_NAME "SEQUEST:TopNumber"
/* def: "Specify \"number\" as value of the CVParam." [PSI:PI]
xref: value-type:xsd\:positiveInteger "The allowed value-type for this CV term."
is_a: MS:1001087 ! SEQUEST:ProcessCV */

#define MS_DATA_STORED_IN_DATABASE_ID "MS:1001107"
#define MS_DATA_STORED_IN_DATABASE_NAME "data stored in database"
/* def: "Source file for this mzIdentML was a data set in a database." [PSI:PI]
is_a: MS:1001040 ! intermediate analysis format */

#define MS_PARAM__A_ION_ID "MS:1001108"
#define MS_PARAM__A_ION_NAME "param: a ion"
/* def: "Parameter information, type of product: a ion with charge on the N-terminal side." [PSI:PI]
is_a: MS:1001066 ! ions series considered in search */

#define MS_SEQUEST_CULLTO_ID "MS:1001109"
#define MS_SEQUEST_CULLTO_NAME "SEQUEST:CullTo"
/* def: "Specify cull string as value of the CVParam." [PSI:PI]
xref: value-type:xsd\:string "The allowed value-type for this CV term."
is_a: MS:1001087 ! SEQUEST:ProcessCV */

#define MS_SEQUEST_MODECV_ID "MS:1001110"
#define MS_SEQUEST_MODECV_NAME "SEQUEST:modeCV"
/* def: "SEQUEST Mode Input Parameters." [PSI:PI]
is_a: MS:1002096 ! SEQUEST input parameter */

#define MS_SEQUEST_FULL_ID "MS:1001111"
#define MS_SEQUEST_FULL_NAME "SEQUEST:Full"
/* xref: value-type:xsd\:boolean "The allowed value-type for this CV term."
is_a: MS:1001110 ! SEQUEST:modeCV */

#define MS_N_TERMINAL_FLANKING_RESIDUE_ID "MS:1001112"
#define MS_N_TERMINAL_FLANKING_RESIDUE_NAME "n-terminal flanking residue"
/* def: "The residue preceeding the first amino acid in the peptide sequence as it occurs in the protein. Use 'N-term' to denote if the peptide starts at the N terminus of the protein." [PSI:PI]
xref: value-type:xsd\:string "The allowed value-type for this CV term."
is_a: MS:1001105 ! peptide result details */

#define MS_C_TERMINAL_FLANKING_RESIDUE_ID "MS:1001113"
#define MS_C_TERMINAL_FLANKING_RESIDUE_NAME "c-terminal flanking residue"
/* def: "The residue following the last amino acid in the peptide sequence as it occurs in the protein. Use 'C-term' to denote if the peptide ends at the C terminus of the protein." [PSI:PI]
xref: value-type:xsd\:string "The allowed value-type for this CV term."
is_a: MS:1001105 ! peptide result details */

// #define MS_RETENTION_TIME(S)_ID "MS:1001114"
// #define MS_RETENTION_TIME(S)_NAME "retention time(s)"
/* def: "OBSOLETE Retention time of the spectrum from the source file." [PSI:PI]
comment: This term was made obsolete because scan start time (MS:1000016) should be used instead.
xref: value-type:xsd\:double "The allowed value-type for this CV term."
is_a: MS:1001105 ! peptide result details
is_a: MS:1001405 ! spectrum identification result details
relationship: has_units UO:0000010 ! second
relationship: has_units UO:0000031 ! minute
is_obsolete: true */

// #define MS_SCAN_NUMBER(S)_ID "MS:1001115"
// #define MS_SCAN_NUMBER(S)_NAME "scan number(s)"
/* def: "OBSOLETE: use spectrumID attribute of SpectrumIdentificationResult. Take from mzData." [PSI:PI]
comment: This former purgatory term was made obsolete.
is_obsolete: true */

#define MS_SINGLE_PROTEIN_RESULT_DETAILS_ID "MS:1001116"
#define MS_SINGLE_PROTEIN_RESULT_DETAILS_NAME "single protein result details"
/* def: "Results specific for one protein as part of a protein ambiguity group (a result not valid for all the other proteins in the protein ambiguity group)." [PSI:PI]
is_a: MS:1001085 ! protein result details */

#define MS_THEORETICAL_MASS_ID "MS:1001117"
#define MS_THEORETICAL_MASS_NAME "theoretical mass"
/* def: "The theoretical mass of the molecule (e.g. the peptide sequence and its modifications)." [PSI:PI]
xref: value-type:xsd\:double "The allowed value-type for this CV term."
is_a: MS:1001105 ! peptide result details
relationship: has_units UO:0000221 ! dalton */

#define MS_PARAM__B_ION_ID "MS:1001118"
#define MS_PARAM__B_ION_NAME "param: b ion"
/* def: "Parameter information, type of product: b ion with charge on the N-terminal side." [PSI:PI]
is_a: MS:1001066 ! ions series considered in search */

#define MS_PARAM__C_ION_ID "MS:1001119"
#define MS_PARAM__C_ION_NAME "param: c ion"
/* def: "Parameter information, type of product: c ion with charge on the N-terminal side." [PSI:PI]
is_a: MS:1001066 ! ions series considered in search */

#define MS_SEQUEST_FORMATANDLINKS_ID "MS:1001120"
#define MS_SEQUEST_FORMATANDLINKS_NAME "SEQUEST:FormatAndLinks"
/* xref: value-type:xsd\:boolean "The allowed value-type for this CV term."
is_a: MS:1001110 ! SEQUEST:modeCV */

#define MS_NUMBER_OF_MATCHED_PEAKS_ID "MS:1001121"
#define MS_NUMBER_OF_MATCHED_PEAKS_NAME "number of matched peaks"
/* def: "The number of peaks that were matched as qualified by the ion series considered field. If a peak matches multiple ions then only 1 would be added the count." [PSI:PI]
xref: value-type:xsd\:nonNegativeInteger "The allowed value-type for this CV term."
is_a: MS:1001105 ! peptide result details */

#define MS_IONS_SERIES_CONSIDERED_ID "MS:1001122"
#define MS_IONS_SERIES_CONSIDERED_NAME "ions series considered"
/* def: "The ion series that were used during the calculation of the count (e.g. b, y, a, b, c, y, z, b, b-NH3, b-H20, b+, z, z-, z+, y-H3PO4, immonium)." [PSI:PI]
is_a: MS:1001105 ! peptide result details */

#define MS_NUMBER_OF_PEAKS_USED_ID "MS:1001123"
#define MS_NUMBER_OF_PEAKS_USED_NAME "number of peaks used"
/* def: "The number of peaks from the original peak list that are used to calculate the scores for a particular search engine. All ions that have the opportunity to match or be counted even if they don't." [PSI:PI]
xref: value-type:xsd\:nonNegativeInteger "The allowed value-type for this CV term."
is_a: MS:1001105 ! peptide result details */

#define MS_NUMBER_OF_PEAKS_SUBMITTED_ID "MS:1001124"
#define MS_NUMBER_OF_PEAKS_SUBMITTED_NAME "number of peaks submitted"
/* def: "The number of peaks from the original peaks listed that were submitted to the search engine." [PSI:PI]
xref: value-type:xsd\:nonNegativeInteger "The allowed value-type for this CV term."
is_a: MS:1001105 ! peptide result details */

#define MS_MANUAL_VALIDATION_ID "MS:1001125"
#define MS_MANUAL_VALIDATION_NAME "manual validation"
/* def: "Result of quality estimation: decision of a manual validation." [PSI:PI]
xref: value-type:xsd\:string "The allowed value-type for this CV term."
is_a: MS:1001092 ! peptide identification confidence metric
is_a: MS:1001198 ! protein identification confidence metric */

#define MS_SEQUEST_FAST_ID "MS:1001126"
#define MS_SEQUEST_FAST_NAME "SEQUEST:Fast"
/* xref: value-type:xsd\:boolean "The allowed value-type for this CV term."
is_a: MS:1001110 ! SEQUEST:modeCV */

#define MS_PEPTIDE_SHARING_DETAILS_ID "MS:1001127"
#define MS_PEPTIDE_SHARING_DETAILS_NAME "peptide sharing details"
/* def: "Accessions Containing Sequence - Accessions for each protein containing this peptide." [PSI:PI]
is_a: MS:1001105 ! peptide result details */

#define MS_SEQUEST_SELECTCV_ID "MS:1001128"
#define MS_SEQUEST_SELECTCV_NAME "SEQUEST:selectCV"
/* def: "SEQUEST Select Input Parameters." [PSI:PI]
is_a: MS:1002096 ! SEQUEST input parameter */

#define MS_QUANTIFICATION_INFORMATION_ID "MS:1001129"
#define MS_QUANTIFICATION_INFORMATION_NAME "quantification information"
/* def: "Quantification information." [PSI:PI]
relationship: part_of MS:1001000 ! spectrum interpretation */

// #define MS_PEPTIDE_RAW_AREA_ID "MS:1001130"
// #define MS_PEPTIDE_RAW_AREA_NAME "peptide raw area"
/* def: "OBSOLETE Peptide raw area." [PSI:PI]
comment: This term was made obsolete because it is replaced by 'MS1 feature area' (MS:1001844).
xref: value-type:xsd\:double "The allowed value-type for this CV term."
is_a: MS:1001805 ! quantification datatype
is_obsolete: true */

#define MS_ERROR_ON_PEPTIDE_AREA_ID "MS:1001131"
#define MS_ERROR_ON_PEPTIDE_AREA_NAME "error on peptide area"
/* def: "Error on peptide area." [PSI:PI]
xref: value-type:xsd\:double "The allowed value-type for this CV term."
is_a: MS:1001805 ! quantification datatype */

#define MS_PEPTIDE_RATIO_ID "MS:1001132"
#define MS_PEPTIDE_RATIO_NAME "peptide ratio"
/* def: "Peptide ratio." [PSI:PI]
xref: value-type:xsd\:double "The allowed value-type for this CV term."
is_a: MS:1001805 ! quantification datatype */

#define MS_ERROR_ON_PEPTIDE_RATIO_ID "MS:1001133"
#define MS_ERROR_ON_PEPTIDE_RATIO_NAME "error on peptide ratio"
/* def: "Error on peptide ratio." [PSI:PI]
xref: value-type:xsd\:double "The allowed value-type for this CV term."
is_a: MS:1001805 ! quantification datatype */

#define MS_PROTEIN_RATIO_ID "MS:1001134"
#define MS_PROTEIN_RATIO_NAME "protein ratio"
/* def: "Protein ratio." [PSI:PI]
xref: value-type:xsd\:double "The allowed value-type for this CV term."
is_a: MS:1001805 ! quantification datatype */

#define MS_ERROR_ON_PROTEIN_RATIO_ID "MS:1001135"
#define MS_ERROR_ON_PROTEIN_RATIO_NAME "error on protein ratio"
/* def: "Error on protein ratio." [PSI:PI]
xref: value-type:xsd\:double "The allowed value-type for this CV term."
is_a: MS:1001805 ! quantification datatype */

// #define MS_P_VALUE__PROTEIN_DIFF_FROM_1_RANDOMLY__ID "MS:1001136"
// #define MS_P_VALUE__PROTEIN_DIFF_FROM_1_RANDOMLY__NAME "p-value (protein diff from 1 randomly)"
/* def: "OBSOLETE P-value (protein diff from 1 randomly)." [PSI:PI]
comment: This term was made obsolete because it is replaced by 't-test p-value' (MS:1001855).
xref: value-type:xsd\:double "The allowed value-type for this CV term."
is_a: MS:1001805 ! quantification datatype
is_obsolete: true */

#define MS_ABSOLUTE_QUANTITY_ID "MS:1001137"
#define MS_ABSOLUTE_QUANTITY_NAME "absolute quantity"
/* def: "Absolute quantity in terms of real concentration or molecule copy number in sample." [PSI:PI]
xref: value-type:xsd\:double "The allowed value-type for this CV term."
is_a: MS:1001805 ! quantification datatype */

#define MS_ERROR_ON_ABSOLUTE_QUANTITY_ID "MS:1001138"
#define MS_ERROR_ON_ABSOLUTE_QUANTITY_NAME "error on absolute quantity"
/* def: "Error on absolute quantity." [PSI:PI]
xref: value-type:xsd\:double "The allowed value-type for this CV term."
is_a: MS:1001805 ! quantification datatype */

#define MS_QUANTITATION_SOFTWARE_NAME_ID "MS:1001139"
#define MS_QUANTITATION_SOFTWARE_NAME_NAME "quantitation software name"
/* def: "Quantitation software name." [PSI:PI]
is_a: MS:1000531 ! software
is_a: MS:1001129 ! quantification information */

// #define MS_QUANTITATION_SOFTWARE_VERSION_ID "MS:1001140"
// #define MS_QUANTITATION_SOFTWARE_VERSION_NAME "quantitation software version"
/* def: "OBSOLETE Quantitation software version." [PSI:PI]
comment: This term was made obsolete because part of mzQuantML schema.
is_a: MS:1001129 ! quantification information
is_obsolete: true */

#define MS_INTENSITY_OF_PRECURSOR_ION_ID "MS:1001141"
#define MS_INTENSITY_OF_PRECURSOR_ION_NAME "intensity of precursor ion"
/* def: "The intensity of the precursor ion." [PSI:PI]
xref: value-type:xsd\:double "The allowed value-type for this CV term."
is_a: MS:1001805 ! quantification datatype */

#define MS_DATABASE_IPI_HUMAN_ID "MS:1001142"
#define MS_DATABASE_IPI_HUMAN_NAME "database IPI_human"
/* def: "International Protein Index database for Homo sapiens sequences." [PSI:PI]
is_a: MS:1001013 ! database name */

#define MS_SEARCH_ENGINE_SPECIFIC_SCORE_FOR_PSMS_ID "MS:1001143"
#define MS_SEARCH_ENGINE_SPECIFIC_SCORE_FOR_PSMS_NAME "search engine specific score for PSMs"
/* def: "Search engine specific peptide spectrum match scores." [PSI:PI]
is_a: MS:1001105 ! peptide result details */

#define MS_SEQUEST_SELECT___DEFAULT_ID "MS:1001144"
#define MS_SEQUEST_SELECT___DEFAULT_NAME "SEQUEST:Select_* default"
/* is_a: MS:1001128 ! SEQUEST:selectCV */

#define MS_SEQUEST_SELECTADVANCEDCV_ID "MS:1001145"
#define MS_SEQUEST_SELECTADVANCEDCV_NAME "SEQUEST:SelectAdvancedCV"
/* def: "SEQUEST Select Advanced Input Parameters." [PSI:PI]
is_a: MS:1001128 ! SEQUEST:selectCV */

#define MS_PARAM__A_ION_NH3_ID "MS:1001146"
#define MS_PARAM__A_ION_NH3_NAME "param: a ion-NH3"
/* def: "Parameter information, type of product: a ion with lost ammonium." [PSI:PI]
is_a: MS:1001066 ! ions series considered in search */

#define MS_PROTEIN_AMBIGUITY_GROUP_RESULT_DETAILS_ID "MS:1001147"
#define MS_PROTEIN_AMBIGUITY_GROUP_RESULT_DETAILS_NAME "protein ambiguity group result details"
/* is_a: MS:1001085 ! protein result details */

#define MS_PARAM__A_ION_H2O_ID "MS:1001148"
#define MS_PARAM__A_ION_H2O_NAME "param: a ion-H2O"
/* def: "Ion a - H2O if a significant and fragment includes STED." [PSI:PI]
is_a: MS:1001066 ! ions series considered in search */

#define MS_PARAM__B_ION_NH3_ID "MS:1001149"
#define MS_PARAM__B_ION_NH3_NAME "param: b ion-NH3"
/* def: "Parameter information, type of product: b ion with lost ammonium." [PSI:PI]
is_a: MS:1001066 ! ions series considered in search */

#define MS_PARAM__B_ION_H2O_ID "MS:1001150"
#define MS_PARAM__B_ION_H2O_NAME "param: b ion-H2O"
/* def: "Ion b - H2O if b significant and fragment includes STED." [PSI:PI]
is_a: MS:1001066 ! ions series considered in search */

#define MS_PARAM__Y_ION_NH3_ID "MS:1001151"
#define MS_PARAM__Y_ION_NH3_NAME "param: y ion-NH3"
/* def: "Parameter information, type of product: y ion with lost ammonium." [PSI:PI]
is_a: MS:1001066 ! ions series considered in search */

#define MS_PARAM__Y_ION_H2O_ID "MS:1001152"
#define MS_PARAM__Y_ION_H2O_NAME "param: y ion-H2O"
/* def: "Ion y - H2O if y significant and fragment includes STED." [PSI:PI]
is_a: MS:1001066 ! ions series considered in search */

#define MS_SEARCH_ENGINE_SPECIFIC_SCORE_ID "MS:1001153"
#define MS_SEARCH_ENGINE_SPECIFIC_SCORE_NAME "search engine specific score"
/* def: "Search engine specific scores." [PSI:PI]
is_a: MS:1001405 ! spectrum identification result details */

#define MS_SEQUEST_PROBABILITY_ID "MS:1001154"
#define MS_SEQUEST_PROBABILITY_NAME "SEQUEST:probability"
/* def: "The SEQUEST result 'Probability'." [PSI:PI]
xref: value-type:xsd\:double "The allowed value-type for this CV term."
is_a: MS:1001143 ! search engine specific score for PSMs
is_a: MS:1001153 ! search engine specific score */

#define MS_SEQUEST_XCORR_ID "MS:1001155"
#define MS_SEQUEST_XCORR_NAME "SEQUEST:xcorr"
/* def: "The SEQUEST result 'XCorr'." [PSI:PI]
xref: value-type:xsd\:double "The allowed value-type for this CV term."
is_a: MS:1001143 ! search engine specific score for PSMs
is_a: MS:1001153 ! search engine specific score
has_order: MS:1002108 ! higher score better */

#define MS_SEQUEST_DELTACN_ID "MS:1001156"
#define MS_SEQUEST_DELTACN_NAME "SEQUEST:deltacn"
/* def: "The SEQUEST result 'DeltaCn'." [PSI:PI]
xref: value-type:xsd\:double "The allowed value-type for this CV term."
is_a: MS:1001143 ! search engine specific score for PSMs
is_a: MS:1001153 ! search engine specific score */

#define MS_SEQUEST_SP_ID "MS:1001157"
#define MS_SEQUEST_SP_NAME "SEQUEST:sp"
/* def: "The SEQUEST result 'Sp' (protein)." [PSI:PI]
xref: value-type:xsd\:double "The allowed value-type for this CV term."
is_a: MS:1001116 ! single protein result details
is_a: MS:1001153 ! search engine specific score */

#define MS_SEQUEST_UNIQ_ID "MS:1001158"
#define MS_SEQUEST_UNIQ_NAME "SEQUEST:Uniq"
/* xref: value-type:xsd\:double "The allowed value-type for this CV term."
is_a: MS:1001116 ! single protein result details
is_a: MS:1001153 ! search engine specific score */

#define MS_SEQUEST_EXPECTATION_VALUE_ID "MS:1001159"
#define MS_SEQUEST_EXPECTATION_VALUE_NAME "SEQUEST:expectation value"
/* def: "The SEQUEST result 'Expectation value'." [PSI:PI]
xref: value-type:xsd\:double "The allowed value-type for this CV term."
is_a: MS:1001153 ! search engine specific score */

#define MS_SEQUEST_SF_ID "MS:1001160"
#define MS_SEQUEST_SF_NAME "SEQUEST:sf"
/* def: "The SEQUEST result 'Sf'." [PSI:PI]
xref: value-type:xsd\:double "The allowed value-type for this CV term."
is_a: MS:1001143 ! search engine specific score for PSMs
is_a: MS:1001153 ! search engine specific score */

#define MS_SEQUEST_MATCHED_IONS_ID "MS:1001161"
#define MS_SEQUEST_MATCHED_IONS_NAME "SEQUEST:matched ions"
/* def: "The SEQUEST result 'Matched Ions'." [PSI:PI]
xref: value-type:xsd\:integer "The allowed value-type for this CV term."
is_a: MS:1001143 ! search engine specific score for PSMs
is_a: MS:1001153 ! search engine specific score */

#define MS_SEQUEST_TOTAL_IONS_ID "MS:1001162"
#define MS_SEQUEST_TOTAL_IONS_NAME "SEQUEST:total ions"
/* def: "The SEQUEST result 'Total Ions'." [PSI:PI]
xref: value-type:xsd\:integer "The allowed value-type for this CV term."
is_a: MS:1001143 ! search engine specific score for PSMs
is_a: MS:1001153 ! search engine specific score */

#define MS_SEQUEST_CONSENSUS_SCORE_ID "MS:1001163"
#define MS_SEQUEST_CONSENSUS_SCORE_NAME "SEQUEST:consensus score"
/* def: "The SEQUEST result 'Consensus Score'." [PSI:PI]
xref: value-type:xsd\:double "The allowed value-type for this CV term."
is_a: MS:1001153 ! search engine specific score */

#define MS_PARAGON_UNUSED_PROTSCORE_ID "MS:1001164"
#define MS_PARAGON_UNUSED_PROTSCORE_NAME "Paragon:unused protscore"
/* def: "The Paragon result 'Unused ProtScore'." [PSI:PI]
xref: value-type:xsd\:double "The allowed value-type for this CV term."
is_a: MS:1001147 ! protein ambiguity group result details
is_a: MS:1001153 ! search engine specific score */

#define MS_PARAGON_TOTAL_PROTSCORE_ID "MS:1001165"
#define MS_PARAGON_TOTAL_PROTSCORE_NAME "Paragon:total protscore"
/* def: "The Paragon result 'Total ProtScore'." [PSI:PI]
xref: value-type:xsd\:double "The allowed value-type for this CV term."
is_a: MS:1001147 ! protein ambiguity group result details
is_a: MS:1001153 ! search engine specific score */

#define MS_PARAGON_SCORE_ID "MS:1001166"
#define MS_PARAGON_SCORE_NAME "Paragon:score"
/* def: "The Paragon result 'Score'." [PSI:PI]
xref: value-type:xsd\:double "The allowed value-type for this CV term."
is_a: MS:1001153 ! search engine specific score */

#define MS_PARAGON_CONFIDENCE_ID "MS:1001167"
#define MS_PARAGON_CONFIDENCE_NAME "Paragon:confidence"
/* def: "The Paragon result 'Confidence'." [PSI:PI]
xref: value-type:xsd\:double "The allowed value-type for this CV term."
is_a: MS:1001153 ! search engine specific score */

#define MS_PARAGON_EXPRESSION_ERROR_FACTOR_ID "MS:1001168"
#define MS_PARAGON_EXPRESSION_ERROR_FACTOR_NAME "Paragon:expression error factor"
/* def: "The Paragon result 'Expression Error Factor'." [PSI:PI]
xref: value-type:xsd\:double "The allowed value-type for this CV term."
is_a: MS:1001153 ! search engine specific score */

#define MS_PARAGON_EXPRESSION_CHANGE_P_VALUE_ID "MS:1001169"
#define MS_PARAGON_EXPRESSION_CHANGE_P_VALUE_NAME "Paragon:expression change p-value"
/* def: "The Paragon result 'Expression change P-value'." [PSI:PI]
xref: value-type:xsd\:double "The allowed value-type for this CV term."
is_a: MS:1001153 ! search engine specific score
is_a: MS:1001871 ! protein-level p-value */

#define MS_PARAGON_CONTRIB_ID "MS:1001170"
#define MS_PARAGON_CONTRIB_NAME "Paragon:contrib"
/* def: "The Paragon result 'Contrib'." [PSI:PI]
xref: value-type:xsd\:double "The allowed value-type for this CV term."
is_a: MS:1001153 ! search engine specific score */

#define MS_MASCOT_SCORE_ID "MS:1001171"
#define MS_MASCOT_SCORE_NAME "Mascot:score"
/* def: "The Mascot result 'Score'." [PSI:PI]
xref: value-type:xsd\:double "The allowed value-type for this CV term."
is_a: MS:1001116 ! single protein result details
is_a: MS:1001143 ! search engine specific score for PSMs
is_a: MS:1001153 ! search engine specific score */

#define MS_MASCOT_EXPECTATION_VALUE_ID "MS:1001172"
#define MS_MASCOT_EXPECTATION_VALUE_NAME "Mascot:expectation value"
/* def: "The Mascot result 'expectation value'." [PSI:PI]
xref: value-type:xsd\:double "The allowed value-type for this CV term."
is_a: MS:1001116 ! single protein result details
is_a: MS:1001143 ! search engine specific score for PSMs
is_a: MS:1001153 ! search engine specific score */

#define MS_MASCOT_MATCHED_IONS_ID "MS:1001173"
#define MS_MASCOT_MATCHED_IONS_NAME "Mascot:matched ions"
/* def: "The Mascot result 'Matched ions'." [PSI:PI]
xref: value-type:xsd\:nonNegativeInteger "The allowed value-type for this CV term."
is_a: MS:1001143 ! search engine specific score for PSMs
is_a: MS:1001153 ! search engine specific score */

#define MS_MASCOT_TOTAL_IONS_ID "MS:1001174"
#define MS_MASCOT_TOTAL_IONS_NAME "Mascot:total ions"
/* def: "The Mascot result 'Total ions'." [PSI:PI]
xref: value-type:xsd\:nonNegativeInteger "The allowed value-type for this CV term."
is_a: MS:1001143 ! search engine specific score for PSMs
is_a: MS:1001153 ! search engine specific score */

#define MS_PEPTIDE_SHARED_IN_MULTIPLE_PROTEINS_ID "MS:1001175"
#define MS_PEPTIDE_SHARED_IN_MULTIPLE_PROTEINS_NAME "peptide shared in multiple proteins"
/* is_a: MS:1001127 ! peptide sharing details */

#define MS_NUMBER_OF_MOLECULAR_HYPOTHESIS_CONSIDERED_ID "MS:1001177"
#define MS_NUMBER_OF_MOLECULAR_HYPOTHESIS_CONSIDERED_NAME "number of molecular hypothesis considered"
/* def: "Number of Molecular Hypothesis Considered - This is the number of molecules (e.g. peptides for proteomics) considered for a particular search." [PSI:PI]
xref: value-type:xsd\:nonNegativeInteger "The allowed value-type for this CV term."
is_a: MS:1001184 ! search statistics */

#define MS_DATABASE_EST_ID "MS:1001178"
#define MS_DATABASE_EST_NAME "database EST"
/* def: "Expressed sequence tag nucleotide sequence database." [PSI:PI]
is_a: MS:1001079 ! database type nucleotide */

#define MS_CLEAVAGE_AGENT_REGULAR_EXPRESSION_ID "MS:1001180"
#define MS_CLEAVAGE_AGENT_REGULAR_EXPRESSION_NAME "Cleavage agent regular expression"
/* def: "Regular expressions for cleavage enzymes." [PSI:PI]
relationship: part_of MS:1001044 ! cleavage agent details */

#define MS_SEARCH_STATISTICS_ID "MS:1001184"
#define MS_SEARCH_STATISTICS_NAME "search statistics"
/* def: "The details of the actual run of the search." [PSI:PI]
is_a: MS:1001405 ! spectrum identification result details */

#define MS_MODIFICATION_SPECIFICITY_PEPTIDE_N_TERM_ID "MS:1001189"
#define MS_MODIFICATION_SPECIFICITY_PEPTIDE_N_TERM_NAME "modification specificity peptide N-term"
/* def: "As parameter for search engine: apply the modification only at the N-terminus of a peptide." [PSI:PI]
is_a: MS:1001056 ! modification specificity rule */

#define MS_MODIFICATION_SPECIFICITY_PEPTIDE_C_TERM_ID "MS:1001190"
#define MS_MODIFICATION_SPECIFICITY_PEPTIDE_C_TERM_NAME "modification specificity peptide C-term"
/* def: "As parameter for search engine: apply the modification only at the C-terminus of a peptide." [PSI:PI]
is_a: MS:1001056 ! modification specificity rule */

// #define MS_P_VALUE_ID "MS:1001191"
// #define MS_P_VALUE_NAME "p-value"
/* def: "OBSOLETE Quality estimation by p-value." [PSI:PI]
comment: This term was made obsolete because now is split into peptide and protein terms.
xref: value-type:xsd\:double "The allowed value-type for this CV term."
is_a: MS:1001092 ! peptide identification confidence metric
is_a: MS:1001198 ! protein identification confidence metric
is_obsolete: true */

#define MS_EXPECT_VALUE_ID "MS:1001192"
#define MS_EXPECT_VALUE_NAME "Expect value"
/* def: "Result of quality estimation: Expect value." [PSI:PI]
xref: value-type:xsd\:double "The allowed value-type for this CV term."
is_a: MS:1001092 ! peptide identification confidence metric
is_a: MS:1001198 ! protein identification confidence metric */

#define MS_CONFIDENCE_SCORE_ID "MS:1001193"
#define MS_CONFIDENCE_SCORE_NAME "confidence score"
/* def: "Result of quality estimation: confidence score." [PSI:PI]
xref: value-type:xsd\:double "The allowed value-type for this CV term."
is_a: MS:1001092 ! peptide identification confidence metric
is_a: MS:1001198 ! protein identification confidence metric */

#define MS_QUALITY_ESTIMATION_WITH_DECOY_DATABASE_ID "MS:1001194"
#define MS_QUALITY_ESTIMATION_WITH_DECOY_DATABASE_NAME "quality estimation with decoy database"
/* def: "Quality estimation by decoy database." [PSI:PI]
xref: value-type:xsd\:boolean "The allowed value-type for this CV term."
is_a: MS:1001060 ! quality estimation method details */

#define MS_DECOY_DB_TYPE_REVERSE_ID "MS:1001195"
#define MS_DECOY_DB_TYPE_REVERSE_NAME "decoy DB type reverse"
/* def: "Decoy type: Amino acids of protein sequences are used in reverse order." [PSI:PI]
is_a: MS:1001450 ! decoy DB details */

#define MS_DECOY_DB_TYPE_RANDOMIZED_ID "MS:1001196"
#define MS_DECOY_DB_TYPE_RANDOMIZED_NAME "decoy DB type randomized"
/* def: "Decoy type: Amino acids of protein sequences are randomized (keeping the original protein mass)." [PSI:PI]
is_a: MS:1001450 ! decoy DB details */

#define MS_DB_COMPOSITION_TARGET_DECOY_ID "MS:1001197"
#define MS_DB_COMPOSITION_TARGET_DECOY_NAME "DB composition target+decoy"
/* def: "Decoy database composition: database contains original (target) and decoy entries." [PSI:PI]
is_a: MS:1001450 ! decoy DB details */

#define MS_PROTEIN_IDENTIFICATION_CONFIDENCE_METRIC_ID "MS:1001198"
#define MS_PROTEIN_IDENTIFICATION_CONFIDENCE_METRIC_NAME "protein identification confidence metric"
/* def: "Identification confidence metric for a protein." [PSI:PI]
is_a: MS:1001116 ! single protein result details */

#define MS_MASCOT_DAT_FILE_ID "MS:1001199"
#define MS_MASCOT_DAT_FILE_NAME "Mascot DAT file"
/* def: "Source file for this mzIdentML was a Mascot DAT file." [PSI:PI]
is_a: MS:1001040 ! intermediate analysis format */

#define MS_SEQUEST_OUT_FILE_ID "MS:1001200"
#define MS_SEQUEST_OUT_FILE_NAME "SEQUEST out file"
/* def: "Source file for this mzIdentML was ONE SEQUEST out file." [PSI:PI]
is_a: MS:1001040 ! intermediate analysis format */

#define MS_DB_MW_FILTER_MAXIMUM_ID "MS:1001201"
#define MS_DB_MW_FILTER_MAXIMUM_NAME "DB MW filter maximum"
/* def: "Maximum value of molecular weight filter." [PSI:PI]
xref: value-type:xsd\:double "The allowed value-type for this CV term."
is_a: MS:1001512 ! Sequence database filters
relationship: has_units UO:0000221 ! dalton
relationship: has_units UO:0000222 ! kilodalton */

#define MS_DB_MW_FILTER_MINIMUM_ID "MS:1001202"
#define MS_DB_MW_FILTER_MINIMUM_NAME "DB MW filter minimum"
/* def: "Minimum value of molecular weight filter." [PSI:PI]
xref: value-type:xsd\:double "The allowed value-type for this CV term."
is_a: MS:1001512 ! Sequence database filters
relationship: has_units UO:0000221 ! dalton
relationship: has_units UO:0000222 ! kilodalton */

#define MS_DB_PI_FILTER_MAXIMUM_ID "MS:1001203"
#define MS_DB_PI_FILTER_MAXIMUM_NAME "DB PI filter maximum"
/* def: "Maximum value of isoelectric point filter." [PSI:PI]
xref: value-type:xsd\:double "The allowed value-type for this CV term."
is_a: MS:1001512 ! Sequence database filters */

#define MS_DB_PI_FILTER_MINIMUM_ID "MS:1001204"
#define MS_DB_PI_FILTER_MINIMUM_NAME "DB PI filter minimum"
/* def: "Minimum value of isoelectric point filter." [PSI:PI]
xref: value-type:xsd\:double "The allowed value-type for this CV term."
is_a: MS:1001512 ! Sequence database filters */

#define MS_MASCOT_ID "MS:1001207"
#define MS_MASCOT_NAME "Mascot"
/* def: "The name of the Mascot search engine." [PSI:PI]
is_a: MS:1001456 ! analysis software */

#define MS_SEQUEST_ID "MS:1001208"
#define MS_SEQUEST_NAME "SEQUEST"
/* def: "The name of the SEQUEST search engine." [PSI:PI]
is_a: MS:1001456 ! analysis software */

#define MS_PHENYX_ID "MS:1001209"
#define MS_PHENYX_NAME "Phenyx"
/* def: "The name of the Phenyx search engine." [PSI:PI]
is_a: MS:1001456 ! analysis software */

#define MS_MASS_TYPE_SETTINGS_ID "MS:1001210"
#define MS_MASS_TYPE_SETTINGS_NAME "mass type settings"
/* def: "The type of mass difference value to be considered by the search engine (monoisotopic or average)." [PSI:PI]
is_a: MS:1001249 ! search input details */

#define MS_PARENT_MASS_TYPE_MONO_ID "MS:1001211"
#define MS_PARENT_MASS_TYPE_MONO_NAME "parent mass type mono"
/* def: "Mass type setting for parent mass was monoisotopic." [PSI:PI]
is_a: MS:1001210 ! mass type settings */

#define MS_PARENT_MASS_TYPE_AVERAGE_ID "MS:1001212"
#define MS_PARENT_MASS_TYPE_AVERAGE_NAME "parent mass type average"
/* def: "Mass type setting for parent mass was average isotopic." [PSI:PI]
is_a: MS:1001210 ! mass type settings */

// #define MS_SEARCH_RESULT_DETAILS_ID "MS:1001213"
// #define MS_SEARCH_RESULT_DETAILS_NAME "search result details"
/* def: "OBSOLETE: Scores and global result characteristics." [PSI:PI]
comment: This former purgatory term was made obsolete.
is_obsolete: true */

#define MS_PROTEIN_LEVEL_GLOBAL_FDR_ID "MS:1001214"
#define MS_PROTEIN_LEVEL_GLOBAL_FDR_NAME "protein-level global FDR"
/* def: "Estimation of the global false discovery rate of proteins." [PSI:PI]
xref: value-type:xsd\:double "The allowed value-type for this CV term."
is_a: MS:1001116 ! single protein result details
has_domain: MS:1002305 ! value between 0 and 1 inclusive */

#define MS_SEQUEST_PEPTIDESP_ID "MS:1001215"
#define MS_SEQUEST_PEPTIDESP_NAME "SEQUEST:PeptideSp"
/* def: "The SEQUEST result 'Sp' in out file (peptide)." [PSI:PI]
xref: value-type:xsd\:double "The allowed value-type for this CV term."
is_a: MS:1001143 ! search engine specific score for PSMs
is_a: MS:1001153 ! search engine specific score */

#define MS_SEQUEST_PEPTIDERANKSP_ID "MS:1001217"
#define MS_SEQUEST_PEPTIDERANKSP_NAME "SEQUEST:PeptideRankSp"
/* def: "The SEQUEST result 'Sp' of 'Rank/Sp' in out file (peptide). Also called 'rsp'." [PSI:PI]
xref: value-type:xsd\:integer "The allowed value-type for this CV term."
is_a: MS:1001143 ! search engine specific score for PSMs
is_a: MS:1001153 ! search engine specific score */

#define MS_SEQUEST_PEPTIDENUMBER_ID "MS:1001218"
#define MS_SEQUEST_PEPTIDENUMBER_NAME "SEQUEST:PeptideNumber"
/* def: "The SEQUEST result '#' in out file (peptide)." [PSI:PI]
xref: value-type:xsd\:integer "The allowed value-type for this CV term."
is_a: MS:1001143 ! search engine specific score for PSMs
is_a: MS:1001153 ! search engine specific score */

#define MS_SEQUEST_PEPTIDEIDNUMBER_ID "MS:1001219"
#define MS_SEQUEST_PEPTIDEIDNUMBER_NAME "SEQUEST:PeptideIdnumber"
/* def: "The SEQUEST result 'Id#' in out file (peptide)." [PSI:PI]
xref: value-type:xsd\:integer "The allowed value-type for this CV term."
is_a: MS:1001143 ! search engine specific score for PSMs
is_a: MS:1001153 ! search engine specific score */

#define MS_FRAG__Y_ION_ID "MS:1001220"
#define MS_FRAG__Y_ION_NAME "frag: y ion"
/* def: "Fragmentation information, type of product: y ion." [PSI:PI]
is_a: MS:1002307 ! fragmentation ion type */

#define MS_FRAGMENTATION_INFORMATION_ID "MS:1001221"
#define MS_FRAGMENTATION_INFORMATION_NAME "fragmentation information"
/* def: "Fragmentation information like ion types." [PSI:PI]
is_a: MS:1001105 ! peptide result details */

#define MS_FRAG__B_ION___H2O_ID "MS:1001222"
#define MS_FRAG__B_ION___H2O_NAME "frag: b ion - H2O"
/* def: "Fragmentation information, type of product: b ion without water." [PSI:PI]
is_a: MS:1002307 ! fragmentation ion type */

#define MS_FRAG__Y_ION___H2O_ID "MS:1001223"
#define MS_FRAG__Y_ION___H2O_NAME "frag: y ion - H2O"
/* def: "Fragmentation information, type of product: y ion without water." [PSI:PI]
is_a: MS:1002307 ! fragmentation ion type */

#define MS_FRAG__B_ION_ID "MS:1001224"
#define MS_FRAG__B_ION_NAME "frag: b ion"
/* def: "Fragmentation information, type of product: b ion." [PSI:PI]
is_a: MS:1002307 ! fragmentation ion type */

#define MS_PRODUCT_ION_M_Z_ID "MS:1001225"
#define MS_PRODUCT_ION_M_Z_NAME "product ion m/z"
/* def: "The m/z of the product ion." [PSI:PI]
synonym: "fragment ion m/z" EXACT []
is_a: MS:1001221 ! fragmentation information
relationship: has_units MS:1000040 ! m/z */

#define MS_PRODUCT_ION_INTENSITY_ID "MS:1001226"
#define MS_PRODUCT_ION_INTENSITY_NAME "product ion intensity"
/* def: "The intensity of a single product ion." [PSI:PI]
xref: value-type:xsd\:float "The allowed value-type for this CV term."
synonym: "fragment ion intensity" EXACT []
is_a: MS:1001221 ! fragmentation information
relationship: has_units MS:1000131 ! number of detector counts
relationship: has_units MS:1000132 ! percent of base peak
relationship: has_units MS:1000814 ! counts per second
relationship: has_units MS:1000905 ! percent of base peak times 100 */

#define MS_PRODUCT_ION_M_Z_ERROR_ID "MS:1001227"
#define MS_PRODUCT_ION_M_Z_ERROR_NAME "product ion m/z error"
/* def: "The product ion m/z error." [PSI:PI]
is_a: MS:1001221 ! fragmentation information
relationship: has_units MS:1000040 ! m/z
relationship: has_units UO:0000166 ! parts per notation unit */

#define MS_FRAG__X_ION_ID "MS:1001228"
#define MS_FRAG__X_ION_NAME "frag: x ion"
/* def: "Fragmentation information, type of product: x ion." [PSI:PI]
is_a: MS:1002307 ! fragmentation ion type */

#define MS_FRAG__A_ION_ID "MS:1001229"
#define MS_FRAG__A_ION_NAME "frag: a ion"
/* def: "Fragmentation information, type of product: a ion." [PSI:PI]
is_a: MS:1002307 ! fragmentation ion type */

#define MS_FRAG__Z_ION_ID "MS:1001230"
#define MS_FRAG__Z_ION_NAME "frag: z ion"
/* def: "Fragmentation information, type of product: z ion." [PSI:PI]
is_a: MS:1002307 ! fragmentation ion type */

#define MS_FRAG__C_ION_ID "MS:1001231"
#define MS_FRAG__C_ION_NAME "frag: c ion"
/* def: "Fragmentation information, type of product: c ion." [PSI:PI]
is_a: MS:1002307 ! fragmentation ion type */

#define MS_FRAG__B_ION___NH3_ID "MS:1001232"
#define MS_FRAG__B_ION___NH3_NAME "frag: b ion - NH3"
/* def: "Fragmentation information, type of product: b ion without ammonium ion." [PSI:PI]
is_a: MS:1002307 ! fragmentation ion type */

#define MS_FRAG__Y_ION___NH3_ID "MS:1001233"
#define MS_FRAG__Y_ION___NH3_NAME "frag: y ion - NH3"
/* def: "Fragmentation information, type of product: y ion without ammonium ion." [PSI:PI]
is_a: MS:1002307 ! fragmentation ion type */

#define MS_FRAG__A_ION___H2O_ID "MS:1001234"
#define MS_FRAG__A_ION___H2O_NAME "frag: a ion - H2O"
/* def: "Fragmentation information, type of product: a ion without water." [PSI:PI]
is_a: MS:1002307 ! fragmentation ion type */

#define MS_FRAG__A_ION___NH3_ID "MS:1001235"
#define MS_FRAG__A_ION___NH3_NAME "frag: a ion - NH3"
/* def: "Fragmentation information, type of product: a ion without ammonium." [PSI:PI]
is_a: MS:1002307 ! fragmentation ion type */

#define MS_FRAG__D_ION_ID "MS:1001236"
#define MS_FRAG__D_ION_NAME "frag: d ion"
/* def: "Fragmentation information, type of product: d ion." [PSI:PI]
is_a: MS:1002307 ! fragmentation ion type */

#define MS_FRAG__V_ION_ID "MS:1001237"
#define MS_FRAG__V_ION_NAME "frag: v ion"
/* def: "Fragmentation information, type of product: v ion." [PSI:PI]
is_a: MS:1002307 ! fragmentation ion type */

#define MS_FRAG__W_ION_ID "MS:1001238"
#define MS_FRAG__W_ION_NAME "frag: w ion"
/* def: "Fragmentation information, type of product: w ion." [PSI:PI]
is_a: MS:1002307 ! fragmentation ion type */

#define MS_FRAG__IMMONIUM_ION_ID "MS:1001239"
#define MS_FRAG__IMMONIUM_ION_NAME "frag: immonium ion"
/* def: "Fragmentation information, type of product: immonium ion." [PSI:PI]
is_a: MS:1002307 ! fragmentation ion type */

#define MS_NON_IDENTIFIED_ION_ID "MS:1001240"
#define MS_NON_IDENTIFIED_ION_NAME "non-identified ion"
/* def: "Non-identified ion." [PSI:PI]
is_a: MS:1001221 ! fragmentation information */

#define MS_CO_ELUTING_ION_ID "MS:1001241"
#define MS_CO_ELUTING_ION_NAME "co-eluting ion"
/* def: "Co-eluting ion." [PSI:PI]
is_a: MS:1001221 ! fragmentation information */

#define MS_SEQUEST_OUT_FOLDER_ID "MS:1001242"
#define MS_SEQUEST_OUT_FOLDER_NAME "SEQUEST out folder"
/* def: "Source file for this mzIdentML was a SEQUEST folder with its out files." [PSI:PI]
is_a: MS:1001040 ! intermediate analysis format */

#define MS_SEQUEST_SUMMARY_ID "MS:1001243"
#define MS_SEQUEST_SUMMARY_NAME "SEQUEST summary"
/* def: "Source file for this mzIdentML was a SEQUEST summary page (proteins)." [PSI:PI]
is_a: MS:1001040 ! intermediate analysis format */

#define MS_PERSEPTIVE_PKS_FILE_ID "MS:1001245"
#define MS_PERSEPTIVE_PKS_FILE_NAME "PerSeptive PKS file"
/* def: "PerSeptive peak list file format." [http://www.matrixscience.com/help/data_file_help.html#PKS]
is_a: MS:1000560 ! mass spectrometer file format */

#define MS_SCIEX_API_III_FILE_ID "MS:1001246"
#define MS_SCIEX_API_III_FILE_NAME "Sciex API III file"
/* def: "PE Sciex peak list file format." [http://www.matrixscience.com/help/data_file_help.html#API]
is_a: MS:1000560 ! mass spectrometer file format */

#define MS_BRUKER_XML_FILE_ID "MS:1001247"
#define MS_BRUKER_XML_FILE_NAME "Bruker XML file"
/* def: "Bruker data exchange xml." [PSI:PI]
is_a: MS:1000560 ! mass spectrometer file format */

#define MS_SEARCH_INPUT_DETAILS_ID "MS:1001249"
#define MS_SEARCH_INPUT_DETAILS_NAME "search input details"
/* def: "Details describing the search input." [PSI:PI]
relationship: part_of MS:1001000 ! spectrum interpretation */

#define MS_LOCAL_FDR_ID "MS:1001250"
#define MS_LOCAL_FDR_NAME "local FDR"
/* def: "Result of quality estimation: the local FDR at the current position of a sorted list." [PSI:PI]
xref: value-type:xsd\:double "The allowed value-type for this CV term."
is_a: MS:1001092 ! peptide identification confidence metric
is_a: MS:1001198 ! protein identification confidence metric
relationship: has_units UO:0000166 ! parts per notation unit
relationship: has_units UO:0000187 ! percent */

#define MS_TRYPSIN_ID "MS:1001251"
#define MS_TRYPSIN_NAME "Trypsin"
/* def: "Enzyme trypsin." [PSI:PI]
is_a: MS:1001045 ! cleavage agent name
relationship: has_regexp MS:1001176 ! (?<=[KR])(?!P) */

#define MS_DB_SOURCE_EBI_ID "MS:1001252"
#define MS_DB_SOURCE_EBI_NAME "DB source EBI"
/* def: "Database source EBI." [PSI:PI]
is_a: MS:1001012 ! database source */

#define MS_DB_SOURCE_NCBI_ID "MS:1001253"
#define MS_DB_SOURCE_NCBI_NAME "DB source NCBI"
/* def: "Database source NCBI." [PSI:PI]
is_a: MS:1001012 ! database source */

#define MS_DB_SOURCE_UNIPROT_ID "MS:1001254"
#define MS_DB_SOURCE_UNIPROT_NAME "DB source UniProt"
/* def: "Database source UniProt." [PSI:PI]
is_a: MS:1001012 ! database source */

#define MS_FRAGMENT_MASS_TYPE_AVERAGE_ID "MS:1001255"
#define MS_FRAGMENT_MASS_TYPE_AVERAGE_NAME "fragment mass type average"
/* def: "Mass type setting for fragment mass was average isotopic." [PSI:PI]
is_a: MS:1001210 ! mass type settings */

#define MS_FRAGMENT_MASS_TYPE_MONO_ID "MS:1001256"
#define MS_FRAGMENT_MASS_TYPE_MONO_NAME "fragment mass type mono"
/* def: "Mass type setting for fragment mass was monoisotopic." [PSI:PI]
is_a: MS:1001210 ! mass type settings */

#define MS_PARAM__V_ION_ID "MS:1001257"
#define MS_PARAM__V_ION_NAME "param: v ion"
/* def: "Parameter information, type of product: side chain loss v ion." [PSI:PI]
is_a: MS:1001066 ! ions series considered in search */

#define MS_PARAM__D_ION_ID "MS:1001258"
#define MS_PARAM__D_ION_NAME "param: d ion"
/* def: "Parameter information, type of product: side chain loss d ion." [PSI:PI]
is_a: MS:1001066 ! ions series considered in search */

#define MS_PARAM__IMMONIUM_ION_ID "MS:1001259"
#define MS_PARAM__IMMONIUM_ION_NAME "param: immonium ion"
/* def: "Parameter information, type of product: immonium ion." [PSI:PI]
is_a: MS:1001066 ! ions series considered in search */

#define MS_PARAM__W_ION_ID "MS:1001260"
#define MS_PARAM__W_ION_NAME "param: w ion"
/* def: "Parameter information, type of product: side chain loss w ion." [PSI:PI]
is_a: MS:1001066 ! ions series considered in search */

#define MS_PARAM__X_ION_ID "MS:1001261"
#define MS_PARAM__X_ION_NAME "param: x ion"
/* def: "Parameter information, type of product: x ion with charge on the C-terminal side." [PSI:PI]
is_a: MS:1001066 ! ions series considered in search */

#define MS_PARAM__Y_ION_ID "MS:1001262"
#define MS_PARAM__Y_ION_NAME "param: y ion"
/* def: "Parameter information, type of product: y ion with charge on the C-terminal side." [PSI:PI]
is_a: MS:1001066 ! ions series considered in search */

#define MS_PARAM__Z_ION_ID "MS:1001263"
#define MS_PARAM__Z_ION_NAME "param: z ion"
/* def: "Parameter information, type of product: z ion with charge on the C-terminal side." [PSI:PI]
is_a: MS:1001066 ! ions series considered in search */

#define MS_ROLE_TYPE_ID "MS:1001266"
#define MS_ROLE_TYPE_NAME "role type"
/* def: "Role of a Person or Organization." [PSI:PI]
is_a: MS:1000585 ! contact attribute */

#define MS_SOFTWARE_VENDOR_ID "MS:1001267"
#define MS_SOFTWARE_VENDOR_NAME "software vendor"
/* def: "Software vendor role." [PSI:PI]
is_a: MS:1001266 ! role type */

#define MS_PROGRAMMER_ID "MS:1001268"
#define MS_PROGRAMMER_NAME "programmer"
/* def: "Programmer role." [PSI:PI]
xref: value-type:xsd\:string "The allowed value-type for this CV term."
is_a: MS:1001266 ! role type */

#define MS_INSTRUMENT_VENDOR_ID "MS:1001269"
#define MS_INSTRUMENT_VENDOR_NAME "instrument vendor"
/* def: "Instrument vendor role." [PSI:PI]
xref: value-type:xsd\:string "The allowed value-type for this CV term."
is_a: MS:1001266 ! role type */

#define MS_LAB_PERSONNEL_ID "MS:1001270"
#define MS_LAB_PERSONNEL_NAME "lab personnel"
/* def: "Lab personnel role." [PSI:PI]
xref: value-type:xsd\:string "The allowed value-type for this CV term."
is_a: MS:1001266 ! role type */

#define MS_RESEARCHER_ID "MS:1001271"
#define MS_RESEARCHER_NAME "researcher"
/* def: "Researcher role." [PSI:PI]
is_a: MS:1001266 ! role type */

#define MS_PROTEINSCAPE_SEARCHEVENT_ID "MS:1001275"
#define MS_PROTEINSCAPE_SEARCHEVENT_NAME "ProteinScape SearchEvent"
/* def: "Source data for this mzIdentML was a ProteinScape SearchEvent." [PSI:PI]
is_a: MS:1001040 ! intermediate analysis format */

#define MS_PROTEINSCAPE_GEL_ID "MS:1001276"
#define MS_PROTEINSCAPE_GEL_NAME "ProteinScape Gel"
/* def: "Source data for this mzIdentML was a ProteinScape Gel." [PSI:PI]
is_a: MS:1001040 ! intermediate analysis format */

#define MS_DECOY_DB_ACCESSION_REGEXP_ID "MS:1001283"
#define MS_DECOY_DB_ACCESSION_REGEXP_NAME "decoy DB accession regexp"
/* def: "Specify the regular expression for decoy accession numbers." [PSI:PI]
xref: value-type:xsd\:string "The allowed value-type for this CV term."
is_a: MS:1001450 ! decoy DB details */

// #define MS_DECOY_DB_DERIVED_FROM_ID "MS:1001284"
// #define MS_DECOY_DB_DERIVED_FROM_NAME "decoy DB derived from"
/* def: "OBSOLETE The name of the database, the search database was derived from." [PSI:PI]
comment: This term was made obsolete, because a combination of database name, DB composition , decoy DB type , decoy DB generation algorithm, decoy DB accession regexp and decoy DB details suffices.
is_a: MS:1001450 ! decoy DB details
is_obsolete: true */

#define MS_DATABASE_IPI_MOUSE_ID "MS:1001285"
#define MS_DATABASE_IPI_MOUSE_NAME "database IPI_mouse"
/* def: "International Protein Index database for Mus musculus sequences." [PSI:PI]
is_a: MS:1001013 ! database name */

#define MS_DATABASE_IPI_RAT_ID "MS:1001286"
#define MS_DATABASE_IPI_RAT_NAME "database IPI_rat"
/* def: "International Protein Index database for Rattus norvegicus sequences." [PSI:PI]
is_a: MS:1001013 ! database name */

#define MS_DATABASE_IPI_ZEBRAFISH_ID "MS:1001287"
#define MS_DATABASE_IPI_ZEBRAFISH_NAME "database IPI_zebrafish"
/* def: "International Protein Index database for Danio rerio sequences." [PSI:PI]
is_a: MS:1001013 ! database name */

#define MS_DATABASE_IPI_CHICKEN_ID "MS:1001288"
#define MS_DATABASE_IPI_CHICKEN_NAME "database IPI_chicken"
/* def: "International Protein Index database for Gallus gallus sequences." [PSI:PI]
is_a: MS:1001013 ! database name */

#define MS_DATABASE_IPI_COW_ID "MS:1001289"
#define MS_DATABASE_IPI_COW_NAME "database IPI_cow"
/* def: "International Protein Index database for Bos taurus sequences." [PSI:PI]
is_a: MS:1001013 ! database name */

#define MS_DATABASE_IPI_ARABIDOPSIS_ID "MS:1001290"
#define MS_DATABASE_IPI_ARABIDOPSIS_NAME "database IPI_arabidopsis"
/* def: "International Protein Index database for Arabidopsis thaliana sequences." [PSI:PI]
is_a: MS:1001013 ! database name */

// #define MS_DECOY_DB_FROM_NR_ID "MS:1001291"
// #define MS_DECOY_DB_FROM_NR_NAME "decoy DB from nr"
/* def: "OBSOLETE Decoy database from a non-redundant GenBank sequence database." [PSI:PI]
comment: This term was made obsolete, because a combination of database name, DB composition , decoy DB type , decoy DB generation algorithm, decoy DB accession regexp and decoy DB details suffices.
is_obsolete: true */

// #define MS_DECOY_DB_FROM_IPI_RAT_ID "MS:1001292"
// #define MS_DECOY_DB_FROM_IPI_RAT_NAME "decoy DB from IPI_rat"
/* def: "OBSOLETE Decoy database from a International Protein Index database for Rattus norvegicus." [PSI:PI]
comment: This term was made obsolete, because a combination of database name, DB composition , decoy DB type , decoy DB generation algorithm, decoy DB accession regexp and decoy DB details suffices.
is_obsolete: true */

// #define MS_DECOY_DB_FROM_IPI_MOUSE_ID "MS:1001293"
// #define MS_DECOY_DB_FROM_IPI_MOUSE_NAME "decoy DB from IPI_mouse"
/* def: "OBSOLETE Decoy database from a International Protein Index database for Mus musculus." [PSI:PI]
comment: This term was made obsolete, because a combination of database name, DB composition , decoy DB type , decoy DB generation algorithm, decoy DB accession regexp and decoy DB details suffices.
is_obsolete: true */

// #define MS_DECOY_DB_FROM_IPI_ARABIDOPSIS_ID "MS:1001294"
// #define MS_DECOY_DB_FROM_IPI_ARABIDOPSIS_NAME "decoy DB from IPI_arabidopsis"
/* def: "OBSOLETE Decoy database from a International Protein Index database for Arabidopsis thaliana." [PSI:PI]
comment: This term was made obsolete, because a combination of database name, DB composition , decoy DB type , decoy DB generation algorithm, decoy DB accession regexp and decoy DB details suffices.
is_obsolete: true */

// #define MS_DECOY_DB_FROM_EST_ID "MS:1001295"
// #define MS_DECOY_DB_FROM_EST_NAME "decoy DB from EST"
/* def: "OBSOLETE Decoy database from an expressed sequence tag nucleotide sequence database." [PSI:PI]
comment: This term was made obsolete, because a combination of database name, DB composition , decoy DB type , decoy DB generation algorithm, decoy DB accession regexp and decoy DB details suffices.
is_obsolete: true */

// #define MS_DECOY_DB_FROM_IPI_ZEBRAFISH_ID "MS:1001296"
// #define MS_DECOY_DB_FROM_IPI_ZEBRAFISH_NAME "decoy DB from IPI_zebrafish"
/* def: "OBSOLETE Decoy database from a International Protein Index database for Danio rerio." [PSI:PI]
comment: This term was made obsolete, because a combination of database name, DB composition , decoy DB type , decoy DB generation algorithm, decoy DB accession regexp and decoy DB details suffices.
is_obsolete: true */

// #define MS_DECOY_DB_FROM_UNIPROTKB_SWISS_PROT_ID "MS:1001297"
// #define MS_DECOY_DB_FROM_UNIPROTKB_SWISS_PROT_NAME "decoy DB from UniProtKB_Swiss-Prot"
/* def: "OBSOLETE Decoy database from a Swiss-Prot protein sequence database." [PSI:PI]
comment: This term was made obsolete, because a combination of database name, DB composition , decoy DB type , decoy DB generation algorithm, decoy DB accession regexp and decoy DB details suffices.
is_obsolete: true */

// #define MS_DECOY_DB_FROM_IPI_CHICKEN_ID "MS:1001298"
// #define MS_DECOY_DB_FROM_IPI_CHICKEN_NAME "decoy DB from IPI_chicken"
/* def: "OBSOLETE Decoy database from a International Protein Index database for Gallus gallus." [PSI:PI]
comment: This term was made obsolete, because a combination of database name, DB composition , decoy DB type , decoy DB generation algorithm, decoy DB accession regexp and decoy DB details suffices.
is_obsolete: true */

// #define MS_DECOY_DB_FROM_IPI_COW_ID "MS:1001299"
// #define MS_DECOY_DB_FROM_IPI_COW_NAME "decoy DB from IPI_cow"
/* def: "OBSOLETE Decoy database from a International Protein Index database for Bos taurus." [PSI:PI]
comment: This term was made obsolete, because a combination of database name, DB composition , decoy DB type , decoy DB generation algorithm, decoy DB accession regexp and decoy DB details suffices.
is_obsolete: true */

// #define MS_DECOY_DB_FROM_IPI_HUMAN_ID "MS:1001300"
// #define MS_DECOY_DB_FROM_IPI_HUMAN_NAME "decoy DB from IPI_human"
/* def: "OBSOLETE Decoy database from a International Protein Index database for Homo sapiens." [PSI:PI]
comment: This term was made obsolete, because a combination of database name, DB composition , decoy DB type , decoy DB generation algorithm, decoy DB accession regexp and decoy DB details suffices.
is_obsolete: true */

#define MS_PROTEIN_RANK_ID "MS:1001301"
#define MS_PROTEIN_RANK_NAME "protein rank"
/* def: "The rank of the protein in a list sorted by the search engine." [PSI:PI]
xref: value-type:xsd\:integer "The allowed value-type for this CV term."
is_a: MS:1001147 ! protein ambiguity group result details */

#define MS_SEARCH_ENGINE_SPECIFIC_INPUT_PARAMETER_ID "MS:1001302"
#define MS_SEARCH_ENGINE_SPECIFIC_INPUT_PARAMETER_NAME "search engine specific input parameter"
/* def: "Search engine specific input parameters." [PSI:PI]
is_a: MS:1002093 ! search engine input parameter */

#define MS_ARG_C_ID "MS:1001303"
#define MS_ARG_C_NAME "Arg-C"
/* def: "Endoproteinase Arg-C." [PSI:PI]
synonym: "Clostripain" EXACT []
is_a: MS:1001045 ! cleavage agent name
relationship: has_regexp MS:1001272 ! (?<=R)(?!P) */

#define MS_ASP_N_ID "MS:1001304"
#define MS_ASP_N_NAME "Asp-N"
/* def: "Endoproteinase Asp-N." [PSI:PI]
is_a: MS:1001045 ! cleavage agent name
relationship: has_regexp MS:1001273 ! (?=[BD]) */

#define MS_ASP_N_AMBIC_ID "MS:1001305"
#define MS_ASP_N_AMBIC_NAME "Asp-N_ambic"
/* def: "Enzyme Asp-N, Ammonium Bicarbonate (AmBic)." [PSI:PI]
is_a: MS:1001045 ! cleavage agent name
relationship: has_regexp MS:1001274 ! (?=[DE]) */

#define MS_CHYMOTRYPSIN_ID "MS:1001306"
#define MS_CHYMOTRYPSIN_NAME "Chymotrypsin"
/* def: "Enzyme chymotrypsin." [PSI:PI]
is_a: MS:1001045 ! cleavage agent name
relationship: has_regexp MS:1001332 ! (?<=[FYWL])(?!P) */

#define MS_CNBR_ID "MS:1001307"
#define MS_CNBR_NAME "CNBr"
/* def: "Cyanogen bromide." [PSI:PI]
is_a: MS:1001045 ! cleavage agent name
relationship: has_regexp MS:1001333 ! (?<=M) */

#define MS_FORMIC_ACID_ID "MS:1001308"
#define MS_FORMIC_ACID_NAME "Formic_acid"
/* def: "Formic acid." [PubChem_Compound:284]
is_a: MS:1001045 ! cleavage agent name
relationship: has_regexp MS:1001334 ! ((?<=D))|((?=D)) */

#define MS_LYS_C_ID "MS:1001309"
#define MS_LYS_C_NAME "Lys-C"
/* def: "Endoproteinase Lys-C." [PSI:PI]
is_a: MS:1001045 ! cleavage agent name
relationship: has_regexp MS:1001335 ! (?<=K)(?!P) */

#define MS_LYS_C_P_ID "MS:1001310"
#define MS_LYS_C_P_NAME "Lys-C_P"
/* is_a: MS:1001045 ! cleavage agent name
relationship: has_regexp MS:1001336 ! (?<=K) */

#define MS_PEPSINA_ID "MS:1001311"
#define MS_PEPSINA_NAME "PepsinA"
/* is_a: MS:1001045 ! cleavage agent name
relationship: has_regexp MS:1001337 ! (?<=[FL]) */

#define MS_TRYPCHYMO_ID "MS:1001312"
#define MS_TRYPCHYMO_NAME "TrypChymo"
/* is_a: MS:1001045 ! cleavage agent name
relationship: has_regexp MS:1001338 ! (?<=[FYWLKR])(?!P) */

#define MS_TRYPSIN_P_ID "MS:1001313"
#define MS_TRYPSIN_P_NAME "Trypsin_P"
/* is_a: MS:1001045 ! cleavage agent name
relationship: has_regexp MS:1001339 ! (?<=[KR]) */

#define MS_V8_DE_ID "MS:1001314"
#define MS_V8_DE_NAME "V8-DE"
/* is_a: MS:1001045 ! cleavage agent name
relationship: has_regexp MS:1001340 ! (?<=[BDEZ])(?!P) */

#define MS_V8_E_ID "MS:1001315"
#define MS_V8_E_NAME "V8-E"
/* is_a: MS:1001045 ! cleavage agent name
relationship: has_regexp MS:1001341 ! (?<=[EZ])(?!P) */

#define MS_MASCOT_SIGTHRESHOLD_ID "MS:1001316"
#define MS_MASCOT_SIGTHRESHOLD_NAME "Mascot:SigThreshold"
/* def: "Significance threshold below which the p-value of a peptide match must lie to be considered statistically significant (default 0.05)." [PSI:PI]
xref: value-type:xsd\:double "The allowed value-type for this CV term."
is_a: MS:1002095 ! Mascot input parameter */

#define MS_MASCOT_MAXPROTEINHITS_ID "MS:1001317"
#define MS_MASCOT_MAXPROTEINHITS_NAME "Mascot:MaxProteinHits"
/* def: "The number of protein hits to display in the report. If 'Auto', all protein hits that have a protein score exceeding the average peptide identity threshold are reported. Otherwise an integer at least 1." [PSI:PI]
xref: value-type:xsd\:string "The allowed value-type for this CV term."
is_a: MS:1002095 ! Mascot input parameter */

#define MS_MASCOT_PROTEINSCORINGMETHOD_ID "MS:1001318"
#define MS_MASCOT_PROTEINSCORINGMETHOD_NAME "Mascot:ProteinScoringMethod"
/* def: "Mascot protein scoring method; either 'Standard' or 'MudPIT'." [PSI:PI]
xref: value-type:xsd\:string "The allowed value-type for this CV term."
is_a: MS:1002095 ! Mascot input parameter */

#define MS_MASCOT_MINMSMSTHRESHOLD_ID "MS:1001319"
#define MS_MASCOT_MINMSMSTHRESHOLD_NAME "Mascot:MinMSMSThreshold"
/* def: "Mascot peptide match ion score threshold. If between 0 and 1, then peptide matches whose expect value exceeds the thresholds are suppressed; if at least 1, then peptide matches whose ion score is below the threshold are suppressed." [PSI:PI]
xref: value-type:xsd\:double "The allowed value-type for this CV term."
is_a: MS:1002095 ! Mascot input parameter */

#define MS_MASCOT_SHOWHOMOLOGOUSPROTEINSWITHSAMEPEPTIDES_ID "MS:1001320"
#define MS_MASCOT_SHOWHOMOLOGOUSPROTEINSWITHSAMEPEPTIDES_NAME "Mascot:ShowHomologousProteinsWithSamePeptides"
/* def: "If true, show (sequence or spectrum) same-set proteins. Otherwise they are suppressed." [PSI:PI]
xref: value-type:xsd\:boolean "The allowed value-type for this CV term."
is_a: MS:1002095 ! Mascot input parameter */

#define MS_MASCOT_SHOWHOMOLOGOUSPROTEINSWITHSUBSETOFPEPTIDES_ID "MS:1001321"
#define MS_MASCOT_SHOWHOMOLOGOUSPROTEINSWITHSUBSETOFPEPTIDES_NAME "Mascot:ShowHomologousProteinsWithSubsetOfPeptides"
/* def: "If true, show (sequence or spectrum) sub-set and subsumable proteins. Otherwise they are suppressed." [PSI:PI]
xref: value-type:xsd\:boolean "The allowed value-type for this CV term."
is_a: MS:1002095 ! Mascot input parameter */

#define MS_MASCOT_REQUIREBOLDRED_ID "MS:1001322"
#define MS_MASCOT_REQUIREBOLDRED_NAME "Mascot:RequireBoldRed"
/* def: "Only used in Peptide Summary and Select Summary reports. If true, a peptide match must be 'bold red' to be included in the report; bold red means the peptide is a top ranking match in a query and appears for the first time (in linear order) in the list of protein hits." [PSI:PI]
xref: value-type:xsd\:boolean "The allowed value-type for this CV term."
is_a: MS:1002095 ! Mascot input parameter */

#define MS_MASCOT_USEUNIGENECLUSTERING_ID "MS:1001323"
#define MS_MASCOT_USEUNIGENECLUSTERING_NAME "Mascot:UseUnigeneClustering"
/* def: "If true, then the search results are against a nucleic acid database and Unigene clustering is enabled. Otherwise UniGene clustering is not in use." [PSI:PI]
xref: value-type:xsd\:boolean "The allowed value-type for this CV term."
is_a: MS:1002095 ! Mascot input parameter */

#define MS_MASCOT_INCLUDEERRORTOLERANTMATCHES_ID "MS:1001324"
#define MS_MASCOT_INCLUDEERRORTOLERANTMATCHES_NAME "Mascot:IncludeErrorTolerantMatches"
/* def: "If true, then the search results are error tolerant and peptide matches from the second pass are included in search results. Otherwise no error tolerant peptide matches are included." [http://www.matrixscience.com/help/error_tolerant_help.html]
xref: value-type:xsd\:boolean "The allowed value-type for this CV term."
is_a: MS:1002095 ! Mascot input parameter */

#define MS_MASCOT_SHOWDECOYMATCHES_ID "MS:1001325"
#define MS_MASCOT_SHOWDECOYMATCHES_NAME "Mascot:ShowDecoyMatches"
/* def: "If true, then the search results are against an automatically generated decoy database and the reported peptide matches and protein hits come from the decoy database. Otherwise peptide matches and protein hits come from the original database." [PSI:PI]
xref: value-type:xsd\:boolean "The allowed value-type for this CV term."
is_a: MS:1002095 ! Mascot input parameter */

// #define MS_ADD_OTHERS_ID "MS:1001326"
// #define MS_ADD_OTHERS_NAME "add_others"
/* def: "OBSOLETE." [PSI:PI]
comment: This former purgatory term was made obsolete.
is_obsolete: true */

#define MS_OMSSA_EVALUE_ID "MS:1001328"
#define MS_OMSSA_EVALUE_NAME "OMSSA:evalue"
/* def: "OMSSA E-value." [PSI:PI]
xref: value-type:xsd\:double "The allowed value-type for this CV term."
is_a: MS:1001143 ! search engine specific score for PSMs
is_a: MS:1001153 ! search engine specific score
is_a: MS:1001872 ! distinct peptide-level e-value */

#define MS_OMSSA_PVALUE_ID "MS:1001329"
#define MS_OMSSA_PVALUE_NAME "OMSSA:pvalue"
/* def: "OMSSA p-value." [PSI:PI]
xref: value-type:xsd\:double "The allowed value-type for this CV term."
is_a: MS:1001143 ! search engine specific score for PSMs
is_a: MS:1001153 ! search engine specific score
is_a: MS:1001870 ! distinct peptide-level p-value */

#define MS_X__TANDEM_EXPECT_ID "MS:1001330"
#define MS_X__TANDEM_EXPECT_NAME "X\!Tandem:expect"
/* def: "The X!Tandem expectation value." [PSI:PI]
xref: value-type:xsd\:double "The allowed value-type for this CV term."
is_a: MS:1001143 ! search engine specific score for PSMs
is_a: MS:1001153 ! search engine specific score
is_a: MS:1001872 ! distinct peptide-level e-value */

#define MS_X__TANDEM_HYPERSCORE_ID "MS:1001331"
#define MS_X__TANDEM_HYPERSCORE_NAME "X\!Tandem:hyperscore"
/* def: "The X!Tandem hyperscore." [PSI:PI]
xref: value-type:xsd\:double "The allowed value-type for this CV term."
is_a: MS:1001143 ! search engine specific score for PSMs
is_a: MS:1001153 ! search engine specific score */

#define MS_DATABASE_SEQUENCE_DETAILS_ID "MS:1001342"
#define MS_DATABASE_SEQUENCE_DETAILS_NAME "database sequence details"
/* def: "Details about a single database sequence." [PSI:PI]
is_a: MS:1001011 ! search database details */

#define MS_NA_SEQUENCE_ID "MS:1001343"
#define MS_NA_SEQUENCE_NAME "NA sequence"
/* def: "The sequence is a nucleic acid sequence." [PSI:PI]
is_a: MS:1001342 ! database sequence details */

#define MS_AA_SEQUENCE_ID "MS:1001344"
#define MS_AA_SEQUENCE_NAME "AA sequence"
/* def: "The sequence is a amino acid sequence." [PSI:PI]
is_a: MS:1001342 ! database sequence details */

#define MS_MASS_TABLE_SOURCE_ID "MS:1001345"
#define MS_MASS_TABLE_SOURCE_NAME "mass table source"
/* def: "Children of this term specify the source of the mass table used." [PSI:PI]
is_a: MS:1001354 ! mass table options */

#define MS_AAINDEX_MASS_TABLE_ID "MS:1001346"
#define MS_AAINDEX_MASS_TABLE_NAME "AAIndex mass table"
/* def: "The masses used in the mass table are taken from AAIndex." [PSI:PI]
is_a: MS:1001345 ! mass table source */

#define MS_DATABASE_FILE_FORMATS_ID "MS:1001347"
#define MS_DATABASE_FILE_FORMATS_NAME "database file formats"
/* def: "The children of this term define file formats of the sequence database used." [PSI:PI]
is_a: MS:1001011 ! search database details */

#define MS_FASTA_FORMAT_ID "MS:1001348"
#define MS_FASTA_FORMAT_NAME "FASTA format"
/* def: "The sequence database was stored in the FASTA format." [PSI:PI]
is_a: MS:1001347 ! database file formats */

#define MS_ASN_1_ID "MS:1001349"
#define MS_ASN_1_NAME "ASN.1"
/* def: "The sequence database was stored in the Abstract Syntax Notation 1 format." [PSI:PI]
is_a: MS:1001347 ! database file formats */

#define MS_NCBI___P__ID "MS:1001350"
#define MS_NCBI___P__NAME "NCBI *.p*"
/* def: "The sequence database was stored in the NCBI formatdb (*.p*) format." [PSI:PI]
is_a: MS:1001347 ! database file formats */

#define MS_CLUSTAL_ALN_ID "MS:1001351"
#define MS_CLUSTAL_ALN_NAME "clustal aln"
/* def: "ClustalW ALN (multiple alignment) format." [PSI:PI]
is_a: MS:1001347 ! database file formats */

#define MS_EMBL_EM_ID "MS:1001352"
#define MS_EMBL_EM_NAME "embl em"
/* def: "EMBL entry format." [PSI:PI]
is_a: MS:1001347 ! database file formats */

#define MS_NBRF_PIR_ID "MS:1001353"
#define MS_NBRF_PIR_NAME "NBRF PIR"
/* def: "The NBRF PIR was used as format." [PSI:PI]
is_a: MS:1001347 ! database file formats */

#define MS_MASS_TABLE_OPTIONS_ID "MS:1001354"
#define MS_MASS_TABLE_OPTIONS_NAME "mass table options"
/* def: "Root node for options for the mass table used." [PSI:PI]
relationship: part_of MS:1001000 ! spectrum interpretation */

#define MS_PEPTIDE_DESCRIPTIONS_ID "MS:1001355"
#define MS_PEPTIDE_DESCRIPTIONS_NAME "peptide descriptions"
/* def: "Descriptions of peptides." [PSI:PI]
is_a: MS:1001105 ! peptide result details */

#define MS_SPECTRUM_DESCRIPTIONS_ID "MS:1001356"
#define MS_SPECTRUM_DESCRIPTIONS_NAME "spectrum descriptions"
/* def: "Descriptions of the input spectra." [PSI:PI]
is_a: MS:1001249 ! search input details */

#define MS_SPECTRUM_QUALITY_DESCRIPTIONS_ID "MS:1001357"
#define MS_SPECTRUM_QUALITY_DESCRIPTIONS_NAME "spectrum quality descriptions"
/* def: "Description of the quality of the input spectrum." [PSI:PI]
is_a: MS:1001356 ! spectrum descriptions */

#define MS_MSMSEVAL_QUALITY_ID "MS:1001358"
#define MS_MSMSEVAL_QUALITY_NAME "msmsEval quality"
/* def: "This term reports the quality of the spectrum assigned by msmsEval." [PSI:PI]
xref: value-type:xsd\:double "The allowed value-type for this CV term."
is_a: MS:1001357 ! spectrum quality descriptions */

#define MS_AMBIGUOUS_RESIDUES_ID "MS:1001359"
#define MS_AMBIGUOUS_RESIDUES_NAME "ambiguous residues"
/* def: "Children of this term describe ambiguous residues." [PSI:PI]
relationship: part_of MS:1001000 ! spectrum interpretation */

#define MS_ALTERNATE_SINGLE_LETTER_CODES_ID "MS:1001360"
#define MS_ALTERNATE_SINGLE_LETTER_CODES_NAME "alternate single letter codes"
/* def: "List of standard residue one letter codes which are used to replace a non-standard." [PSI:PI]
xref: value-type:xsd\:string "The allowed value-type for this CV term."
is_a: MS:1001359 ! ambiguous residues */

#define MS_ALTERNATE_MASS_ID "MS:1001361"
#define MS_ALTERNATE_MASS_NAME "alternate mass"
/* def: "List of masses a non-standard letter code is replaced with." [PSI:PI]
xref: value-type:xsd\:double "The allowed value-type for this CV term."
is_a: MS:1001359 ! ambiguous residues
relationship: has_units UO:0000221 ! dalton */

#define MS_NUMBER_OF_UNMATCHED_PEAKS_ID "MS:1001362"
#define MS_NUMBER_OF_UNMATCHED_PEAKS_NAME "number of unmatched peaks"
/* def: "The number of unmatched peaks." [PSI:PI]
xref: value-type:xsd\:nonNegativeInteger "The allowed value-type for this CV term."
is_a: MS:1001116 ! single protein result details
is_a: MS:1001405 ! spectrum identification result details */

#define MS_PEPTIDE_UNIQUE_TO_ONE_PROTEIN_ID "MS:1001363"
#define MS_PEPTIDE_UNIQUE_TO_ONE_PROTEIN_NAME "peptide unique to one protein"
/* is_a: MS:1001127 ! peptide sharing details */

#define MS_DISTINCT_PEPTIDE_LEVEL_GLOBAL_FDR_ID "MS:1001364"
#define MS_DISTINCT_PEPTIDE_LEVEL_GLOBAL_FDR_NAME "distinct peptide-level global FDR"
/* def: "Estimation of the global false discovery rate for distinct peptides once redundant identifications of the same peptide have been removed (id est multiple PSMs have been collapsed to one entry)." [PSI:PI]
xref: value-type:xsd\:double "The allowed value-type for this CV term."
is_a: MS:1001105 ! peptide result details
has_domain: MS:1002305 ! value between 0 and 1 inclusive */

#define MS_FRAG__INTERNAL_YB_ION_ID "MS:1001365"
#define MS_FRAG__INTERNAL_YB_ION_NAME "frag: internal yb ion"
/* def: "Fragmentation information, type of product: internal yb ion." [PSI:PI]
is_a: MS:1002307 ! fragmentation ion type */

#define MS_FRAG__INTERNAL_YA_ION_ID "MS:1001366"
#define MS_FRAG__INTERNAL_YA_ION_NAME "frag: internal ya ion"
/* def: "Fragmentation information, type of product: internal ya ion." [PSI:PI]
is_a: MS:1002307 ! fragmentation ion type */

#define MS_FRAG__Z_1_ION_ID "MS:1001367"
#define MS_FRAG__Z_1_ION_NAME "frag: z+1 ion"
/* def: "Fragmentation information, type of product: z+1 ion." [PSI:PI]
is_a: MS:1002307 ! fragmentation ion type */

#define MS_FRAG__Z_2_ION_ID "MS:1001368"
#define MS_FRAG__Z_2_ION_NAME "frag: z+2 ion"
/* def: "Fragmentation information, type of product: z+2 ion." [PSI:PI]
is_a: MS:1002307 ! fragmentation ion type */

#define MS_TEXT_FILE_ID "MS:1001369"
#define MS_TEXT_FILE_NAME "text file"
/* def: "Simple text file of \"m/z [intensity]\" values for a PMF (or single MS-MS) search." [PSI:PI]
is_a: MS:1000560 ! mass spectrometer file format */

#define MS_MASCOT_HOMOLOGY_THRESHOLD_ID "MS:1001370"
#define MS_MASCOT_HOMOLOGY_THRESHOLD_NAME "Mascot:homology threshold"
/* def: "The Mascot result 'homology threshold'." [PSI:PI]
xref: value-type:xsd\:double "The allowed value-type for this CV term."
is_a: MS:1001143 ! search engine specific score for PSMs
is_a: MS:1001153 ! search engine specific score
is_a: MS:1001405 ! spectrum identification result details */

#define MS_MASCOT_IDENTITY_THRESHOLD_ID "MS:1001371"
#define MS_MASCOT_IDENTITY_THRESHOLD_NAME "Mascot:identity threshold"
/* def: "The Mascot result 'identity threshold'." [PSI:PI]
xref: value-type:xsd\:double "The allowed value-type for this CV term."
is_a: MS:1001116 ! single protein result details
is_a: MS:1001143 ! search engine specific score for PSMs
is_a: MS:1001153 ! search engine specific score
is_a: MS:1001405 ! spectrum identification result details */

#define MS_SEQUEST_SEQUENCES_ID "MS:1001372"
#define MS_SEQUEST_SEQUENCES_NAME "SEQUEST:Sequences"
/* xref: value-type:xsd\:positiveInteger "The allowed value-type for this CV term."
is_a: MS:1001116 ! single protein result details
is_a: MS:1001153 ! search engine specific score */

#define MS_SEQUEST_TIC_ID "MS:1001373"
#define MS_SEQUEST_TIC_NAME "SEQUEST:TIC"
/* def: "SEQUEST total ion current." [PSI:PI]
xref: value-type:xsd\:double "The allowed value-type for this CV term."
is_a: MS:1001116 ! single protein result details
is_a: MS:1001153 ! search engine specific score */

#define MS_SEQUEST_SUM_ID "MS:1001374"
#define MS_SEQUEST_SUM_NAME "SEQUEST:Sum"
/* xref: value-type:xsd\:double "The allowed value-type for this CV term."
is_a: MS:1001116 ! single protein result details
is_a: MS:1001153 ! search engine specific score */

#define MS_PHENYX_INSTRUMENT_TYPE_ID "MS:1001375"
#define MS_PHENYX_INSTRUMENT_TYPE_NAME "Phenyx:Instrument Type"
/* def: "The instrument type parameter value in Phenyx." [PSI:PI]
xref: value-type:xsd\:string "The allowed value-type for this CV term."
is_a: MS:1002097 ! Phenyx input parameter */

#define MS_PHENYX_SCORING_MODEL_ID "MS:1001376"
#define MS_PHENYX_SCORING_MODEL_NAME "Phenyx:Scoring Model"
/* def: "The selected scoring model in Phenyx." [PSI:PI]
xref: value-type:xsd\:string "The allowed value-type for this CV term."
is_a: MS:1002097 ! Phenyx input parameter */

#define MS_PHENYX____DEFAULT_PARENT_CHARGE_ID "MS:1001377"
#define MS_PHENYX____DEFAULT_PARENT_CHARGE_NAME "Phenyx:_* default Parent Charge"
/* def: "The default parent charge value in Phenyx." [PSI:PI]
xref: value-type:xsd\:string "The allowed value-type for this CV term."
is_a: MS:1002097 ! Phenyx input parameter */

#define MS_PHENYX_TRUST_PARENT_CHARGE_ID "MS:1001378"
#define MS_PHENYX_TRUST_PARENT_CHARGE_NAME "Phenyx:Trust Parent Charge"
/* def: "The parameter in Phenyx that specifies if the experimental charge state is to be considered as correct." [PSI:PI]
xref: value-type:xsd\:string "The allowed value-type for this CV term."
is_a: MS:1002097 ! Phenyx input parameter */

#define MS_PHENYX_TURBO_ID "MS:1001379"
#define MS_PHENYX_TURBO_NAME "Phenyx:Turbo"
/* def: "The turbo mode parameter in Phenyx." [PSI:PI]
xref: value-type:xsd\:boolean "The allowed value-type for this CV term."
is_a: MS:1002097 ! Phenyx input parameter */

#define MS_PHENYX_TURBO_ERRORTOL_ID "MS:1001380"
#define MS_PHENYX_TURBO_ERRORTOL_NAME "Phenyx:Turbo:ErrorTol"
/* def: "The maximal allowed fragment m/z error filter considered in the turbo mode of Phenyx." [PSI:PI]
xref: value-type:xsd\:double "The allowed value-type for this CV term."
is_a: MS:1002097 ! Phenyx input parameter */

#define MS_PHENYX_TURBO_COVERAGE_ID "MS:1001381"
#define MS_PHENYX_TURBO_COVERAGE_NAME "Phenyx:Turbo:Coverage"
/* def: "The minimal peptide sequence coverage value, expressed in percent, considered in the turbo mode of Phenyx." [PSI:PI]
xref: value-type:xsd\:double "The allowed value-type for this CV term."
is_a: MS:1002097 ! Phenyx input parameter */

#define MS_PHENYX_TURBO_SERIES_ID "MS:1001382"
#define MS_PHENYX_TURBO_SERIES_NAME "Phenyx:Turbo:Series"
/* def: "The list of ion series considered in the turbo mode of Phenyx." [PSI:PI]
xref: value-type:xsd\:string "The allowed value-type for this CV term."
is_a: MS:1002097 ! Phenyx input parameter */

#define MS_PHENYX_MINPEPLENGTH_ID "MS:1001383"
#define MS_PHENYX_MINPEPLENGTH_NAME "Phenyx:MinPepLength"
/* def: "The minimal number of residues for a peptide to be considered for a valid identification in Phenyx." [PSI:PI]
xref: value-type:xsd\:positiveInteger "The allowed value-type for this CV term."
is_a: MS:1002097 ! Phenyx input parameter */

#define MS_PHENYX_MINPEPZSCORE_ID "MS:1001384"
#define MS_PHENYX_MINPEPZSCORE_NAME "Phenyx:MinPepzscore"
/* def: "The minimal peptide z-score for a peptide to be considered for a valid identification in Phenyx." [PSI:PI]
xref: value-type:xsd\:double "The allowed value-type for this CV term."
is_a: MS:1002097 ! Phenyx input parameter */

#define MS_PHENYX_MAXPEPPVALUE_ID "MS:1001385"
#define MS_PHENYX_MAXPEPPVALUE_NAME "Phenyx:MaxPepPvalue"
/* def: "The maximal peptide p-value for a peptide to be considered for a valid identification in Phenyx." [PSI:PI]
xref: value-type:xsd\:double "The allowed value-type for this CV term."
is_a: MS:1002097 ! Phenyx input parameter */

#define MS_PHENYX_AC_SCORE_ID "MS:1001386"
#define MS_PHENYX_AC_SCORE_NAME "Phenyx:AC Score"
/* def: "The minimal protein score required for a protein database entry to be displayed in the list of identified proteins in Phenyx." [PSI:PI]
xref: value-type:xsd\:double "The allowed value-type for this CV term."
is_a: MS:1002097 ! Phenyx input parameter */

#define MS_PHENYX_CONFLICT_RESOLUTION_ID "MS:1001387"
#define MS_PHENYX_CONFLICT_RESOLUTION_NAME "Phenyx:Conflict Resolution"
/* def: "The parameter in Phenyx that specifies if the conflict resolution algorithm is to be used." [PSI:PI]
xref: value-type:xsd\:boolean "The allowed value-type for this CV term."
is_a: MS:1002097 ! Phenyx input parameter */

#define MS_PHENYX_AC_ID "MS:1001388"
#define MS_PHENYX_AC_NAME "Phenyx:AC"
/* def: "The primary sequence database identifier of a protein in Phenyx." [PSI:PI]
xref: value-type:xsd\:string "The allowed value-type for this CV term."
is_a: MS:1001116 ! single protein result details
is_a: MS:1001153 ! search engine specific score */

#define MS_PHENYX_ID_ID "MS:1001389"
#define MS_PHENYX_ID_NAME "Phenyx:ID"
/* xref: value-type:xsd\:string "The allowed value-type for this CV term."
def: "A secondary sequence database identifier of a protein in Phenyx." [PSI:PI]
is_a: MS:1001116 ! single protein result details
is_a: MS:1001153 ! search engine specific score */

#define MS_PHENYX_SCORE_ID "MS:1001390"
#define MS_PHENYX_SCORE_NAME "Phenyx:Score"
/* def: "The protein score of a protein match in Phenyx." [PSI:PI]
xref: value-type:xsd\:double "The allowed value-type for this CV term."
is_a: MS:1001116 ! single protein result details
is_a: MS:1001153 ! search engine specific score */

#define MS_PHENYX_PEPTIDES1_ID "MS:1001391"
#define MS_PHENYX_PEPTIDES1_NAME "Phenyx:Peptides1"
/* def: "First number of phenyx result \"#Peptides\"." [PSI:PI]
xref: value-type:xsd\:positiveInteger "The allowed value-type for this CV term."
is_a: MS:1001116 ! single protein result details
is_a: MS:1001153 ! search engine specific score */

#define MS_PHENYX_PEPTIDES2_ID "MS:1001392"
#define MS_PHENYX_PEPTIDES2_NAME "Phenyx:Peptides2"
/* def: "Second number of phenyx result \"#Peptides\"." [PSI:PI]
xref: value-type:xsd\:positiveInteger "The allowed value-type for this CV term."
is_a: MS:1001116 ! single protein result details
is_a: MS:1001153 ! search engine specific score */

#define MS_PHENYX_AUTO_ID "MS:1001393"
#define MS_PHENYX_AUTO_NAME "Phenyx:Auto"
/* def: "The value of the automatic peptide acceptance filter in Phenyx." [PSI:PI]
xref: value-type:xsd\:boolean "The allowed value-type for this CV term."
is_a: MS:1001143 ! search engine specific score for PSMs
is_a: MS:1001153 ! search engine specific score */

#define MS_PHENYX_USER_ID "MS:1001394"
#define MS_PHENYX_USER_NAME "Phenyx:User"
/* def: "The value of the user-defined peptide acceptance filter in Phenyx." [PSI:PI]
xref: value-type:xsd\:boolean "The allowed value-type for this CV term."
is_a: MS:1001143 ! search engine specific score for PSMs
is_a: MS:1001153 ! search engine specific score */

#define MS_PHENYX_PEPZSCORE_ID "MS:1001395"
#define MS_PHENYX_PEPZSCORE_NAME "Phenyx:Pepzscore"
/* def: "The z-score value of a peptide sequence match in Phenyx." [PSI:PI]
xref: value-type:xsd\:double "The allowed value-type for this CV term."
is_a: MS:1001143 ! search engine specific score for PSMs
is_a: MS:1001153 ! search engine specific score */

#define MS_PHENYX_PEPPVALUE_ID "MS:1001396"
#define MS_PHENYX_PEPPVALUE_NAME "Phenyx:PepPvalue"
/* def: "The p-value of a peptide sequence match in Phenyx." [PSI:PI]
xref: value-type:xsd\:double "The allowed value-type for this CV term."
is_a: MS:1001143 ! search engine specific score for PSMs
is_a: MS:1001153 ! search engine specific score
is_a: MS:1001870 ! p-value for peptides */

#define MS_PHENYX_NUMBEROFMC_ID "MS:1001397"
#define MS_PHENYX_NUMBEROFMC_NAME "Phenyx:NumberOfMC"
/* def: "The number of missed cleavages of a peptide sequence in Phenyx." [PSI:PI]
xref: value-type:xsd\:positiveInteger "The allowed value-type for this CV term."
is_a: MS:1001143 ! search engine specific score for PSMs
is_a: MS:1001153 ! search engine specific score */

#define MS_PHENYX_MODIF_ID "MS:1001398"
#define MS_PHENYX_MODIF_NAME "Phenyx:Modif"
/* def: "The expression of the nature and position(s) of modified residue(s) on a matched peptide sequence in Phenyx." [PSI:PI]
xref: value-type:xsd\:string "The allowed value-type for this CV term."
is_a: MS:1001143 ! search engine specific score for PSMs
is_a: MS:1001153 ! search engine specific score */

#define MS_OMSSA_CSV_FILE_ID "MS:1001399"
#define MS_OMSSA_CSV_FILE_NAME "OMSSA csv file"
/* def: "Source file for this mzIdentML was an OMSSA csv file." [PSI:PI]
is_a: MS:1001040 ! intermediate analysis format */

#define MS_OMSSA_XML_FILE_ID "MS:1001400"
#define MS_OMSSA_XML_FILE_NAME "OMSSA xml file"
/* def: "Source file for this mzIdentML was an OMSSA xml file." [PSI:PI]
is_a: MS:1001040 ! intermediate analysis format */

#define MS_X__TANDEM_XML_FILE_ID "MS:1001401"
#define MS_X__TANDEM_XML_FILE_NAME "X\!Tandem xml file"
/* def: "Source file for this mzIdentML was an X!Tandem xml file." [PSI:PI]
is_a: MS:1001040 ! intermediate analysis format */

#define MS_SPECTRUM_IDENTIFICATION_RESULT_DETAILS_ID "MS:1001405"
#define MS_SPECTRUM_IDENTIFICATION_RESULT_DETAILS_NAME "spectrum identification result details"
/* def: "This subsection describes terms which can describe details of spectrum identification results." [PSI:PI]
relationship: part_of MS:1001000 ! spectrum interpretation */

#define MS_PARAM__INTERNAL_YB_ION_ID "MS:1001406"
#define MS_PARAM__INTERNAL_YB_ION_NAME "param: internal yb ion"
/* def: "Parameter information, type of product: internal yb ion." [PSI:PI]
is_a: MS:1001066 ! ions series considered in search */

#define MS_PARAM__INTERNAL_YA_ION_ID "MS:1001407"
#define MS_PARAM__INTERNAL_YA_ION_NAME "param: internal ya ion"
/* def: "Parameter information, type of product: internal ya ion." [PSI:PI]
is_a: MS:1001066 ! ions series considered in search */

#define MS_PARAM__Z_1_ION_ID "MS:1001408"
#define MS_PARAM__Z_1_ION_NAME "param: z+1 ion"
/* def: "Parameter information, type of product: z+1 ion." [PSI:PI]
is_a: MS:1001066 ! ions series considered in search */

#define MS_PARAM__Z_2_ION_ID "MS:1001409"
#define MS_PARAM__Z_2_ION_NAME "param: z+2 ion"
/* def: "Parameter information, type of product: z+2 ion." [PSI:PI]
is_a: MS:1001066 ! ions series considered in search */

#define MS_TRANSLATION_START_CODONS_ID "MS:1001410"
#define MS_TRANSLATION_START_CODONS_NAME "translation start codons"
/* def: "The translation start codons used to translate the nucleotides to amino acids." [PSI:PI]
xref: value-type:xsd\:string "The allowed value-type for this CV term."
is_a: MS:1001011 ! search database details */

#define MS_SEARCH_TOLERANCE_SPECIFICATION_ID "MS:1001411"
#define MS_SEARCH_TOLERANCE_SPECIFICATION_NAME "search tolerance specification"
/* is_a: MS:1001249 ! search input details */

#define MS_SEARCH_TOLERANCE_PLUS_VALUE_ID "MS:1001412"
#define MS_SEARCH_TOLERANCE_PLUS_VALUE_NAME "search tolerance plus value"
/* xref: value-type:xsd\:double "The allowed value-type for this CV term."
is_a: MS:1001411 ! search tolerance specification
relationship: has_units UO:0000166 ! parts per notation unit
relationship: has_units UO:0000187 ! percent
relationship: has_units UO:0000221 ! dalton */

#define MS_SEARCH_TOLERANCE_MINUS_VALUE_ID "MS:1001413"
#define MS_SEARCH_TOLERANCE_MINUS_VALUE_NAME "search tolerance minus value"
/* xref: value-type:xsd\:double "The allowed value-type for this CV term."
is_a: MS:1001411 ! search tolerance specification
relationship: has_units UO:0000166 ! parts per notation unit
relationship: has_units UO:0000187 ! percent
relationship: has_units UO:0000221 ! dalton */

// #define MS_MGF_SCANS_ID "MS:1001414"
// #define MS_MGF_SCANS_NAME "MGF scans"
/* def: "OBSOLETE: replaced by MS:1000797 (peak list scans): This term can hold the scans attribute from an MGF input file." [PSI:PI]
xref: value-type:xsd\:string "The allowed value-type for this CV term."
is_a: MS:1001405 ! spectrum identification result details
is_obsolete: true
replaced_by: MS:1000797 */

// #define MS_MGF_RAW_SCANS_ID "MS:1001415"
// #define MS_MGF_RAW_SCANS_NAME "MGF raw scans"
/* def: "OBSOLETE: replaced by MS:1000798 (peak list raw scans): This term can hold the raw scans attribute from an MGF input file." [PSI:PI]
xref: value-type:xsd\:string "The allowed value-type for this CV term."
is_a: MS:1001405 ! spectrum identification result details
is_obsolete: true
replaced_by: MS:1000798 */

// #define MS_SPECTRUM_TITLE_ID "MS:1001416"
// #define MS_SPECTRUM_TITLE_NAME "spectrum title"
/* def: "OBSOLETE: replaced by MS:1000796 (spectrum title): Holds the spectrum title from different input file formats, e.g. MGF TITLE." [PSI:PI]
xref: value-type:xsd\:string "The allowed value-type for this CV term."
is_a: MS:1001405 ! spectrum identification result details
is_obsolete: true
replaced_by: MS:1000796 */

#define MS_SPECTRAST_DOT_ID "MS:1001417"
#define MS_SPECTRAST_DOT_NAME "SpectraST:dot"
/* def: "SpectraST dot product of two spectra, measuring spectral similarity." [PSI:PI]
xref: value-type:xsd\:double "The allowed value-type for this CV term."
is_a: MS:1001143 ! search engine specific score for PSMs
is_a: MS:1001153 ! search engine specific score */

#define MS_SPECTRAST_DOT_BIAS_ID "MS:1001418"
#define MS_SPECTRAST_DOT_BIAS_NAME "SpectraST:dot_bias"
/* def: "SpectraST measure of how much of the dot product is dominated by a few peaks." [PSI:PI]
xref: value-type:xsd\:double "The allowed value-type for this CV term."
is_a: MS:1001143 ! search engine specific score for PSMs
is_a: MS:1001153 ! search engine specific score */

#define MS_SPECTRAST_DISCRIMINANT_SCORE_F_ID "MS:1001419"
#define MS_SPECTRAST_DISCRIMINANT_SCORE_F_NAME "SpectraST:discriminant score F"
/* def: "SpectraST spectrum score." [PSI:PI]
xref: value-type:xsd\:double "The allowed value-type for this CV term."
is_a: MS:1001143 ! search engine specific score for PSMs
is_a: MS:1001153 ! search engine specific score */

#define MS_SPECTRAST_DELTA_ID "MS:1001420"
#define MS_SPECTRAST_DELTA_NAME "SpectraST:delta"
/* def: "SpectraST normalised difference between dot product of top hit and runner-up." [PSI:PI]
xref: value-type:xsd\:double "The allowed value-type for this CV term."
is_a: MS:1001143 ! search engine specific score for PSMs
is_a: MS:1001153 ! search engine specific score */

#define MS_PEPXML_FILE_ID "MS:1001421"
#define MS_PEPXML_FILE_NAME "pepXML file"
/* def: "Source file for this mzIdentML was a pepXML file." [PSI:PI]
is_a: MS:1001040 ! intermediate analysis format */

#define MS_PROTXML_FILE_ID "MS:1001422"
#define MS_PROTXML_FILE_NAME "protXML file"
/* def: "Source file for this mzIdentML was a protXML file." [PSI:PI]
is_a: MS:1001040 ! intermediate analysis format */

#define MS_TRANSLATION_TABLE_DESCRIPTION_ID "MS:1001423"
#define MS_TRANSLATION_TABLE_DESCRIPTION_NAME "translation table description"
/* def: "A URL that describes the translation table used to translate the nucleotides to amino acids." [PSI:PI]
xref: value-type:xsd\:anyURI "The allowed value-type for this CV term."
is_a: MS:1001011 ! search database details */

#define MS_PROTEINEXTRACTOR_METHODNAME_ID "MS:1001424"
#define MS_PROTEINEXTRACTOR_METHODNAME_NAME "ProteinExtractor:Methodname"
/* xref: value-type:xsd\:string "The allowed value-type for this CV term."
is_a: MS:1002098 ! ProteinExtractor input parameter */

#define MS_PROTEINEXTRACTOR_GENERATENONREDUNDANT_ID "MS:1001425"
#define MS_PROTEINEXTRACTOR_GENERATENONREDUNDANT_NAME "ProteinExtractor:GenerateNonRedundant"
/* xref: value-type:xsd\:boolean "The allowed value-type for this CV term."
is_a: MS:1002098 ! ProteinExtractor input parameter */

#define MS_PROTEINEXTRACTOR_INCLUDEIDENTIFIED_ID "MS:1001426"
#define MS_PROTEINEXTRACTOR_INCLUDEIDENTIFIED_NAME "ProteinExtractor:IncludeIdentified"
/* xref: value-type:xsd\:boolean "The allowed value-type for this CV term."
is_a: MS:1002098 ! ProteinExtractor input parameter */

#define MS_PROTEINEXTRACTOR_MAXNUMBEROFPROTEINS_ID "MS:1001427"
#define MS_PROTEINEXTRACTOR_MAXNUMBEROFPROTEINS_NAME "ProteinExtractor:MaxNumberOfProteins"
/* xref: value-type:xsd\:positiveInteger "The allowed value-type for this CV term."
is_a: MS:1002098 ! ProteinExtractor input parameter */

#define MS_PROTEINEXTRACTOR_MAXPROTEINMASS_ID "MS:1001428"
#define MS_PROTEINEXTRACTOR_MAXPROTEINMASS_NAME "ProteinExtractor:MaxProteinMass"
/* xref: value-type:xsd\:double "The allowed value-type for this CV term."
is_a: MS:1002098 ! ProteinExtractor input parameter */

#define MS_PROTEINEXTRACTOR_MINNUMBEROFPEPTIDES_ID "MS:1001429"
#define MS_PROTEINEXTRACTOR_MINNUMBEROFPEPTIDES_NAME "ProteinExtractor:MinNumberOfPeptides"
/* xref: value-type:xsd\:positiveInteger "The allowed value-type for this CV term."
is_a: MS:1002098 ! ProteinExtractor input parameter */

#define MS_PROTEINEXTRACTOR_USEMASCOT_ID "MS:1001430"
#define MS_PROTEINEXTRACTOR_USEMASCOT_NAME "ProteinExtractor:UseMascot"
/* def: "Flag indicating to include Mascot scoring for calculation of the ProteinExtractor metascore." [PSI:PI]
xref: value-type:xsd\:boolean "The allowed value-type for this CV term."
is_a: MS:1002098 ! ProteinExtractor input parameter */

#define MS_PROTEINEXTRACTOR_MASCOTPEPTIDESCORETHRESHOLD_ID "MS:1001431"
#define MS_PROTEINEXTRACTOR_MASCOTPEPTIDESCORETHRESHOLD_NAME "ProteinExtractor:MascotPeptideScoreThreshold"
/* def: "Only peptides with scores higher than that threshold are taken into account in Mascot scoring for calculation of the ProteinExtractor metascore." [DOI:10.4172/jpb.1000056]
xref: value-type:xsd\:double "The allowed value-type for this CV term."
is_a: MS:1002098 ! ProteinExtractor input parameter */

#define MS_PROTEINEXTRACTOR_MASCOTUNIQUESCORE_ID "MS:1001432"
#define MS_PROTEINEXTRACTOR_MASCOTUNIQUESCORE_NAME "ProteinExtractor:MascotUniqueScore"
/* def: "In the final result each protein must have at least one peptide above this Mascot score threshold in ProteinExtractor metascore calculation." [DOI:10.4172/jpb.1000056]
xref: value-type:xsd\:double "The allowed value-type for this CV term."
is_a: MS:1002098 ! ProteinExtractor input parameter */

#define MS_PROTEINEXTRACTOR_MASCOTUSEIDENTITYSCORE_ID "MS:1001433"
#define MS_PROTEINEXTRACTOR_MASCOTUSEIDENTITYSCORE_NAME "ProteinExtractor:MascotUseIdentityScore"
/* xref: value-type:xsd\:boolean "The allowed value-type for this CV term."
is_a: MS:1002098 ! ProteinExtractor input parameter */

#define MS_PROTEINEXTRACTOR_MASCOTWEIGHTING_ID "MS:1001434"
#define MS_PROTEINEXTRACTOR_MASCOTWEIGHTING_NAME "ProteinExtractor:MascotWeighting"
/* def: "Influence of Mascot search engine in the process of merging the search engine specific protein lists into the global protein list of ProteinExtractor." [DOI:10.4172/jpb.1000056]
xref: value-type:xsd\:double "The allowed value-type for this CV term."
is_a: MS:1002098 ! ProteinExtractor input parameter */

#define MS_PROTEINEXTRACTOR_USESEQUEST_ID "MS:1001435"
#define MS_PROTEINEXTRACTOR_USESEQUEST_NAME "ProteinExtractor:UseSequest"
/* def: "Flag indicating to include SEQUEST scoring for calculation of the ProteinExtractor metascore." [PSI:PI]
xref: value-type:xsd\:boolean "The allowed value-type for this CV term."
is_a: MS:1002098 ! ProteinExtractor input parameter */

#define MS_PROTEINEXTRACTOR_SEQUESTPEPTIDESCORETHRESHOLD_ID "MS:1001436"
#define MS_PROTEINEXTRACTOR_SEQUESTPEPTIDESCORETHRESHOLD_NAME "ProteinExtractor:SequestPeptideScoreThreshold"
/* def: "Only peptides with scores higher than that threshold are taken into account in SEQUEST scoring for calculation of the ProteinExtractor metascore." [DOI:10.4172/jpb.1000056]
xref: value-type:xsd\:double "The allowed value-type for this CV term."
is_a: MS:1002098 ! ProteinExtractor input parameter */

#define MS_PROTEINEXTRACTOR_SEQUESTUNIQUESCORE_ID "MS:1001437"
#define MS_PROTEINEXTRACTOR_SEQUESTUNIQUESCORE_NAME "ProteinExtractor:SequestUniqueScore"
/* def: "In the final result each protein must have at least one peptide above this SEQUEST score threshold in ProteinExtractor metascore calculation." [DOI:10.4172/jpb.1000056]
xref: value-type:xsd\:double "The allowed value-type for this CV term."
is_a: MS:1002098 ! ProteinExtractor input parameter */

#define MS_PROTEINEXTRACTOR_SEQUESTWEIGHTING_ID "MS:1001438"
#define MS_PROTEINEXTRACTOR_SEQUESTWEIGHTING_NAME "ProteinExtractor:SequestWeighting"
/* def: "Influence of SEQUEST search engine in the process of merging the search engine specific protein lists into the global protein list of ProteinExtractor." [DOI:10.4172/jpb.1000056]
xref: value-type:xsd\:double "The allowed value-type for this CV term."
is_a: MS:1002098 ! ProteinExtractor input parameter */

#define MS_PROTEINEXTRACTOR_USEPROTEINSOLVER_ID "MS:1001439"
#define MS_PROTEINEXTRACTOR_USEPROTEINSOLVER_NAME "ProteinExtractor:UseProteinSolver"
/* def: "Flag indicating to include ProteinSolver scoring for calculation of the ProteinExtractor metascore." [PSI:PI]
xref: value-type:xsd\:boolean "The allowed value-type for this CV term."
is_a: MS:1002098 ! ProteinExtractor input parameter */

#define MS_PROTEINEXTRACTOR_PROTEINSOLVERPEPTIDESCORETHRESHOLD_ID "MS:1001440"
#define MS_PROTEINEXTRACTOR_PROTEINSOLVERPEPTIDESCORETHRESHOLD_NAME "ProteinExtractor:ProteinSolverPeptideScoreThreshold"
/* def: "Only peptides with scores higher than that threshold are taken into account in ProteinSolver scoring for calculation of the ProteinExtractor metascore." [DOI:10.4172/jpb.1000056]
xref: value-type:xsd\:double "The allowed value-type for this CV term."
is_a: MS:1002098 ! ProteinExtractor input parameter */

#define MS_PROTEINEXTRACTOR_PROTEINSOLVERUNIQUESCORE_ID "MS:1001441"
#define MS_PROTEINEXTRACTOR_PROTEINSOLVERUNIQUESCORE_NAME "ProteinExtractor:ProteinSolverUniqueScore"
/* def: "In the final result each protein must have at least one peptide above this ProteinSolver score threshold in ProteinExtractor metascore calculation." [DOI:10.4172/jpb.1000056]
xref: value-type:xsd\:double "The allowed value-type for this CV term."
is_a: MS:1002098 ! ProteinExtractor input parameter */

#define MS_PROTEINEXTRACTOR_PROTEINSOLVERWEIGHTING_ID "MS:1001442"
#define MS_PROTEINEXTRACTOR_PROTEINSOLVERWEIGHTING_NAME "ProteinExtractor:ProteinSolverWeighting"
/* def: "Influence of ProteinSolver search engine in the process of merging the search engine specific protein lists into the global protein list of ProteinExtractor." [DOI:10.4172/jpb.1000056]
xref: value-type:xsd\:double "The allowed value-type for this CV term."
is_a: MS:1002098 ! ProteinExtractor input parameter */

#define MS_PROTEINEXTRACTOR_USEPHENYX_ID "MS:1001443"
#define MS_PROTEINEXTRACTOR_USEPHENYX_NAME "ProteinExtractor:UsePhenyx"
/* def: "Flag indicating to include Phenyx scoring for calculation of the ProteinExtractor metascore." [PSI:PI]
xref: value-type:xsd\:boolean "The allowed value-type for this CV term."
is_a: MS:1002098 ! ProteinExtractor input parameter */

#define MS_PROTEINEXTRACTOR_PHENYXPEPTIDESCORETHRESHOLD_ID "MS:1001444"
#define MS_PROTEINEXTRACTOR_PHENYXPEPTIDESCORETHRESHOLD_NAME "ProteinExtractor:PhenyxPeptideScoreThreshold"
/* def: "Only peptides with scores higher than that threshold are taken into account in Phenyx scoring for calculation of the ProteinExtractor metascore." [DOI:10.4172/jpb.1000056]
xref: value-type:xsd\:double "The allowed value-type for this CV term."
is_a: MS:1002098 ! ProteinExtractor input parameter */

#define MS_PROTEINEXTRACTOR_PHENYXUNIQUESCORE_ID "MS:1001445"
#define MS_PROTEINEXTRACTOR_PHENYXUNIQUESCORE_NAME "ProteinExtractor:PhenyxUniqueScore"
/* def: "In the final result each protein must have at least one peptide above this Phenyx score threshold in ProteinExtractor metascore calculation." [DOI:10.4172/jpb.1000056]
xref: value-type:xsd\:double "The allowed value-type for this CV term."
is_a: MS:1002098 ! ProteinExtractor input parameter */

#define MS_PROTEINEXTRACTOR_PHENYXWEIGHTING_ID "MS:1001446"
#define MS_PROTEINEXTRACTOR_PHENYXWEIGHTING_NAME "ProteinExtractor:PhenyxWeighting"
/* def: "Influence of Phenyx search engine in the process of merging the search engine specific protein lists into the global protein list of ProteinExtractor." [DOI:10.4172/jpb.1000056]
xref: value-type:xsd\:double "The allowed value-type for this CV term."
is_a: MS:1002098 ! ProteinExtractor input parameter */

#define MS_PROT_FDR_THRESHOLD_ID "MS:1001447"
#define MS_PROT_FDR_THRESHOLD_NAME "prot:FDR threshold"
/* def: "False-discovery rate threshold for proteins." [PSI:PI]
xref: value-type:xsd\:double "The allowed value-type for this CV term."
is_a: MS:1001060 ! quality estimation method details */

#define MS_PEP_FDR_THRESHOLD_ID "MS:1001448"
#define MS_PEP_FDR_THRESHOLD_NAME "pep:FDR threshold"
/* def: "False-discovery rate threshold for peptides." [PSI:PI]
xref: value-type:xsd\:double "The allowed value-type for this CV term."
is_a: MS:1001060 ! quality estimation method details */

#define MS_OMSSA_E_VALUE_THRESHOLD_ID "MS:1001449"
#define MS_OMSSA_E_VALUE_THRESHOLD_NAME "OMSSA e-value threshold"
/* def: "Threshold for OMSSA e-value for quality estimation." [PSI:PI]
xref: value-type:xsd\:double "The allowed value-type for this CV term."
is_a: MS:1002099 ! OMSSA input parameter */

#define MS_DECOY_DB_DETAILS_ID "MS:1001450"
#define MS_DECOY_DB_DETAILS_NAME "decoy DB details"
/* def: "Details of decoy generation and database structure." [PSI:PI]
is_a: MS:1001011 ! search database details */

#define MS_DECOY_DB_GENERATION_ALGORITHM_ID "MS:1001451"
#define MS_DECOY_DB_GENERATION_ALGORITHM_NAME "decoy DB generation algorithm"
/* def: "Name of algorithm used for decoy generation." [PSI:PI]
xref: value-type:xsd\:string "The allowed value-type for this CV term."
is_a: MS:1001450 ! decoy DB details */

#define MS_DECOY_DB_TYPE_SHUFFLE_ID "MS:1001452"
#define MS_DECOY_DB_TYPE_SHUFFLE_NAME "decoy DB type shuffle"
/* def: "Decoy type: Amino acids of protein sequences are used in a random order." [PSI:PI]
is_a: MS:1001450 ! decoy DB details */

#define MS_DB_COMPOSITION_ONLY_DECOY_ID "MS:1001453"
#define MS_DB_COMPOSITION_ONLY_DECOY_NAME "DB composition only decoy"
/* def: "Decoy database composition: database contains only decoy entries." [PSI:PI]
is_a: MS:1001450 ! decoy DB details */

#define MS_QUALITY_ESTIMATION_WITH_IMPLICITE_DECOY_SEQUENCES_ID "MS:1001454"
#define MS_QUALITY_ESTIMATION_WITH_IMPLICITE_DECOY_SEQUENCES_NAME "quality estimation with implicite decoy sequences"
/* def: "Decoy entries are generated during the search, not explicitly stored in a database (like Mascot Decoy)." [PSI:PI]
is_a: MS:1001060 ! quality estimation method details */

#define MS_ACQUISITION_SOFTWARE_ID "MS:1001455"
#define MS_ACQUISITION_SOFTWARE_NAME "acquisition software"
/* def: "Acquisition software." [PSI:MS]
is_a: MS:1000531 ! software */

#define MS_ANALYSIS_SOFTWARE_ID "MS:1001456"
#define MS_ANALYSIS_SOFTWARE_NAME "analysis software"
/* def: "Analysis software." [PSI:MS]
is_a: MS:1000531 ! software */

#define MS_DATA_PROCESSING_SOFTWARE_ID "MS:1001457"
#define MS_DATA_PROCESSING_SOFTWARE_NAME "data processing software"
/* def: "Conversion software." [PSI:MS]
is_a: MS:1000531 ! software */

#define MS_SPECTRUM_GENERATION_INFORMATION_ID "MS:1001458"
#define MS_SPECTRUM_GENERATION_INFORMATION_NAME "spectrum generation information"
/* def: "Vocabularies describing the spectrum generation information." [PSI:PI]
relationship: part_of MS:0000000 ! Proteomics Standards Initiative Mass Spectrometry Vocabularies */

#define MS_FILE_FORMAT_ID "MS:1001459"
#define MS_FILE_FORMAT_NAME "file format"
/* def: "Format of data files." [PSI:MS]
relationship: part_of MS:0000000 ! Proteomics Standards Initiative Mass Spectrometry Vocabularies */

#define MS_UNKNOWN_MODIFICATION_ID "MS:1001460"
#define MS_UNKNOWN_MODIFICATION_NAME "unknown modification"
/* def: "This term should be given if the modification was unknown." [PSI:PI]
xref: value-type:xsd\:string "The allowed value-type for this CV term."
is_a: MS:1001471 ! peptide modification details */

#define MS_GREYLAG_ID "MS:1001461"
#define MS_GREYLAG_NAME "greylag"
/* def: "Greylag identification software." [http://greylag.org/]
is_a: MS:1001456 ! analysis software */

#define MS_PEFF_FORMAT_ID "MS:1001462"
#define MS_PEFF_FORMAT_NAME "PEFF format"
/* def: "The sequence database was stored in the PEFF (PSI enhanced FastA file) format." [PSI:PI]
is_a: MS:1001347 ! database file formats */

#define MS_PHENYX_XML_FORMAT_ID "MS:1001463"
#define MS_PHENYX_XML_FORMAT_NAME "Phenyx XML format"
/* def: "Phenyx open XML file format." [PSI:PI]
is_a: MS:1000560 ! mass spectrometer file format
is_a: MS:1001040 ! intermediate analysis format */

#define MS_DTASELECT_FILE_ID "MS:1001464"
#define MS_DTASELECT_FILE_NAME "DTASelect file"
/* def: "DTASelect file." [PMID:12643522, http://www.scripps.edu/cravatt/protomap/dtaselect_instructions.html]
is_a: MS:1001040 ! intermediate analysis format */

#define MS_MS2_FILE_ID "MS:1001466"
#define MS_MS2_FILE_NAME "MS2 file"
/* def: "MS2 file for MS/MS spectral data." [PMID:15317041, DOI:10.1002/rcm.1603, http://fields.scripps.edu/sequest/SQTFormat.html]
is_a: MS:1001040 ! intermediate analysis format */

#define MS_TAXONOMY__NCBI_TAXID_ID "MS:1001467"
#define MS_TAXONOMY__NCBI_TAXID_NAME "taxonomy: NCBI TaxID"
/* def: "This term is used if a NCBI TaxID is specified, e.g. 9606 for Homo sapiens." [PSI:PI]
xref: value-type:xsd\:positiveInteger "The allowed value-type for this CV term."
is_a: MS:1001089 ! molecule taxonomy */

#define MS_TAXONOMY__COMMON_NAME_ID "MS:1001468"
#define MS_TAXONOMY__COMMON_NAME_NAME "taxonomy: common name"
/* def: "This term is used if a common name is specified, e.g. human. Recommend using MS:1001467 (taxonomy: NCBI TaxID) where possible." [PSI:PI]
xref: value-type:xsd\:string "The allowed value-type for this CV term."
is_a: MS:1001089 ! molecule taxonomy */

#define MS_TAXONOMY__SCIENTIFIC_NAME_ID "MS:1001469"
#define MS_TAXONOMY__SCIENTIFIC_NAME_NAME "taxonomy: scientific name"
/* def: "This term is used if a scientific name is specified, e.g. Homo sapiens. Recommend using MS:1001467 (taxonomy: NCBI TaxID) where possible." [PSI:PI]
xref: value-type:xsd\:string "The allowed value-type for this CV term."
is_a: MS:1001089 ! molecule taxonomy */

#define MS_TAXONOMY__SWISS_PROT_ID_ID "MS:1001470"
#define MS_TAXONOMY__SWISS_PROT_ID_NAME "taxonomy: Swiss-Prot ID"
/* def: "This term is used if a swiss prot taxonomy id is specified, e.g. Human. Recommend using MS:1001467 (taxonomy: NCBI TaxID) where possible." [PSI:PI]
xref: value-type:xsd\:string "The allowed value-type for this CV term."
is_a: MS:1001089 ! molecule taxonomy */

#define MS_PEPTIDE_MODIFICATION_DETAILS_ID "MS:1001471"
#define MS_PEPTIDE_MODIFICATION_DETAILS_NAME "peptide modification details"
/* def: "The children of this term can be used to describe modifications." [PSI:PI]
relationship: part_of MS:1001000 ! spectrum interpretation */

#define MS_SELECTED_ION_MONITORING_CHROMATOGRAM_ID "MS:1001472"
#define MS_SELECTED_ION_MONITORING_CHROMATOGRAM_NAME "selected ion monitoring chromatogram"
/* def: "Chromatogram created by creating an array of the measurements of a selectively monitored ion at each time point." [PSI:MS]
synonym: "SIM chromatogram" EXACT []
is_a: MS:1000524 ! data file content
is_a: MS:1000810 ! mass chromatogram */

#define MS_SELECTED_REACTION_MONITORING_CHROMATOGRAM_ID "MS:1001473"
#define MS_SELECTED_REACTION_MONITORING_CHROMATOGRAM_NAME "selected reaction monitoring chromatogram"
/* def: "Chromatogram created by creating an array of the measurements of a selectively monitored reaction at each time point." [PSI:MS]
synonym: "SRM chromatogram" EXACT []
is_a: MS:1000524 ! data file content
is_a: MS:1000810 ! mass chromatogram */

// #define MS_CONSECUTIVE_REACTION_MONITORING_CHROMATOGRAM_ID "MS:1001474"
// #define MS_CONSECUTIVE_REACTION_MONITORING_CHROMATOGRAM_NAME "consecutive reaction monitoring chromatogram"
/* def: "OBSOLETE Chromatogram created by creating an array of the measurements of a series of monitored reactions at each time point." [PSI:MS]
comment: This term was made obsolete because, by design, it can't be properly represented in mzML 1.1. CRM experiments must be represented in a spectrum-centric way.
synonym: "CRM chromatogram" EXACT []
is_a: MS:1000524 ! data file content
is_a: MS:1000810 ! mass chromatogram
is_obsolete: true */

#define MS_OMSSA_ID "MS:1001475"
#define MS_OMSSA_NAME "OMSSA"
/* def: "Open Mass Spectrometry Search Algorithm was used to analyze the spectra." [PSI:PI]
is_a: MS:1001456 ! analysis software */

#define MS_X__TANDEM_ID "MS:1001476"
#define MS_X__TANDEM_NAME "X\!Tandem"
/* def: "X!Tandem was used to analyze the spectra." [PSI:PI]
is_a: MS:1001456 ! analysis software */

#define MS_SPECTRAST_ID "MS:1001477"
#define MS_SPECTRAST_NAME "SpectraST"
/* def: "SpectraST was used to analyze the spectra." [PSI:PI]
is_a: MS:1001456 ! analysis software */

#define MS_MASCOT_PARSER_ID "MS:1001478"
#define MS_MASCOT_PARSER_NAME "Mascot Parser"
/* def: "Mascot Parser was used to analyze the spectra." [PSI:PI]
is_a: MS:1001456 ! analysis software */

#define MS_NULL_TERMINATED_ASCII_STRING_ID "MS:1001479"
#define MS_NULL_TERMINATED_ASCII_STRING_NAME "null-terminated ASCII string"
/* def: "Sequence of zero or more non-zero ASCII characters terminated by a single null (0) byte." [PSI:MS]
is_a: MS:1000518 ! binary data type */

#define MS_AB_SCIEX_TOF_TOF_NATIVEID_FORMAT_ID "MS:1001480"
#define MS_AB_SCIEX_TOF_TOF_NATIVEID_FORMAT_NAME "AB SCIEX TOF_TOF nativeID format"
/* def: "jobRun=xsd:nonNegativeInteger spotLabel=xsd:string spectrum=xsd:nonNegativeInteger." [PSI:MS]
is_a: MS:1000767 ! native spectrum identifier format */

#define MS_AB_SCIEX_TOF_TOF_DATABASE_ID "MS:1001481"
#define MS_AB_SCIEX_TOF_TOF_DATABASE_NAME "AB SCIEX TOF_TOF database"
/* def: "Applied Biosystems/MDS Analytical Technologies TOF/TOF instrument database." [PSI:MS]
is_a: MS:1000560 ! mass spectrometer file format */

#define MS_TOF_TOF_5800_ID "MS:1001482"
#define MS_TOF_TOF_5800_NAME "TOF_TOF 5800"
/* def: "AB SCIEX or Applied Biosystems|MDS Analytical Technologies AB SCIEX TOF/TOF 5800 Analyzer." [PSI:MS]
is_a: MS:1000121 ! AB SCIEX instrument model */

#define MS_AB_SCIEX_TOF_TOF_SERIES_EXPLORER_SOFTWARE_ID "MS:1001483"
#define MS_AB_SCIEX_TOF_TOF_SERIES_EXPLORER_SOFTWARE_NAME "AB SCIEX TOF_TOF Series Explorer Software"
/* def: "AB SCIEX or Applied Biosystems software for TOF/TOF data acquisition and analysis." [PSI:MS]
is_a: MS:1000690 ! AB SCIEX software
is_a: MS:1001455 ! acquisition software
is_a: MS:1001456 ! analysis software
is_a: MS:1001457 ! data processing software */

#define MS_INTENSITY_NORMALIZATION_ID "MS:1001484"
#define MS_INTENSITY_NORMALIZATION_NAME "intensity normalization"
/* def: "Normalization of data point intensities." [PSI:MS]
is_a: MS:1000543 ! data processing action */

#define MS_M_Z_CALIBRATION_ID "MS:1001485"
#define MS_M_Z_CALIBRATION_NAME "m/z calibration"
/* def: "Calibration of data point m/z positions." [PSI:MS]
is_a: MS:1000543 ! data processing action */

#define MS_DATA_FILTERING_ID "MS:1001486"
#define MS_DATA_FILTERING_NAME "data filtering"
/* def: "Filtering out part of the data." [PSI:MS]
is_a: MS:1000543 ! data processing action */

#define MS_PROTEINEXTRACTOR_ID "MS:1001487"
#define MS_PROTEINEXTRACTOR_NAME "ProteinExtractor"
/* def: "An algorithm for protein determination/assembly integrated into Bruker's ProteinScape." [PSI:MS]
is_a: MS:1000692 ! Bruker software
is_a: MS:1001456 ! analysis software */

#define MS_MASCOT_DISTILLER_ID "MS:1001488"
#define MS_MASCOT_DISTILLER_NAME "Mascot Distiller"
/* def: "Mascot Distiller." [PSI:PI]
is_a: MS:1001139 ! quantitation software name
is_a: MS:1001456 ! analysis software */

#define MS_MASCOT_INTEGRA_ID "MS:1001489"
#define MS_MASCOT_INTEGRA_NAME "Mascot Integra"
/* def: "Mascot Integra." [PSI:PI]
is_a: MS:1001456 ! analysis software */

#define MS_PERCOLATOR_ID "MS:1001490"
#define MS_PERCOLATOR_NAME "Percolator"
/* def: "Percolator." [PSI:PI]
is_a: MS:1001456 ! analysis software */

#define MS_PERCOLATOR_Q_VALUE_ID "MS:1001491"
#define MS_PERCOLATOR_Q_VALUE_NAME "percolator:Q value"
/* def: "Percolator:Q value." [PSI:PI]
xref: value-type:xsd\:double "The allowed value-type for this CV term."
is_a: MS:1001143 ! search engine specific score for PSMs
is_a: MS:1001153 ! search engine specific score
is_a: MS:1001868 ! distinct peptide-level q-value */

#define MS_PERCOLATOR_SCORE_ID "MS:1001492"
#define MS_PERCOLATOR_SCORE_NAME "percolator:score"
/* def: "Percolator:score." [PSI:PI]
xref: value-type:xsd\:double "The allowed value-type for this CV term."
is_a: MS:1001143 ! search engine specific score for PSMs
is_a: MS:1001153 ! search engine specific score */

#define MS_PERCOLATOR_PEP_ID "MS:1001493"
#define MS_PERCOLATOR_PEP_NAME "percolator:PEP"
/* def: "Posterior error probability." [PSI:PI]
xref: value-type:xsd\:double "The allowed value-type for this CV term."
is_a: MS:1001143 ! search engine specific score for PSMs
is_a: MS:1001153 ! search engine specific score */

#define MS_NO_THRESHOLD_ID "MS:1001494"
#define MS_NO_THRESHOLD_NAME "no threshold"
/* def: "In case no threshold was used." [PSI:PI]
is_a: MS:1001060 ! quality estimation method details */

#define MS_PROTEINSCAPE_SEARCHRESULTID_ID "MS:1001495"
#define MS_PROTEINSCAPE_SEARCHRESULTID_NAME "ProteinScape:SearchResultId"
/* def: "The SearchResultId of this peptide as SearchResult in the ProteinScape database." [PSI:PI]
xref: value-type:xsd\:positiveInteger "The allowed value-type for this CV term."
is_a: MS:1001143 ! search engine specific score for PSMs
is_a: MS:1001153 ! search engine specific score */

#define MS_PROTEINSCAPE_SEARCHEVENTID_ID "MS:1001496"
#define MS_PROTEINSCAPE_SEARCHEVENTID_NAME "ProteinScape:SearchEventId"
/* def: "The SearchEventId of the SearchEvent in the ProteinScape database." [PSI:PI]
xref: value-type:xsd\:positiveInteger "The allowed value-type for this CV term."
is_a: MS:1001143 ! search engine specific score for PSMs
is_a: MS:1001153 ! search engine specific score */

#define MS_PROTEINSCAPE_PROFOUNDPROBABILITY_ID "MS:1001497"
#define MS_PROTEINSCAPE_PROFOUNDPROBABILITY_NAME "ProteinScape:ProfoundProbability"
/* def: "The Profound probability score stored by ProteinScape." [PSI:PI]
xref: value-type:xsd\:double "The allowed value-type for this CV term."
is_a: MS:1001143 ! search engine specific score for PSMs
is_a: MS:1001153 ! search engine specific score */

#define MS_PROFOUND_Z_VALUE_ID "MS:1001498"
#define MS_PROFOUND_Z_VALUE_NAME "Profound:z value"
/* def: "The Profound z value." [PSI:PI]
xref: value-type:xsd\:double "The allowed value-type for this CV term."
is_a: MS:1001143 ! search engine specific score for PSMs
is_a: MS:1001153 ! search engine specific score */

#define MS_PROFOUND_CLUSTER_ID "MS:1001499"
#define MS_PROFOUND_CLUSTER_NAME "Profound:Cluster"
/* def: "The Profound cluster score." [PSI:PI]
xref: value-type:xsd\:double "The allowed value-type for this CV term."
is_a: MS:1001143 ! search engine specific score for PSMs
is_a: MS:1001153 ! search engine specific score */

#define MS_PROFOUND_CLUSTERRANK_ID "MS:1001500"
#define MS_PROFOUND_CLUSTERRANK_NAME "Profound:ClusterRank"
/* def: "The Profound cluster rank." [PSI:PI]
xref: value-type:xsd\:positiveInteger "The allowed value-type for this CV term."
is_a: MS:1001143 ! search engine specific score for PSMs
is_a: MS:1001153 ! search engine specific score */

#define MS_MSFIT_MOWSE_SCORE_ID "MS:1001501"
#define MS_MSFIT_MOWSE_SCORE_NAME "MSFit:Mowse score"
/* def: "The MSFit Mowse score." [PSI:PI]
xref: value-type:xsd\:double "The allowed value-type for this CV term."
is_a: MS:1001143 ! search engine specific score for PSMs
is_a: MS:1001153 ! search engine specific score */

#define MS_SONAR_SCORE_ID "MS:1001502"
#define MS_SONAR_SCORE_NAME "Sonar:Score"
/* def: "The Sonar score." [PSI:PI]
xref: value-type:xsd\:double "The allowed value-type for this CV term."
is_a: MS:1001143 ! search engine specific score for PSMs
is_a: MS:1001153 ! search engine specific score */

#define MS_PROTEINSCAPE_PFFSOLVEREXP_ID "MS:1001503"
#define MS_PROTEINSCAPE_PFFSOLVEREXP_NAME "ProteinScape:PFFSolverExp"
/* def: "The ProteinSolver exp value stored by ProteinScape." [PSI:PI]
xref: value-type:xsd\:double "The allowed value-type for this CV term."
is_a: MS:1001143 ! search engine specific score for PSMs
is_a: MS:1001153 ! search engine specific score */

#define MS_PROTEINSCAPE_PFFSOLVERSCORE_ID "MS:1001504"
#define MS_PROTEINSCAPE_PFFSOLVERSCORE_NAME "ProteinScape:PFFSolverScore"
/* def: "The ProteinSolver score stored by ProteinScape." [PSI:PI]
xref: value-type:xsd\:double "The allowed value-type for this CV term."
is_a: MS:1001143 ! search engine specific score for PSMs
is_a: MS:1001153 ! search engine specific score */

#define MS_PROTEINSCAPE_INTENSITYCOVERAGE_ID "MS:1001505"
#define MS_PROTEINSCAPE_INTENSITYCOVERAGE_NAME "ProteinScape:IntensityCoverage"
/* def: "The intensity coverage of the identified peaks in the spectrum calculated by ProteinScape." [PSI:PI]
xref: value-type:xsd\:double "The allowed value-type for this CV term."
is_a: MS:1001143 ! search engine specific score for PSMs
is_a: MS:1001153 ! search engine specific score */

#define MS_PROTEINSCAPE_SEQUESTMETASCORE_ID "MS:1001506"
#define MS_PROTEINSCAPE_SEQUESTMETASCORE_NAME "ProteinScape:SequestMetaScore"
/* def: "The SEQUEST meta score calculated by ProteinScape from the original SEQUEST scores." [PSI:PI]
xref: value-type:xsd\:double "The allowed value-type for this CV term."
is_a: MS:1001143 ! search engine specific score for PSMs
is_a: MS:1001153 ! search engine specific score */

#define MS_PROTEINEXTRACTOR_SCORE_ID "MS:1001507"
#define MS_PROTEINEXTRACTOR_SCORE_NAME "ProteinExtractor:Score"
/* def: "The score calculated by ProteinExtractor." [PSI:PI]
xref: value-type:xsd\:double "The allowed value-type for this CV term."
is_a: MS:1001116 ! single protein result details
is_a: MS:1001153 ! search engine specific score */

#define MS_AGILENT_MASSHUNTER_NATIVEID_FORMAT_ID "MS:1001508"
#define MS_AGILENT_MASSHUNTER_NATIVEID_FORMAT_NAME "Agilent MassHunter nativeID format"
/* def: "scanId=xsd:nonNegativeInteger." [PSI:PI]
is_a: MS:1000767 ! native spectrum identifier format */

#define MS_AGILENT_MASSHUNTER_FILE_ID "MS:1001509"
#define MS_AGILENT_MASSHUNTER_FILE_NAME "Agilent MassHunter file"
/* def: "A data file found in an Agilent MassHunter directory which contains raw data acquired by an Agilent mass spectrometer." [PSI:PI]
is_a: MS:1000560 ! mass spectrometer file format */

#define MS_TSQ_VANTAGE_ID "MS:1001510"
#define MS_TSQ_VANTAGE_NAME "TSQ Vantage"
/* def: "TSQ Vantage." [PSI:MS]
is_a: MS:1000494 ! Thermo Scientific instrument model */

#define MS_SEQUENCE_DATABASE_FILTER_TYPES_ID "MS:1001511"
#define MS_SEQUENCE_DATABASE_FILTER_TYPES_NAME "Sequence database filter types"
/* def: "Filter types which are used to filter a sequence database." [PSI:PI]
is_a: MS:1001019 ! database filtering */

#define MS_SEQUENCE_DATABASE_FILTERS_ID "MS:1001512"
#define MS_SEQUENCE_DATABASE_FILTERS_NAME "Sequence database filters"
/* def: "Sequence database filters which actually can contains values, e.g. to limit PI value of the sequences used to search." [PSI:PI]
is_a: MS:1001019 ! database filtering */

#define MS_DB_SEQUENCE_FILTER_PATTERN_ID "MS:1001513"
#define MS_DB_SEQUENCE_FILTER_PATTERN_NAME "DB sequence filter pattern"
/* def: "DB sequence filter pattern." [PSI:MS]
xref: value-type:xsd\:string "The allowed value-type for this CV term."
is_a: MS:1001512 ! Sequence database filters */

#define MS_DB_ACCESSION_FILTER_STRING_ID "MS:1001514"
#define MS_DB_ACCESSION_FILTER_STRING_NAME "DB accession filter string"
/* def: "DB accession filter string." [PSI:MS]
xref: value-type:xsd\:string "The allowed value-type for this CV term."
is_a: MS:1001512 ! Sequence database filters */

#define MS_FRAG__C_ION___H2O_ID "MS:1001515"
#define MS_FRAG__C_ION___H2O_NAME "frag: c ion - H2O"
/* def: "Fragmentation information, type of product: c ion without water." [PSI:PI]
is_a: MS:1002307 ! fragmentation ion type */

#define MS_FRAG__C_ION___NH3_ID "MS:1001516"
#define MS_FRAG__C_ION___NH3_NAME "frag: c ion - NH3"
/* def: "Fragmentation information, type of product: c ion without ammonia." [PSI:PI]
is_a: MS:1002307 ! fragmentation ion type */

#define MS_FRAG__Z_ION___H2O_ID "MS:1001517"
#define MS_FRAG__Z_ION___H2O_NAME "frag: z ion - H2O"
/* def: "Fragmentation information, type of product: z ion without water." [PSI:PI]
is_a: MS:1002307 ! fragmentation ion type */

#define MS_FRAG__Z_ION___NH3_ID "MS:1001518"
#define MS_FRAG__Z_ION___NH3_NAME "frag: z ion - NH3"
/* def: "Fragmentation information, type of product: z ion without ammonia." [PSI:PI]
is_a: MS:1002307 ! fragmentation ion type */

#define MS_FRAG__X_ION___H2O_ID "MS:1001519"
#define MS_FRAG__X_ION___H2O_NAME "frag: x ion - H2O"
/* def: "Fragmentation information, type of product: x ion without water." [PSI:PI]
is_a: MS:1002307 ! fragmentation ion type */

#define MS_FRAG__X_ION___NH3_ID "MS:1001520"
#define MS_FRAG__X_ION___NH3_NAME "frag: x ion - NH3"
/* def: "Fragmentation information, type of product: x ion without ammonia." [PSI:PI]
is_a: MS:1002307 ! fragmentation ion type */

#define MS_FRAG__PRECURSOR_ION___H2O_ID "MS:1001521"
#define MS_FRAG__PRECURSOR_ION___H2O_NAME "frag: precursor ion - H2O"
/* def: "Fragmentation information, type of product: precursor ion without water." [PSI:PI]
is_a: MS:1002307 ! fragmentation ion type */

#define MS_FRAG__PRECURSOR_ION___NH3_ID "MS:1001522"
#define MS_FRAG__PRECURSOR_ION___NH3_NAME "frag: precursor ion - NH3"
/* def: "Fragmentation information, type of product: precursor ion without ammonia." [PSI:PI]
is_a: MS:1002307 ! fragmentation ion type */

#define MS_FRAG__PRECURSOR_ION_ID "MS:1001523"
#define MS_FRAG__PRECURSOR_ION_NAME "frag: precursor ion"
/* def: "Fragmentation information, type of product: precursor ion." [PSI:PI]
is_a: MS:1002307 ! fragmentation ion type */

#define MS_FRAGMENT_NEUTRAL_LOSS_ID "MS:1001524"
#define MS_FRAGMENT_NEUTRAL_LOSS_NAME "fragment neutral loss"
/* def: "This term can describe a neutral loss m/z value that is lost from an ion." [PSI:PI]
xref: value-type:xsd\:double "The allowed value-type for this CV term."
is_a: MS:1001471 ! peptide modification details
relationship: has_units UO:0000221 ! dalton */

#define MS_PRECURSOR_NEUTRAL_LOSS_ID "MS:1001525"
#define MS_PRECURSOR_NEUTRAL_LOSS_NAME "precursor neutral loss"
/* def: "This term can describe a neutral loss m/z value that is lost from an ion." [PSI:PI]
xref: value-type:xsd\:double "The allowed value-type for this CV term."
is_a: MS:1001471 ! peptide modification details
relationship: has_units UO:0000221 ! dalton */

#define MS_SPECTRUM_FROM_DATABASE_INTEGER_NATIVEID_FORMAT_ID "MS:1001526"
#define MS_SPECTRUM_FROM_DATABASE_INTEGER_NATIVEID_FORMAT_NAME "spectrum from database integer nativeID format"
/* def: "databasekey=xsd:long." [PSI:MS]
comment: A unique identifier of a spectrum stored in a database (e.g. a PRIMARY KEY identifier).
is_a: MS:1000767 ! native spectrum identifier format */

#define MS_PROTEINSCAPE_SPECTRA_ID "MS:1001527"
#define MS_PROTEINSCAPE_SPECTRA_NAME "Proteinscape spectra"
/* def: "Spectra from Bruker/Protagen Proteinscape database." [PSI:MS]
is_a: MS:1000560 ! mass spectrometer file format */

#define MS_MASCOT_QUERY_NUMBER_ID "MS:1001528"
#define MS_MASCOT_QUERY_NUMBER_NAME "Mascot query number"
/* def: "query=xsd:nonNegativeInteger." [PSI:MS]
comment: The spectrum (query) number in a Mascot results file, starting from 1.
is_a: MS:1000767 ! native spectrum identifier format
is_a: MS:1001405 ! spectrum identification result details */

#define MS_SPECTRA_DATA_DETAILS_ID "MS:1001529"
#define MS_SPECTRA_DATA_DETAILS_NAME "spectra data details"
/* def: "Child-terms contain information to map the results back to spectra." [PSI:MS]
is_a: MS:1001249 ! search input details */

#define MS_MZML_UNIQUE_IDENTIFIER_ID "MS:1001530"
#define MS_MZML_UNIQUE_IDENTIFIER_NAME "mzML unique identifier"
/* def: "mzMLid=xsd:IDREF." [PSI:MS]
comment: A unique identifier of a spectrum stored in an mzML file.
is_a: MS:1001529 ! spectra data details */

#define MS_SPECTRUM_FROM_PROTEINSCAPE_DATABASE_NATIVEID_FORMAT_ID "MS:1001531"
#define MS_SPECTRUM_FROM_PROTEINSCAPE_DATABASE_NATIVEID_FORMAT_NAME "spectrum from ProteinScape database nativeID format"
/* def: "databasekey=xsd:long." [PSI:MS]
comment: A unique identifier of a spectrum stored in a ProteinScape database.
is_a: MS:1000767 ! native spectrum identifier format
is_a: MS:1001529 ! spectra data details */

#define MS_SPECTRUM_FROM_DATABASE_STRING_NATIVEID_FORMAT_ID "MS:1001532"
#define MS_SPECTRUM_FROM_DATABASE_STRING_NATIVEID_FORMAT_NAME "spectrum from database string nativeID format"
/* def: "databasekey=xsd:string." [PSI:MS]
comment: A unique identifier of a spectrum stored in a database (e.g. a PRIMARY KEY identifier).
is_a: MS:1000767 ! native spectrum identifier format
is_a: MS:1001529 ! spectra data details */

#define MS_BRUKER_DALTONICS_ESQUIRE_SERIES_ID "MS:1001533"
#define MS_BRUKER_DALTONICS_ESQUIRE_SERIES_NAME "Bruker Daltonics esquire series"
/* def: "Bruker Daltonics' esquire series." [PSI:MS]
is_a: MS:1000122 ! Bruker Daltonics instrument model */

#define MS_BRUKER_DALTONICS_FLEX_SERIES_ID "MS:1001534"
#define MS_BRUKER_DALTONICS_FLEX_SERIES_NAME "Bruker Daltonics flex series"
/* def: "Bruker Daltonics' flex series." [PSI:MS]
is_a: MS:1000122 ! Bruker Daltonics instrument model */

#define MS_BRUKER_DALTONICS_BIOTOF_SERIES_ID "MS:1001535"
#define MS_BRUKER_DALTONICS_BIOTOF_SERIES_NAME "Bruker Daltonics BioTOF series"
/* def: "Bruker Daltonics' BioTOF series." [PSI:MS]
is_a: MS:1000122 ! Bruker Daltonics instrument model */

#define MS_BRUKER_DALTONICS_MICROTOF_SERIES_ID "MS:1001536"
#define MS_BRUKER_DALTONICS_MICROTOF_SERIES_NAME "Bruker Daltonics micrOTOF series"
/* def: "Bruker Daltonics' micrOTOF series." [PSI:MS]
is_a: MS:1000122 ! Bruker Daltonics instrument model */

#define MS_BIOTOF_ID "MS:1001537"
#define MS_BIOTOF_NAME "BioTOF"
/* def: "Bruker Daltonics' BioTOF: ESI TOF." [PSI:MS]
is_a: MS:1001535 ! Bruker Daltonics BioTOF series */

#define MS_BIOTOF_III_ID "MS:1001538"
#define MS_BIOTOF_III_NAME "BioTOF III"
/* def: "Bruker Daltonics' BioTOF III: ESI TOF." [PSI:MS]
is_a: MS:1001535 ! Bruker Daltonics BioTOF series */

#define MS_ULTROTOF_Q_ID "MS:1001539"
#define MS_ULTROTOF_Q_NAME "UltroTOF-Q"
/* def: "Bruker Daltonics' UltroTOF-Q: ESI Q-TOF (MALDI optional)." [PSI:MS]
is_a: MS:1001535 ! Bruker Daltonics BioTOF series */

#define MS_MICROTOF_II_ID "MS:1001540"
#define MS_MICROTOF_II_NAME "micrOTOF II"
/* def: "Bruker Daltonics' micrOTOF II: ESI TOF, Nanospray, APCI, APPI." [PSI:MS]
is_a: MS:1001536 ! Bruker Daltonics micrOTOF series */

#define MS_MAXIS_ID "MS:1001541"
#define MS_MAXIS_NAME "maXis"
/* def: "Bruker Daltonics' maXis: ESI Q-TOF, Nanospray, APCI, APPI." [PSI:MS]
is_a: MS:1001547 ! Bruker Daltonics maXis series */

#define MS_AMAZON_ETD_ID "MS:1001542"
#define MS_AMAZON_ETD_NAME "amaZon ETD"
/* def: "Bruker Daltonics' amaZon ETD: ESI quadrupole ion trap, Nanospray, APCI, APPI, ETD, PTR." [PSI:MS]
is_a: MS:1001545 ! Bruker Daltonics amaZon series */

#define MS_MICROFLEX_LRF_ID "MS:1001543"
#define MS_MICROFLEX_LRF_NAME "microflex LRF"
/* def: "Bruker Daltonics' microflex LRF: MALDI TOF." [PSI:MS]
is_a: MS:1001534 ! Bruker Daltonics flex series */

#define MS_ULTRAFLEXTREME_ID "MS:1001544"
#define MS_ULTRAFLEXTREME_NAME "ultrafleXtreme"
/* def: "Bruker Daltonics' ultrafleXtreme: MALDI TOF." [PSI:MS]
is_a: MS:1001534 ! Bruker Daltonics flex series */

#define MS_BRUKER_DALTONICS_AMAZON_SERIES_ID "MS:1001545"
#define MS_BRUKER_DALTONICS_AMAZON_SERIES_NAME "Bruker Daltonics amaZon series"
/* def: "Bruker Daltonics' amaZon series." [PSI:MS]
is_a: MS:1000122 ! Bruker Daltonics instrument model */

#define MS_AMAZON_X_ID "MS:1001546"
#define MS_AMAZON_X_NAME "amaZon X"
/* def: "Bruker Daltonics' amaZon X: ESI quadrupole ion trap, APCI, APPI, ETD, PTR." [PSI:MS]
is_a: MS:1001545 ! Bruker Daltonics amaZon series */

#define MS_BRUKER_DALTONICS_MAXIS_SERIES_ID "MS:1001547"
#define MS_BRUKER_DALTONICS_MAXIS_SERIES_NAME "Bruker Daltonics maXis series"
/* def: "Bruker Daltonics' maXis series." [PSI:MS]
is_a: MS:1000122 ! Bruker Daltonics instrument model */

#define MS_BRUKER_DALTONICS_SOLARIX_SERIES_ID "MS:1001548"
#define MS_BRUKER_DALTONICS_SOLARIX_SERIES_NAME "Bruker Daltonics solarix series"
/* def: "Bruker Daltonics' solarix: ESI quadrupole ion trap, APCI, APPI, ETD, PTR." [PSI:MS]
is_a: MS:1000122 ! Bruker Daltonics instrument model */

#define MS_SOLARIX_ID "MS:1001549"
#define MS_SOLARIX_NAME "solariX"
/* def: "Bruker Daltonics' solariX: ESI, MALDI, Qh-FT_ICR." [PSI:MS]
is_a: MS:1001548 ! Bruker Daltonics solarix series */

#define MS_MICROFLEX_II_ID "MS:1001550"
#define MS_MICROFLEX_II_NAME "microflex II"
/* def: "Bruker Daltonics' microflex II: MALDI TOF." [PSI:MS]
is_a: MS:1001534 ! Bruker Daltonics flex series */

#define MS_AUTOFLEX_II_TOF_TOF_ID "MS:1001553"
#define MS_AUTOFLEX_II_TOF_TOF_NAME "autoflex II TOF_TOF"
/* def: "Bruker Daltonics' autoflex II TOF/TOF: MALDI TOF." [PSI:MS]
is_a: MS:1001534 ! Bruker Daltonics flex series */

#define MS_AUTOFLEX_III_TOF_TOF_SMARTBEAM_ID "MS:1001554"
#define MS_AUTOFLEX_III_TOF_TOF_SMARTBEAM_NAME "autoflex III TOF_TOF smartbeam"
/* def: "Bruker Daltonics' autoflex III TOF/TOF smartbeam: MALDI TOF." [PSI:MS]
is_a: MS:1001534 ! Bruker Daltonics flex series */

#define MS_AUTOFLEX_ID "MS:1001555"
#define MS_AUTOFLEX_NAME "autoflex"
/* def: "Bruker Daltonics' autoflex: MALDI TOF." [PSI:MS]
is_a: MS:1001534 ! Bruker Daltonics flex series */

#define MS_BRUKER_DALTONICS_APEX_SERIES_ID "MS:1001556"
#define MS_BRUKER_DALTONICS_APEX_SERIES_NAME "Bruker Daltonics apex series"
/* def: "Bruker Daltonics' apex series." [PSI:MS]
is_a: MS:1000122 ! Bruker Daltonics instrument model */

#define MS_SHIMADZU_CORPORATION_SOFTWARE_ID "MS:1001557"
#define MS_SHIMADZU_CORPORATION_SOFTWARE_NAME "Shimadzu Corporation software"
/* def: "Shimadzu Corporation software." [PSI:MS]
is_a: MS:1000531 ! software */

#define MS_MALDI_SOLUTIONS_ID "MS:1001558"
#define MS_MALDI_SOLUTIONS_NAME "MALDI Solutions"
/* def: "Shimadzu Biotech software for data acquisition, processing, and analysis." [PSI:MS]
is_a: MS:1001455 ! acquisition software
is_a: MS:1001456 ! analysis software
is_a: MS:1001457 ! data processing software
is_a: MS:1001557 ! Shimadzu Corporation software */

#define MS_AB_SCIEX_TOF_TOF_T2D_NATIVEID_FORMAT_ID "MS:1001559"
#define MS_AB_SCIEX_TOF_TOF_T2D_NATIVEID_FORMAT_NAME "AB SCIEX TOF_TOF T2D nativeID format"
/* def: "file=xsd:IDREF." [PSI:MS]
is_a: MS:1000767 ! native spectrum identifier format */

#define MS_AB_SCIEX_TOF_TOF_T2D_FILE_ID "MS:1001560"
#define MS_AB_SCIEX_TOF_TOF_T2D_FILE_NAME "AB SCIEX TOF_TOF T2D file"
/* def: "Applied Biosystems/MDS Analytical Technologies TOF/TOF instrument export format." [PSI:MS]
is_a: MS:1000560 ! mass spectrometer file format */

#define MS_SCAFFOLD_ID "MS:1001561"
#define MS_SCAFFOLD_NAME "Scaffold"
/* def: "Scaffold analysis software." [http://www.proteomesoftware.com]
is_a: MS:1001456 ! analysis software */

#define MS_SCAFFOLD_NATIVEID_FORMAT_ID "MS:1001562"
#define MS_SCAFFOLD_NATIVEID_FORMAT_NAME "Scaffold nativeID format"
/* def: "Scaffold native ID format." [PSI:MS]
is_a: MS:1000767 ! native spectrum identifier format */

#define MS_SEQUEST_SQT_ID "MS:1001563"
#define MS_SEQUEST_SQT_NAME "SEQUEST SQT"
/* def: "Source file for this mzIdentML was a SEQUEST SQT." [PSI:PI]
is_a: MS:1001040 ! intermediate analysis format */

#define MS_DISCOVERER_MSF_ID "MS:1001564"
#define MS_DISCOVERER_MSF_NAME "Discoverer MSF"
/* def: "Source file for this mzIdentML was in Thermo Scientific Discoverer MSF format." [PSI:PI]
is_a: MS:1001040 ! intermediate analysis format */

#define MS_IDENTITYE_XML_ID "MS:1001565"
#define MS_IDENTITYE_XML_NAME "IdentityE XML"
/* def: "Source file for this mzIdentML was in Waters IdentityE XML format." [PSI:PI]
is_a: MS:1001040 ! intermediate analysis format */

#define MS_PROTEINLYNX_XML_ID "MS:1001566"
#define MS_PROTEINLYNX_XML_NAME "ProteinLynx XML"
/* def: "Source file for this mzIdentML was in Waters ProteinLynx XML format." [PSI:PI]
is_a: MS:1001040 ! intermediate analysis format */

#define MS_SPECTRUMMILL_DIRECTORIES_ID "MS:1001567"
#define MS_SPECTRUMMILL_DIRECTORIES_NAME "SpectrumMill directories"
/* def: "Source file for this mzIdentML was in Agilent SpectrumMill directory format." [PSI:PI]
is_a: MS:1001040 ! intermediate analysis format */

#define MS_SCAFFOLD_PEPTIDE_PROBABILITY_ID "MS:1001568"
#define MS_SCAFFOLD_PEPTIDE_PROBABILITY_NAME "Scaffold:Peptide Probability"
/* def: "Scaffold peptide probability score." [PSI:PI]
xref: value-type:xsd\:double "The allowed value-type for this CV term."
is_a: MS:1001143 ! search engine specific score for PSMs
is_a: MS:1001153 ! search engine specific score */

#define MS_IDENTITYE_SCORE_ID "MS:1001569"
#define MS_IDENTITYE_SCORE_NAME "IdentityE Score"
/* def: "Waters IdentityE peptide score." [PSI:PI]
xref: value-type:xsd\:double "The allowed value-type for this CV term."
is_a: MS:1001143 ! search engine specific score for PSMs
is_a: MS:1001153 ! search engine specific score */

#define MS_PROTEINLYNX_LOG_LIKELIHOOD_ID "MS:1001570"
#define MS_PROTEINLYNX_LOG_LIKELIHOOD_NAME "ProteinLynx:Log Likelihood"
/* def: "ProteinLynx log likelihood score." [PSI:PI]
xref: value-type:xsd\:double "The allowed value-type for this CV term."
is_a: MS:1001143 ! search engine specific score for PSMs
is_a: MS:1001153 ! search engine specific score */

#define MS_PROTEINLYNX_LADDER_SCORE_ID "MS:1001571"
#define MS_PROTEINLYNX_LADDER_SCORE_NAME "ProteinLynx:Ladder Score"
/* def: "Waters ProteinLynx Ladder score." [PSI:PI]
xref: value-type:xsd\:double "The allowed value-type for this CV term."
is_a: MS:1001143 ! search engine specific score for PSMs
is_a: MS:1001153 ! search engine specific score */

#define MS_SPECTRUMMILL_SCORE_ID "MS:1001572"
#define MS_SPECTRUMMILL_SCORE_NAME "SpectrumMill:Score"
/* def: "Spectrum mill peptide score." [PSI:PI]
xref: value-type:xsd\:double "The allowed value-type for this CV term."
is_a: MS:1001143 ! search engine specific score for PSMs
is_a: MS:1001153 ! search engine specific score */

#define MS_SPECTRUMMILL_SPI_ID "MS:1001573"
#define MS_SPECTRUMMILL_SPI_NAME "SpectrumMill:SPI"
/* def: "SpectrumMill SPI score (%)." [PSI:PI]
xref: value-type:xsd\:double "The allowed value-type for this CV term."
is_a: MS:1001143 ! search engine specific score for PSMs
is_a: MS:1001153 ! search engine specific score */

#define MS_REPORT_ONLY_SPECTRA_ASSIGNED_TO_IDENTIFIED_PROTEINS_ID "MS:1001574"
#define MS_REPORT_ONLY_SPECTRA_ASSIGNED_TO_IDENTIFIED_PROTEINS_NAME "report only spectra assigned to identified proteins"
/* def: "Flag indicating to report only the spectra assigned to identified proteins." [PSI:PI]
is_a: MS:1001060 ! quality estimation method details */

#define MS_SCAFFOLD__MINIMUM_PEPTIDE_COUNT_ID "MS:1001575"
#define MS_SCAFFOLD__MINIMUM_PEPTIDE_COUNT_NAME "Scaffold: Minimum Peptide Count"
/* def: "Minimum number of peptides a protein must have to be accepted." [PSI:PI]
xref: value-type:xsd\:positiveInteger "The allowed value-type for this CV term."
is_a: MS:1002106 ! Scaffold input parameter */

#define MS_SCAFFOLD__MINIMUM_PROTEIN_PROBABILITY_ID "MS:1001576"
#define MS_SCAFFOLD__MINIMUM_PROTEIN_PROBABILITY_NAME "Scaffold: Minimum Protein Probability"
/* def: "Minimum protein probability a protein must have to be accepted." [PSI:PI]
xref: value-type:xsd\:double "The allowed value-type for this CV term."
is_a: MS:1002106 ! Scaffold input parameter */

#define MS_SCAFFOLD__MINIMUM_PEPTIDE_PROBABILITY_ID "MS:1001577"
#define MS_SCAFFOLD__MINIMUM_PEPTIDE_PROBABILITY_NAME "Scaffold: Minimum Peptide Probability"
/* def: "Minimum probability a peptide must have to be accepted for protein scoring." [PSI:PI]
xref: value-type:xsd\:double "The allowed value-type for this CV term."
is_a: MS:1002106 ! Scaffold input parameter */

#define MS_MINIMUM_NUMBER_OF_ENZYMATIC_TERMINI_ID "MS:1001578"
#define MS_MINIMUM_NUMBER_OF_ENZYMATIC_TERMINI_NAME "minimum number of enzymatic termini"
/* def: "Minimum number of enzymatic termini a pepide must have to be accepted." [PSI:PI]
xref: value-type:xsd\:nonNegativeInteger "The allowed value-type for this CV term."
is_a: MS:1002094 ! common search engine input parameter */

#define MS_SCAFFOLD_PROTEIN_PROBABILITY_ID "MS:1001579"
#define MS_SCAFFOLD_PROTEIN_PROBABILITY_NAME "Scaffold:Protein Probability"
/* def: "Scaffold protein probability score." [PSI:PI]
xref: value-type:xsd\:double "The allowed value-type for this CV term."
is_a: MS:1001116 ! single protein result details
is_a: MS:1001153 ! search engine specific score */

#define MS_SPECTRUMMILL_DISCRIMINANT_SCORE_ID "MS:1001580"
#define MS_SPECTRUMMILL_DISCRIMINANT_SCORE_NAME "SpectrumMill:Discriminant Score"
/* def: "Discriminant score from Agilent SpectrumMill software." [PSI:PI]
xref: value-type:xsd\:double "The allowed value-type for this CV term."
is_a: MS:1001143 ! search engine specific score for PSMs
is_a: MS:1001153 ! search engine specific score */

#define MS_FAIMS_COMPENSATION_VOLTAGE_ID "MS:1001581"
#define MS_FAIMS_COMPENSATION_VOLTAGE_NAME "FAIMS compensation voltage"
/* def: "The DC potential applied to the asymmetric waveform in FAIMS that compensates for the difference between high and low field mobility of an ion." [PSI:MS]
synonym: "FAIMS CV" EXACT []
is_a: MS:1000503 ! scan attribute */

#define MS_XCMS_ID "MS:1001582"
#define MS_XCMS_NAME "XCMS"
/* def: "Bioconductor package XCMS for preprocessing high-throughput, untargeted analyte profiling data." [PSI:MS]
is_a: MS:1001456 ! analysis software
is_a: MS:1001457 ! data processing software */

#define MS_MAXQUANT_ID "MS:1001583"
#define MS_MAXQUANT_NAME "MaxQuant"
/* def: "MaxQuant is a quantitative proteomics software package designed for analyzing large mass spectrometric data sets. It is specifically aimed at high resolution MS data." [PSI:MS]
is_a: MS:1001139 ! quantitation software name
is_a: MS:1001456 ! analysis software */

#define MS_COMBINED_PMF___MS_MS_SEARCH_ID "MS:1001584"
#define MS_COMBINED_PMF___MS_MS_SEARCH_NAME "combined pmf + ms-ms search"
/* def: "Search that includes data from Peptide Mass Fingerprint (PMF) and MS/MS (aka Peptide Fragment Fingerprint - PFF)." [PSI:MS]
is_a: MS:1001080 ! search type */

#define MS_MYRIMATCH_ID "MS:1001585"
#define MS_MYRIMATCH_NAME "MyriMatch"
/* def: "Tabb Lab software for directly comparing peptides in a database to tandem mass spectra." [PSI:MS]
is_a: MS:1001456 ! analysis software */

#define MS_DIRECTAG_ID "MS:1001586"
#define MS_DIRECTAG_NAME "DirecTag"
/* def: "Tabb Lab software for generating sequence tags from tandem mass spectra." [PSI:MS]
is_a: MS:1001456 ! analysis software */

#define MS_TAGRECON_ID "MS:1001587"
#define MS_TAGRECON_NAME "TagRecon"
/* def: "Tabb Lab software for reconciling sequence tags to a protein database." [PSI:MS]
is_a: MS:1001456 ! analysis software */

#define MS_PEPITOME_ID "MS:1001588"
#define MS_PEPITOME_NAME "Pepitome"
/* def: "Tabb Lab software for spectral library searches on tandem mass spectra." [PSI:MS]
is_a: MS:1001456 ! analysis software */

#define MS_MYRIMATCH_MVH_ID "MS:1001589"
#define MS_MYRIMATCH_MVH_NAME "MyriMatch:MVH"
/* def: "Using the multivariate hypergeometric distribution and a peak list divided into several intensity classes, this score is the negative natural log probability that the predicted peaks matched to experimental peaks by random chance." [PSI:MS]
synonym: "Pepitome:MVH" EXACT []
synonym: "TagRecon:MVH" EXACT []
is_a: MS:1001143 ! search engine specific score for PSMs
is_a: MS:1001153 ! search engine specific score */

#define MS_MYRIMATCH_MZFIDELITY_ID "MS:1001590"
#define MS_MYRIMATCH_MZFIDELITY_NAME "MyriMatch:mzFidelity"
/* def: "The negative natural log probability that predicted peaks match to experimental peaks by random chance by scoring the m/z delta of the matches in a multinomial distribution." [PSI:MS]
synonym: "Pepitome:mzFidelity" EXACT []
synonym: "TagRecon:mzFidelity" EXACT []
is_a: MS:1001143 ! search engine specific score for PSMs
is_a: MS:1001153 ! search engine specific score */

#define MS_ANCHOR_PROTEIN_ID "MS:1001591"
#define MS_ANCHOR_PROTEIN_NAME "anchor protein"
/* def: "A representative protein selected from a set of sequence same-set or spectrum same-set proteins." [PSI:MS]
xref: value-type:xsd\:string "The allowed value-type for this CV term."
is_a: MS:1001101 ! protein group or subset relationship */

#define MS_FAMILY_MEMBER_PROTEIN_ID "MS:1001592"
#define MS_FAMILY_MEMBER_PROTEIN_NAME "family member protein"
/* def: "A protein with significant homology to another protein, but some distinguishing peptide matches." [PSI:MS]
xref: value-type:xsd\:string "The allowed value-type for this CV term."
is_a: MS:1001101 ! protein group or subset relationship */

#define MS_GROUP_MEMBER_WITH_UN___DEFINED_RELATIONSHIP_OR_ORTHOLOG_PROTEIN_ID "MS:1001593"
#define MS_GROUP_MEMBER_WITH_UN___DEFINED_RELATIONSHIP_OR_ORTHOLOG_PROTEIN_NAME "group member with un_* defined relationship OR ortholog protein"
/* def: "TO ENDETAIL: a really generic relationship OR ortholog protein." [PSI:MS]
is_a: MS:1001101 ! protein group or subset relationship */

#define MS_SEQUENCE_SAME_SET_PROTEIN_ID "MS:1001594"
#define MS_SEQUENCE_SAME_SET_PROTEIN_NAME "sequence same-set protein"
/* def: "A protein which is indistinguishable or equivalent to another protein, having matches to an identical set of peptide sequences." [PSI:MS]
xref: value-type:xsd\:string "The allowed value-type for this CV term."
is_a: MS:1001101 ! protein group or subset relationship */

#define MS_SPECTRUM_SAME_SET_PROTEIN_ID "MS:1001595"
#define MS_SPECTRUM_SAME_SET_PROTEIN_NAME "spectrum same-set protein"
/* def: "A protein which is indistinguishable or equivalent to another protein, having matches to a set of peptide sequences that cannot be distinguished using the evidence in the mass spectra." [PSI:MS]
xref: value-type:xsd\:string "The allowed value-type for this CV term."
is_a: MS:1001101 ! protein group or subset relationship */

#define MS_SEQUENCE_SUB_SET_PROTEIN_ID "MS:1001596"
#define MS_SEQUENCE_SUB_SET_PROTEIN_NAME "sequence sub-set protein"
/* def: "A protein with a sub-set of the peptide sequence matches for another protein, and no distinguishing peptide matches." [PSI:MS]
xref: value-type:xsd\:string "The allowed value-type for this CV term."
is_a: MS:1001101 ! protein group or subset relationship */

#define MS_SPECTRUM_SUB_SET_PROTEIN_ID "MS:1001597"
#define MS_SPECTRUM_SUB_SET_PROTEIN_NAME "spectrum sub-set protein"
/* def: "A protein with a sub-set of the matched spectra for another protein, where the matches cannot be distinguished using the evidence in the mass spectra, and no distinguishing peptide matches." [PSI:MS]
xref: value-type:xsd\:string "The allowed value-type for this CV term."
is_a: MS:1001101 ! protein group or subset relationship */

#define MS_SEQUENCE_SUBSUMABLE_PROTEIN_ID "MS:1001598"
#define MS_SEQUENCE_SUBSUMABLE_PROTEIN_NAME "sequence subsumable protein"
/* def: "A sequence same-set or sequence sub-set protein where the matches are distributed across two or more proteins." [PSI:MS]
xref: value-type:xsd\:string "The allowed value-type for this CV term."
is_a: MS:1001101 ! protein group or subset relationship */

#define MS_SPECTRUM_SUBSUMABLE_PROTEIN_ID "MS:1001599"
#define MS_SPECTRUM_SUBSUMABLE_PROTEIN_NAME "spectrum subsumable protein"
/* def: "A spectrum same-set or spectrum sub-set protein where the matches are distributed across two or more proteins." [PSI:MS]
xref: value-type:xsd\:string "The allowed value-type for this CV term."
is_a: MS:1001101 ! protein group or subset relationship */

#define MS_PROTEIN_INFERENCE_CONFIDENCE_CATEGORY_ID "MS:1001600"
#define MS_PROTEIN_INFERENCE_CONFIDENCE_CATEGORY_NAME "protein inference confidence category"
/* def: "Confidence category of inferred protein (conclusive, non conclusive, ambiguous group or indistinguishable)." [PSI:MS]
xref: value-type:xsd\:string "The allowed value-type for this CV term."
is_a: MS:1001101 ! protein group or subset relationship */

// #define MS_PROTEOMEDISCOVERER_SPECTRUM_FILES_RAW_FILE_NAMES_ID "MS:1001601"
// #define MS_PROTEOMEDISCOVERER_SPECTRUM_FILES_RAW_FILE_NAMES_NAME "ProteomeDiscoverer:Spectrum Files:Raw File names"
/* def: "OBSOLETE Name and location of the .raw file or files." [PSI:MS]
comment: This term was made obsolete because it's recommended to use one of the 'mass spectrometer file format' terms (MS:1000560) instead.
xref: value-type:xsd\:string "The allowed value-type for this CV term."
is_a: MS:1002101 ! ProteomeDiscoverer input parameter
is_obsolete: true */

// #define MS_PROTEOMEDISCOVERER_SRF_FILE_SELECTOR_SRF_FILE_PATH_ID "MS:1001602"
// #define MS_PROTEOMEDISCOVERER_SRF_FILE_SELECTOR_SRF_FILE_PATH_NAME "ProteomeDiscoverer:SRF File Selector:SRF File Path"
/* def: "OBSOLETE Path and name of the .srf (SEQUEST Result Format) file." [PSI:MS]
comment: This term was made obsolete. Use attribute in mzIdentML / mzQuantML instead.
xref: value-type:xsd\:string "The allowed value-type for this CV term."
is_a: MS:1002101 ! ProteomeDiscoverer input parameter
is_obsolete: true */

// #define MS_PROTEOMEDISCOVERER_SPECTRUM_SELECTOR_IONIZATION_SOURCE_ID "MS:1001603"
// #define MS_PROTEOMEDISCOVERER_SPECTRUM_SELECTOR_IONIZATION_SOURCE_NAME "ProteomeDiscoverer:Spectrum Selector:Ionization Source"
/* def: "OBSOLETE Ionization source (electro-, nano-, thermospray, electron impact, APCI, MALDI, FAB etc)." [PSI:MS]
comment: This term was made obsolete because it's recommended to use one of the 'inlet type' (MS:1000007) or 'ionization type' (MS:1000008) terms instead.
xref: value-type:xsd\:string "The allowed value-type for this CV term."
is_a: MS:1002101 ! ProteomeDiscoverer input parameter
is_obsolete: true */

// #define MS_PROTEOMEDISCOVERER_ACTIVATION_TYPE_ID "MS:1001604"
// #define MS_PROTEOMEDISCOVERER_ACTIVATION_TYPE_NAME "ProteomeDiscoverer:Activation Type"
/* def: "OBSOLETE Fragmentation method used (CID, MPD, ECD, PQD, ETD, HCD, Any)." [PSI:MS]
comment: This term was made obsolete because it's recommended to use one of the 'ionization type' terms (MS:1000008) instead.
xref: value-type:xsd\:string "The allowed value-type for this CV term."
is_a: MS:1002101 ! ProteomeDiscoverer input parameter
is_obsolete: true */

#define MS_PROTEOMEDISCOVERER_SPECTRUM_SELECTOR_LOWER_RT_LIMIT_ID "MS:1001605"
#define MS_PROTEOMEDISCOVERER_SPECTRUM_SELECTOR_LOWER_RT_LIMIT_NAME "ProteomeDiscoverer:Spectrum Selector:Lower RT Limit"
/* def: "Lower retention-time limit." [PSI:MS]
xref: value-type:xsd\:float "The allowed value-type for this CV term."
is_a: MS:1002101 ! ProteomeDiscoverer input parameter */

// #define MS_PROTEOMEDISCOVERER_MASS_ANALYZER_ID "MS:1001606"
// #define MS_PROTEOMEDISCOVERER_MASS_ANALYZER_NAME "ProteomeDiscoverer:Mass Analyzer"
/* def: "OBSOLETE Type of mass spectrometer used (ITMS, FTMS, TOFMS, SQMS, TQMS, SectorMS)." [PSI:MS]
comment: This term was made obsolete because it's recommended to use mass analyzer type (MS:1000443) instead.
xref: value-type:xsd\:string "The allowed value-type for this CV term."
is_a: MS:1002101 ! ProteomeDiscoverer input parameter
is_obsolete: true */

#define MS_PROTEOMEDISCOVERER_MAX_PRECURSOR_MASS_ID "MS:1001607"
#define MS_PROTEOMEDISCOVERER_MAX_PRECURSOR_MASS_NAME "ProteomeDiscoverer:Max Precursor Mass"
/* def: "Maximum mass limit of a singly charged precursor ion." [PSI:MS]
xref: value-type:xsd\:float "The allowed value-type for this CV term."
is_a: MS:1002101 ! ProteomeDiscoverer input parameter */

#define MS_PROTEOMEDISCOVERER_MIN_PRECURSOR_MASS_ID "MS:1001608"
#define MS_PROTEOMEDISCOVERER_MIN_PRECURSOR_MASS_NAME "ProteomeDiscoverer:Min Precursor Mass"
/* def: "Minimum mass limit of a singly charged precursor ion." [PSI:MS]
xref: value-type:xsd\:float "The allowed value-type for this CV term."
is_a: MS:1002101 ! ProteomeDiscoverer input parameter */

#define MS_PROTEOMEDISCOVERER_SPECTRUM_SELECTOR_MINIMUM_PEAK_COUNT_ID "MS:1001609"
#define MS_PROTEOMEDISCOVERER_SPECTRUM_SELECTOR_MINIMUM_PEAK_COUNT_NAME "ProteomeDiscoverer:Spectrum Selector:Minimum Peak Count"
/* def: "Minimum number of peaks in a tandem mass spectrum that is allowed to pass the filter and to be subjected to further processing in the workflow." [PSI:MS]
xref: value-type:xsd\:int "The allowed value-type for this CV term."
is_a: MS:1002101 ! ProteomeDiscoverer input parameter */

// #define MS_PROTEOMEDISCOVERER_MS_ORDER_ID "MS:1001610"
// #define MS_PROTEOMEDISCOVERER_MS_ORDER_NAME "ProteomeDiscoverer:MS Order"
/* def: "OBSOLETE Level of the mass spectrum (MS/MS=MS2 ... MS10)." [PSI:MS]
comment: This term was made obsolete because it's recommended to use MS1 spectrum (MS:1000579) or MSn spectrum (MS:1000580) instead.
xref: value-type:xsd\:string "The allowed value-type for this CV term."
is_a: MS:1002101 ! ProteomeDiscoverer input parameter
is_obsolete: true */

// #define MS_PROTEOMEDISCOVERER_POLARITY_MODE_ID "MS:1001611"
// #define MS_PROTEOMEDISCOVERER_POLARITY_MODE_NAME "ProteomeDiscoverer:Polarity Mode"
/* def: "OBSOLETE Polarity mode (positive or negative)." [PSI:MS]
comment: This term was made obsolete because it's recommended to use scan polarity (MS:1000465) instead.
xref: value-type:xsd\:string "The allowed value-type for this CV term."
is_a: MS:1002101 ! ProteomeDiscoverer input parameter
is_obsolete: true */

#define MS_PROTEOMEDISCOVERER_SPECTRUM_SELECTOR_PRECURSOR_SELECTION_ID "MS:1001612"
#define MS_PROTEOMEDISCOVERER_SPECTRUM_SELECTOR_PRECURSOR_SELECTION_NAME "ProteomeDiscoverer:Spectrum Selector:Precursor Selection"
/* def: "Determines which precursor mass to use for a given MSn scan. This option applies only to higher-order MSn scans (n >= 3)." [PSI:MS]
xref: value-type:xsd\:string "The allowed value-type for this CV term."
is_a: MS:1002101 ! ProteomeDiscoverer input parameter */

#define MS_PROTEOMEDISCOVERER_SN_THRESHOLD_ID "MS:1001613"
#define MS_PROTEOMEDISCOVERER_SN_THRESHOLD_NAME "ProteomeDiscoverer:SN Threshold"
/* def: "Signal-to-Noise ratio below which peaks are removed." [PSI:MS]
xref: value-type:xsd\:float "The allowed value-type for this CV term."
is_a: MS:1002101 ! ProteomeDiscoverer input parameter */

// #define MS_PROTEOMEDISCOVERER_SCAN_TYPE_ID "MS:1001614"
// #define MS_PROTEOMEDISCOVERER_SCAN_TYPE_NAME "ProteomeDiscoverer:Scan Type"
/* def: "OBSOLETE Scan type for the precursor ion (full, Single Ion Monitoring (SIM), Single Reaction Monitoring (SRM))." [PSI:MS]
comment: This term was made obsolete because it's recommended to use MS1 spectrum (MS:1000579), MSn spectrum (MS:1000580), CRM spectrum (MS:1000581), SIM spectrum (MS:1000582) or SRM spectrum (MS:1000583) instead.
xref: value-type:xsd\:string "The allowed value-type for this CV term."
is_a: MS:1002101 ! ProteomeDiscoverer input parameter
is_obsolete: true */

#define MS_PROTEOMEDISCOVERER_SPECTRUM_SELECTOR_TOTAL_INTENSITY_THRESHOLD_ID "MS:1001615"
#define MS_PROTEOMEDISCOVERER_SPECTRUM_SELECTOR_TOTAL_INTENSITY_THRESHOLD_NAME "ProteomeDiscoverer:Spectrum Selector:Total Intensity Threshold"
/* def: "Used to filter out tandem mass spectra that have a total intensity current(sum of the intensities of all peaks in a spectrum) below the specified value." [PSI:MS]
xref: value-type:xsd\:float "The allowed value-type for this CV term."
is_a: MS:1002101 ! ProteomeDiscoverer input parameter */

#define MS_PROTEOMEDISCOVERER_SPECTRUM_SELECTOR_UNRECOGNIZED_ACTIVATION_TYPE_REPLACEMENTS_ID "MS:1001616"
#define MS_PROTEOMEDISCOVERER_SPECTRUM_SELECTOR_UNRECOGNIZED_ACTIVATION_TYPE_REPLACEMENTS_NAME "ProteomeDiscoverer:Spectrum Selector:Unrecognized Activation Type Replacements"
/* def: "Specifies the fragmentation method to use in the search algorithm if it is not included in the scan header." [PSI:MS]
xref: value-type:xsd\:string "The allowed value-type for this CV term."
is_a: MS:1002101 ! ProteomeDiscoverer input parameter */

#define MS_PROTEOMEDISCOVERER_SPECTRUM_SELECTOR_UNRECOGNIZED_CHARGE_REPLACEMENTS_ID "MS:1001617"
#define MS_PROTEOMEDISCOVERER_SPECTRUM_SELECTOR_UNRECOGNIZED_CHARGE_REPLACEMENTS_NAME "ProteomeDiscoverer:Spectrum Selector:Unrecognized Charge Replacements"
/* def: "Specifies the charge state of the precursor ions, if it is not defined in the scan header." [PSI:MS]
xref: value-type:xsd\:int "The allowed value-type for this CV term."
is_a: MS:1002101 ! ProteomeDiscoverer input parameter */

#define MS_PROTEOMEDISCOVERER_SPECTRUM_SELECTOR_UNRECOGNIZED_MASS_ANALYZER_REPLACEMENTS_ID "MS:1001618"
#define MS_PROTEOMEDISCOVERER_SPECTRUM_SELECTOR_UNRECOGNIZED_MASS_ANALYZER_REPLACEMENTS_NAME "ProteomeDiscoverer:Spectrum Selector:Unrecognized Mass Analyzer Replacements"
/* def: "Specifies the mass spectrometer to use to produce the spectra, if it is not included in the scan header." [PSI:MS]
xref: value-type:xsd\:string "The allowed value-type for this CV term."
is_a: MS:1002101 ! ProteomeDiscoverer input parameter */

#define MS_PROTEOMEDISCOVERER_SPECTRUM_SELECTOR_UNRECOGNIZED_MS_ORDER_REPLACEMENTS_ID "MS:1001619"
#define MS_PROTEOMEDISCOVERER_SPECTRUM_SELECTOR_UNRECOGNIZED_MS_ORDER_REPLACEMENTS_NAME "ProteomeDiscoverer:Spectrum Selector:Unrecognized MS Order Replacements"
/* def: "Specifies the MS scan order used to produce the product spectra, if it is not included in the scan header." [PSI:MS]
xref: value-type:xsd\:string "The allowed value-type for this CV term."
is_a: MS:1002101 ! ProteomeDiscoverer input parameter */

#define MS_PROTEOMEDISCOVERER_SPECTRUM_SELECTOR_UNRECOGNIZED_POLARITY_REPLACEMENTS_ID "MS:1001620"
#define MS_PROTEOMEDISCOVERER_SPECTRUM_SELECTOR_UNRECOGNIZED_POLARITY_REPLACEMENTS_NAME "ProteomeDiscoverer:Spectrum Selector:Unrecognized Polarity Replacements"
/* def: "Specifies the polarity of the ions monitored if it is not included in the scan header." [PSI:MS]
xref: value-type:xsd\:string "The allowed value-type for this CV term."
is_a: MS:1002101 ! ProteomeDiscoverer input parameter */

#define MS_PROTEOMEDISCOVERER_SPECTRUM_SELECTOR_UPPER_RT_LIMIT_ID "MS:1001621"
#define MS_PROTEOMEDISCOVERER_SPECTRUM_SELECTOR_UPPER_RT_LIMIT_NAME "ProteomeDiscoverer:Spectrum Selector:Upper RT Limit"
/* def: "Upper retention-time limit." [PSI:MS]
xref: value-type:xsd\:float "The allowed value-type for this CV term."
is_a: MS:1002101 ! ProteomeDiscoverer input parameter */

#define MS_PROTEOMEDISCOVERER_NON_FRAGMENT_FILTER_MASS_WINDOW_OFFSET_ID "MS:1001622"
#define MS_PROTEOMEDISCOVERER_NON_FRAGMENT_FILTER_MASS_WINDOW_OFFSET_NAME "ProteomeDiscoverer:Non-Fragment Filter:Mass Window Offset"
/* def: "Specifies the size of the mass-to-charge ratio (m/z) window in daltons used to remove precursors." [PSI:MS]
xref: value-type:xsd\:float "The allowed value-type for this CV term."
is_a: MS:1002101 ! ProteomeDiscoverer input parameter */

#define MS_PROTEOMEDISCOVERER_NON_FRAGMENT_FILTER_MAXIMUM_NEUTRAL_LOSS_MASS_ID "MS:1001623"
#define MS_PROTEOMEDISCOVERER_NON_FRAGMENT_FILTER_MAXIMUM_NEUTRAL_LOSS_MASS_NAME "ProteomeDiscoverer:Non-Fragment Filter:Maximum Neutral Loss Mass"
/* def: "Maximum allowed mass of a neutral loss." [PSI:MS]
xref: value-type:xsd\:float "The allowed value-type for this CV term."
is_a: MS:1002101 ! ProteomeDiscoverer input parameter */

#define MS_PROTEOMEDISCOVERER_NON_FRAGMENT_FILTER_REMOVE_CHARGE_REDUCED_PRECURSOR_ID "MS:1001624"
#define MS_PROTEOMEDISCOVERER_NON_FRAGMENT_FILTER_REMOVE_CHARGE_REDUCED_PRECURSOR_NAME "ProteomeDiscoverer:Non-Fragment Filter:Remove Charge Reduced Precursor"
/* def: "Determines whether the charge-reduced precursor peaks found in an ETD or ECD spectrum are removed." [PSI:MS]
xref: value-type:xsd\:boolean "The allowed value-type for this CV term."
is_a: MS:1002101 ! ProteomeDiscoverer input parameter */

#define MS_PROTEOMEDISCOVERER_NON_FRAGMENT_FILTER_REMOVE_NEUTRAL_LOSS_PEAKS_ID "MS:1001625"
#define MS_PROTEOMEDISCOVERER_NON_FRAGMENT_FILTER_REMOVE_NEUTRAL_LOSS_PEAKS_NAME "ProteomeDiscoverer:Non-Fragment Filter:Remove Neutral Loss Peaks"
/* def: "Determines whether neutral loss peaks are removed from ETD and ECD spectra." [PSI:MS]
xref: value-type:xsd\:boolean "The allowed value-type for this CV term."
is_a: MS:1002101 ! ProteomeDiscoverer input parameter */

#define MS_PROTEOMEDISCOVERER_NON_FRAGMENT_FILTER_REMOVE_ONLY_KNOWN_MASSES_ID "MS:1001626"
#define MS_PROTEOMEDISCOVERER_NON_FRAGMENT_FILTER_REMOVE_ONLY_KNOWN_MASSES_NAME "ProteomeDiscoverer:Non-Fragment Filter:Remove Only Known Masses"
/* def: "Determines whether overtone peaks are removed from LTQ FT or LTQ FT Ultra ECD spectra." [PSI:MS]
xref: value-type:xsd\:boolean "The allowed value-type for this CV term."
is_a: MS:1002101 ! ProteomeDiscoverer input parameter */

#define MS_PROTEOMEDISCOVERER_NON_FRAGMENT_FILTER_REMOVE_PRECURSOR_OVERTONES_ID "MS:1001627"
#define MS_PROTEOMEDISCOVERER_NON_FRAGMENT_FILTER_REMOVE_PRECURSOR_OVERTONES_NAME "ProteomeDiscoverer:Non-Fragment Filter:Remove Precursor Overtones"
/* def: "Determines whether precursor overtone peaks in the spectrum are removed from the input spectrum." [PSI:MS]
xref: value-type:xsd\:boolean "The allowed value-type for this CV term."
is_a: MS:1002101 ! ProteomeDiscoverer input parameter */

#define MS_PROTEOMEDISCOVERER_NON_FRAGMENT_FILTER_REMOVE_PRECURSOR_PEAK_ID "MS:1001628"
#define MS_PROTEOMEDISCOVERER_NON_FRAGMENT_FILTER_REMOVE_PRECURSOR_PEAK_NAME "ProteomeDiscoverer:Non-Fragment Filter:Remove Precursor Peak"
/* def: "Determines whether precursor artifact peaks from the MS/MS input spectra are removed." [PSI:MS]
xref: value-type:xsd\:boolean "The allowed value-type for this CV term."
is_a: MS:1002101 ! ProteomeDiscoverer input parameter */

#define MS_PROTEOMEDISCOVERER_SPECTRUM_GROUPER_ALLOW_MASS_ANALYZER_MISMATCH_ID "MS:1001629"
#define MS_PROTEOMEDISCOVERER_SPECTRUM_GROUPER_ALLOW_MASS_ANALYZER_MISMATCH_NAME "ProteomeDiscoverer:Spectrum Grouper:Allow Mass Analyzer Mismatch"
/* def: "Determines whether the fragment spectrum for scans with the same precursor mass is grouped, regardless of mass analyzer and activation type." [PSI:MS]
xref: value-type:xsd\:boolean "The allowed value-type for this CV term."
is_a: MS:1002101 ! ProteomeDiscoverer input parameter */

#define MS_PROTEOMEDISCOVERER_SPECTRUM_GROUPER_ALLOW_MS_ORDER_MISMATCH_ID "MS:1001630"
#define MS_PROTEOMEDISCOVERER_SPECTRUM_GROUPER_ALLOW_MS_ORDER_MISMATCH_NAME "ProteomeDiscoverer:Spectrum Grouper:Allow MS Order Mismatch"
/* def: "Determines whether spectra from different MS order scans can be grouped together." [PSI:MS]
xref: value-type:xsd\:boolean "The allowed value-type for this CV term."
is_a: MS:1002101 ! ProteomeDiscoverer input parameter */

// #define MS_PROTEOMEDISCOVERER_SPECTRUM_GROUPER_MAX_RT_DIFFERENCE_ID "MS:1001631"
// #define MS_PROTEOMEDISCOVERER_SPECTRUM_GROUPER_MAX_RT_DIFFERENCE_NAME "ProteomeDiscoverer:Spectrum Grouper:Max RT Difference"
/* def: "OBSOLETE Chromatographic window where precursors to be grouped must reside to be considered the same species." [PSI:MS]
comment: This term was made obsolete because it's recommended to use retention time window width (MS:1001907) instead.
xref: value-type:xsd\:float "The allowed value-type for this CV term."
is_a: MS:1002101 ! ProteomeDiscoverer input parameter
is_obsolete: true */

#define MS_PROTEOMEDISCOVERER_SPECTRUM_GROUPER_PRECURSOR_MASS_CRITERION_ID "MS:1001632"
#define MS_PROTEOMEDISCOVERER_SPECTRUM_GROUPER_PRECURSOR_MASS_CRITERION_NAME "ProteomeDiscoverer:Spectrum Grouper:Precursor Mass Criterion"
/* def: "Groups spectra measured within the given mass and retention-time tolerances into a single spectrum for analysis." [PSI:MS]
xref: value-type:xsd\:string "The allowed value-type for this CV term."
is_a: MS:1002101 ! ProteomeDiscoverer input parameter */

#define MS_PROTEOMEDISCOVERER_XTRACT_HIGHEST_CHARGE_ID "MS:1001633"
#define MS_PROTEOMEDISCOVERER_XTRACT_HIGHEST_CHARGE_NAME "ProteomeDiscoverer:Xtract:Highest Charge"
/* def: "Highest charge state that is allowed for the deconvolution of multiply charged data." [PSI:MS]
xref: value-type:xsd\:int "The allowed value-type for this CV term."
is_a: MS:1002101 ! ProteomeDiscoverer input parameter */

// #define MS_PROTEOMEDISCOVERER_XTRACT_HIGHEST_MZ_ID "MS:1001634"
// #define MS_PROTEOMEDISCOVERER_XTRACT_HIGHEST_MZ_NAME "ProteomeDiscoverer:Xtract:Highest MZ"
/* def: "OBSOLETE Highest mass-to-charge (mz) value for spectral peaks in the measured spectrum that are considered for Xtract." [PSI:MS]
comment: This term was made obsolete because it's recommended to use scan window upper limit (MS:1000500) instead.
xref: value-type:xsd\:float "The allowed value-type for this CV term."
is_a: MS:1002101 ! ProteomeDiscoverer input parameter
is_obsolete: true */

#define MS_PROTEOMEDISCOVERER_XTRACT_LOWEST_CHARGE_ID "MS:1001635"
#define MS_PROTEOMEDISCOVERER_XTRACT_LOWEST_CHARGE_NAME "ProteomeDiscoverer:Xtract:Lowest Charge"
/* def: "Lowest charge state that is allowed for the deconvolution of multiply charged data." [PSI:MS]
xref: value-type:xsd\:int "The allowed value-type for this CV term."
is_a: MS:1002101 ! ProteomeDiscoverer input parameter */

// #define MS_PROTEOMEDISCOVERER_XTRACT_LOWEST_MZ_ID "MS:1001636"
// #define MS_PROTEOMEDISCOVERER_XTRACT_LOWEST_MZ_NAME "ProteomeDiscoverer:Xtract:Lowest MZ"
/* def: "OBSOLETE Lowest mass-to-charge (mz) value for spectral peaks in the measured spectrum that are considered for Xtract." [PSI:MS]
comment: This term was made obsolete because it's recommended to use scan window lower limit (MS:1000501) instead.
xref: value-type:xsd\:float "The allowed value-type for this CV term."
is_a: MS:1002101 ! ProteomeDiscoverer input parameter
is_obsolete: true */

#define MS_PROTEOMEDISCOVERER_XTRACT_MONOISOTOPIC_MASS_ONLY_ID "MS:1001637"
#define MS_PROTEOMEDISCOVERER_XTRACT_MONOISOTOPIC_MASS_ONLY_NAME "ProteomeDiscoverer:Xtract:Monoisotopic Mass Only"
/* def: "Determines whether the isotopic pattern, i.e. all isotopes of a mass are removed from the spectrum." [PSI:MS]
xref: value-type:xsd\:boolean "The allowed value-type for this CV term."
is_a: MS:1002101 ! ProteomeDiscoverer input parameter */

#define MS_PROTEOMEDISCOVERER_XTRACT_OVERLAPPING_REMAINDER_ID "MS:1001638"
#define MS_PROTEOMEDISCOVERER_XTRACT_OVERLAPPING_REMAINDER_NAME "ProteomeDiscoverer:Xtract:Overlapping Remainder"
/* def: "Fraction of the more abundant peak that an overlapping multiplet must exceed in order to be processed (deconvoluted)." [PSI:MS]
xref: value-type:xsd\:float "The allowed value-type for this CV term."
is_a: MS:1002101 ! ProteomeDiscoverer input parameter */

#define MS_PROTEOMEDISCOVERER_XTRACT_REQUIRED_FITTING_ACCURACY_ID "MS:1001639"
#define MS_PROTEOMEDISCOVERER_XTRACT_REQUIRED_FITTING_ACCURACY_NAME "ProteomeDiscoverer:Xtract:Required Fitting Accuracy"
/* def: "Accuracy required for a pattern fit to be considered valid." [PSI:MS]
xref: value-type:xsd\:float "The allowed value-type for this CV term."
is_a: MS:1002101 ! ProteomeDiscoverer input parameter */

#define MS_PROTEOMEDISCOVERER_XTRACT_RESOLUTION_AT_400_ID "MS:1001640"
#define MS_PROTEOMEDISCOVERER_XTRACT_RESOLUTION_AT_400_NAME "ProteomeDiscoverer:Xtract:Resolution At 400"
/* def: "Resolution at mass 400." [PSI:MS]
xref: value-type:xsd\:float "The allowed value-type for this CV term."
is_a: MS:1002101 ! ProteomeDiscoverer input parameter */

#define MS_PROTEOMEDISCOVERER_LOWEST_CHARGE_STATE_ID "MS:1001641"
#define MS_PROTEOMEDISCOVERER_LOWEST_CHARGE_STATE_NAME "ProteomeDiscoverer:Lowest Charge State"
/* def: "Minimum charge state below which peptides are filtered out." [PSI:MS]
xref: value-type:xsd\:int "The allowed value-type for this CV term."
is_a: MS:1002101 ! ProteomeDiscoverer input parameter */

#define MS_PROTEOMEDISCOVERER_HIGHEST_CHARGE_STATE_ID "MS:1001642"
#define MS_PROTEOMEDISCOVERER_HIGHEST_CHARGE_STATE_NAME "ProteomeDiscoverer:Highest Charge State"
/* def: "Maximum charge above which peptides are filtered out." [PSI:MS]
xref: value-type:xsd\:int "The allowed value-type for this CV term."
is_a: MS:1002101 ! ProteomeDiscoverer input parameter */

#define MS_PROTEOMEDISCOVERER_SPECTRUM_SCORE_FILTER_LET_PASS_ABOVE_SCORES_ID "MS:1001643"
#define MS_PROTEOMEDISCOVERER_SPECTRUM_SCORE_FILTER_LET_PASS_ABOVE_SCORES_NAME "ProteomeDiscoverer:Spectrum Score Filter:Let Pass Above Scores"
/* def: "Determines whether spectra with scores above the threshold score are retained rather than filtered out." [PSI:MS]
xref: value-type:xsd\:boolean "The allowed value-type for this CV term."
is_a: MS:1002101 ! ProteomeDiscoverer input parameter */

#define MS_PROTEOMEDISCOVERER_DYNAMIC_MODIFICATIONS_ID "MS:1001644"
#define MS_PROTEOMEDISCOVERER_DYNAMIC_MODIFICATIONS_NAME "ProteomeDiscoverer:Dynamic Modifications"
/* def: "Determine dynamic post-translational modifications (PTMs)." [PSI:MS]
xref: value-type:xsd\:string "The allowed value-type for this CV term."
is_a: MS:1002101 ! ProteomeDiscoverer input parameter */

#define MS_PROTEOMEDISCOVERER_STATIC_MODIFICATIONS_ID "MS:1001645"
#define MS_PROTEOMEDISCOVERER_STATIC_MODIFICATIONS_NAME "ProteomeDiscoverer:Static Modifications"
/* def: "Static Modification to all occurrences of a named amino acid." [PSI:MS]
xref: value-type:xsd\:string "The allowed value-type for this CV term."
is_a: MS:1002101 ! ProteomeDiscoverer input parameter */

// #define MS_PROTEOMEDISCOVERER_MASCOT_DECOY_SEARCH_ID "MS:1001646"
// #define MS_PROTEOMEDISCOVERER_MASCOT_DECOY_SEARCH_NAME "ProteomeDiscoverer:Mascot:Decoy Search"
/* def: "OBSOLETE Determines whether the Proteome Discoverer application searches an additional decoy database." [PSI:MS]
comment: This term was made obsolete because it's recommended to use quality estimation with decoy database (MS:1001194) instead.
xref: value-type:xsd\:boolean "The allowed value-type for this CV term."
is_a: MS:1002101 ! ProteomeDiscoverer input parameter
is_obsolete: true */

#define MS_PROTEOMEDISCOVERER_MASCOT_ERROR_TOLERANT_SEARCH_ID "MS:1001647"
#define MS_PROTEOMEDISCOVERER_MASCOT_ERROR_TOLERANT_SEARCH_NAME "ProteomeDiscoverer:Mascot:Error tolerant Search"
/* def: "Determines whether to search error-tolerant." [PSI:MS]
xref: value-type:xsd\:boolean "The allowed value-type for this CV term."
is_a: MS:1002101 ! ProteomeDiscoverer input parameter */

#define MS_PROTEOMEDISCOVERER_MASCOT_MAX_MGF_FILE_SIZE_ID "MS:1001648"
#define MS_PROTEOMEDISCOVERER_MASCOT_MAX_MGF_FILE_SIZE_NAME "ProteomeDiscoverer:Mascot:Max MGF File Size"
/* def: "Maximum size of the .mgf (Mascot Generic Format) file in MByte." [PSI:MS]
xref: value-type:xsd\:int "The allowed value-type for this CV term."
is_a: MS:1002101 ! ProteomeDiscoverer input parameter */

#define MS_PROTEOMEDISCOVERER_MASCOT_MASCOT_SERVER_URL_ID "MS:1001649"
#define MS_PROTEOMEDISCOVERER_MASCOT_MASCOT_SERVER_URL_NAME "ProteomeDiscoverer:Mascot:Mascot Server URL"
/* def: "URL (Uniform resource Locator) of the Mascot server." [PSI:MS]
xref: value-type:xsd\:string "The allowed value-type for this CV term."
is_a: MS:1002101 ! ProteomeDiscoverer input parameter */

#define MS_PROTEOMEDISCOVERER_MASCOT_NUMBER_OF_ATTEMPTS_TO_SUBMIT_THE_SEARCH_ID "MS:1001650"
#define MS_PROTEOMEDISCOVERER_MASCOT_NUMBER_OF_ATTEMPTS_TO_SUBMIT_THE_SEARCH_NAME "ProteomeDiscoverer:Mascot:Number of attempts to submit the search"
/* def: "Number of attempts to submit the Mascot search." [PSI:MS]
xref: value-type:xsd\:int "The allowed value-type for this CV term."
is_a: MS:1002101 ! ProteomeDiscoverer input parameter */

#define MS_PROTEOMEDISCOVERER_MASCOT_X_STATIC_MODIFICATION_ID "MS:1001651"
#define MS_PROTEOMEDISCOVERER_MASCOT_X_STATIC_MODIFICATION_NAME "ProteomeDiscoverer:Mascot:X Static Modification"
/* def: "Number of attempts to submit the Mascot search." [PSI:MS]
xref: value-type:xsd\:string "The allowed value-type for this CV term."
is_a: MS:1002101 ! ProteomeDiscoverer input parameter */

// #define MS_PROTEOMEDISCOVERER_MASCOT_USER_NAME_ID "MS:1001652"
// #define MS_PROTEOMEDISCOVERER_MASCOT_USER_NAME_NAME "ProteomeDiscoverer:Mascot:User Name"
/* def: "OBSOLETE Name of the user submitting the Mascot search." [PSI:MS]
comment: This term was made obsolete because it's recommended to use researcher (MS:1001271) instead.
xref: value-type:xsd\:string "The allowed value-type for this CV term."
is_a: MS:1002101 ! ProteomeDiscoverer input parameter
is_obsolete: true */

#define MS_PROTEOMEDISCOVERER_MASCOT_TIME_INTERVAL_BETWEEN_ATTEMPTS_TO_SUBMIT_A_SEARCH_ID "MS:1001653"
#define MS_PROTEOMEDISCOVERER_MASCOT_TIME_INTERVAL_BETWEEN_ATTEMPTS_TO_SUBMIT_A_SEARCH_NAME "ProteomeDiscoverer:Mascot:Time interval between attempts to submit a search"
/* def: "Time interval between attempts to submit a search in seconds." [PSI:MS]
xref: value-type:xsd\:string "The allowed value-type for this CV term."
is_a: MS:1002101 ! ProteomeDiscoverer input parameter */

// #define MS_PROTEOMEDISCOVERER_ENZYME_NAME_ID "MS:1001654"
// #define MS_PROTEOMEDISCOVERER_ENZYME_NAME_NAME "ProteomeDiscoverer:Enzyme Name"
/* def: "OBSOLETE Specifies the enzyme reagent used for protein digestion." [PSI:MS]
comment: This term was made obsolete because it's recommended to use cleavage agent name (MS:1001045) instead.
xref: value-type:xsd\:string "The allowed value-type for this CV term."
is_a: MS:1002101 ! ProteomeDiscoverer input parameter
is_obsolete: true */

// #define MS_PROTEOMEDISCOVERER_FRAGMENT_MASS_TOLERANCE_ID "MS:1001655"
// #define MS_PROTEOMEDISCOVERER_FRAGMENT_MASS_TOLERANCE_NAME "ProteomeDiscoverer:Fragment Mass Tolerance"
/* def: "OBSOLETE Mass tolerance used for matching fragment peaks in Da or mmu." [PSI:MS]
comment: This term was made obsolete because it's recommended to use search tolerance minus value (MS:1001413) or search tolerance plus value (MS:1001412) instead.
xref: value-type:xsd\:float "The allowed value-type for this CV term."
is_a: MS:1002101 ! ProteomeDiscoverer input parameter
is_obsolete: true */

#define MS_MASCOT_INSTRUMENT_ID "MS:1001656"
#define MS_MASCOT_INSTRUMENT_NAME "Mascot:Instrument"
/* def: "Type of instrument used to acquire the data in the raw file." [PSI:MS]
xref: value-type:xsd\:string "The allowed value-type for this CV term."
is_a: MS:1002095 ! Mascot input parameter */

#define MS_PROTEOMEDISCOVERER_MAXIMUM_MISSED_CLEAVAGE_SITES_ID "MS:1001657"
#define MS_PROTEOMEDISCOVERER_MAXIMUM_MISSED_CLEAVAGE_SITES_NAME "ProteomeDiscoverer:Maximum Missed Cleavage Sites"
/* def: "Maximum number of missed cleavage sites to consider during the digest." [PSI:MS]
xref: value-type:xsd\:int "The allowed value-type for this CV term."
is_a: MS:1002101 ! ProteomeDiscoverer input parameter */

#define MS_PROTEOMEDISCOVERER_MASCOT_PEPTIDE_CUTOFF_SCORE_ID "MS:1001658"
#define MS_PROTEOMEDISCOVERER_MASCOT_PEPTIDE_CUTOFF_SCORE_NAME "ProteomeDiscoverer:Mascot:Peptide CutOff Score"
/* def: "Minimum score in the IonScore column that each peptide must exceed in order to be reported." [PSI:MS]
xref: value-type:xsd\:float "The allowed value-type for this CV term."
is_a: MS:1002101 ! ProteomeDiscoverer input parameter */

// #define MS_PROTEOMEDISCOVERER_PRECURSOR_MASS_TOLERANCE_ID "MS:1001659"
// #define MS_PROTEOMEDISCOVERER_PRECURSOR_MASS_TOLERANCE_NAME "ProteomeDiscoverer:Precursor Mass Tolerance"
/* def: "OBSOLETE Mass window for which precursor ions are considered to be the same species." [PSI:MS]
comment: This term was made obsolete because it's recommended to use search tolerance minus value (MS:1001413) or search tolerance plus value (MS:1001412) instead.
xref: value-type:xsd\:float "The allowed value-type for this CV term."
is_a: MS:1002101 ! ProteomeDiscoverer input parameter
is_obsolete: true */

#define MS_PROTEOMEDISCOVERER_MASCOT_PROTEIN_CUTOFF_SCORE_ID "MS:1001660"
#define MS_PROTEOMEDISCOVERER_MASCOT_PROTEIN_CUTOFF_SCORE_NAME "ProteomeDiscoverer:Mascot:Protein CutOff Score"
/* def: "Minimum protein score in the IonScore column that each protein must exceed in order to be reported." [PSI:MS]
xref: value-type:xsd\:float "The allowed value-type for this CV term."
is_a: MS:1002101 ! ProteomeDiscoverer input parameter */

// #define MS_PROTEOMEDISCOVERER_PROTEIN_DATABASE_ID "MS:1001661"
// #define MS_PROTEOMEDISCOVERER_PROTEIN_DATABASE_NAME "ProteomeDiscoverer:Protein Database"
/* def: "OBSOLETE Database to use in the search (configured on the Mascot server)." [PSI:MS]
comment: This term was made obsolete because it's recommended to use database name (MS:1001013) instead.
xref: value-type:xsd\:string "The allowed value-type for this CV term."
is_a: MS:1002101 ! ProteomeDiscoverer input parameter
is_obsolete: true */

#define MS_PROTEOMEDISCOVERER_MASCOT_PROTEIN_RELEVANCE_FACTOR_ID "MS:1001662"
#define MS_PROTEOMEDISCOVERER_MASCOT_PROTEIN_RELEVANCE_FACTOR_NAME "ProteomeDiscoverer:Mascot:Protein Relevance Factor"
/* def: "Specifies a factor that is used in calculating a threshold that determines whether a protein appears in the results report." [PSI:MS]
xref: value-type:xsd\:float "The allowed value-type for this CV term."
is_a: MS:1002101 ! ProteomeDiscoverer input parameter */

#define MS_PROTEOMEDISCOVERER_TARGET_FDR_RELAXED_ID "MS:1001663"
#define MS_PROTEOMEDISCOVERER_TARGET_FDR_RELAXED_NAME "ProteomeDiscoverer:Target FDR Relaxed"
/* def: "Specifies the relaxed target false discovery rate (FDR, 0.0 - 1.0) for peptide hits with moderate confidence." [PSI:MS]
xref: value-type:xsd\:float "The allowed value-type for this CV term."
is_a: MS:1002101 ! ProteomeDiscoverer input parameter */

#define MS_PROTEOMEDISCOVERER_TARGET_FDR_STRICT_ID "MS:1001664"
#define MS_PROTEOMEDISCOVERER_TARGET_FDR_STRICT_NAME "ProteomeDiscoverer:Target FDR Strict"
/* def: "Specifies the strict target false discovery rate (FDR, 0.0 - 1.0) for peptide hits with high confidence." [PSI:MS]
xref: value-type:xsd\:float "The allowed value-type for this CV term."
is_a: MS:1002101 ! ProteomeDiscoverer input parameter */

// #define MS_PROTEOMEDISCOVERER_MASCOT_TAXONOMY_ID "MS:1001665"
// #define MS_PROTEOMEDISCOVERER_MASCOT_TAXONOMY_NAME "ProteomeDiscoverer:Mascot:Taxonomy"
/* def: "OBSOLETE Limits searches to entries from a particular species or group of species." [PSI:MS]
comment: This term was made obsolete because it's recommended to use taxonomy: scientific name (MS:1001469) instead.
xref: value-type:xsd\:string "The allowed value-type for this CV term."
is_a: MS:1002101 ! ProteomeDiscoverer input parameter
is_obsolete: true */

// #define MS_PROTEOMEDISCOVERER_USE_AVERAGE_PRECURSOR_MASS_ID "MS:1001666"
// #define MS_PROTEOMEDISCOVERER_USE_AVERAGE_PRECURSOR_MASS_NAME "ProteomeDiscoverer:Use Average Precursor Mass"
/* def: "OBSOLETE Use average mass for the precursor." [PSI:MS]
comment: This term was made obsolete because it's recommended to use parent mass type average (MS:1001212) instead.
xref: value-type:xsd\:boolean "The allowed value-type for this CV term."
is_a: MS:1002101 ! ProteomeDiscoverer input parameter
is_obsolete: true */

// #define MS_MASCOT_USE_MUDPIT_SCORING_ID "MS:1001667"
// #define MS_MASCOT_USE_MUDPIT_SCORING_NAME "Mascot:use MudPIT scoring"
/* def: "OBSOLETE Determines whether to use MudPIT or normal scoring." [PSI:MS]
comment: This term was made obsolete because it's recommended to use Mascot:ProteinScoringMethod (MS:1001318) instead.
xref: value-type:xsd\:boolean "The allowed value-type for this CV term."
is_a: MS:1002095 ! Mascot input parameter
is_obsolete: true */

#define MS_PROTEOMEDISCOVERER_ABSOLUTE_XCORR_THRESHOLD_ID "MS:1001668"
#define MS_PROTEOMEDISCOVERER_ABSOLUTE_XCORR_THRESHOLD_NAME "ProteomeDiscoverer:Absolute XCorr Threshold"
/* def: "Minimum cross-correlation threshold that determines whether peptides in an .srf file are imported." [PSI:MS]
xref: value-type:xsd\:float "The allowed value-type for this CV term."
is_a: MS:1002101 ! ProteomeDiscoverer input parameter */

#define MS_PROTEOMEDISCOVERER_SEQUEST_CALCULATE_PROBABILITY_SCORE_ID "MS:1001669"
#define MS_PROTEOMEDISCOVERER_SEQUEST_CALCULATE_PROBABILITY_SCORE_NAME "ProteomeDiscoverer:SEQUEST:Calculate Probability Score"
/* def: "Determines whether to calculate a probability score for every peptide match." [PSI:MS]
xref: value-type:xsd\:boolean "The allowed value-type for this CV term."
is_a: MS:1002101 ! ProteomeDiscoverer input parameter */

#define MS_PROTEOMEDISCOVERER_SEQUEST_CTERMINAL_MODIFICATION_ID "MS:1001670"
#define MS_PROTEOMEDISCOVERER_SEQUEST_CTERMINAL_MODIFICATION_NAME "ProteomeDiscoverer:SEQUEST:CTerminal Modification"
/* def: "Dynamic C-terminal modification that is used during the search." [PSI:MS]
xref: value-type:xsd\:string "The allowed value-type for this CV term."
is_a: MS:1002101 ! ProteomeDiscoverer input parameter */

#define MS_PROTEOMEDISCOVERER_SEQUEST_FRAGMENT_ION_CUTOFF_PERCENTAGE_ID "MS:1001671"
#define MS_PROTEOMEDISCOVERER_SEQUEST_FRAGMENT_ION_CUTOFF_PERCENTAGE_NAME "ProteomeDiscoverer:SEQUEST:Fragment Ion Cutoff Percentage"
/* def: "Percentage of the theoretical ions that must be found in order for a peptide to be scored and retained." [PSI:MS]
xref: value-type:xsd\:float "The allowed value-type for this CV term."
is_a: MS:1002101 ! ProteomeDiscoverer input parameter */

#define MS_PROTEOMEDISCOVERER_SEQUEST_MAX_IDENTICAL_MODIFICATIONS_PER_PEPTIDE_ID "MS:1001672"
#define MS_PROTEOMEDISCOVERER_SEQUEST_MAX_IDENTICAL_MODIFICATIONS_PER_PEPTIDE_NAME "ProteomeDiscoverer:SEQUEST:Max Identical Modifications Per Peptide"
/* def: "Maximum number of identical modifications that a single peptide can have." [PSI:MS]
xref: value-type:xsd\:int "The allowed value-type for this CV term."
is_a: MS:1002101 ! ProteomeDiscoverer input parameter */

#define MS_PROTEOMEDISCOVERER_MAX_MODIFICATIONS_PER_PEPTIDE_ID "MS:1001673"
#define MS_PROTEOMEDISCOVERER_MAX_MODIFICATIONS_PER_PEPTIDE_NAME "ProteomeDiscoverer:Max Modifications Per Peptide"
/* def: "Maximum number of different modifications that a peptide can have, e.g. because of steric hindrance." [PSI:MS]
xref: value-type:xsd\:int "The allowed value-type for this CV term."
is_a: MS:1002101 ! ProteomeDiscoverer input parameter */

#define MS_PROTEOMEDISCOVERER_SEQUEST_MAXIMUM_PEPTIDES_CONSIDERED_ID "MS:1001674"
#define MS_PROTEOMEDISCOVERER_SEQUEST_MAXIMUM_PEPTIDES_CONSIDERED_NAME "ProteomeDiscoverer:SEQUEST:Maximum Peptides Considered"
/* def: "Maximum number of peptides that are searched and scored per spectrum." [PSI:MS]
xref: value-type:xsd\:int "The allowed value-type for this CV term."
is_a: MS:1002101 ! ProteomeDiscoverer input parameter */

#define MS_PROTEOMEDISCOVERER_MAXIMUM_PEPTIDES_OUTPUT_ID "MS:1001675"
#define MS_PROTEOMEDISCOVERER_MAXIMUM_PEPTIDES_OUTPUT_NAME "ProteomeDiscoverer:Maximum Peptides Output"
/* def: "Maximum number of peptide matches reported per spectrum." [PSI:MS]
xref: value-type:xsd\:int "The allowed value-type for this CV term."
is_a: MS:1002101 ! ProteomeDiscoverer input parameter */

#define MS_PROTEOMEDISCOVERER_MAXIMUM_PROTEIN_REFERENCES_PER_PEPTIDE_ID "MS:1001676"
#define MS_PROTEOMEDISCOVERER_MAXIMUM_PROTEIN_REFERENCES_PER_PEPTIDE_NAME "ProteomeDiscoverer:Maximum Protein References Per Peptide"
/* def: "Maximum number of proteins that a single identified peptide can be associated with during protein assembly." [PSI:MS]
xref: value-type:xsd\:int "The allowed value-type for this CV term."
is_a: MS:1002101 ! ProteomeDiscoverer input parameter */

#define MS_PROTEOMEDISCOVERER_SEQUEST_NTERMINAL_MODIFICATION_ID "MS:1001677"
#define MS_PROTEOMEDISCOVERER_SEQUEST_NTERMINAL_MODIFICATION_NAME "ProteomeDiscoverer:SEQUEST:NTerminal Modification"
/* def: "Dynamic N-terminal modification that is used during the search." [PSI:MS]
xref: value-type:xsd\:string "The allowed value-type for this CV term."
is_a: MS:1002101 ! ProteomeDiscoverer input parameter */

#define MS_PROTEOMEDISCOVERER_PEPTIDE_CTERMINUS_ID "MS:1001678"
#define MS_PROTEOMEDISCOVERER_PEPTIDE_CTERMINUS_NAME "ProteomeDiscoverer:Peptide CTerminus"
/* def: "Static modification for the C terminal of the peptide used during the search." [PSI:MS]
xref: value-type:xsd\:string "The allowed value-type for this CV term."
is_a: MS:1002101 ! ProteomeDiscoverer input parameter */

#define MS_PROTEOMEDISCOVERER_PEPTIDE_NTERMINUS_ID "MS:1001679"
#define MS_PROTEOMEDISCOVERER_PEPTIDE_NTERMINUS_NAME "ProteomeDiscoverer:Peptide NTerminus"
/* def: "Static modification for the N terminal of the peptide used during the search." [PSI:MS]
xref: value-type:xsd\:string "The allowed value-type for this CV term."
is_a: MS:1002101 ! ProteomeDiscoverer input parameter */

#define MS_PROTEOMEDISCOVERER_SEQUEST_PEPTIDE_RELEVANCE_FACTOR_ID "MS:1001680"
#define MS_PROTEOMEDISCOVERER_SEQUEST_PEPTIDE_RELEVANCE_FACTOR_NAME "ProteomeDiscoverer:SEQUEST:Peptide Relevance Factor"
/* def: "Specifies a factor to apply to the protein score." [PSI:MS]
xref: value-type:xsd\:float "The allowed value-type for this CV term."
is_a: MS:1002101 ! ProteomeDiscoverer input parameter */

#define MS_PROTEOMEDISCOVERER_PROTEIN_RELEVANCE_THRESHOLD_ID "MS:1001681"
#define MS_PROTEOMEDISCOVERER_PROTEIN_RELEVANCE_THRESHOLD_NAME "ProteomeDiscoverer:Protein Relevance Threshold"
/* def: "Specifies a peptide threshold that determines whether the protein that it is a part of is scored and retained in the report." [PSI:MS]
xref: value-type:xsd\:float "The allowed value-type for this CV term."
is_a: MS:1002101 ! ProteomeDiscoverer input parameter */

// #define MS_PROTEOMEDISCOVERER_SEARCH_AGAINST_DECOY_DATABASE_ID "MS:1001682"
// #define MS_PROTEOMEDISCOVERER_SEARCH_AGAINST_DECOY_DATABASE_NAME "ProteomeDiscoverer:Search Against Decoy Database"
/* def: "OBSOLETE Determines whether the Proteome Discoverer application searches against a decoy database." [PSI:MS]
comment: This term was made obsolete because it's recommended to use quality estimation with decoy database (MS:1001194) instead.
xref: value-type:xsd\:boolean "The allowed value-type for this CV term."
is_a: MS:1002101 ! ProteomeDiscoverer input parameter
is_obsolete: true */

#define MS_PROTEOMEDISCOVERER_SEQUEST_USE_AVERAGE_FRAGMENT_MASSES_ID "MS:1001683"
#define MS_PROTEOMEDISCOVERER_SEQUEST_USE_AVERAGE_FRAGMENT_MASSES_NAME "ProteomeDiscoverer:SEQUEST:Use Average Fragment Masses"
/* def: "Use average masses for the fragments." [PSI:MS]
xref: value-type:xsd\:boolean "The allowed value-type for this CV term."
is_a: MS:1002101 ! ProteomeDiscoverer input parameter */

#define MS_PROTEOMEDISCOVERER_USE_NEUTRAL_LOSS_A_IONS_ID "MS:1001684"
#define MS_PROTEOMEDISCOVERER_USE_NEUTRAL_LOSS_A_IONS_NAME "ProteomeDiscoverer:Use Neutral Loss a Ions"
/* def: "Determines whether a ions with neutral loss are used for spectrum matching." [PSI:MS]
xref: value-type:xsd\:boolean "The allowed value-type for this CV term."
is_a: MS:1002101 ! ProteomeDiscoverer input parameter */

#define MS_PROTEOMEDISCOVERER_USE_NEUTRAL_LOSS_B_IONS_ID "MS:1001685"
#define MS_PROTEOMEDISCOVERER_USE_NEUTRAL_LOSS_B_IONS_NAME "ProteomeDiscoverer:Use Neutral Loss b Ions"
/* def: "Determines whether b ions with neutral loss are used for spectrum matching." [PSI:MS]
xref: value-type:xsd\:boolean "The allowed value-type for this CV term."
is_a: MS:1002101 ! ProteomeDiscoverer input parameter */

#define MS_PROTEOMEDISCOVERER_USE_NEUTRAL_LOSS_Y_IONS_ID "MS:1001686"
#define MS_PROTEOMEDISCOVERER_USE_NEUTRAL_LOSS_Y_IONS_NAME "ProteomeDiscoverer:Use Neutral Loss y Ions"
/* def: "Determines whether y ions with neutral loss are used for spectrum matching." [PSI:MS]
xref: value-type:xsd\:boolean "The allowed value-type for this CV term."
is_a: MS:1002101 ! ProteomeDiscoverer input parameter */

#define MS_PROTEOMEDISCOVERER_USE_NEUTRAL_LOSS_Z_IONS_ID "MS:1001687"
#define MS_PROTEOMEDISCOVERER_USE_NEUTRAL_LOSS_Z_IONS_NAME "ProteomeDiscoverer:Use Neutral Loss z Ions"
/* def: "Determines whether z ions with neutral loss are used for spectrum matching." [PSI:MS]
xref: value-type:xsd\:boolean "The allowed value-type for this CV term."
is_a: MS:1002101 ! ProteomeDiscoverer input parameter */

#define MS_PROTEOMEDISCOVERER_SEQUEST_WEIGHT_OF_A_IONS_ID "MS:1001688"
#define MS_PROTEOMEDISCOVERER_SEQUEST_WEIGHT_OF_A_IONS_NAME "ProteomeDiscoverer:SEQUEST:Weight of a Ions"
/* def: "Uses a ions for spectrum matching with this relative factor." [PSI:MS]
xref: value-type:xsd\:float "The allowed value-type for this CV term."
is_a: MS:1002101 ! ProteomeDiscoverer input parameter */

#define MS_PROTEOMEDISCOVERER_SEQUEST_WEIGHT_OF_B_IONS_ID "MS:1001689"
#define MS_PROTEOMEDISCOVERER_SEQUEST_WEIGHT_OF_B_IONS_NAME "ProteomeDiscoverer:SEQUEST:Weight of b Ions"
/* def: "Uses b ions for spectrum matching with this relative factor." [PSI:MS]
xref: value-type:xsd\:float "The allowed value-type for this CV term."
is_a: MS:1002101 ! ProteomeDiscoverer input parameter */

#define MS_PROTEOMEDISCOVERER_SEQUEST_WEIGHT_OF_C_IONS_ID "MS:1001690"
#define MS_PROTEOMEDISCOVERER_SEQUEST_WEIGHT_OF_C_IONS_NAME "ProteomeDiscoverer:SEQUEST:Weight of c Ions"
/* def: "Uses c ions for spectrum matching with this relative factor." [PSI:MS]
xref: value-type:xsd\:float "The allowed value-type for this CV term."
is_a: MS:1002101 ! ProteomeDiscoverer input parameter */

#define MS_PROTEOMEDISCOVERER_SEQUEST_WEIGHT_OF_D_IONS_ID "MS:1001691"
#define MS_PROTEOMEDISCOVERER_SEQUEST_WEIGHT_OF_D_IONS_NAME "ProteomeDiscoverer:SEQUEST:Weight of d Ions"
/* def: "Uses c ions for spectrum matching with this relative factor." [PSI:MS]
xref: value-type:xsd\:float "The allowed value-type for this CV term."
is_a: MS:1002101 ! ProteomeDiscoverer input parameter */

#define MS_PROTEOMEDISCOVERER_SEQUEST_WEIGHT_OF_V_IONS_ID "MS:1001692"
#define MS_PROTEOMEDISCOVERER_SEQUEST_WEIGHT_OF_V_IONS_NAME "ProteomeDiscoverer:SEQUEST:Weight of v Ions"
/* def: "Uses c ions for spectrum matching with this relative factor." [PSI:MS]
xref: value-type:xsd\:float "The allowed value-type for this CV term."
is_a: MS:1002101 ! ProteomeDiscoverer input parameter */

#define MS_PROTEOMEDISCOVERER_SEQUEST_WEIGHT_OF_W_IONS_ID "MS:1001693"
#define MS_PROTEOMEDISCOVERER_SEQUEST_WEIGHT_OF_W_IONS_NAME "ProteomeDiscoverer:SEQUEST:Weight of w Ions"
/* def: "Uses c ions for spectrum matching with this relative factor." [PSI:MS]
xref: value-type:xsd\:float "The allowed value-type for this CV term."
is_a: MS:1002101 ! ProteomeDiscoverer input parameter */

#define MS_PROTEOMEDISCOVERER_SEQUEST_WEIGHT_OF_X_IONS_ID "MS:1001694"
#define MS_PROTEOMEDISCOVERER_SEQUEST_WEIGHT_OF_X_IONS_NAME "ProteomeDiscoverer:SEQUEST:Weight of x Ions"
/* def: "Uses x ions for spectrum matching with this relative factor." [PSI:MS]
xref: value-type:xsd\:float "The allowed value-type for this CV term."
is_a: MS:1002101 ! ProteomeDiscoverer input parameter */

#define MS_PROTEOMEDISCOVERER_SEQUEST_WEIGHT_OF_Y_IONS_ID "MS:1001695"
#define MS_PROTEOMEDISCOVERER_SEQUEST_WEIGHT_OF_Y_IONS_NAME "ProteomeDiscoverer:SEQUEST:Weight of y Ions"
/* def: "Uses y ions for spectrum matching with this relative factor." [PSI:MS]
xref: value-type:xsd\:float "The allowed value-type for this CV term."
is_a: MS:1002101 ! ProteomeDiscoverer input parameter */

#define MS_PROTEOMEDISCOVERER_SEQUEST_WEIGHT_OF_Z_IONS_ID "MS:1001696"
#define MS_PROTEOMEDISCOVERER_SEQUEST_WEIGHT_OF_Z_IONS_NAME "ProteomeDiscoverer:SEQUEST:Weight of z Ions"
/* def: "Uses z ions for spectrum matching with this relative factor." [PSI:MS]
xref: value-type:xsd\:float "The allowed value-type for this CV term."
is_a: MS:1002101 ! ProteomeDiscoverer input parameter */

#define MS_PROTEOMEDISCOVERER_ZCORE_PROTEIN_SCORE_CUTOFF_ID "MS:1001697"
#define MS_PROTEOMEDISCOVERER_ZCORE_PROTEIN_SCORE_CUTOFF_NAME "ProteomeDiscoverer:ZCore:Protein Score Cutoff"
/* def: "Sets a minimum protein score that each protein must exceed in order to be reported." [PSI:MS]
xref: value-type:xsd\:float "The allowed value-type for this CV term."
is_a: MS:1002101 ! ProteomeDiscoverer input parameter */

#define MS_PROTEOMEDISCOVERER_REPORTER_IONS_QUANTIZER_INTEGRATION_METHOD_ID "MS:1001698"
#define MS_PROTEOMEDISCOVERER_REPORTER_IONS_QUANTIZER_INTEGRATION_METHOD_NAME "ProteomeDiscoverer:Reporter Ions Quantizer:Integration Method"
/* def: "Specifies which peak to select if more than one peak is found inside the integration window." [PSI:MS]
xref: value-type:xsd\:string "The allowed value-type for this CV term."
is_a: MS:1002101 ! ProteomeDiscoverer input parameter */

#define MS_PROTEOMEDISCOVERER_REPORTER_IONS_QUANTIZER_INTEGRATION_WINDOW_TOLERANCE_ID "MS:1001699"
#define MS_PROTEOMEDISCOVERER_REPORTER_IONS_QUANTIZER_INTEGRATION_WINDOW_TOLERANCE_NAME "ProteomeDiscoverer:Reporter Ions Quantizer:Integration Window Tolerance"
/* def: "Specifies the mass-to-charge window that enables one to look for the reporter peaks." [PSI:MS]
xref: value-type:xsd\:float "The allowed value-type for this CV term."
is_a: MS:1002101 ! ProteomeDiscoverer input parameter */

#define MS_PROTEOMEDISCOVERER_REPORTER_IONS_QUANTIZER_QUANTITATION_METHOD_ID "MS:1001700"
#define MS_PROTEOMEDISCOVERER_REPORTER_IONS_QUANTIZER_QUANTITATION_METHOD_NAME "ProteomeDiscoverer:Reporter Ions Quantizer:Quantitation Method"
/* def: "Quantitation method for isobarically labeled quantitation." [PSI:MS]
xref: value-type:xsd\:string "The allowed value-type for this CV term."
is_a: MS:1002101 ! ProteomeDiscoverer input parameter */

// #define MS_PROTEOMEDISCOVERER_SPECTRUM_EXPORTER_EXPORT_FORMAT_ID "MS:1001701"
// #define MS_PROTEOMEDISCOVERER_SPECTRUM_EXPORTER_EXPORT_FORMAT_NAME "ProteomeDiscoverer:Spectrum Exporter:Export Format"
/* def: "OBSOLETE Format of the exported spectra (dta, mgf or mzData)." [PSI:MS]
comment: This term was made obsolete because it's recommended to use one of the 'mass spectrometer file format' terms (MS:1000560) instead.
xref: value-type:xsd\:string "The allowed value-type for this CV term."
is_a: MS:1002101 ! ProteomeDiscoverer input parameter
is_obsolete: true */

#define MS_PROTEOMEDISCOVERER_SPECTRUM_EXPORTER_FILE_NAME_ID "MS:1001702"
#define MS_PROTEOMEDISCOVERER_SPECTRUM_EXPORTER_FILE_NAME_NAME "ProteomeDiscoverer:Spectrum Exporter:File name"
/* def: "Name of the output file that contains the exported data." [PSI:MS]
xref: value-type:xsd\:string "The allowed value-type for this CV term."
is_a: MS:1002101 ! ProteomeDiscoverer input parameter */

#define MS_PROTEOMEDISCOVERER_SEARCH_MODIFICATIONS_ONLY_FOR_IDENTIFIED_PROTEINS_ID "MS:1001703"
#define MS_PROTEOMEDISCOVERER_SEARCH_MODIFICATIONS_ONLY_FOR_IDENTIFIED_PROTEINS_NAME "ProteomeDiscoverer:Search Modifications Only For Identified Proteins"
/* def: "Influences the modifications search." [PSI:MS]
xref: value-type:xsd\:boolean "The allowed value-type for this CV term."
is_a: MS:1002101 ! ProteomeDiscoverer input parameter */

#define MS_PROTEOMEDISCOVERER_SEQUEST_STD_HIGH_CONFIDENCE_XCORR_CHARGE1_ID "MS:1001704"
#define MS_PROTEOMEDISCOVERER_SEQUEST_STD_HIGH_CONFIDENCE_XCORR_CHARGE1_NAME "ProteomeDiscoverer:SEQUEST:Std High Confidence XCorr Charge1"
/* def: "Standard high confidence XCorr parameter for charge = 1." [PSI:MS]
xref: value-type:xsd\:float "The allowed value-type for this CV term."
is_a: MS:1002101 ! ProteomeDiscoverer input parameter */

#define MS_PROTEOMEDISCOVERER_SEQUEST_STD_HIGH_CONFIDENCE_XCORR_CHARGE2_ID "MS:1001705"
#define MS_PROTEOMEDISCOVERER_SEQUEST_STD_HIGH_CONFIDENCE_XCORR_CHARGE2_NAME "ProteomeDiscoverer:SEQUEST:Std High Confidence XCorr Charge2"
/* def: "Standard high confidence XCorr parameter for charge = 2." [PSI:MS]
xref: value-type:xsd\:float "The allowed value-type for this CV term."
is_a: MS:1002101 ! ProteomeDiscoverer input parameter */

#define MS_PROTEOMEDISCOVERER_SEQUEST_STD_HIGH_CONFIDENCE_XCORR_CHARGE3_ID "MS:1001706"
#define MS_PROTEOMEDISCOVERER_SEQUEST_STD_HIGH_CONFIDENCE_XCORR_CHARGE3_NAME "ProteomeDiscoverer:SEQUEST:Std High Confidence XCorr Charge3"
/* def: "Standard high confidence XCorr parameter for charge = 3." [PSI:MS]
xref: value-type:xsd\:float "The allowed value-type for this CV term."
is_a: MS:1002101 ! ProteomeDiscoverer input parameter */

#define MS_PROTEOMEDISCOVERER_SEQUEST_STD_HIGH_CONFIDENCE_XCORR_CHARGE4_ID "MS:1001707"
#define MS_PROTEOMEDISCOVERER_SEQUEST_STD_HIGH_CONFIDENCE_XCORR_CHARGE4_NAME "ProteomeDiscoverer:SEQUEST:Std High Confidence XCorr Charge4"
/* def: "Standard high confidence XCorr parameter for charge >= 4." [PSI:MS]
xref: value-type:xsd\:float "The allowed value-type for this CV term."
is_a: MS:1002101 ! ProteomeDiscoverer input parameter */

#define MS_PROTEOMEDISCOVERER_SEQUEST_STD_MEDIUM_CONFIDENCE_XCORR_CHARGE1_ID "MS:1001708"
#define MS_PROTEOMEDISCOVERER_SEQUEST_STD_MEDIUM_CONFIDENCE_XCORR_CHARGE1_NAME "ProteomeDiscoverer:SEQUEST:Std Medium Confidence XCorr Charge1"
/* def: "Standard medium confidence XCorr parameter for charge = 1." [PSI:MS]
xref: value-type:xsd\:float "The allowed value-type for this CV term."
is_a: MS:1002101 ! ProteomeDiscoverer input parameter */

#define MS_PROTEOMEDISCOVERER_SEQUEST_STD_MEDIUM_CONFIDENCE_XCORR_CHARGE2_ID "MS:1001709"
#define MS_PROTEOMEDISCOVERER_SEQUEST_STD_MEDIUM_CONFIDENCE_XCORR_CHARGE2_NAME "ProteomeDiscoverer:SEQUEST:Std Medium Confidence XCorr Charge2"
/* def: "Standard medium confidence XCorr parameter for charge = 2." [PSI:MS]
xref: value-type:xsd\:float "The allowed value-type for this CV term."
is_a: MS:1002101 ! ProteomeDiscoverer input parameter */

#define MS_PROTEOMEDISCOVERER_SEQUEST_STD_MEDIUM_CONFIDENCE_XCORR_CHARGE3_ID "MS:1001710"
#define MS_PROTEOMEDISCOVERER_SEQUEST_STD_MEDIUM_CONFIDENCE_XCORR_CHARGE3_NAME "ProteomeDiscoverer:SEQUEST:Std Medium Confidence XCorr Charge3"
/* def: "Standard medium confidence XCorr parameter for charge = 3." [PSI:MS]
xref: value-type:xsd\:float "The allowed value-type for this CV term."
is_a: MS:1002101 ! ProteomeDiscoverer input parameter */

#define MS_PROTEOMEDISCOVERER_SEQUEST_STD_MEDIUM_CONFIDENCE_XCORR_CHARGE4_ID "MS:1001711"
#define MS_PROTEOMEDISCOVERER_SEQUEST_STD_MEDIUM_CONFIDENCE_XCORR_CHARGE4_NAME "ProteomeDiscoverer:SEQUEST:Std Medium Confidence XCorr Charge4"
/* def: "Standard medium confidence XCorr parameter for charge >= 4." [PSI:MS]
xref: value-type:xsd\:float "The allowed value-type for this CV term."
is_a: MS:1002101 ! ProteomeDiscoverer input parameter */

#define MS_PROTEOMEDISCOVERER_SEQUEST_FT_HIGH_CONFIDENCE_XCORR_CHARGE1_ID "MS:1001712"
#define MS_PROTEOMEDISCOVERER_SEQUEST_FT_HIGH_CONFIDENCE_XCORR_CHARGE1_NAME "ProteomeDiscoverer:SEQUEST:FT High Confidence XCorr Charge1"
/* def: "FT high confidence XCorr parameter for charge = 1." [PSI:MS]
xref: value-type:xsd\:float "The allowed value-type for this CV term."
is_a: MS:1002101 ! ProteomeDiscoverer input parameter */

#define MS_PROTEOMEDISCOVERER_SEQUEST_FT_HIGH_CONFIDENCE_XCORR_CHARGE2_ID "MS:1001713"
#define MS_PROTEOMEDISCOVERER_SEQUEST_FT_HIGH_CONFIDENCE_XCORR_CHARGE2_NAME "ProteomeDiscoverer:SEQUEST:FT High Confidence XCorr Charge2"
/* def: "FT high confidence XCorr parameter for charge = 2." [PSI:MS]
xref: value-type:xsd\:float "The allowed value-type for this CV term."
is_a: MS:1002101 ! ProteomeDiscoverer input parameter */

#define MS_PROTEOMEDISCOVERER_SEQUEST_FT_HIGH_CONFIDENCE_XCORR_CHARGE3_ID "MS:1001714"
#define MS_PROTEOMEDISCOVERER_SEQUEST_FT_HIGH_CONFIDENCE_XCORR_CHARGE3_NAME "ProteomeDiscoverer:SEQUEST:FT High Confidence XCorr Charge3"
/* def: "FT high confidence XCorr parameter for charge = 3." [PSI:MS]
xref: value-type:xsd\:float "The allowed value-type for this CV term."
is_a: MS:1002101 ! ProteomeDiscoverer input parameter */

#define MS_PROTEOMEDISCOVERER_SEQUEST_FT_HIGH_CONFIDENCE_XCORR_CHARGE4_ID "MS:1001715"
#define MS_PROTEOMEDISCOVERER_SEQUEST_FT_HIGH_CONFIDENCE_XCORR_CHARGE4_NAME "ProteomeDiscoverer:SEQUEST:FT High Confidence XCorr Charge4"
/* def: "FT high confidence XCorr parameter for charge >= 4." [PSI:MS]
xref: value-type:xsd\:float "The allowed value-type for this CV term."
is_a: MS:1002101 ! ProteomeDiscoverer input parameter */

#define MS_PROTEOMEDISCOVERER_SEQUEST_FT_MEDIUM_CONFIDENCE_XCORR_CHARGE1_ID "MS:1001716"
#define MS_PROTEOMEDISCOVERER_SEQUEST_FT_MEDIUM_CONFIDENCE_XCORR_CHARGE1_NAME "ProteomeDiscoverer:SEQUEST:FT Medium Confidence XCorr Charge1"
/* def: "FT medium confidence XCorr parameter for charge = 1." [PSI:MS]
xref: value-type:xsd\:float "The allowed value-type for this CV term."
is_a: MS:1002101 ! ProteomeDiscoverer input parameter */

#define MS_PROTEOMEDISCOVERER_SEQUEST_FT_MEDIUM_CONFIDENCE_XCORR_CHARGE2_ID "MS:1001717"
#define MS_PROTEOMEDISCOVERER_SEQUEST_FT_MEDIUM_CONFIDENCE_XCORR_CHARGE2_NAME "ProteomeDiscoverer:SEQUEST:FT Medium Confidence XCorr Charge2"
/* def: "FT medium confidence XCorr parameter for charge = 2." [PSI:MS]
xref: value-type:xsd\:float "The allowed value-type for this CV term."
is_a: MS:1002101 ! ProteomeDiscoverer input parameter */

#define MS_PROTEOMEDISCOVERER_SEQUEST_FT_MEDIUM_CONFIDENCE_XCORR_CHARGE3_ID "MS:1001718"
#define MS_PROTEOMEDISCOVERER_SEQUEST_FT_MEDIUM_CONFIDENCE_XCORR_CHARGE3_NAME "ProteomeDiscoverer:SEQUEST:FT Medium Confidence XCorr Charge3"
/* def: "FT medium confidence XCorr parameter for charge = 3." [PSI:MS]
xref: value-type:xsd\:float "The allowed value-type for this CV term."
is_a: MS:1002101 ! ProteomeDiscoverer input parameter */

#define MS_PROTEOMEDISCOVERER_SEQUEST_FT_MEDIUM_CONFIDENCE_XCORR_CHARGE4_ID "MS:1001719"
#define MS_PROTEOMEDISCOVERER_SEQUEST_FT_MEDIUM_CONFIDENCE_XCORR_CHARGE4_NAME "ProteomeDiscoverer:SEQUEST:FT Medium Confidence XCorr Charge4"
/* def: "FT medium confidence XCorr parameter for charge >= 4." [PSI:MS]
xref: value-type:xsd\:float "The allowed value-type for this CV term."
is_a: MS:1002101 ! ProteomeDiscoverer input parameter */

#define MS_PROTEOMEDISCOVERER_1__DYNAMIC_MODIFICATION_ID "MS:1001720"
#define MS_PROTEOMEDISCOVERER_1__DYNAMIC_MODIFICATION_NAME "ProteomeDiscoverer:1. Dynamic Modification"
/* def: "Determine 1st dynamic post-translational modifications (PTMs)." [PSI:MS]
xref: value-type:xsd\:string "The allowed value-type for this CV term."
is_a: MS:1002101 ! ProteomeDiscoverer input parameter */

#define MS_PROTEOMEDISCOVERER_2__DYNAMIC_MODIFICATION_ID "MS:1001721"
#define MS_PROTEOMEDISCOVERER_2__DYNAMIC_MODIFICATION_NAME "ProteomeDiscoverer:2. Dynamic Modification"
/* def: "Determine 2nd dynamic post-translational modifications (PTMs)." [PSI:MS]
xref: value-type:xsd\:string "The allowed value-type for this CV term."
is_a: MS:1002101 ! ProteomeDiscoverer input parameter */

#define MS_PROTEOMEDISCOVERER_3__DYNAMIC_MODIFICATION_ID "MS:1001722"
#define MS_PROTEOMEDISCOVERER_3__DYNAMIC_MODIFICATION_NAME "ProteomeDiscoverer:3. Dynamic Modification"
/* def: "Determine 3rd dynamic post-translational modifications (PTMs)." [PSI:MS]
xref: value-type:xsd\:string "The allowed value-type for this CV term."
is_a: MS:1002101 ! ProteomeDiscoverer input parameter */

#define MS_PROTEOMEDISCOVERER_4__DYNAMIC_MODIFICATION_ID "MS:1001723"
#define MS_PROTEOMEDISCOVERER_4__DYNAMIC_MODIFICATION_NAME "ProteomeDiscoverer:4. Dynamic Modification"
/* def: "Determine 4th dynamic post-translational modifications (PTMs)." [PSI:MS]
xref: value-type:xsd\:string "The allowed value-type for this CV term."
is_a: MS:1002101 ! ProteomeDiscoverer input parameter */

#define MS_PROTEOMEDISCOVERER_STATIC_MODIFICATION_FOR_X_ID "MS:1001724"
#define MS_PROTEOMEDISCOVERER_STATIC_MODIFICATION_FOR_X_NAME "ProteomeDiscoverer:Static Modification for X"
/* def: "Static Modification for X." [PSI:MS]
xref: value-type:xsd\:string "The allowed value-type for this CV term."
is_a: MS:1002101 ! ProteomeDiscoverer input parameter */

#define MS_PROTEOMEDISCOVERER_INITIAL_MINIMAL_PEPTIDE_PROBABILITY_ID "MS:1001725"
#define MS_PROTEOMEDISCOVERER_INITIAL_MINIMAL_PEPTIDE_PROBABILITY_NAME "ProteomeDiscoverer:Initial minimal peptide probability"
/* def: "Minimal initial peptide probability to contribute to analysis." [PSI:MS]
xref: value-type:xsd\:float "The allowed value-type for this CV term."
is_a: MS:1002101 ! ProteomeDiscoverer input parameter */

#define MS_PROTEOMEDISCOVERER_MINIMAL_PEPTIDE_PROBABILITY_ID "MS:1001726"
#define MS_PROTEOMEDISCOVERER_MINIMAL_PEPTIDE_PROBABILITY_NAME "ProteomeDiscoverer:Minimal peptide probability"
/* def: "Minimum adjusted peptide probability contributing to protein probability." [PSI:MS]
xref: value-type:xsd\:float "The allowed value-type for this CV term."
is_a: MS:1002101 ! ProteomeDiscoverer input parameter */

#define MS_PROTEOMEDISCOVERER_MINIMAL_PEPTIDE_WEIGHT_ID "MS:1001727"
#define MS_PROTEOMEDISCOVERER_MINIMAL_PEPTIDE_WEIGHT_NAME "ProteomeDiscoverer:Minimal peptide weight"
/* def: "Minimum peptide weight contributing to protein probability." [PSI:MS]
xref: value-type:xsd\:float "The allowed value-type for this CV term."
is_a: MS:1002101 ! ProteomeDiscoverer input parameter */

#define MS_PROTEOMEDISCOVERER_NUMBER_OF_INPUT1_SPECTRA_ID "MS:1001728"
#define MS_PROTEOMEDISCOVERER_NUMBER_OF_INPUT1_SPECTRA_NAME "ProteomeDiscoverer:Number of input1 spectra"
/* def: "Number of spectra from 1+ precursor ions." [PSI:MS]
xref: value-type:xsd\:int "The allowed value-type for this CV term."
is_a: MS:1002101 ! ProteomeDiscoverer input parameter */

#define MS_PROTEOMEDISCOVERER_NUMBER_OF_INPUT2_SPECTRA_ID "MS:1001729"
#define MS_PROTEOMEDISCOVERER_NUMBER_OF_INPUT2_SPECTRA_NAME "ProteomeDiscoverer:Number of input2 spectra"
/* def: "Number of spectra from 2+ precursor ions." [PSI:MS]
xref: value-type:xsd\:int "The allowed value-type for this CV term."
is_a: MS:1002101 ! ProteomeDiscoverer input parameter */

#define MS_PROTEOMEDISCOVERER_NUMBER_OF_INPUT3_SPECTRA_ID "MS:1001730"
#define MS_PROTEOMEDISCOVERER_NUMBER_OF_INPUT3_SPECTRA_NAME "ProteomeDiscoverer:Number of input3 spectra"
/* def: "Number of spectra from 3+ precursor ions." [PSI:MS]
xref: value-type:xsd\:int "The allowed value-type for this CV term."
is_a: MS:1002101 ! ProteomeDiscoverer input parameter */

#define MS_PROTEOMEDISCOVERER_NUMBER_OF_INPUT4_SPECTRA_ID "MS:1001731"
#define MS_PROTEOMEDISCOVERER_NUMBER_OF_INPUT4_SPECTRA_NAME "ProteomeDiscoverer:Number of input4 spectra"
/* def: "Number of spectra from 4+ precursor ions." [PSI:MS]
xref: value-type:xsd\:int "The allowed value-type for this CV term."
is_a: MS:1002101 ! ProteomeDiscoverer input parameter */

#define MS_PROTEOMEDISCOVERER_NUMBER_OF_INPUT5_SPECTRA_ID "MS:1001732"
#define MS_PROTEOMEDISCOVERER_NUMBER_OF_INPUT5_SPECTRA_NAME "ProteomeDiscoverer:Number of input5 spectra"
/* def: "Number of spectra from 5+ precursor ions." [PSI:MS]
xref: value-type:xsd\:int "The allowed value-type for this CV term."
is_a: MS:1002101 ! ProteomeDiscoverer input parameter */

#define MS_PROTEOMEDISCOVERER_NUMBER_OF_PREDICTED_CORRECT_PROTEINS_ID "MS:1001733"
#define MS_PROTEOMEDISCOVERER_NUMBER_OF_PREDICTED_CORRECT_PROTEINS_NAME "ProteomeDiscoverer:Number of predicted correct proteins"
/* def: "Total number of predicted correct protein ids (sum of probabilities)." [PSI:MS]
xref: value-type:xsd\:float "The allowed value-type for this CV term."
is_a: MS:1002101 ! ProteomeDiscoverer input parameter */

// #define MS_PROTEOMEDISCOVERER_ORGANISM_ID "MS:1001734"
// #define MS_PROTEOMEDISCOVERER_ORGANISM_NAME "ProteomeDiscoverer:Organism"
/* def: "OBSOLETE Sample organism (used for annotation purposes)." [PSI:MS]
comment: This term was made obsolete because it's recommended to use taxonomy: scientific name (MS:1001469) instead.
xref: value-type:xsd\:string "The allowed value-type for this CV term."
is_a: MS:1002101 ! ProteomeDiscoverer input parameter
is_obsolete: true */

// #define MS_PROTEOMEDISCOVERER_REFERENCE_DATABASE_ID "MS:1001735"
// #define MS_PROTEOMEDISCOVERER_REFERENCE_DATABASE_NAME "ProteomeDiscoverer:Reference Database"
/* def: "OBSOLETE Full path database name." [PSI:MS]
comment: This term was made obsolete. Use attribute in mzIdentML / mzQuantML instead.
xref: value-type:xsd\:string "The allowed value-type for this CV term."
is_a: MS:1002101 ! ProteomeDiscoverer input parameter
is_obsolete: true */

#define MS_PROTEOMEDISCOVERER_RESIDUE_SUBSTITUTION_LIST_ID "MS:1001736"
#define MS_PROTEOMEDISCOVERER_RESIDUE_SUBSTITUTION_LIST_NAME "ProteomeDiscoverer:Residue substitution list"
/* def: "Residues considered equivalent when comparing peptides." [PSI:MS]
xref: value-type:xsd\:string "The allowed value-type for this CV term."
is_a: MS:1002101 ! ProteomeDiscoverer input parameter */

// #define MS_PROTEOMEDISCOVERER_SOURCE_FILE_EXTENSION_ID "MS:1001737"
// #define MS_PROTEOMEDISCOVERER_SOURCE_FILE_EXTENSION_NAME "ProteomeDiscoverer:Source file extension"
/* def: "OBSOLETE File type (if not pepXML)." [PSI:MS]
comment: This term was made obsolete because it's recommended to use mass spectrometer file format (MS:1000560) instead.
xref: value-type:xsd\:string "The allowed value-type for this CV term."
is_a: MS:1002101 ! ProteomeDiscoverer input parameter
is_obsolete: true */

// #define MS_PROTEOMEDISCOVERER_SOURCE_FILES_ID "MS:1001738"
// #define MS_PROTEOMEDISCOVERER_SOURCE_FILES_NAME "ProteomeDiscoverer:Source Files"
/* def: "OBSOLETE Input pepXML files." [PSI:MS]
comment: This term was made obsolete because it's recommended to use pepXML file (MS:1001421) instead.
xref: value-type:xsd\:string "The allowed value-type for this CV term."
is_a: MS:1002101 ! ProteomeDiscoverer input parameter
is_obsolete: true */

// #define MS_PROTEOMEDISCOVERER_SOURCE_FILES_OLD_ID "MS:1001739"
// #define MS_PROTEOMEDISCOVERER_SOURCE_FILES_OLD_NAME "ProteomeDiscoverer:Source Files old"
/* def: "OBSOLETE Input pepXML files (old)." [PSI:MS]
comment: This term was made obsolete because it's recommended to use pepXML file (MS:1001421) instead.
xref: value-type:xsd\:string "The allowed value-type for this CV term."
is_a: MS:1002101 ! ProteomeDiscoverer input parameter
is_obsolete: true */

#define MS_PROTEOMEDISCOVERER_WINCYG_REFERENCE_DATABASE_ID "MS:1001740"
#define MS_PROTEOMEDISCOVERER_WINCYG_REFERENCE_DATABASE_NAME "ProteomeDiscoverer:WinCyg reference database"
/* def: "Windows full path for database." [PSI:MS]
xref: value-type:xsd\:string "The allowed value-type for this CV term."
is_a: MS:1002101 ! ProteomeDiscoverer input parameter */

#define MS_PROTEOMEDISCOVERER_WINCYG_SOURCE_FILES_ID "MS:1001741"
#define MS_PROTEOMEDISCOVERER_WINCYG_SOURCE_FILES_NAME "ProteomeDiscoverer:WinCyg source files"
/* def: "Windows pepXML file names." [PSI:MS]
xref: value-type:xsd\:string "The allowed value-type for this CV term."
is_a: MS:1002101 ! ProteomeDiscoverer input parameter */

#define MS_LTQ_ORBITRAP_VELOS_ID "MS:1001742"
#define MS_LTQ_ORBITRAP_VELOS_NAME "LTQ Orbitrap Velos"
/* def: "Finnigan LTQ Orbitrap Velos MS." [PSI:MS]
is_a: MS:1000494 ! Thermo Scientific instrument model */

#define MS_PROTEOMEDISCOVERER_MASCOT_WEIGHT_OF_A_IONS_ID "MS:1001743"
#define MS_PROTEOMEDISCOVERER_MASCOT_WEIGHT_OF_A_IONS_NAME "ProteomeDiscoverer:Mascot:Weight of A Ions"
/* def: "Determines if to use A ions for spectrum matching." [PSI:MS]
xref: value-type:xsd\:boolean "The allowed value-type for this CV term."
is_a: MS:1002101 ! ProteomeDiscoverer input parameter */

#define MS_PROTEOMEDISCOVERER_MASCOT_WEIGHT_OF_B_IONS_ID "MS:1001744"
#define MS_PROTEOMEDISCOVERER_MASCOT_WEIGHT_OF_B_IONS_NAME "ProteomeDiscoverer:Mascot:Weight of B Ions"
/* def: "Determines if to use B ions for spectrum matching." [PSI:MS]
xref: value-type:xsd\:boolean "The allowed value-type for this CV term."
is_a: MS:1002101 ! ProteomeDiscoverer input parameter */

#define MS_PROTEOMEDISCOVERER_MASCOT_WEIGHT_OF_C_IONS_ID "MS:1001745"
#define MS_PROTEOMEDISCOVERER_MASCOT_WEIGHT_OF_C_IONS_NAME "ProteomeDiscoverer:Mascot:Weight of C Ions"
/* def: "Determines if to use C ions for spectrum matching." [PSI:MS]
xref: value-type:xsd\:boolean "The allowed value-type for this CV term."
is_a: MS:1002101 ! ProteomeDiscoverer input parameter */

#define MS_PROTEOMEDISCOVERER_MASCOT_WEIGHT_OF_D_IONS_ID "MS:1001746"
#define MS_PROTEOMEDISCOVERER_MASCOT_WEIGHT_OF_D_IONS_NAME "ProteomeDiscoverer:Mascot:Weight of D Ions"
/* def: "Determines if to use D ions for spectrum matching." [PSI:MS]
xref: value-type:xsd\:boolean "The allowed value-type for this CV term."
is_a: MS:1002101 ! ProteomeDiscoverer input parameter */

#define MS_PROTEOMEDISCOVERER_MASCOT_WEIGHT_OF_V_IONS_ID "MS:1001747"
#define MS_PROTEOMEDISCOVERER_MASCOT_WEIGHT_OF_V_IONS_NAME "ProteomeDiscoverer:Mascot:Weight of V Ions"
/* def: "Determines if to use V ions for spectrum matching." [PSI:MS]
xref: value-type:xsd\:boolean "The allowed value-type for this CV term."
is_a: MS:1002101 ! ProteomeDiscoverer input parameter */

#define MS_PROTEOMEDISCOVERER_MASCOT_WEIGHT_OF_W_IONS_ID "MS:1001748"
#define MS_PROTEOMEDISCOVERER_MASCOT_WEIGHT_OF_W_IONS_NAME "ProteomeDiscoverer:Mascot:Weight of W Ions"
/* def: "Determines if to use W ions for spectrum matching." [PSI:MS]
xref: value-type:xsd\:boolean "The allowed value-type for this CV term."
is_a: MS:1002101 ! ProteomeDiscoverer input parameter */

#define MS_PROTEOMEDISCOVERER_MASCOT_WEIGHT_OF_X_IONS_ID "MS:1001749"
#define MS_PROTEOMEDISCOVERER_MASCOT_WEIGHT_OF_X_IONS_NAME "ProteomeDiscoverer:Mascot:Weight of X Ions"
/* def: "Determines if to use X ions for spectrum matching." [PSI:MS]
xref: value-type:xsd\:boolean "The allowed value-type for this CV term."
is_a: MS:1002101 ! ProteomeDiscoverer input parameter */

#define MS_PROTEOMEDISCOVERER_MASCOT_WEIGHT_OF_Y_IONS_ID "MS:1001750"
#define MS_PROTEOMEDISCOVERER_MASCOT_WEIGHT_OF_Y_IONS_NAME "ProteomeDiscoverer:Mascot:Weight of Y Ions"
/* def: "Determines if to use Y ions for spectrum matching." [PSI:MS]
xref: value-type:xsd\:boolean "The allowed value-type for this CV term."
is_a: MS:1002101 ! ProteomeDiscoverer input parameter */

#define MS_PROTEOMEDISCOVERER_MASCOT_WEIGHT_OF_Z_IONS_ID "MS:1001751"
#define MS_PROTEOMEDISCOVERER_MASCOT_WEIGHT_OF_Z_IONS_NAME "ProteomeDiscoverer:Mascot:Weight of Z Ions"
/* def: "Determines if to use z ions for spectrum matching." [PSI:MS]
xref: value-type:xsd\:boolean "The allowed value-type for this CV term."
is_a: MS:1002101 ! ProteomeDiscoverer input parameter */

#define MS_PROTEOMEDISCOVERER_SPECTRUM_SELECTOR_USE_NEW_PRECURSOR_REEVALUATION_ID "MS:1001752"
#define MS_PROTEOMEDISCOVERER_SPECTRUM_SELECTOR_USE_NEW_PRECURSOR_REEVALUATION_NAME "ProteomeDiscoverer:Spectrum Selector:Use New Precursor Reevaluation"
/* def: "Determines if to use precursor reevaluation." [PSI:MS]
xref: value-type:xsd\:boolean "The allowed value-type for this CV term."
is_a: MS:1002101 ! ProteomeDiscoverer input parameter */

#define MS_PROTEOMEDISCOVERER_SPECTRUM_SELECTOR_SN_THRESHOLD_FTONLY_ID "MS:1001753"
#define MS_PROTEOMEDISCOVERER_SPECTRUM_SELECTOR_SN_THRESHOLD_FTONLY_NAME "ProteomeDiscoverer:Spectrum Selector:SN Threshold FTonly"
/* def: "Signal-to-Noise ratio below which peaks are removed (in FT mode only)." [PSI:MS]
xref: value-type:xsd\:float "The allowed value-type for this CV term."
is_a: MS:1002101 ! ProteomeDiscoverer input parameter */

#define MS_PROTEOMEDISCOVERER_MASCOT_PLEASE_DO_NOT_TOUCH_THIS_ID "MS:1001754"
#define MS_PROTEOMEDISCOVERER_MASCOT_PLEASE_DO_NOT_TOUCH_THIS_NAME "ProteomeDiscoverer:Mascot:Please Do not Touch this"
/* def: "Unknown Mascot parameter which ProteomeDiscoverer uses for mascot searches." [PSI:MS]
xref: value-type:xsd\:string "The allowed value-type for this CV term."
is_a: MS:1002101 ! ProteomeDiscoverer input parameter */

#define MS_CONTACT_PHONE_NUMBER_ID "MS:1001755"
#define MS_CONTACT_PHONE_NUMBER_NAME "contact phone number"
/* def: "Phone number of the contact person or organization." [PSI:MS]
xref: value-type:xsd\:string "The allowed value-type for this CV term."
is_a: MS:1000585 ! contact attribute */

#define MS_CONTACT_FAX_NUMBER_ID "MS:1001756"
#define MS_CONTACT_FAX_NUMBER_NAME "contact fax number"
/* def: "Fax number for the contact person or organization." [PSI:MS]
xref: value-type:xsd\:string "The allowed value-type for this CV term."
is_a: MS:1000585 ! contact attribute */

#define MS_CONTACT_TOLL_FREE_PHONE_NUMBER_ID "MS:1001757"
#define MS_CONTACT_TOLL_FREE_PHONE_NUMBER_NAME "contact toll-free phone number"
/* def: "Toll-free phone number of the contact person or organization." [PSI:MS]
xref: value-type:xsd\:string "The allowed value-type for this CV term."
is_a: MS:1000585 ! contact attribute */

#define MS_MASCOT_SIGTHRESHOLDTYPE_ID "MS:1001758"
#define MS_MASCOT_SIGTHRESHOLDTYPE_NAME "Mascot:SigThresholdType"
/* def: "Significance threshold type used in Mascot reporting (either 'identity' or 'homology')." [PSI:MS]
xref: value-type:xsd\:string "The allowed value-type for this CV term."
is_a: MS:1002095 ! Mascot input parameter */

#define MS_MASCOT_PROTEINGROUPING_ID "MS:1001759"
#define MS_MASCOT_PROTEINGROUPING_NAME "Mascot:ProteinGrouping"
/* def: "Strategy used by Mascot to group proteins with same peptide matches (one of 'none', 'Occam's razor' or 'family clustering')." [PSI:MS]
xref: value-type:xsd\:string "The allowed value-type for this CV term."
is_a: MS:1002095 ! Mascot input parameter */

#define MS_PERCOLATOR_FEATURES_ID "MS:1001760"
#define MS_PERCOLATOR_FEATURES_NAME "Percolator:features"
/* def: "List of Percolator features that were used in processing the peptide matches. Typical Percolator features are 'retentionTime', 'dM', 'mScore', 'lgDScore', 'mrCalc', 'charge' and 'dMppm'." [PSI:MS]
xref: value-type:xsd\:string "The allowed value-type for this CV term."
is_a: MS:1002107 ! Percolator input parameter */

#define MS_ACQUITY_UPLC_ID "MS:1001761"
#define MS_ACQUITY_UPLC_NAME "ACQUITY UPLC"
/* def: "Waters LC-system ACQUITY UPLC." [PSI:MS]
is_a: MS:1000126 ! Waters instrument model */

#define MS_ACQUITY_UPLC_H_CLASS_ID "MS:1001762"
#define MS_ACQUITY_UPLC_H_CLASS_NAME "ACQUITY UPLC H-Class"
/* def: "Waters LC-system ACQUITY UPLC H-Class." [PSI:MS]
is_a: MS:1000126 ! Waters instrument model */

#define MS_ACQUITY_UPLC_H_CLASS_BIO_ID "MS:1001763"
#define MS_ACQUITY_UPLC_H_CLASS_BIO_NAME "ACQUITY UPLC H-Class Bio"
/* def: "Waters LC-system ACQUITY UPLC H-Class Bio." [PSI:MS]
is_a: MS:1000126 ! Waters instrument model */

#define MS_ACQUITY_UPLC_I_CLASS_ID "MS:1001764"
#define MS_ACQUITY_UPLC_I_CLASS_NAME "ACQUITY UPLC I-Class"
/* def: "Waters LC-system ACQUITY UPLC I-Class." [PSI:MS]
is_a: MS:1000126 ! Waters instrument model */

#define MS_ACQUITY_UPLC_SYSTEMS_WITH_2D_TECHNOLOGY_ID "MS:1001765"
#define MS_ACQUITY_UPLC_SYSTEMS_WITH_2D_TECHNOLOGY_NAME "ACQUITY UPLC Systems with 2D Technology"
/* def: "Waters LC-system ACQUITY UPLC Systems with 2D Technology." [PSI:MS]
is_a: MS:1000126 ! Waters instrument model */

#define MS_NANOACQUITY_UPLC_ID "MS:1001766"
#define MS_NANOACQUITY_UPLC_NAME "nanoACQUITY UPLC"
/* def: "Waters LC-system nanoACQUITY UPLC." [PSI:MS]
is_a: MS:1000126 ! Waters instrument model */

#define MS_NANOACQUITY_UPLC_SYSTEM_WITH_TECHNOLOGY_ID "MS:1001767"
#define MS_NANOACQUITY_UPLC_SYSTEM_WITH_TECHNOLOGY_NAME "nanoACQUITY UPLC System with Technology"
/* def: "Waters LC-system nanoACQUITY UPLC System with Technology." [PSI:MS]
is_a: MS:1000126 ! Waters instrument model */

#define MS_NANOACQUITY_UPLC_WITH_HDX_TECHNOLOGY_ID "MS:1001768"
#define MS_NANOACQUITY_UPLC_WITH_HDX_TECHNOLOGY_NAME "nanoACQUITY UPLC with HDX Technology"
/* def: "Waters LC-system nanoACQUITY UPLC with HDX Technology." [PSI:MS]
is_a: MS:1000126 ! Waters instrument model */

#define MS_TRIZAIC_UPLC_NANOTILE_ID "MS:1001769"
#define MS_TRIZAIC_UPLC_NANOTILE_NAME "TRIZAIC UPLC nanoTile"
/* def: "Waters LC-system TRIZAIC UPLC nanoTile." [PSI:MS]
is_a: MS:1000126 ! Waters instrument model */

#define MS_GCT_PREMIER_ID "MS:1001770"
#define MS_GCT_PREMIER_NAME "GCT Premier"
/* def: "Waters oa-ToF based GCT Premier." [PSI:MS]
is_a: MS:1000126 ! Waters instrument model */

#define MS_MALDI_SYNAPT_G2_HDMS_ID "MS:1001771"
#define MS_MALDI_SYNAPT_G2_HDMS_NAME "MALDI Synapt G2 HDMS"
/* def: "Waters oa-ToF based MALDI Synapt G2 HDMS." [PSI:MS]
is_a: MS:1000126 ! Waters instrument model */

#define MS_MALDI_SYNAPT_G2_MS_ID "MS:1001772"
#define MS_MALDI_SYNAPT_G2_MS_NAME "MALDI Synapt G2 MS"
/* def: "Waters oa-ToF based MALDI Synapt G2 MS." [PSI:MS]
is_a: MS:1000126 ! Waters instrument model */

#define MS_MALDI_SYNAPT_G2_S_HDMS_ID "MS:1001773"
#define MS_MALDI_SYNAPT_G2_S_HDMS_NAME "MALDI Synapt G2-S HDMS"
/* def: "Waters oa-ToF based MALDI Synapt G2 MS." [PSI:MS]
is_a: MS:1000126 ! Waters instrument model */

#define MS_MALDI_SYNAPT_G2_S_MS_ID "MS:1001774"
#define MS_MALDI_SYNAPT_G2_S_MS_NAME "MALDI Synapt G2-S MS"
/* def: "Waters oa-ToF based MALDI Synapt G2-S MS." [PSI:MS]
is_a: MS:1000126 ! Waters instrument model */

#define MS_MALDI_SYNAPT_HDMS_ID "MS:1001775"
#define MS_MALDI_SYNAPT_HDMS_NAME "MALDI Synapt HDMS"
/* def: "Waters oa-ToF based MALDI Synapt HDMS." [PSI:MS]
is_a: MS:1000126 ! Waters instrument model */

#define MS_MALDI_SYNAPT_MS_ID "MS:1001776"
#define MS_MALDI_SYNAPT_MS_NAME "MALDI Synapt MS"
/* def: "Waters oa-ToF based MALDI Synapt MS." [PSI:MS]
is_a: MS:1000126 ! Waters instrument model */

#define MS_SYNAPT_G2_HDMS_ID "MS:1001777"
#define MS_SYNAPT_G2_HDMS_NAME "Synapt G2 HDMS"
/* def: "Waters oa-ToF based Synapt G2 HDMS." [PSI:MS]
is_a: MS:1000126 ! Waters instrument model */

#define MS_SYNAPT_G2_MS_ID "MS:1001778"
#define MS_SYNAPT_G2_MS_NAME "Synapt G2 MS"
/* def: "Waters oa-ToF based Synapt G2 MS." [PSI:MS]
is_a: MS:1000126 ! Waters instrument model */

#define MS_SYNAPT_G2_S_HDMS_ID "MS:1001779"
#define MS_SYNAPT_G2_S_HDMS_NAME "Synapt G2-S HDMS"
/* def: "Waters oa-ToF based Synapt G2-S HDMS." [PSI:MS]
is_a: MS:1000126 ! Waters instrument model */

#define MS_SYNAPT_G2_S_MS_ID "MS:1001780"
#define MS_SYNAPT_G2_S_MS_NAME "Synapt G2-S MS"
/* def: "Waters oa-ToF based Synapt G2-S MS." [PSI:MS]
is_a: MS:1000126 ! Waters instrument model */

#define MS_SYNAPT_HDMS_ID "MS:1001781"
#define MS_SYNAPT_HDMS_NAME "Synapt HDMS"
/* def: "Waters oa-ToF based Synapt HDMS." [PSI:MS]
is_a: MS:1000126 ! Waters instrument model */

#define MS_SYNAPT_MS_ID "MS:1001782"
#define MS_SYNAPT_MS_NAME "Synapt MS"
/* def: "Waters oa-ToF based Synapt MS." [PSI:MS]
is_a: MS:1000126 ! Waters instrument model */

#define MS_XEVO_G2_Q_TOF_ID "MS:1001783"
#define MS_XEVO_G2_Q_TOF_NAME "Xevo G2 Q-Tof"
/* def: "Waters oa-ToF based Xevo G2 Q-Tof." [PSI:MS]
is_a: MS:1000126 ! Waters instrument model */

#define MS_XEVO_G2_TOF_ID "MS:1001784"
#define MS_XEVO_G2_TOF_NAME "Xevo G2 Tof"
/* def: "Waters oa-ToF based Xevo G2 Tof." [PSI:MS]
is_a: MS:1000126 ! Waters instrument model */

#define MS_XEVO_Q_TOF_ID "MS:1001785"
#define MS_XEVO_Q_TOF_NAME "Xevo Q-Tof"
/* def: "Waters oa-ToF based Xevo Q-Tof." [PSI:MS]
is_a: MS:1000126 ! Waters instrument model */

#define MS__3100_ID "MS:1001786"
#define MS__3100_NAME "3100"
/* def: "Waters quadrupole based 3100." [PSI:MS]
is_a: MS:1000126 ! Waters instrument model */

#define MS_ACQUITY_SQD_ID "MS:1001787"
#define MS_ACQUITY_SQD_NAME "Acquity SQD"
/* def: "Waters quadrupole based Acquity SQD." [PSI:MS]
is_a: MS:1000126 ! Waters instrument model */

#define MS_ACQUITY_TQD_ID "MS:1001788"
#define MS_ACQUITY_TQD_NAME "Acquity TQD"
/* def: "Waters quadrupole based Acquity TQD." [PSI:MS]
is_a: MS:1000126 ! Waters instrument model */

#define MS_QUATTRO_MICRO_GC_ID "MS:1001789"
#define MS_QUATTRO_MICRO_GC_NAME "Quattro micro GC"
/* def: "Waters quadrupole based Quattro micro GC." [PSI:MS]
is_a: MS:1000126 ! Waters instrument model */

#define MS_XEVO_TQ_MS_ID "MS:1001790"
#define MS_XEVO_TQ_MS_NAME "Xevo TQ MS"
/* def: "Waters quadrupole based Xevo TQ MS." [PSI:MS]
is_a: MS:1000126 ! Waters instrument model */

#define MS_XEVO_TQD_ID "MS:1001791"
#define MS_XEVO_TQD_NAME "Xevo TQD"
/* def: "Waters quadrupole based Xevo TQD." [PSI:MS]
is_a: MS:1000126 ! Waters instrument model */

#define MS_XEVO_TQ_S_ID "MS:1001792"
#define MS_XEVO_TQ_S_NAME "Xevo TQ-S"
/* def: "Waters quadrupole based Xevo TQ-S." [PSI:MS]
is_a: MS:1000126 ! Waters instrument model */

#define MS_MASCOT_PREFERREDTAXONOMY_ID "MS:1001793"
#define MS_MASCOT_PREFERREDTAXONOMY_NAME "Mascot:PreferredTaxonomy"
/* def: "NCBI TaxID taxonomy ID to prefer when two or more proteins match the same set of peptides or when protein entry in database represents multiple sequences." [PSI:MS]
xref: value-type:xsd\:string "The allowed value-type for this CV term."
is_a: MS:1002095 ! Mascot input parameter */

#define MS_EMPOWER_ID "MS:1001795"
#define MS_EMPOWER_NAME "Empower"
/* def: "Waters Empower software for liquid chromatography and mass spectrometry acquisition." [PSI:MS]
is_a: MS:1000694 ! Waters software
is_a: MS:1001456 ! analysis software
is_a: MS:1001457 ! data processing software */

#define MS_UNIFY_ID "MS:1001796"
#define MS_UNIFY_NAME "Unify"
/* def: "Waters Unify software for liquid chromatography and mass spectrometry acquisition." [PSI:MS]
is_a: MS:1000694 ! Waters software
is_a: MS:1001456 ! analysis software
is_a: MS:1001457 ! data processing software */

#define MS_TRAVELLING_WAVE_ION_MOBILITY_MASS_SPECTROMETER_ID "MS:1001797"
#define MS_TRAVELLING_WAVE_ION_MOBILITY_MASS_SPECTROMETER_NAME "travelling wave ion mobility mass spectrometer"
/* def: "An ion mobility mass spectrometry technique based on the superimposition of travelling voltage waves on a radially-confining RF voltage in a gas-filled, stacked-ring ion guide." [PSI:MS]
synonym: "TWIMS" EXACT []
is_a: MS:1000261 ! ion mobility spectrometry */

#define MS_LECO_SOFTWARE_ID "MS:1001798"
#define MS_LECO_SOFTWARE_NAME "LECO software"
/* def: "LECO software for data acquisition and analysis." [PSI:MS]
is_a: MS:1000531 ! software */

#define MS_CHROMATOF_SOFTWARE_ID "MS:1001799"
#define MS_CHROMATOF_SOFTWARE_NAME "ChromaTOF software"
/* def: "Software for acquisition, processing and analysis of data for LECO instruments." [PSI:MS]
is_a: MS:1001455 ! acquisition software
is_a: MS:1001456 ! analysis software
is_a: MS:1001457 ! data processing software
is_a: MS:1001798 ! LECO software */

#define MS_LECO_INSTRUMENT_MODEL_ID "MS:1001800"
#define MS_LECO_INSTRUMENT_MODEL_NAME "LECO instrument model"
/* def: "LECO instrument model." [PSI:MS]
is_a: MS:1000031 ! instrument model */

#define MS_PEGASUS_HRT_ID "MS:1001801"
#define MS_PEGASUS_HRT_NAME "Pegasus HRT"
/* def: "LECO high resolution time-of-flight GC mass spectrometer." [PSI:MS]
is_a: MS:1001800 ! LECO instrument model */

#define MS_CITIUS_HRT_ID "MS:1001802"
#define MS_CITIUS_HRT_NAME "Citius HRT"
/* def: "LECO high resolution time-of-flight LC mass spectrometer." [PSI:MS]
is_a: MS:1001800 ! LECO instrument model */

#define MS_PEGASUS_ID "MS:1001803"
#define MS_PEGASUS_NAME "Pegasus"
/* def: "LECO GC time-of-flight mass spectrometer." [PSI:MS]
is_a: MS:1001800 ! LECO instrument model */

#define MS_TRUTOF_ID "MS:1001804"
#define MS_TRUTOF_NAME "TruTOF"
/* def: "LECO bench-top GC time-of-flight mass spectrometer." [PSI:MS]
is_a: MS:1001800 ! LECO instrument model */

#define MS_QUANTIFICATION_DATATYPE_ID "MS:1001805"
#define MS_QUANTIFICATION_DATATYPE_NAME "quantification datatype"
/* def: "The data type of the value reported in a QuantLayer for a feature, peptide, protein, protein group." [PSI:MS]
is_a: MS:1001129 ! quantification information */

#define MS_QUANTIFICATION_OBJECT_ATTRIBUTE_ID "MS:1001806"
#define MS_QUANTIFICATION_OBJECT_ATTRIBUTE_NAME "quantification object attribute"
/* def: "Attributes describing the details of an object relevant for reporting quantification workflows or values." [PSI:MS]
is_a: MS:1001129 ! quantification information */

#define MS_STUDY_VARIABLE_ATTRIBUTE_ID "MS:1001807"
#define MS_STUDY_VARIABLE_ATTRIBUTE_NAME "study variable attribute"
/* def: "Attribute describing a study variable." [PSI:MS]
is_a: MS:1001806 ! quantification object attribute */

#define MS_TECHNICAL_REPLICATE_ID "MS:1001808"
#define MS_TECHNICAL_REPLICATE_NAME "technical replicate"
/* def: "The study variable is 'technical replicate'. The string value denotes the category of technical replicate, e.g. 'run generated from same sample'." [PSI:MS]
xref: value-type:xsd\:string "The allowed value-type for this CV term."
is_a: MS:1001807 ! study variable attribute */

#define MS_BIOLOGICAL_REPLICATE_ID "MS:1001809"
#define MS_BIOLOGICAL_REPLICATE_NAME "biological replicate"
/* def: "The study variable is 'biological replicate'. This means, the run was generated from another individuum or sample." [PSI:MS]
is_a: MS:1001807 ! study variable attribute */

#define MS_EXPERIMENTAL_CONDITION_CASE_ID "MS:1001810"
#define MS_EXPERIMENTAL_CONDITION_CASE_NAME "experimental condition 'case'"
/* def: "The experimental condition is 'case' in contrast to 'control'." [PSI:MS]
is_a: MS:1001807 ! study variable attribute */

#define MS_EXPERIMENTAL_CONDITION_CONTROL_ID "MS:1001811"
#define MS_EXPERIMENTAL_CONDITION_CONTROL_NAME "experimental condition 'control'"
/* def: "The experimental condition is 'control' in contrast to 'case'." [PSI:MS]
is_a: MS:1001807 ! study variable attribute */

#define MS_EXPERIMENTAL_CONDITION_DISEASE_ID "MS:1001812"
#define MS_EXPERIMENTAL_CONDITION_DISEASE_NAME "experimental condition 'disease'"
/* def: "The experimental condition is 'disease' in contrast to 'healthy'." [PSI:MS]
is_a: MS:1001807 ! study variable attribute */

#define MS_EXPERIMENTAL_CONDITION_HEALTHY_ID "MS:1001813"
#define MS_EXPERIMENTAL_CONDITION_HEALTHY_NAME "experimental condition 'healthy'"
/* def: "The experimental condition is 'healthy' in contrast to 'disease'." [PSI:MS]
is_a: MS:1001807 ! study variable attribute */

#define MS_GENERIC_EXPERIMENTAL_CONDITION_ID "MS:1001814"
#define MS_GENERIC_EXPERIMENTAL_CONDITION_NAME "generic experimental condition"
/* def: "The experimental condition is given in the value of this term." [PSI:MS]
xref: value-type:xsd\:string "The allowed value-type for this CV term."
is_a: MS:1001807 ! study variable attribute */

#define MS_TIME_SERIES__TIME_POINT_X_ID "MS:1001815"
#define MS_TIME_SERIES__TIME_POINT_X_NAME "time series, time point X"
/* def: "The experimental design followed a time series design. The time point of this run is given in the value of this term." [PSI:MS]
xref: value-type:xsd\:int "The allowed value-type for this CV term."
is_a: MS:1001807 ! study variable attribute */

#define MS_DILUTION_SERIES__CONCENTRATION_X_ID "MS:1001816"
#define MS_DILUTION_SERIES__CONCENTRATION_X_NAME "dilution series, concentration X"
/* def: "The experimental design followed a dilution series design. The concentration of this run is given in the value of this term." [PSI:MS]
xref: value-type:xsd\:double "The allowed value-type for this CV term."
is_a: MS:1001807 ! study variable attribute */

#define MS_RAW_FILE_ATTRIBUTE_ID "MS:1001817"
#define MS_RAW_FILE_ATTRIBUTE_NAME "raw file attribute"
/* def: "Attribute describing a raw file." [PSI:MS]
is_a: MS:1001806 ! quantification object attribute */

#define MS_ONE_SAMPLE_RUN_ID "MS:1001818"
#define MS_ONE_SAMPLE_RUN_NAME "one sample run"
/* def: "The raw file contains the run of one sample (e.g. spectral counting, LC-MS label-free)." [PSI:MS]
is_a: MS:1001817 ! raw file attribute */

#define MS_TWO_SAMPLE_RUN_ID "MS:1001819"
#define MS_TWO_SAMPLE_RUN_NAME "two sample run"
/* def: "The raw file contains the run of two samples (e.g. SILAC, metabolic labeling)." [PSI:MS]
is_a: MS:1001817 ! raw file attribute */

#define MS_THREE_SAMPLE_RUN_ID "MS:1001820"
#define MS_THREE_SAMPLE_RUN_NAME "three sample run"
/* def: "The raw file contains the run of three samples (e.g. 3-plex SILAC)." [PSI:MS]
is_a: MS:1001817 ! raw file attribute */

#define MS_FOUR_SAMPLE_RUN_ID "MS:1001821"
#define MS_FOUR_SAMPLE_RUN_NAME "four sample run"
/* def: "The raw file contains the run of four samples (e.g. 4-plex iTraq)." [PSI:MS]
is_a: MS:1001817 ! raw file attribute */

#define MS_EIGHT_SAMPLE_RUN_ID "MS:1001822"
#define MS_EIGHT_SAMPLE_RUN_NAME "eight sample run"
/* def: "The raw file contains the run of eight samples (e.g. 8-plex iTraq)." [PSI:MS]
is_a: MS:1001817 ! raw file attribute */

#define MS_RAW_FILES_GROUP_ATTRIBUTE_ID "MS:1001823"
#define MS_RAW_FILES_GROUP_ATTRIBUTE_NAME "raw files group attribute"
/* def: "Attribute describing, how raw files build a raw file group." [PSI:MS]
is_a: MS:1001806 ! quantification object attribute */

#define MS_MERGE_OF_RUNS_OF_1D_GEL_BANDS_ID "MS:1001824"
#define MS_MERGE_OF_RUNS_OF_1D_GEL_BANDS_NAME "merge of runs of 1D gel bands"
/* def: "Attribute describing, how raw files build a raw file group." [PSI:MS]
is_a: MS:1001823 ! raw files group attribute */

#define MS_FEATURE_LIST_ATTRIBUTE_ID "MS:1001825"
#define MS_FEATURE_LIST_ATTRIBUTE_NAME "feature list attribute"
/* def: "Attribute describing a feature list." [PSI:MS]
is_a: MS:1001806 ! quantification object attribute */

#define MS_MASS_TRACE_REPORTING__RECTANGLES_ID "MS:1001826"
#define MS_MASS_TRACE_REPORTING__RECTANGLES_NAME "mass trace reporting: rectangles"
/* def: "The mass trace of the features of this feature list specifies rectangles. Each mass trace has the syntax (RT_start,MZ_start,RT_end,MZ_end), i.e. opposite corners are given." [PSI:MS]
is_a: MS:1001825 ! feature list attribute */

#define MS_MASS_TRACE_REPORTING__POLYGONS_ID "MS:1001827"
#define MS_MASS_TRACE_REPORTING__POLYGONS_NAME "mass trace reporting: polygons"
/* def: "The mass trace of the features of this feature list specifies polygons. Each mass trace has the syntax (RT_1, MZ_1, RT_2, MZ_2, ... , RT_i, MZ_i, ... , RT_n, MZ_n), where the line (RT_n, MZ_n)->(RT_1, MZ_1) is implicite." [PSI:MS]
is_a: MS:1001825 ! feature list attribute */

#define MS_FEATURE_ATTRIBUTE_ID "MS:1001828"
#define MS_FEATURE_ATTRIBUTE_NAME "feature attribute"
/* def: "Attribute describing a feature." [PSI:MS]
is_a: MS:1001806 ! quantification object attribute */

#define MS_SRM_TRANSITION_ID_ID "MS:1001829"
#define MS_SRM_TRANSITION_ID_NAME "SRM transition ID"
/* def: "Identifier for an SRM transition in an external document describing additional information about the transition." [PSI:MS]
xref: value-type:xsd\:string "The allowed value-type for this CV term."
is_a: MS:1001828 ! feature attribute */

#define MS_PROGENESIS_LC_MS_ID "MS:1001830"
#define MS_PROGENESIS_LC_MS_NAME "Progenesis LC-MS"
/* def: "Software from Nonlinear Dynamics for LC-MS label-free workflow." [PSI:MS]
is_a: MS:1001139 ! quantitation software name */

#define MS_SILACANALYZER_ID "MS:1001831"
#define MS_SILACANALYZER_NAME "SILACAnalyzer"
/* def: "Software for SILAC workflow." [PSI:MS]
is_a: MS:1001139 ! quantitation software name
is_a: MS:1000752 ! TOPP software */

#define MS_QUANTITATION_SOFTWARE_COMMENT_OR_CUSTOMIZATIONS_ID "MS:1001832"
#define MS_QUANTITATION_SOFTWARE_COMMENT_OR_CUSTOMIZATIONS_NAME "quantitation software comment or customizations"
/* def: "Quantitation software comment or any customizations to the default setup of the software." [PSI:PI]
xref: value-type:xsd\:string "The allowed value-type for this CV term."
is_a: MS:1001129 ! quantification information */

#define MS_QUANTITATION_ANALYSIS_SUMMARY_ID "MS:1001833"
#define MS_QUANTITATION_ANALYSIS_SUMMARY_NAME "quantitation analysis summary"
/* def: "The overall workflow of this quantitation report." [PSI:PI]
is_a: MS:1001129 ! quantification information */

#define MS_LC_MS_LABEL_FREE_QUANTITATION_ANALYSIS_ID "MS:1001834"
#define MS_LC_MS_LABEL_FREE_QUANTITATION_ANALYSIS_NAME "LC-MS label-free quantitation analysis"
/* def: "LC-MS label-free workflow (RT m/z map)." [PSI:PI]
is_a: MS:1001833 ! quantitation analysis summary */

#define MS_SILAC_QUANTITATION_ANALYSIS_ID "MS:1001835"
#define MS_SILAC_QUANTITATION_ANALYSIS_NAME "SILAC quantitation analysis"
/* def: "SILAC workflow (heavy, light, and sometimes medium peak)." [PSI:PI]
is_a: MS:1001833 ! quantitation analysis summary */

#define MS_SPECTRAL_COUNTING_QUANTITATION_ANALYSIS_ID "MS:1001836"
#define MS_SPECTRAL_COUNTING_QUANTITATION_ANALYSIS_NAME "spectral counting quantitation analysis"
/* def: "Spectral counting workflow (number of identified MS/MS spectra as approximation of peptide / protein quant)." [PSI:PI]
is_a: MS:1001833 ! quantitation analysis summary */

#define MS_ITRAQ_QUANTITATION_ANALYSIS_ID "MS:1001837"
#define MS_ITRAQ_QUANTITATION_ANALYSIS_NAME "iTRAQ quantitation analysis"
/* def: "Quantification analysis using the AB SCIEX iTRAQ isobaric labeling workflow, wherein 2-8 reporter ions are measured in MS/MS spectra near 114 m/z." [PSI:PI]
is_a: MS:1002009 ! isobaric label quantitation analysis */

#define MS_SRM_QUANTITATION_ANALYSIS_ID "MS:1001838"
#define MS_SRM_QUANTITATION_ANALYSIS_NAME "SRM quantitation analysis"
/* def: "Selected Reaction Monitoring workflow (XIC quantitation of precursor / fragment mass pair)." [PSI:PI]
is_a: MS:1001833 ! quantitation analysis summary */

#define MS_METABOLIC_LABELING_14N___15N_QUANTITATION_ANALYSIS_ID "MS:1001839"
#define MS_METABOLIC_LABELING_14N___15N_QUANTITATION_ANALYSIS_NAME "metabolic labeling 14N _ 15N quantitation analysis"
/* def: "Metabolic labeling workflow (heavy and light versions of peptides, depending on number of nitrogens)." [PSI:PI]
is_a: MS:1001833 ! quantitation analysis summary */

#define MS_LC_MS_FEATURE_INTENSITY_ID "MS:1001840"
#define MS_LC_MS_FEATURE_INTENSITY_NAME "LC-MS feature intensity"
/* def: "Maximum peak intensity of the LC-MS feature." [PSI:PI]
xref: value-type:xsd\:double "The allowed value-type for this CV term."
is_a: MS:1001805 ! quantification datatype */

#define MS_LC_MS_FEATURE_VOLUME_ID "MS:1001841"
#define MS_LC_MS_FEATURE_VOLUME_NAME "LC-MS feature volume"
/* def: "Real (intensity times area) volume of the LC-MS feature." [PSI:PI]
xref: value-type:xsd\:double "The allowed value-type for this CV term."
is_a: MS:1001805 ! quantification datatype */

#define MS_PEPTIDE_PSM_COUNT_ID "MS:1001842"
#define MS_PEPTIDE_PSM_COUNT_NAME "peptide PSM count"
/* def: "The number of MS/MS spectra identified for this peptide in spectral counting." [PSI:PI]
xref: value-type:xsd\:integer "The allowed value-type for this CV term."
is_a: MS:1001805 ! quantification datatype */

#define MS_MS1_FEATURE_MAXIMUM_INTENSITY_ID "MS:1001843"
#define MS_MS1_FEATURE_MAXIMUM_INTENSITY_NAME "MS1 feature maximum intensity"
/* def: "Maximum intensity of MS1 feature." [PSI:PI]
xref: value-type:xsd\:double "The allowed value-type for this CV term."
is_a: MS:1001805 ! quantification datatype */

#define MS_MS1_FEATURE_AREA_ID "MS:1001844"
#define MS_MS1_FEATURE_AREA_NAME "MS1 feature area"
/* def: "Area of MS1 feature." [PSI:PI]
xref: value-type:xsd\:double "The allowed value-type for this CV term."
is_a: MS:1001805 ! quantification datatype */

// #define MS_PEAK_AREA_ID "MS:1001845"
// #define MS_PEAK_AREA_NAME "peak area"
/* def: "OBSOLETE Area of MS1 peak (e.g. SILAC, 15N)." [PSI:PI]
comment: This term was made obsolete because it was a duplication of MS:1001844.
xref: value-type:xsd\:double "The allowed value-type for this CV term."
is_a: MS:1001805 ! quantification datatype
is_obsolete: true */

#define MS_ISOTOPIC_PATTERN_AREA_ID "MS:1001846"
#define MS_ISOTOPIC_PATTERN_AREA_NAME "isotopic pattern area"
/* def: "Area of all peaks belonging to the isotopic pattern of light or heavy peak (e.g. 15N)." [PSI:PI]
xref: value-type:xsd\:double "The allowed value-type for this CV term."
is_a: MS:1001805 ! quantification datatype */

#define MS_REPORTER_ION_INTENSITY_ID "MS:1001847"
#define MS_REPORTER_ION_INTENSITY_NAME "reporter ion intensity"
/* def: "Intensity of MS/MS reporter ion (e.g. iTraq)." [PSI:PI]
xref: value-type:xsd\:double "The allowed value-type for this CV term."
is_a: MS:1001805 ! quantification datatype */

#define MS_SIMPLE_RATIO_OF_TWO_VALUES_ID "MS:1001848"
#define MS_SIMPLE_RATIO_OF_TWO_VALUES_NAME "simple ratio of two values"
/* def: "Simple ratio of two values (enumerator and denominator)." [PSI:PI]
is_a: MS:1002066 ! ratio calculation method */

// #define MS_SUM_OF_MATCHEDFEATURE_VALUES_ID "MS:1001849"
// #define MS_SUM_OF_MATCHEDFEATURE_VALUES_NAME "sum of MatchedFeature values"
/* def: "OBSOLETE Peptide quantification value calculated as sum of MatchedFeature quantification values." [PSI:PI]
comment: This term was made obsolete because the concept MatchedFeature was dropped.
xref: value-type:xsd\:double "The allowed value-type for this CV term."
is_a: MS:1001805 ! quantification datatype
is_obsolete: true */

#define MS_NORMALIZED_PEPTIDE_VALUE_ID "MS:1001850"
#define MS_NORMALIZED_PEPTIDE_VALUE_NAME "normalized peptide value"
/* def: "Normalized peptide value." [PSI:PI]
xref: value-type:xsd\:double "The allowed value-type for this CV term."
is_a: MS:1001805 ! quantification datatype */

#define MS_PROTEIN_VALUE__SUM_OF_PEPTIDE_VALUES_ID "MS:1001851"
#define MS_PROTEIN_VALUE__SUM_OF_PEPTIDE_VALUES_NAME "protein value: sum of peptide values"
/* def: "Protein quantification value calculated as sum of peptide values." [PSI:PI]
xref: value-type:xsd\:double "The allowed value-type for this CV term."
is_a: MS:1001805 ! quantification datatype */

#define MS_NORMALIZED_PROTEIN_VALUE_ID "MS:1001852"
#define MS_NORMALIZED_PROTEIN_VALUE_NAME "normalized protein value"
/* def: "Normalized protein value." [PSI:PI]
xref: value-type:xsd\:double "The allowed value-type for this CV term."
is_a: MS:1001805 ! quantification datatype */

#define MS_MAX_FOLD_CHANGE_ID "MS:1001853"
#define MS_MAX_FOLD_CHANGE_NAME "max fold change"
/* def: "Global datatype: Maximum of all pair-wise fold changes of group means (e.g. Progenesis)." [PSI:PI]
xref: value-type:xsd\:double "The allowed value-type for this CV term."
is_a: MS:1001805 ! quantification datatype */

#define MS_ANOVA_P_VALUE_ID "MS:1001854"
#define MS_ANOVA_P_VALUE_NAME "ANOVA p-value"
/* def: "Global datatype: p-value of ANOVA of group means (e.g. Progenesis)." [PSI:PI]
xref: value-type:xsd\:double "The allowed value-type for this CV term."
is_a: MS:1002072 ! p-value */

#define MS_T_TEST_P_VALUE_ID "MS:1001855"
#define MS_T_TEST_P_VALUE_NAME "t-test p-value"
/* def: "P-value of t-Test of two groups." [PSI:PI]
xref: value-type:xsd\:double "The allowed value-type for this CV term."
is_a: MS:1002072 ! p-value */

#define MS_REPORTER_ION_RAW_VALUE_ID "MS:1001856"
#define MS_REPORTER_ION_RAW_VALUE_NAME "reporter ion raw value"
/* def: "Intensity (or area) of MS/MS reporter ion (e.g. iTraq)." [PSI:PI]
xref: value-type:xsd\:double "The allowed value-type for this CV term."
is_a: MS:1001805 ! quantification datatype */

#define MS_REPORTER_ION_NORMALIZED_VALUE_ID "MS:1001857"
#define MS_REPORTER_ION_NORMALIZED_VALUE_NAME "reporter ion normalized value"
/* def: "Normalized value of MS/MS reporter ion (e.g. iTraq)." [PSI:PI]
xref: value-type:xsd\:double "The allowed value-type for this CV term."
is_a: MS:1001805 ! quantification datatype */

#define MS_XIC_AREA_ID "MS:1001858"
#define MS_XIC_AREA_NAME "XIC area"
/* def: "Area of the extracted ion chromatogram (e.g. of a transition in SRM)." [PSI:PI]
xref: value-type:xsd\:double "The allowed value-type for this CV term."
is_a: MS:1001805 ! quantification datatype */

#define MS_NORMALIZED_XIC_AREA_ID "MS:1001859"
#define MS_NORMALIZED_XIC_AREA_NAME "normalized XIC area"
/* def: "Normalized area of the extracted ion chromatogram (e.g. of a transition in SRM)." [PSI:PI]
xref: value-type:xsd\:double "The allowed value-type for this CV term."
is_a: MS:1001805 ! quantification datatype */

#define MS_PROTEIN_VALUE__MEAN_OF_PEPTIDE_RATIOS_ID "MS:1001860"
#define MS_PROTEIN_VALUE__MEAN_OF_PEPTIDE_RATIOS_NAME "protein value: mean of peptide ratios"
/* def: "Protein quantification value calculated as mean of peptide ratios." [PSI:PI]
xref: value-type:xsd\:double "The allowed value-type for this CV term."
is_a: MS:1001805 ! quantification datatype */

#define MS_QUANTIFICATION_DATA_PROCESSING_ID "MS:1001861"
#define MS_QUANTIFICATION_DATA_PROCESSING_NAME "quantification data processing"
/* def: "Terms used to describe types of quantification data processing." [PSI:MS]
relationship: part_of MS:1001000 ! spectrum interpretation */

#define MS_NORMALIZATION_TO_MEAN_OF_SUM_OF_ALL_PROTEINS_ID "MS:1001862"
#define MS_NORMALIZATION_TO_MEAN_OF_SUM_OF_ALL_PROTEINS_NAME "normalization to mean of sum of all proteins"
/* def: "Normalization of protein values to the mean of the sum of all protein PSM counts (e.g. spectral counting)." [PSI:MS]
is_a: MS:1001861 ! quantification data processing */

#define MS_QUANTILE_NORMALIZATION__PROTEINS_ID "MS:1001863"
#define MS_QUANTILE_NORMALIZATION__PROTEINS_NAME "quantile normalization, proteins"
/* def: "Normalization of protein values to approach the same distribution." [PSI:MS]
is_a: MS:1001861 ! quantification data processing */

#define MS_QUANTILE_NORMALIZATION__PEPTIDES_ID "MS:1001864"
#define MS_QUANTILE_NORMALIZATION__PEPTIDES_NAME "quantile normalization, peptides"
/* def: "Normalization of peptide values to approach the same distribution." [PSI:MS]
is_a: MS:1001861 ! quantification data processing */

#define MS_PROGENESIS_AUTOMATIC_ALIGNMENT_ID "MS:1001865"
#define MS_PROGENESIS_AUTOMATIC_ALIGNMENT_NAME "Progenesis automatic alignment"
/* def: "Automatic RT alignment of Progenesis software." [PSI:MS]
is_a: MS:1001861 ! quantification data processing */

#define MS_PROGENESIS_MANUAL_ALIGNMENT_ID "MS:1001866"
#define MS_PROGENESIS_MANUAL_ALIGNMENT_NAME "Progenesis manual alignment"
/* def: "RT alignment of Progenesis software using automatic and manual vectors." [PSI:MS]
is_a: MS:1001861 ! quantification data processing */

#define MS_PROGENESIS_NORMALIZATION_ID "MS:1001867"
#define MS_PROGENESIS_NORMALIZATION_NAME "Progenesis normalization"
/* def: "Normalization as performed by Progenesis LC-MS." [PSI:MS]
is_a: MS:1001861 ! quantification data processing */

#define MS_DISTINCT_PEPTIDE_LEVEL_Q_VALUE_ID "MS:1001868"
#define MS_DISTINCT_PEPTIDE_LEVEL_Q_VALUE_NAME "distinct peptide-level q-value"
/* def: "Estimation of the q-value for distinct peptides once redundant identifications of the same peptide have been removed (id est multiple PSMs have been collapsed to one entry)." [PSI:PI]
xref: value-type:xsd\:double "The allowed value-type for this CV term."
is_a: MS:1002347 ! PSM-level identification confidence metric
relationship: has_units UO:0000166 ! parts per notation unit
relationship: has_units UO:0000187 ! percent
has_domain: MS:1002305 ! value between 0 and 1 inclusive */

#define MS_PROTEIN_LEVEL_Q_VALUE_ID "MS:1001869"
#define MS_PROTEIN_LEVEL_Q_VALUE_NAME "protein-level q-value"
/* def: "Estimation of the q-value for proteins." [PSI:PI]
xref: value-type:xsd\:double "The allowed value-type for this CV term."
is_a: MS:1001198 ! protein identification confidence metric
relationship: has_units UO:0000166 ! parts per notation unit
relationship: has_units UO:0000187 ! percent
has_domain: MS:1002305 ! value between 0 and 1 inclusive */

#define MS_DISTINCT_PEPTIDE_LEVEL_P_VALUE_ID "MS:1001870"
#define MS_DISTINCT_PEPTIDE_LEVEL_P_VALUE_NAME "distinct peptide-level p-value"
/* def: "Estimation of the p-value for distinct peptides once redundant identifications of the same peptide have been removed (id est multiple PSMs have been collapsed to one entry)." [PSI:PI]
xref: value-type:xsd\:double "The allowed value-type for this CV term."
is_a: MS:1002347 ! PSM-level identification confidence metric
has_domain: MS:1002305 ! value between 0 and 1 inclusive */

#define MS_PROTEIN_LEVEL_P_VALUE_ID "MS:1001871"
#define MS_PROTEIN_LEVEL_P_VALUE_NAME "protein-level p-value"
/* def: "Estimation of the p-value for proteins." [PSI:PI]
xref: value-type:xsd\:double "The allowed value-type for this CV term."
is_a: MS:1001198 ! protein identification confidence metric
has_domain: MS:1002305 ! value between 0 and 1 inclusive */

#define MS_DISTINCT_PEPTIDE_LEVEL_E_VALUE_ID "MS:1001872"
#define MS_DISTINCT_PEPTIDE_LEVEL_E_VALUE_NAME "distinct peptide-level e-value"
/* def: "Estimation of the e-value for distinct peptides once redundant identifications of the same peptide have been removed (id est multiple PSMs have been collapsed to one entry)." [PSI:PI]
xref: value-type:xsd\:double "The allowed value-type for this CV term."
is_a: MS:1002347 ! PSM-level identification confidence metric
has_domain: MS:1002306 ! value greater than zero */

#define MS_PROTEIN_LEVEL_E_VALUE_ID "MS:1001873"
#define MS_PROTEIN_LEVEL_E_VALUE_NAME "protein-level e-value"
/* def: "Estimation of the e-value for proteins." [PSI:PI]
xref: value-type:xsd\:double "The allowed value-type for this CV term."
is_a: MS:1001198 ! protein identification confidence metric
has_domain: MS:1002306 ! value greater than zero */

// #define MS_FDRSCORE_ID "MS:1001874"
// #define MS_FDRSCORE_NAME "FDRScore"
/* def: "OBSOLETE A smoothing of the distribution of q-values calculated for PSMs from individual search engines, such that ordering of result quality is maintained and all FDRScore values are guaranteed to have a value > 0." [PMID:19253293]
comment: This term was made obsolete because it was split into the more specific terms for PSM-level FDRScore (1002355), distinct peptide-level FDRScore (MS:1002360), protein-level FDRScore (MS:1002365) and protein group-level FDRScore (MS:1002374).
xref: value-type:xsd\:double "The allowed value-type for this CV term."
is_a: MS:1001143 ! search engine specific score for PSMs
is_a: MS:1001153 ! search engine specific score
is_obsolete: true */

#define MS_MODIFICATION_MOTIF_ID "MS:1001875"
#define MS_MODIFICATION_MOTIF_NAME "modification motif"
/* def: "The regular expression describing the sequence motif for a modification." [PSI:PI]
xref: value-type:xsd\:string "The allowed value-type for this CV term."
is_a: MS:1001056 ! modification specificity rule */

#define MS_MODIFICATION_PROBABILITY_ID "MS:1001876"
#define MS_MODIFICATION_PROBABILITY_NAME "modification probability"
/* def: "The a priori probability of a modification." [PSI:PI]
is_a: MS:1001056 ! modification specificity rule */

#define MS_CHROMATOF_HRT_SOFTWARE_ID "MS:1001877"
#define MS_CHROMATOF_HRT_SOFTWARE_NAME "ChromaTOF HRT software"
/* def: "Software for acquisition, processing and analysis of data for LECO instruments." [PSI:MS]
is_a: MS:1001455 ! acquisition software
is_a: MS:1001456 ! analysis software
is_a: MS:1001457 ! data processing software
is_a: MS:1001798 ! LECO software */

#define MS_MALDI_SOLUTIONS_MICROBIAL_IDENTIFICATION_ID "MS:1001878"
#define MS_MALDI_SOLUTIONS_MICROBIAL_IDENTIFICATION_NAME "MALDI Solutions Microbial Identification"
/* def: "Shimadzu Biotech software for data acquisition, processing, and analysis." [PSI:MS]
is_a: MS:1001455 ! acquisition software
is_a: MS:1001456 ! analysis software
is_a: MS:1001457 ! data processing software
is_a: MS:1001558 ! MALDI Solutions */

#define MS_OFFSET_VOLTAGE_ID "MS:1001879"
#define MS_OFFSET_VOLTAGE_NAME "offset voltage"
/* def: "The potential difference between two adjacent interface voltages affecting in-source collision induced dissociation." [PSI:MS]
xref: value-type:xsd\:float "The allowed value-type for this CV term."
is_a: MS:1000482 ! source attribute
relationship: has_units UO:0000218 ! volt */

#define MS_IN_SOURCE_COLLISION_INDUCED_DISSOCIATION_ID "MS:1001880"
#define MS_IN_SOURCE_COLLISION_INDUCED_DISSOCIATION_NAME "in-source collision-induced dissociation"
/* def: "The dissociation of an ion as a result of collisional excitation during ion transfer from an atmospheric pressure ion source and the mass spectrometer vacuum." [PSI:MS]
is_a: MS:1000044 ! dissociation method */

#define MS_MZ5_FILE_ID "MS:1001881"
#define MS_MZ5_FILE_NAME "mz5 file"
/* def: "mz5 file format, modeled after mzML." [PSI:MS]
is_a: MS:1000560 ! mass spectrometer file format */

#define MS_TRANSITION_VALIDATION_ATTRIBUTE_ID "MS:1001882"
#define MS_TRANSITION_VALIDATION_ATTRIBUTE_NAME "transition validation attribute"
/* def: "Attributes of the quality of a transition that affect its selection as appropriate." [PSI:MS]
relationship: part_of MS:1000908 ! transition */

#define MS_COEFFICIENT_OF_VARIATION_ID "MS:1001883"
#define MS_COEFFICIENT_OF_VARIATION_NAME "coefficient of variation"
/* def: "Variation of a set of signal measurements calculated as the standard deviation relative to the mean." [PSI:MS]
xref: value-type:xsd\:float "The allowed value-type for this CV term."
is_a: MS:1001882 ! transition validation attribute */

#define MS_SIGNAL_TO_NOISE_RATIO_ID "MS:1001884"
#define MS_SIGNAL_TO_NOISE_RATIO_NAME "signal-to-noise ratio"
/* def: "Unitless number providing the ratio of the total measured intensity of a signal relative to the estimated noise level for that signal." [PSI:MS]
xref: value-type:xsd\:float "The allowed value-type for this CV term."
is_a: MS:1001882 ! transition validation attribute */

#define MS_COMMAND_LINE_PARAMETERS_ID "MS:1001885"
#define MS_COMMAND_LINE_PARAMETERS_NAME "command-line parameters"
/* def: "Parameters string passed to a command-line interface software application, omitting the executable name." [PSI:MS]
xref: value-type:xsd\:string "The allowed value-type for this CV term."
is_a: MS:1000630 ! data processing parameter */

#define MS_SQID_ID "MS:1001886"
#define MS_SQID_NAME "SQID"
/* def: "Software for data analysis of peptides and proteins." [PSI:MS]
is_a: MS:1001456 ! analysis software
is_a: MS:1001457 ! data processing software */

#define MS_SQID_SCORE_ID "MS:1001887"
#define MS_SQID_SCORE_NAME "SQID:score"
/* def: "The SQID result 'Score'." [PSI:PI]
is_a: MS:1001143 ! search engine specific score for PSMs
is_a: MS:1001153 ! search engine specific score */

#define MS_SQID_DELTASCORE_ID "MS:1001888"
#define MS_SQID_DELTASCORE_NAME "SQID:deltaScore"
/* def: "The SQID result 'deltaScore'." [PSI:PI]
is_a: MS:1001143 ! search engine specific score for PSMs
is_a: MS:1001153 ! search engine specific score */

#define MS_SQID_PROTEIN_SCORE_ID "MS:1001889"
#define MS_SQID_PROTEIN_SCORE_NAME "SQID:protein score"
/* def: "The SQID result 'protein score'." [PSI:PI]
is_a: MS:1001116 ! single protein result details
is_a: MS:1001153 ! search engine specific score */

#define MS_PROGENESIS_PROTEIN_NORMALISED_ABUNDANCE_ID "MS:1001890"
#define MS_PROGENESIS_PROTEIN_NORMALISED_ABUNDANCE_NAME "Progenesis:protein normalised abundance"
/* def: "The data type normalised abundance for proteins produced by Progenesis LC-MS." [PSI:MS]
xref: value-type:xsd\:double "The allowed value-type for this CV term."
is_a: MS:1001805 ! quantification datatype */

#define MS_PROGENESIS_PEPTIDE_NORMALISED_ABUNDANCE_ID "MS:1001891"
#define MS_PROGENESIS_PEPTIDE_NORMALISED_ABUNDANCE_NAME "Progenesis:peptide normalised abundance"
/* def: "The data type normalised abundance for peptides produced by Progenesis LC-MS." [PSI:MS]
xref: value-type:xsd\:double "The allowed value-type for this CV term."
is_a: MS:1001805 ! quantification datatype */

#define MS_PROGENESIS_PROTEIN_RAW_ABUNDANCE_ID "MS:1001892"
#define MS_PROGENESIS_PROTEIN_RAW_ABUNDANCE_NAME "Progenesis:protein raw abundance"
/* def: "The data type raw abundance for proteins produced by Progenesis LC-MS." [PSI:MS]
xref: value-type:xsd\:double "The allowed value-type for this CV term."
is_a: MS:1001805 ! quantification datatype */

#define MS_PROGENESIS_PEPTIDE_RAW_ABUNDANCE_ID "MS:1001893"
#define MS_PROGENESIS_PEPTIDE_RAW_ABUNDANCE_NAME "Progenesis:peptide raw abundance"
/* def: "The data type raw abundance for peptide produced by Progenesis LC-MS." [PSI:MS]
xref: value-type:xsd\:double "The allowed value-type for this CV term."
is_a: MS:1001805 ! quantification datatype */

#define MS_PROGENESIS_CONFIDENCE_SCORE_ID "MS:1001894"
#define MS_PROGENESIS_CONFIDENCE_SCORE_NAME "Progenesis:confidence score"
/* def: "The data type confidence score produced by Progenesis LC-MS." [PSI:MS]
xref: value-type:xsd\:double "The allowed value-type for this CV term."
is_a: MS:1001805 ! quantification datatype */

#define MS_PROGENESIS_PEPTIDE_COUNT_ID "MS:1001895"
#define MS_PROGENESIS_PEPTIDE_COUNT_NAME "Progenesis:peptide count"
/* def: "The data type peptide count produced by Progenesis LC-MS." [PSI:MS]
xref: value-type:xsd\:integer "The allowed value-type for this CV term."
is_a: MS:1001805 ! quantification datatype */

#define MS_PROGENESIS_FEATURE_INTENSITY_ID "MS:1001896"
#define MS_PROGENESIS_FEATURE_INTENSITY_NAME "Progenesis:feature intensity"
/* def: "The data type feature intensity produced by Progenesis LC-MS." [PSI:MS]
xref: value-type:xsd\:double "The allowed value-type for this CV term."
is_a: MS:1001805 ! quantification datatype */

#define MS_MAXQUANT_PEPTIDE_COUNTS__UNIQUE__ID "MS:1001897"
#define MS_MAXQUANT_PEPTIDE_COUNTS__UNIQUE__NAME "MaxQuant:peptide counts (unique)"
/* def: "The data type peptide counts (unique) produced by MaxQuant." [PSI:MS]
xref: value-type:xsd\:integer "The allowed value-type for this CV term."
is_a: MS:1001805 ! quantification datatype */

#define MS_MAXQUANT_PEPTIDE_COUNTS__ALL__ID "MS:1001898"
#define MS_MAXQUANT_PEPTIDE_COUNTS__ALL__NAME "MaxQuant:peptide counts (all)"
/* def: "The data type peptide counts (all) produced by MaxQuant." [PSI:MS]
xref: value-type:xsd\:integer "The allowed value-type for this CV term."
is_a: MS:1001805 ! quantification datatype */

#define MS_MAXQUANT_PEPTIDE_COUNTS__RAZOR_UNIQUE__ID "MS:1001899"
#define MS_MAXQUANT_PEPTIDE_COUNTS__RAZOR_UNIQUE__NAME "MaxQuant:peptide counts (razor+unique)"
/* def: "The data type peptide counts (razor+unique) produced by MaxQuant." [PSI:MS]
xref: value-type:xsd\:integer "The allowed value-type for this CV term."
is_a: MS:1001805 ! quantification datatype */

#define MS_MAXQUANT_SEQUENCE_LENGTH_ID "MS:1001900"
#define MS_MAXQUANT_SEQUENCE_LENGTH_NAME "MaxQuant:sequence length"
/* def: "The data type sequence length produced by MaxQuant." [PSI:MS]
xref: value-type:xsd\:integer "The allowed value-type for this CV term."
is_a: MS:1001805 ! quantification datatype */

#define MS_MAXQUANT_PEP_ID "MS:1001901"
#define MS_MAXQUANT_PEP_NAME "MaxQuant:PEP"
/* def: "The data type PEP (posterior error probability) produced by MaxQuant." [PSI:MS]
xref: value-type:xsd\:double "The allowed value-type for this CV term."
is_a: MS:1001805 ! quantification datatype */

#define MS_MAXQUANT_LFQ_INTENSITY_ID "MS:1001902"
#define MS_MAXQUANT_LFQ_INTENSITY_NAME "MaxQuant:LFQ intensity"
/* def: "The data type LFQ intensity produced by MaxQuant." [PSI:MS]
xref: value-type:xsd\:double "The allowed value-type for this CV term."
is_a: MS:1001805 ! quantification datatype */

#define MS_MAXQUANT_FEATURE_INTENSITY_ID "MS:1001903"
#define MS_MAXQUANT_FEATURE_INTENSITY_NAME "MaxQuant:feature intensity"
/* def: "The data type feature intensity produced by MaxQuant." [PSI:MS]
xref: value-type:xsd\:double "The allowed value-type for this CV term."
is_a: MS:1001805 ! quantification datatype */

#define MS_MAXQUANT_MS_MS_COUNT_ID "MS:1001904"
#define MS_MAXQUANT_MS_MS_COUNT_NAME "MaxQuant:MS_MS count"
/* def: "The data type MS/MS count produced by MaxQuant." [PSI:MS]
xref: value-type:xsd\:integer "The allowed value-type for this CV term."
is_a: MS:1001805 ! quantification datatype */

#define MS_EMPAI_VALUE_ID "MS:1001905"
#define MS_EMPAI_VALUE_NAME "emPAI value"
/* def: "The emPAI value of protein abundance, produced from the emPAI algorithm." [PSI:MS]
xref: value-type:xsd\:double "The allowed value-type for this CV term."
is_a: MS:1001805 ! quantification datatype */

#define MS_APEX_VALUE_ID "MS:1001906"
#define MS_APEX_VALUE_NAME "APEX value"
/* def: "The APEX value of protein abundance, produced from the APEX software." [PSI:MS]
xref: value-type:xsd\:double "The allowed value-type for this CV term."
is_a: MS:1001805 ! quantification datatype */

#define MS_RETENTION_TIME_WINDOW_WIDTH_ID "MS:1001907"
#define MS_RETENTION_TIME_WINDOW_WIDTH_NAME "retention time window width"
/* def: "The full width of a retention time window for a chromatographic peak." [PSI:MS]
xref: value-type:xsd\:float "The allowed value-type for this CV term."
is_a: MS:1000915 ! retention time window attribute
relationship: has_units UO:0000010 ! second
relationship: has_units UO:0000031 ! minute */

#define MS_ISQ_ID "MS:1001908"
#define MS_ISQ_NAME "ISQ"
/* def: "Thermo Scientific ISQ single quadrupole MS with the ExtractraBrite source." [PSI:MS]
is_a: MS:1000494 ! Thermo Scientific instrument model */

#define MS_VELOS_PLUS_ID "MS:1001909"
#define MS_VELOS_PLUS_NAME "Velos Plus"
/* def: "Thermo Scientific second generation Velos." [PSI:MS]
is_a: MS:1000494 ! Thermo Scientific instrument model */

#define MS_LTQ_ORBITRAP_ELITE_ID "MS:1001910"
#define MS_LTQ_ORBITRAP_ELITE_NAME "LTQ Orbitrap Elite"
/* def: "Thermo Scientific second generation Velos and Orbitrap." [PSI:MS]
is_a: MS:1000494 ! Thermo Scientific instrument model */

#define MS_Q_EXACTIVE_ID "MS:1001911"
#define MS_Q_EXACTIVE_NAME "Q Exactive"
/* def: "Thermo Scientific Q Exactive." [PSI:MS]
is_a: MS:1000494 ! Thermo Scientific instrument model */

#define MS_PINPOINT_ID "MS:1001912"
#define MS_PINPOINT_NAME "PinPoint"
/* def: "Thermo Scientific PinPoint SRM analysis software." [PSI:MS]
is_a: MS:1000693 ! Thermo Finnigan software
is_a: MS:1001456 ! analysis software
is_a: MS:1001457 ! data processing software */

#define MS_S_LENS_VOLTAGE_ID "MS:1001913"
#define MS_S_LENS_VOLTAGE_NAME "S-lens voltage"
/* def: "Potential difference setting of the Thermo Scientific S-lens stacked-ring ion guide in volts." [PSI:MS]
xref: value-type:xsd\:float "The allowed value-type for this CV term."
is_a: MS:1000482 ! source attribute
relationship: has_units UO:0000218 ! volt */

#define MS_PYMZML_ID "MS:1001914"
#define MS_PYMZML_NAME "pymzML"
/* def: "Python module to interface mzML Data." [PSI:MS]
is_a: MS:1001457 ! data processing software */

#define MS_LEUKOCYTE_ELASTASE_ID "MS:1001915"
#define MS_LEUKOCYTE_ELASTASE_NAME "leukocyte elastase"
/* def: "Enzyme leukocyte elastase (EC 3.4.21.37)." [BRENDA:3.4.21.37]
is_a: MS:1001045 ! cleavage agent name
relationship: has_regexp MS:1001957 ! (?<=[ALIV])(?!P) */

#define MS_PROLINE_ENDOPEPTIDASE_ID "MS:1001916"
#define MS_PROLINE_ENDOPEPTIDASE_NAME "proline endopeptidase"
/* def: "Enzyme proline endopeptidase (EC 3.4.21.26)." [BRENDA:3.4.21.26]
is_a: MS:1001045 ! cleavage agent name
relationship: has_regexp MS:1001958 ! (?<=[HKR]P)(?!P) */

#define MS_GLUTAMYL_ENDOPEPTIDASE_ID "MS:1001917"
#define MS_GLUTAMYL_ENDOPEPTIDASE_NAME "glutamyl endopeptidase"
/* def: "Enzyme glutamyl endopeptidase (EC 3.4.21.19)." [BRENDA:3.4.21.19]
synonym: "staphylococcal protease" EXACT []
synonym: "Glu-C" EXACT []
is_a: MS:1001045 ! cleavage agent name
relationship: has_regexp MS:1001959 ! (?<=[^E]E) */

#define MS__2_IODOBENZOATE_ID "MS:1001918"
#define MS__2_IODOBENZOATE_NAME "2-iodobenzoate"
/* def: "Chemical iodobenzoate. Cleaves after W." [PubChem_Compound:4739928]
is_a: MS:1001045 ! cleavage agent name
relationship: has_regexp MS:1001960 ! (?<=W) */

#define MS_PROTEOMEXCHANGE_ACCESSION_NUMBER_ID "MS:1001919"
#define MS_PROTEOMEXCHANGE_ACCESSION_NUMBER_NAME "ProteomeXchange accession number"
/* def: "Main identifier of a ProteomeXchange dataset." [PSI:PI]
xref: value-type:xsd\:string "The allowed value-type for this CV term."
is_a: MS:1000878 ! external reference identifier */

#define MS_PROTEOMEXCHANGE_ACCESSION_NUMBER_VERSION_NUMBER_ID "MS:1001921"
#define MS_PROTEOMEXCHANGE_ACCESSION_NUMBER_VERSION_NUMBER_NAME "ProteomeXchange accession number version number"
/* def: "Version number of a ProteomeXchange accession number." [PSI:PI]
xref: value-type:xsd\:nonNegativeInteger "The allowed value-type for this CV term."
is_a: MS:1000878 ! external reference identifier */

#define MS_DIGITAL_OBJECT_IDENTIFIER__DOI__ID "MS:1001922"
#define MS_DIGITAL_OBJECT_IDENTIFIER__DOI__NAME "Digital Object Identifier (DOI)"
/* def: "DOI unique identifier." [PSI:PI]
xref: value-type:xsd\:string "The allowed value-type for this CV term."
is_a: MS:1000878 ! external reference identifier
relationship: has_regexp MS:1001176 ! (10\.(\d)+(\.(\d)*)?/[^\*].+) */

#define MS_EXTERNAL_REFERENCE_KEYWORD_ID "MS:1001923"
#define MS_EXTERNAL_REFERENCE_KEYWORD_NAME "external reference keyword"
/* def: "Free text attribute that can enrich the information about an entity." [PSI:PI]
xref: value-type:xsd\:string "The allowed value-type for this CV term."
is_a: MS:1000878 ! external reference identifier */

#define MS_JOURNAL_ARTICLE_KEYWORD_ID "MS:1001924"
#define MS_JOURNAL_ARTICLE_KEYWORD_NAME "journal article keyword"
/* def: "Keyword present in a scientific publication." [PSI:PI]
xref: value-type:xsd\:string "The allowed value-type for this CV term."
is_a: MS:1001923 ! external reference keyword */

#define MS_SUBMITTER_KEYWORD_ID "MS:1001925"
#define MS_SUBMITTER_KEYWORD_NAME "submitter keyword"
/* def: "Keyword assigned by the data submitter." [PSI:PI]
xref: value-type:xsd\:string "The allowed value-type for this CV term."
is_a: MS:1001923 ! external reference keyword */

#define MS_CURATOR_KEYWORD_ID "MS:1001926"
#define MS_CURATOR_KEYWORD_NAME "curator keyword"
/* def: "Keyword assigned by a data curator." [PSI:PI]
xref: value-type:xsd\:string "The allowed value-type for this CV term."
is_a: MS:1001923 ! external reference keyword */

#define MS_TRANCHE_FILE_HASH_ID "MS:1001927"
#define MS_TRANCHE_FILE_HASH_NAME "Tranche file hash"
/* def: "Hash assigned by the Tranche resource to an individual file." [PSI:PI]
xref: value-type:xsd\:string "The allowed value-type for this CV term."
is_a: MS:1000878 ! external reference identifier */

#define MS_TRANCHE_PROJECT_HASH_ID "MS:1001928"
#define MS_TRANCHE_PROJECT_HASH_NAME "Tranche project hash"
/* def: "Hash assigned by the Tranche resource to a whole project." [PSI:PI]
xref: value-type:xsd\:string "The allowed value-type for this CV term."
is_a: MS:1000878 ! external reference identifier */

#define MS_PRIDE_EXPERIMENT_URI_ID "MS:1001929"
#define MS_PRIDE_EXPERIMENT_URI_NAME "PRIDE experiment URI"
/* def: "URI that allows the access to one experiment in the PRIDE database." [PSI:PI]
xref: value-type:xsd\:anyURI "The allowed value-type for this CV term."
is_a: MS:1000878 ! external reference identifier */

#define MS_PRIDE_PROJECT_URI_ID "MS:1001930"
#define MS_PRIDE_PROJECT_URI_NAME "PRIDE project URI"
/* def: "URI that allows the access to one project in the PRIDE database." [PSI:PI]
xref: value-type:xsd\:anyURI "The allowed value-type for this CV term."
is_a: MS:1000878 ! external reference identifier */

#define MS_SOURCE_INTERFACE_ID "MS:1001931"
#define MS_SOURCE_INTERFACE_NAME "source interface"
/* def: "The source interface." [PSI:MS]
relationship: part_of MS:1000458 ! source */

#define MS_SOURCE_INTERFACE_MODEL_ID "MS:1001932"
#define MS_SOURCE_INTERFACE_MODEL_NAME "source interface model"
/* def: "The source interface model." [PSI:MS]
xref: value-type:xsd\:string "The allowed value-type for this CV term."
relationship: part_of MS:1001931 ! source interface */

#define MS_SOURCE_SPRAYER_ID "MS:1001933"
#define MS_SOURCE_SPRAYER_NAME "source sprayer"
/* def: "The source sprayer." [PSI:MS]
xref: value-type:xsd\:string "The allowed value-type for this CV term."
relationship: part_of MS:1000458 ! source */

#define MS_SOURCE_SPRAYER_TYPE_ID "MS:1001934"
#define MS_SOURCE_SPRAYER_TYPE_NAME "source sprayer type"
/* def: "The source sprayer type." [PSI:MS]
relationship: part_of MS:1001933 ! source sprayer */

#define MS_SOURCE_SPRAYER_MANUFACTURER_ID "MS:1001935"
#define MS_SOURCE_SPRAYER_MANUFACTURER_NAME "source sprayer manufacturer"
/* def: "The source sprayer manufacturer." [PSI:MS]
xref: value-type:xsd\:string "The allowed value-type for this CV term."
relationship: part_of MS:1001933 ! source sprayer */

#define MS_SOURCE_SPRAYER_MODEL_ID "MS:1001936"
#define MS_SOURCE_SPRAYER_MODEL_NAME "source sprayer model"
/* def: "The source sprayer model." [PSI:MS]
xref: value-type:xsd\:string "The allowed value-type for this CV term."
relationship: part_of MS:1001933 ! source sprayer */

#define MS_SAMPLE_PLATE_ID "MS:1001937"
#define MS_SAMPLE_PLATE_NAME "sample plate"
/* def: "Plate where the sample solution is spotted in a MALDI or similar instrument." [PSI:MS]
relationship: part_of MS:1000458 ! source */

#define MS_SAMPLE_PLATE_TYPE_ID "MS:1001938"
#define MS_SAMPLE_PLATE_TYPE_NAME "sample plate type"
/* def: "The sample plate type." [PSI:MS]
relationship: part_of MS:1001937 ! sample plate */

#define MS_STAINLESS_STEEL_PLATE_ID "MS:1001939"
#define MS_STAINLESS_STEEL_PLATE_NAME "stainless steel plate"
/* def: "Stainless steel plate." [PSI:MS]
is_a: MS:1001938 ! sample plate type */

#define MS_COATED_GLASS_PLATE_ID "MS:1001940"
#define MS_COATED_GLASS_PLATE_NAME "coated glass plate"
/* def: "Coated glass plate." [PSI:MS]
is_a: MS:1001938 ! sample plate type */

#define MS_ELECTROSPRAY_SUPPLY_TYPE_ID "MS:1001941"
#define MS_ELECTROSPRAY_SUPPLY_TYPE_NAME "electrospray supply type"
/* def: "Whether the sprayer is fed or is loaded with sample once." [PSI:MS]
relationship: part_of MS:1000458 ! source */

#define MS_STATIC_SUPPLY_ELECTROSPRAY_ID "MS:1001942"
#define MS_STATIC_SUPPLY_ELECTROSPRAY_NAME "static supply electrospray"
/* def: "The sprayer is loaded with sample once." [PSI:MS]
is_a: MS:1001941 ! electrospray supply type */

#define MS_FED_SUPPLY_ELECTROSPRAY_ID "MS:1001943"
#define MS_FED_SUPPLY_ELECTROSPRAY_NAME "fed supply electrospray"
/* def: "The sprayer is continuously fed with sample." [PSI:MS]
is_a: MS:1001941 ! electrospray supply type */

#define MS_COLLISION_CELL_EXIT_POTENTIAL_ID "MS:1001944"
#define MS_COLLISION_CELL_EXIT_POTENTIAL_NAME "Collision cell exit potential"
/* def: "Potential difference between Q2 and Q3 in a triple quadrupole instrument in volts." [PSI:MS]	
xref: value-type:xsd\:float "The allowed value-type for this CV term."
is_a: MS:1000510 ! precursor activation attribute
relationship: has_units UO:0000218 ! volt
synonym: "CXP" EXACT [] */

#define MS_PEGASUS_4D_ID "MS:1001945"
#define MS_PEGASUS_4D_NAME "Pegasus 4D"
/* def: "LECO nominal mass resolution time-of-flight GCxGC mass spectrometer." [PSI:MS]
is_a: MS:1001800 ! LECO instrument model */

#define MS_PEAKS_STUDIO_ID "MS:1001946"
#define MS_PEAKS_STUDIO_NAME "PEAKS Studio"
/* def: "PEAKS Studio software for data analysis." [PSI:MS]
is_a: MS:1001139 ! quantitation software name
is_a: MS:1001456 ! analysis software
is_a: MS:1001457 ! data processing software */

#define MS_PEAKS_ONLINE_ID "MS:1001947"
#define MS_PEAKS_ONLINE_NAME "PEAKS Online"
/* def: "PEAKS Online software for high throughput data analysis." [PSI:MS]
is_a: MS:1001139 ! quantitation software name
is_a: MS:1001456 ! analysis software
is_a: MS:1001457 ! data processing software */

#define MS_PEAKS_NODE_ID "MS:1001948"
#define MS_PEAKS_NODE_NAME "PEAKS Node"
/* def: "PEAKS Node software for high throughput data analysis." [PSI:MS]
is_a: MS:1001139 ! quantitation software name
is_a: MS:1001456 ! analysis software
is_a: MS:1001457 ! data processing software */

#define MS_BSI_SOFTWARE_ID "MS:1001949"
#define MS_BSI_SOFTWARE_NAME "BSI software"
/* def: "Bioinformatics Solutions Inc. Software for data processing and analysis." [PSI:MS]
is_a: MS:1000531 ! software */

#define MS_PEAKS_PEPTIDESCORE_ID "MS:1001950"
#define MS_PEAKS_PEPTIDESCORE_NAME "PEAKS:peptideScore"
/* def: "The PEAKS peptide '-10lgP Score'." [PSI:MS]
xref: value-type:xsd\:double "The allowed value-type for this CV term."
is_a: MS:1001143 ! search engine specific score for PSMs
is_a: MS:1001153 ! search engine specific score */

#define MS_PEAKS_PROTEINSCORE_ID "MS:1001951"
#define MS_PEAKS_PROTEINSCORE_NAME "PEAKS:proteinScore"
/* def: "The PEAKS protein '-10lgP Score'." [PSI:MS]
xref: value-type:xsd\:double "The allowed value-type for this CV term."
is_a: MS:1001116 ! single protein result details
is_a: MS:1001153 ! search engine specific score */

#define MS_ZCORE_PROBSCORE_ID "MS:1001952"
#define MS_ZCORE_PROBSCORE_NAME "ZCore:probScore"
/* def: "The ZCore probability score." [PSI:MS]
xref: value-type:xsd\:double "The allowed value-type for this CV term."
is_a: MS:1001143 ! search engine specific score for PSMs
is_a: MS:1001153 ! search engine specific score */

#define MS_SOURCE_INTERFACE_MANUFACTURER_ID "MS:1001953"
#define MS_SOURCE_INTERFACE_MANUFACTURER_NAME "source interface manufacturer"
/* def: "The source interface manufacturer." [PSI:MS]
xref: value-type:xsd\:string "The allowed value-type for this CV term."
relationship: part_of MS:1001931 ! source interface */

#define MS_ACQUISITION_PARAMETER_ID "MS:1001954"
#define MS_ACQUISITION_PARAMETER_NAME "acquisition parameter"
/* def: "Parameters used in the mass spectrometry acquisition." [PSI:MS]
xref: value-type:xsd\:string "The allowed value-type for this CV term."
relationship: part_of MS:1001458 ! spectrum generation information */

#define MS_NO_CLEAVAGE_ID "MS:1001955"
#define MS_NO_CLEAVAGE_NAME "no cleavage"
/* def: "No cleavage." [PSI:MS]
is_a: MS:1001045 ! cleavage agent name */

#define MS_UNSPECIFIC_CLEAVAGE_ID "MS:1001956"
#define MS_UNSPECIFIC_CLEAVAGE_NAME "unspecific cleavage"
/* def: "Unspecific cleavage." [PSI:MS]
is_a: MS:1001045 ! cleavage agent name */

#define MS_PEPTIDE_SPECTRUM_MATCH_SCORING_ALGORITHM_ID "MS:1001961"
#define MS_PEPTIDE_SPECTRUM_MATCH_SCORING_ALGORITHM_NAME "peptide spectrum match scoring algorithm"
/* def: "Algorithm used to score the match between a spectrum and a peptide ion." [PSI:MS]
xref: value-type:xsd\:string "The allowed value-type for this CV term."
relationship: part_of MS:1001458 ! spectrum generation information */

#define MS_MASCOT_C13_COUNTS_ID "MS:1001962"
#define MS_MASCOT_C13_COUNTS_NAME "Mascot:C13 counts"
/* def: "C13 peaks to use in peak detection." [PSI:MS]
xref: value-type:xsd\:nonNegativeInteger "The allowed value-type for this CV term."
is_a: MS:1002095 ! Mascot input parameter */

#define MS_PROTEINEXTRACTOR_WEIGHTING_ID "MS:1001963"
#define MS_PROTEINEXTRACTOR_WEIGHTING_NAME "ProteinExtractor:Weighting"
/* def: "Weighting factor for protein list compilation by ProteinExtractor." [PSI:MS]
xref: value-type:xsd\:float "The allowed value-type for this CV term."
is_a: MS:1002098 ! ProteinExtractor input parameter */

#define MS_PROTEINSCAPE_SECOND_ROUND_MASCOT_ID "MS:1001964"
#define MS_PROTEINSCAPE_SECOND_ROUND_MASCOT_NAME "ProteinScape:second round Mascot"
/* def: "Flag indicating a second round search with Mascot." [PSI:MS]
xref: value-type:xsd\:boolean "The allowed value-type for this CV term."
is_a: MS:1002100 ! ProteinScape input parameter */

#define MS_PROTEINSCAPE_SECOND_ROUND_PHENYX_ID "MS:1001965"
#define MS_PROTEINSCAPE_SECOND_ROUND_PHENYX_NAME "ProteinScape:second round Phenyx"
/* def: "Flag indicating a second round search with Phenyx." [PSI:MS]
xref: value-type:xsd\:boolean "The allowed value-type for this CV term."
is_a: MS:1002100 ! ProteinScape input parameter */

#define MS_PRODUCT_ION_MOBILITY_ID "MS:1001966"
#define MS_PRODUCT_ION_MOBILITY_NAME "product ion mobility"
/* def: "The mobility of an MS2 product ion, as measured by ion mobility mass spectrometry." [PSI:MS]
is_a: MS:1001221 ! fragmentation information */

#define MS_PRODUCT_ION_DRIFT_TIME_ID "MS:1001967"
#define MS_PRODUCT_ION_DRIFT_TIME_NAME "product ion drift time"
/* def: "The ion drift time of an MS2 product ion." [PSI:MS]
xref: value-type:xsd\:float "The allowed value-type for this CV term."
is_a: MS:1002222 ! SRM transition attribute
relationship: has_units UO:0000028 ! millisecond */

#define MS_PTM_LOCALIZATION_SCORE_ID "MS:1001968"
#define MS_PTM_LOCALIZATION_SCORE_NAME "PTM localization score"
/* def: "A score that assign confidence to the localization of an amino acid modification on a peptide sequence." [PSI:MS]
is_a: MS:1001143 ! search engine specific score for PSMs */

#define MS_PROTEOMEDISCOVERER_PHOSPHORS_SCORE_ID "MS:1001969"
#define MS_PROTEOMEDISCOVERER_PHOSPHORS_SCORE_NAME "ProteomeDiscoverer:phosphoRS score"
/* def: "Peptide score based on the cumulative binomial probability that the observed match is a random event." [DOI:10.1021/pr200611n, PMID:22073976]
is_a: MS:1001968 ! PTM localization score */

#define MS_PROTEOMEDISCOVERER_PHOSPHORS_SEQUENCE_PROBABILITY_ID "MS:1001970"
#define MS_PROTEOMEDISCOVERER_PHOSPHORS_SEQUENCE_PROBABILITY_NAME "ProteomeDiscoverer:phosphoRS sequence probability"
/* def: "Probability that the respective isoform is correct." [DOI:10.1021/pr200611n, PMID:22073976]
is_a: MS:1001968 ! PTM localization score */

#define MS_PROTEOMEDISCOVERER_PHOSPHORS_SITE_PROBABILITY_ID "MS:1001971"
#define MS_PROTEOMEDISCOVERER_PHOSPHORS_SITE_PROBABILITY_NAME "ProteomeDiscoverer:phosphoRS site probability"
/* def: "Estimate of the probability that the respective site is truly phosphorylated." [DOI:10.1021/pr200611n, PMID:22073976]
is_a: MS:1001968 ! PTM localization score */

#define MS_PTM_SCORING_ALGORITHM_VERSION_ID "MS:1001972"
#define MS_PTM_SCORING_ALGORITHM_VERSION_NAME "PTM scoring algorithm version"
/* def: "Version of the post-translational modification scoring algorithm." [PSI:MS]
xref: value-type:xsd\:string "The allowed value-type for this CV term."
is_a: MS:1001471 ! peptide modification details */

#define MS_DEBUNKER_ID "MS:1001973"
#define MS_DEBUNKER_NAME "DeBunker"
/* def: "DeBunker software." [PSI:MS]
is_a: MS:1001456 ! analysis software */

#define MS_DEBUNKER_SCORE_ID "MS:1001974"
#define MS_DEBUNKER_SCORE_NAME "DeBunker:score"
/* def: "Score specific to DeBunker." [PSI:MS]
is_a: MS:1001153 ! search engine specific score
is_a: MS:1001143 ! search engine specific score for PSMs
is_a: MS:1001405 ! spectrum identification result details
is_a: MS:1001968 ! PTM localization score */

#define MS_DELTA_M_Z_ID "MS:1001975"
#define MS_DELTA_M_Z_NAME "delta m/z"
/* def: "The difference between a theoretically calculated m/z and the corresponding experimentally measured m/z. It can be expressed as absolute or relative value." [PSI:MS]
synonym: "m/z difference" EXACT []
is_a: MS:1001405 ! spectrum identification result details
relationship: has_units UO:0000166 ! parts per notation unit
relationship: has_units UO:0000187 ! percent
relationship: has_units UO:0000221 ! dalton */

#define MS_DELTA_M_ID "MS:1001976"
#define MS_DELTA_M_NAME "delta M"
/* def: "The difference between a theoretically calculated molecular mass M and the corresponding experimentally measured M. It can be expressed as absolute or relative value." [PSI:MS]
synonym: "mass difference" EXACT []
is_a: MS:1001405 ! spectrum identification result details
relationship: has_units UO:0000166 ! parts per notation unit
relationship: has_units UO:0000187 ! percent
relationship: has_units UO:0000221 ! dalton */

#define MS_MSQUANT_ID "MS:1001977"
#define MS_MSQUANT_NAME "MSQuant"
/* def: "MSQuant software." [PSI:MS]
is_a: MS:1001456 ! analysis software */

#define MS_MSQUANT_PTM_SCORE_ID "MS:1001978"
#define MS_MSQUANT_PTM_SCORE_NAME "MSQuant:PTM-score"
/* def: "The PTM score from MSQuant software." [DOI:10.1021/pr900721e, PMID:19888749]
is_a: MS:1001153 ! search engine specific score
is_a: MS:1001143 ! search engine specific score for PSMs
is_a: MS:1001405 ! spectrum identification result details
is_a: MS:1001968 ! PTM localization score */

#define MS_MAXQUANT_PTM_SCORE_ID "MS:1001979"
#define MS_MAXQUANT_PTM_SCORE_NAME "MaxQuant:PTM Score"
/* def: "The PTM score from MaxQuant software." [PSI:MS]
is_a: MS:1001153 ! search engine specific score
is_a: MS:1001143 ! search engine specific score for PSMs
is_a: MS:1001405 ! spectrum identification result details
is_a: MS:1001968 ! PTM localization score */

#define MS_MAXQUANT_PHOSPHO__STY__PROBABILITIES_ID "MS:1001980"
#define MS_MAXQUANT_PHOSPHO__STY__PROBABILITIES_NAME "MaxQuant:Phospho (STY) Probabilities"
/* def: "The Phospho (STY) Probabilities from MaxQuant software." [PSI:MS]
is_a: MS:1001153 ! search engine specific score
is_a: MS:1001143 ! search engine specific score for PSMs
is_a: MS:1001405 ! spectrum identification result details
is_a: MS:1001968 ! PTM localization score */

#define MS_MAXQUANT_PHOSPHO__STY__SCORE_DIFFS_ID "MS:1001981"
#define MS_MAXQUANT_PHOSPHO__STY__SCORE_DIFFS_NAME "MaxQuant:Phospho (STY) Score Diffs"
/* def: "The Phospho (STY) Score Diffs from MaxQuant software." [PSI:MS]
is_a: MS:1001153 ! search engine specific score
is_a: MS:1001143 ! search engine specific score for PSMs
is_a: MS:1001405 ! spectrum identification result details
is_a: MS:1001968 ! PTM localization score */

#define MS_MAXQUANT_P_SITE_LOCALIZATION_PROBABILITY_ID "MS:1001982"
#define MS_MAXQUANT_P_SITE_LOCALIZATION_PROBABILITY_NAME "MaxQuant:P-site localization probability"
/* def: "The P-site localization probability value from MaxQuant software." [PSI:MS]
is_a: MS:1001153 ! search engine specific score
is_a: MS:1001143 ! search engine specific score for PSMs
is_a: MS:1001405 ! spectrum identification result details
is_a: MS:1001968 ! PTM localization score */

#define MS_MAXQUANT_PTM_DELTA_SCORE_ID "MS:1001983"
#define MS_MAXQUANT_PTM_DELTA_SCORE_NAME "MaxQuant:PTM Delta Score"
/* def: "The PTM Delta Score value from MaxQuant software (Difference between highest scoring site and second highest)." [PSI:MS]
is_a: MS:1001153 ! search engine specific score
is_a: MS:1001143 ! search engine specific score for PSMs
is_a: MS:1001405 ! spectrum identification result details
is_a: MS:1001968 ! PTM localization score */

#define MS_ASCORE_ID "MS:1001984"
#define MS_ASCORE_NAME "Ascore"
/* def: "Ascore software." [PSI:MS]
is_a: MS:1001456 ! analysis software */

#define MS_ASCORE_ASCORE_ID "MS:1001985"
#define MS_ASCORE_ASCORE_NAME "Ascore:Ascore"
/* def: "The Ascore score value from Ascore software." [DOI:10.1038/nbt1240, PMID:16964243]
is_a: MS:1001153 ! search engine specific score
is_a: MS:1001143 ! search engine specific score for PSMs
is_a: MS:1001405 ! spectrum identification result details
is_a: MS:1001968 ! PTM localization score */

#define MS_H_SCORE_ID "MS:1001986"
#define MS_H_SCORE_NAME "H-Score"
/* def: "H-Score for peptide phosphorylation site location." [DOI:10.1021/pr1006813, PMID:20836569]
is_a: MS:1001968 ! PTM localization score */

#define MS_VACUUM_DRYING_MALDI_SAMPLE_PREPARATION_ID "MS:1001987"
#define MS_VACUUM_DRYING_MALDI_SAMPLE_PREPARATION_NAME "vacuum drying MALDI sample preparation"
/* def: "Vacuum-drying MALDI sample preparation crystallization method." [PSI:MS]
is_a: MS:1000833 ! matrix application type */

#define MS_CRUSHED_CRYSTAL_MALDI_SAMPLE_PREPARATION_ID "MS:1001988"
#define MS_CRUSHED_CRYSTAL_MALDI_SAMPLE_PREPARATION_NAME "crushed crystal MALDI sample preparation"
/* def: "Crushed-crystal MALDI sample preparation method." [PSI:MS]
is_a: MS:1000833 ! matrix application type */

#define MS_FAST_EVAPORATION_MALDI_SAMPLE_PREPARATION_ID "MS:1001989"
#define MS_FAST_EVAPORATION_MALDI_SAMPLE_PREPARATION_NAME "fast evaporation MALDI sample preparation"
/* def: "Fast-evaporation MALDI sample preparation method." [DOI:10.1021/ac00091a044]
is_a: MS:1000833 ! matrix application type */

#define MS_OVERLAYER_MALDI_SAMPLE_PREPARATION_ID "MS:1001990"
#define MS_OVERLAYER_MALDI_SAMPLE_PREPARATION_NAME "overlayer MALDI sample preparation"
/* def: "Overlayer method combining features of the crushed-crystal method and the fast-evaporation method." [PSI:MS]
is_a: MS:1000833 ! matrix application type */

#define MS_SANDWICH_MALDI_SAMPLE_PREPARATION_ID "MS:1001991"
#define MS_SANDWICH_MALDI_SAMPLE_PREPARATION_NAME "sandwich MALDI sample preparation"
/* def: "Sandwich MALDI sample preparation method." [DOI:10.1002/(SICI)1096-9888(199706)32:6<593::AID-JMS511>3.3.CO;2-4]
is_a: MS:1000833 ! matrix application type */

#define MS_SPIN_COATING_MALDI_SAMPLE_PREPARATION_ID "MS:1001992"
#define MS_SPIN_COATING_MALDI_SAMPLE_PREPARATION_NAME "spin coating MALDI sample preparation"
/* def: "Spin coating MALDI sample preparation method." [DOI:10.1021/cc0500710, PMID:16283807]
is_a: MS:1000833 ! matrix application type */

#define MS_QUICK_AND_DIRTY_MALDI_SAMPLE_PREPARATION_ID "MS:1001993"
#define MS_QUICK_AND_DIRTY_MALDI_SAMPLE_PREPARATION_NAME "quick and dirty MALDI sample preparation"
/* def: "Quick & dirty (Q&D) sample preparation separating matrix handling from sample handling." [PSI:MS]
is_a: MS:1000833 ! matrix application type */

#define MS_TOP_HAT_BASELINE_REDUCTION_ID "MS:1001994"
#define MS_TOP_HAT_BASELINE_REDUCTION_NAME "top hat baseline reduction"
/* def: "Top-hat morphological filter based on the basic morphological operations 'erosion' and 'dilatation'." [PSI:MS]
is_a: MS:1000593 ! baseline reduction */

#define MS_CONVEX_HULL_BASELINE_REDUCTION_ID "MS:1001995"
#define MS_CONVEX_HULL_BASELINE_REDUCTION_NAME "convex hull baseline reduction"
/* def: "Constructs the baseline by fitting multiple parabolas to the spectrum starting with the large scale structures." [PSI:MS]
is_a: MS:1000593 ! baseline reduction */

#define MS_MEDIAN_BASELINE_REDUCTION_ID "MS:1001996"
#define MS_MEDIAN_BASELINE_REDUCTION_NAME "median baseline reduction"
/* def: "The spectrum that will be baseline subtracted is divided into a number of segments." [PSI:MS]
is_a: MS:1000593 ! baseline reduction */

#define MS_WAVELET_TRANSFORMATION_SMOOTHING_ID "MS:1001997"
#define MS_WAVELET_TRANSFORMATION_SMOOTHING_NAME "wavelet transformation smoothing"
/* def: "The random noise is removed by using the undecimated wavelet transform." [DOI:10.1093/bioinformatics/btl355, PMID:16820428]
is_a: MS:1000592 ! smoothing */

#define MS_SOPHISTICATED_NUMERICAL_ANNOTATION_PROCEDURE_ID "MS:1001998"
#define MS_SOPHISTICATED_NUMERICAL_ANNOTATION_PROCEDURE_NAME "sophisticated numerical annotation procedure"
/* def: "It searches for known patterns in the measured spectrum." [DOI:10.1021/ac951158i, PMID:21619291]
synonym: "SNAP" EXACT []
is_a: MS:1000801 ! area peak picking */

#define MS_AREA_NORMALIZATION_ID "MS:1001999"
#define MS_AREA_NORMALIZATION_NAME "area normalization"
/* def: "Normalization of areas below the curves." [PSI:MS]
is_a: MS:1001484 ! intensity normalization */

#define MS_LIFT_ID "MS:1002000"
#define MS_LIFT_NAME "LIFT"
/* def: "A Bruker's proprietary technique where molecular ions are initially accelerated at lower energy, then collide with inert gas in a collision cell that is then 'lifted' to high potential. The use of inert gas is optional, as it could lift also fragments provided by LID." [DOI:10.1007/s00216-003-2057-0 , PMID:12830354]
is_a: MS:1000044 ! dissociation method */

#define MS_MS1_LABEL_BASED_RAW_FEATURE_QUANTITATION_ID "MS:1002001"
#define MS_MS1_LABEL_BASED_RAW_FEATURE_QUANTITATION_NAME "MS1 label-based raw feature quantitation"
/* def: "MS1 label-based raw feature quantitation." [PSI:PI]
xref: value-type:xsd\:boolean "The allowed value-type for this CV term."
is_a: MS:1002018 ! MS1 label-based analysis */

#define MS_MS1_LABEL_BASED_PEPTIDE_LEVEL_QUANTITATION_ID "MS:1002002"
#define MS_MS1_LABEL_BASED_PEPTIDE_LEVEL_QUANTITATION_NAME "MS1 label-based peptide level quantitation"
/* def: "MS1 label-based peptide level quantitation." [PSI:PI]
xref: value-type:xsd\:boolean "The allowed value-type for this CV term."
is_a: MS:1002018 ! MS1 label-based analysis */

#define MS_MS1_LABEL_BASED_PROTEIN_LEVEL_QUANTITATION_ID "MS:1002003"
#define MS_MS1_LABEL_BASED_PROTEIN_LEVEL_QUANTITATION_NAME "MS1 label-based protein level quantitation"
/* def: "MS1 label-based protein level quantitation." [PSI:PI]
xref: value-type:xsd\:boolean "The allowed value-type for this CV term."
is_a: MS:1002018 ! MS1 label-based analysis */

#define MS_MS1_LABEL_BASED_PROTEINGROUP_LEVEL_QUANTITATION_ID "MS:1002004"
#define MS_MS1_LABEL_BASED_PROTEINGROUP_LEVEL_QUANTITATION_NAME "MS1 label-based proteingroup level quantitation"
/* def: "MS1 label-based proteingroup level quantitation." [PSI:PI]
xref: value-type:xsd\:boolean "The allowed value-type for this CV term."
is_a: MS:1002018 ! MS1 label-based analysis */

#define MS_IRT_RETENTION_TIME_NORMALIZATION_STANDARD_ID "MS:1002005"
#define MS_IRT_RETENTION_TIME_NORMALIZATION_STANDARD_NAME "iRT retention time normalization standard"
/* def: "A de facto standard providing the retention times at which a specific set of 10 reference peptides exit the reference chromatographic column. The kit may be obtain from Biognosys." [DOI:10.1002/pmic.201100463, http://www.biognosys.ch/products/rt-kit.html]
is_a: MS:1000901 ! retention time normalization standard */

#define MS_SRM_TRANSITION_TYPE_ID "MS:1002006"
#define MS_SRM_TRANSITION_TYPE_NAME "SRM transition type"
/* def: "The type of the transitions, e.g. target or decoy." [PSI:MS]
synonym: "MRM transition type" EXACT []
relationship: part_of MS:1000908 ! transition */

#define MS_TARGET_SRM_TRANSITION_ID "MS:1002007"
#define MS_TARGET_SRM_TRANSITION_NAME "target SRM transition"
/* def: "A transition used to target a specific compound that may be in the sample." [PSI:MS]
synonym: "target MRM transition" EXACT []
is_a: MS:1002006 ! SRM transition type */

#define MS_DECOY_SRM_TRANSITION_ID "MS:1002008"
#define MS_DECOY_SRM_TRANSITION_NAME "decoy SRM transition"
/* def: "A transition not expected to be present in the sample and used to calculate statistical confidence of target transition detections in some workflows." [PSI:MS]
synonym: "decoy MRM transition" EXACT []
is_a: MS:1002006 ! SRM transition type */

#define MS_ISOBARIC_LABEL_QUANTITATION_ANALYSIS_ID "MS:1002009"
#define MS_ISOBARIC_LABEL_QUANTITATION_ANALYSIS_NAME "isobaric label quantitation analysis"
/* def: "Quantitation analysis using an isobaric labeling workflow." [PSI:PI]
is_a: MS:1001833 ! quantitation analysis summary */

#define MS_TMT_QUANTITATION_ANALYSIS_ID "MS:1002010"
#define MS_TMT_QUANTITATION_ANALYSIS_NAME "TMT quantitation analysis"
/* def: "Quantitation analysis using the Thermo Fisher tandem mass tag (TMT) labeling workflow." [PSI:PI]
is_a: MS:1001833 ! quantitation analysis summary
is_a: MS:1002009 ! isobaric label quantitation analysis */

#define MS_DESORPTION_ELECTROSPRAY_IONIZATION_ID "MS:1002011"
#define MS_DESORPTION_ELECTROSPRAY_IONIZATION_NAME "desorption electrospray ionization"
/* def: "Combination of electrospray and desorption ionization method that ionizes gases, liquids and solids in open air under atmospheric pressure." [DOI:10.1126/science.1104404, PMID:15486296]
synonym: "DESI" EXACT []
is_a: MS:1000240 ! atmospheric pressure ionization */

#define MS_MASCOT_PTM_SITE_ASSIGNMENT_CONFIDENCE_ID "MS:1002012"
#define MS_MASCOT_PTM_SITE_ASSIGNMENT_CONFIDENCE_NAME "Mascot:PTM site assignment confidence"
/* def: "Relative probability that PTM site assignment is correct, derived from the Mascot score difference between matches to the same spectrum (Mascot Delta Score)." [http://www.matrixscience.com/help/pt_mods_help.html#SITE]
xref: value-type:xsd\:float "The allowed value-type for this CV term."
is_a: MS:1001153 ! search engine specific score
is_a: MS:1001143 ! search engine specific score for PSMs
is_a: MS:1001405 ! spectrum identification result details
is_a: MS:1001968 ! PTM localization score
relationship: has_units UO:0000187 ! percent */

#define MS_COLLISION_ENERGY_RAMP_START_ID "MS:1002013"
#define MS_COLLISION_ENERGY_RAMP_START_NAME "collision energy ramp start"
/* def: "Collision energy at the start of the collision energy ramp." [PSI:PI]
xref: value-type:xsd\:float "The allowed value-type for this CV term."
is_a: MS:1000045 ! collision energy
relationship: has_units UO:0000266 ! electronvolt */

#define MS_COLLISION_ENERGY_RAMP_END_ID "MS:1002014"
#define MS_COLLISION_ENERGY_RAMP_END_NAME "collision energy ramp end"
/* def: "Collision energy at the end of the collision energy ramp." [PSI:PI]
xref: value-type:xsd\:float "The allowed value-type for this CV term."
is_a: MS:1000045 ! collision energy
relationship: has_units UO:0000266 ! electronvolt */

#define MS_SPECTRAL_COUNT_PEPTIDE_LEVEL_QUANTITATION_ID "MS:1002015"
#define MS_SPECTRAL_COUNT_PEPTIDE_LEVEL_QUANTITATION_NAME "spectral count peptide level quantitation"
/* def: "Spectral count peptide level quantitation." [PSI:PI]
xref: value-type:xsd\:boolean "The allowed value-type for this CV term."
is_a: MS:1001836 ! spectral counting quantitation analysis */

#define MS_SPECTRAL_COUNT_PROTEIN_LEVEL_QUANTITATION_ID "MS:1002016"
#define MS_SPECTRAL_COUNT_PROTEIN_LEVEL_QUANTITATION_NAME "spectral count protein level quantitation"
/* def: "Spectral count protein level quantitation." [PSI:PI]
xref: value-type:xsd\:boolean "The allowed value-type for this CV term."
is_a: MS:1001836 ! spectral counting quantitation analysis */

#define MS_SPECTRAL_COUNT_PROTEINGROUP_LEVEL_QUANTITATION_ID "MS:1002017"
#define MS_SPECTRAL_COUNT_PROTEINGROUP_LEVEL_QUANTITATION_NAME "spectral count proteingroup level quantitation"
/* def: "Spectral count proteingroup level quantitation." [PSI:PI]
xref: value-type:xsd\:boolean "The allowed value-type for this CV term."
is_a: MS:1001836 ! spectral counting quantitation analysis */

#define MS_MS1_LABEL_BASED_ANALYSIS_ID "MS:1002018"
#define MS_MS1_LABEL_BASED_ANALYSIS_NAME "MS1 label-based analysis"
/* def: "MS1 label-based analysis." [PSI:PI]
is_a: MS:1001833 ! quantitation analysis summary */

#define MS_LABEL_FREE_RAW_FEATURE_QUANTITATION_ID "MS:1002019"
#define MS_LABEL_FREE_RAW_FEATURE_QUANTITATION_NAME "label-free raw feature quantitation"
/* def: "Label-free raw feature quantitation." [PSI:PI]
xref: value-type:xsd\:boolean "The allowed value-type for this CV term."
is_a: MS:1001834 ! LC-MS label-free quantitation analysis */

#define MS_LABEL_FREE_PEPTIDE_LEVEL_QUANTITATION_ID "MS:1002020"
#define MS_LABEL_FREE_PEPTIDE_LEVEL_QUANTITATION_NAME "label-free peptide level quantitation"
/* def: "Label-free peptide level quantitation." [PSI:PI]
xref: value-type:xsd\:boolean "The allowed value-type for this CV term."
is_a: MS:1001834 ! LC-MS label-free quantitation analysis */

#define MS_LABEL_FREE_PROTEIN_LEVEL_QUANTITATION_ID "MS:1002021"
#define MS_LABEL_FREE_PROTEIN_LEVEL_QUANTITATION_NAME "label-free protein level quantitation"
/* def: "Label-free protein level quantitation." [PSI:PI]
xref: value-type:xsd\:boolean "The allowed value-type for this CV term."
is_a: MS:1001834 ! LC-MS label-free quantitation analysis */

#define MS_LABEL_FREE_PROTEINGROUP_LEVEL_QUANTITATION_ID "MS:1002022"
#define MS_LABEL_FREE_PROTEINGROUP_LEVEL_QUANTITATION_NAME "label-free proteingroup level quantitation"
/* def: "Label-free proteingroup level quantitation." [PSI:PI]
xref: value-type:xsd\:boolean "The allowed value-type for this CV term."
is_a: MS:1001834 ! LC-MS label-free quantitation analysis */

#define MS_MS2_TAG_BASED_ANALYSIS_ID "MS:1002023"
#define MS_MS2_TAG_BASED_ANALYSIS_NAME "MS2 tag-based analysis"
/* def: "MS2 tag-based analysis." [PSI:PI]
is_a: MS:1001833 ! quantitation analysis summary */

#define MS_MS2_TAG_BASED_FEATURE_LEVEL_QUANTITATION_ID "MS:1002024"
#define MS_MS2_TAG_BASED_FEATURE_LEVEL_QUANTITATION_NAME "MS2 tag-based feature level quantitation"
/* def: "MS2 tag-based feature level quantitation." [PSI:PI]
xref: value-type:xsd\:boolean "The allowed value-type for this CV term."
is_a: MS:1002023 ! MS2 tag-based analysis */

#define MS_MS2_TAG_BASED_PEPTIDE_LEVEL_QUANTITATION_ID "MS:1002025"
#define MS_MS2_TAG_BASED_PEPTIDE_LEVEL_QUANTITATION_NAME "MS2 tag-based peptide level quantitation"
/* def: "MS2 tag-based peptide level quantitation." [PSI:PI]
xref: value-type:xsd\:boolean "The allowed value-type for this CV term."
is_a: MS:1002023 ! MS2 tag-based analysis */

#define MS_MS2_TAG_BASED_PROTEIN_LEVEL_QUANTITATION_ID "MS:1002026"
#define MS_MS2_TAG_BASED_PROTEIN_LEVEL_QUANTITATION_NAME "MS2 tag-based protein level quantitation"
/* def: "MS2 tag-based protein level quantitation." [PSI:PI]
xref: value-type:xsd\:boolean "The allowed value-type for this CV term."
is_a: MS:1002023 ! MS2 tag-based analysis */

#define MS_MS2_TAG_BASED_PROTEINGROUP_LEVEL_QUANTITATION_ID "MS:1002027"
#define MS_MS2_TAG_BASED_PROTEINGROUP_LEVEL_QUANTITATION_NAME "MS2 tag-based proteingroup level quantitation"
/* def: "MS2 tag-based proteingroup level quantitation." [PSI:PI]
xref: value-type:xsd\:boolean "The allowed value-type for this CV term."
is_a: MS:1002023 ! MS2 tag-based analysis */

#define MS_NUCLEIC_ACID_BASE_MODIFICATION_ID "MS:1002028"
#define MS_NUCLEIC_ACID_BASE_MODIFICATION_NAME "nucleic acid base modification"
/* def: "Nucleic acid base modification (substitution, insertion or deletion)." [PSI:PI]
xref: value-type:xsd\:string "The allowed value-type for this CV term."
is_a: MS:1001471 ! peptide modification details */

#define MS_ORIGINAL_NUCLEIC_ACID_SEQUENCE_ID "MS:1002029"
#define MS_ORIGINAL_NUCLEIC_ACID_SEQUENCE_NAME "original nucleic acid sequence"
/* def: "Specification of the original nucleic acid sequence, prior to a modification. The value slot should hold the DNA or RNA sequence." [PSI:PI]
xref: value-type:xsd\:string "The allowed value-type for this CV term."
is_a: MS:1001471 ! peptide modification details */

#define MS_MODIFIED_NUCLEIC_ACID_SEQUENCE_ID "MS:1002030"
#define MS_MODIFIED_NUCLEIC_ACID_SEQUENCE_NAME "modified nucleic acid sequence"
/* def: "Specification of the modified nucleic acid sequence. The value slot should hold the DNA or RNA sequence." [PSI:PI]
xref: value-type:xsd\:string "The allowed value-type for this CV term."
is_a: MS:1001471 ! peptide modification details */

#define MS_PASSEL_TRANSITION_GROUP_BROWSER_URI_ID "MS:1002031"
#define MS_PASSEL_TRANSITION_GROUP_BROWSER_URI_NAME "PASSEL transition group browser URI"
/* def: "URI to retrieve transition group data for a PASSEL (PeptideAtlas SRM Experiment Library) experiment." [PSI:PI]
is_a: MS:1000878 ! external reference identifier */

#define MS_PEPTIDEATLAS_DATASET_URI_ID "MS:1002032"
#define MS_PEPTIDEATLAS_DATASET_URI_NAME "PeptideAtlas dataset URI"
/* def: "URI that allows access to a PeptideAtlas dataset." [PSI:PI]
is_a: MS:1000878 ! external reference identifier */

#define MS_CONTACT_ROLE_ID "MS:1002033"
#define MS_CONTACT_ROLE_NAME "contact role"
/* def: "Role of the contact person." [PSI:PI]
is_a: MS:1000585 ! contact attribute */

#define MS_FIRST_AUTHOR_ID "MS:1002034"
#define MS_FIRST_AUTHOR_NAME "first author"
/* def: "The first of a set of authors associated with a publication or release. There may be more than one first author in cases where several authors share primary attribution." [PSI:MS]
is_a: MS:1002033 ! contact role */

#define MS_SENIOR_AUTHOR_ID "MS:1002035"
#define MS_SENIOR_AUTHOR_NAME "senior author"
/* def: "The last of a set of authors associated with a publication or release. There may be more than one senior author in cases where several authors share senior attribution." [PSI:MS]
is_a: MS:1002033 ! contact role */

#define MS_CO_AUTHOR_ID "MS:1002036"
#define MS_CO_AUTHOR_NAME "co-author"
/* def: "One of a set of authors associated with a publication or release." [PSI:MS]
is_a: MS:1002033 ! contact role */

#define MS_DATASET_SUBMITTER_ID "MS:1002037"
#define MS_DATASET_SUBMITTER_NAME "dataset submitter"
/* def: "A person who submits a dataset to a repository." [PSI:MS]
is_a: MS:1002033 ! contact role */

#define MS_UNLABELED_SAMPLE_ID "MS:1002038"
#define MS_UNLABELED_SAMPLE_NAME "unlabeled sample"
/* def: "A sample that has not been labelled or modified. This is often referred to as \"light\" to distinguish from \"heavy\"." [PSI:PI]
synonym: "light labeled sample" EXACT []
is_a: MS:1000548 ! sample attribute */

#define MS_INLET_ATTRIBUTE_ID "MS:1002039"
#define MS_INLET_ATTRIBUTE_NAME "inlet attribute"
/* def: "Inlet properties that are associated with a value." [PSI:MS]
is_a: MS:1000547 ! object attribute
relationship: part_of MS:1000458 ! source */

#define MS_INLET_TEMPERATURE_ID "MS:1002040"
#define MS_INLET_TEMPERATURE_NAME "inlet temperature"
/* def: "The temperature of the inlet of a mass spectrometer." [PSI:MS]
xref: value-type:xsd\:float "The allowed value-type for this CV term."
is_a: MS:1000482 ! source attribute
is_a: MS:1002039 ! inlet attribute
relationship: has_units UO:0000012 ! kelvin
relationship: has_units UO:0000027 ! degree Celsius */

#define MS_SOURCE_TEMPERATURE_ID "MS:1002041"
#define MS_SOURCE_TEMPERATURE_NAME "source temperature"
/* def: "The temperature of the source of a mass spectrometer." [PSI:MS]
xref: value-type:xsd\:float "The allowed value-type for this CV term."
is_a: MS:1000482 ! source attribute
relationship: has_units UO:0000012 ! kelvin
relationship: has_units UO:0000027 ! degree Celsius */

#define MS_MODULATION_TIME_ID "MS:1002042"
#define MS_MODULATION_TIME_NAME "modulation time"
/* def: "The duration of a complete cycle of modulation in a comprehensive two-dimensional separation system, equals the length of a second dimension chromatogram, i.e., the time between two successive injections into the second column." [http://chromatographyonline.findanalytichem.com/lcgc/Column:+Coupling+Matters/Nomenclature-and-Conventions-in-Comprehensive-Mult/ArticleStandard/Article/detail/58429]
xref: value-type:xsd\:string "The allowed value-type for this CV term."
is_a: MS:1000857 ! run attribute
relationship: has_units UO:0000010 ! second
relationship: has_units UO:0000031 ! minute */

#define MS_PROTEINPROSPECTOR_ID "MS:1002043"
#define MS_PROTEINPROSPECTOR_NAME "ProteinProspector"
/* def: "ProteinProspector software for data acquisition and analysis." [PSI:PI]
is_a: MS:1001456 ! analysis software */

#define MS_PROTEINPROSPECTOR_SCORE_ID "MS:1002044"
#define MS_PROTEINPROSPECTOR_SCORE_NAME "ProteinProspector:score"
/* def: "The ProteinProspector result 'Score'." [PSI:PI]
xref: value-type:xsd\:double "The allowed value-type for this CV term."
is_a: MS:1001143 ! search engine specific score for PSMs
is_a: MS:1001153 ! search engine specific score */

#define MS_PROTEINPROSPECTOR_EXPECTATION_VALUE_ID "MS:1002045"
#define MS_PROTEINPROSPECTOR_EXPECTATION_VALUE_NAME "ProteinProspector:expectation value"
/* def: "The ProteinProspector result 'Expectation value'." [PSI:PI]
xref: value-type:xsd\:double "The allowed value-type for this CV term."
is_a: MS:1001143 ! search engine specific score for PSMs
is_a: MS:1001153 ! search engine specific score */

#define MS_NATIVE_SOURCE_PATH_ID "MS:1002046"
#define MS_NATIVE_SOURCE_PATH_NAME "native source path"
/* def: "The original source path used for directory-based sources." [PSI:MS]
xref: value-type:xsd\:string "The allowed value-type for this CV term."
is_a: MS:1001458 ! spectrum generation information */

#define MS_MS_GF_ID "MS:1002047"
#define MS_MS_GF_NAME "MS-GF"
/* def: "MS-GF software used to re-score the peptide-spectrum matches." [DOI:10.1074/mcp.M110.003731, PMID:20829449]
is_a: MS:1001456 ! analysis software */

#define MS_MS_GF__ID "MS:1002048"
#define MS_MS_GF__NAME "MS-GF+"
/* synonym: "MS-GFDB" EXACT []
def: "MS-GF+ software used to analyze the spectra." [PSI:PI]
is_a: MS:1001456 ! analysis software */

#define MS_MS_GF_RAWSCORE_ID "MS:1002049"
#define MS_MS_GF_RAWSCORE_NAME "MS-GF:RawScore"
/* def: "MS-GF raw score." [PSI:PI]
xref: value-type:xsd\:int "The allowed value-type for this CV term."
is_a: MS:1001143 ! search engine specific score for PSMs
is_a: MS:1001153 ! search engine specific score */

#define MS_MS_GF_DENOVOSCORE_ID "MS:1002050"
#define MS_MS_GF_DENOVOSCORE_NAME "MS-GF:DeNovoScore"
/* def: "MS-GF de novo score." [PSI:PI]
xref: value-type:xsd\:int "The allowed value-type for this CV term."
is_a: MS:1001153 ! search engine specific score */

#define MS_MS_GF_ENERGY_ID "MS:1002051"
#define MS_MS_GF_ENERGY_NAME "MS-GF:Energy"
/* def: "MS-GF energy score." [PSI:PI]
xref: value-type:xsd\:nonNegativeInteger "The allowed value-type for this CV term."
is_a: MS:1001143 ! search engine specific score for PSMs
is_a: MS:1001153 ! search engine specific score */

#define MS_MS_GF_SPECEVALUE_ID "MS:1002052"
#define MS_MS_GF_SPECEVALUE_NAME "MS-GF:SpecEValue"
/* def: "MS-GF spectral E-value." [PSI:PI]
xref: value-type:xsd\:double "The allowed value-type for this CV term."
is_a: MS:1001143 ! search engine specific score for PSMs
is_a: MS:1001153 ! search engine specific score
is_a: MS:1001872 ! distinct peptide-level e-value */

#define MS_MS_GF_EVALUE_ID "MS:1002053"
#define MS_MS_GF_EVALUE_NAME "MS-GF:EValue"
/* def: "MS-GF E-value." [PSI:PI]
xref: value-type:xsd\:double "The allowed value-type for this CV term."
is_a: MS:1001143 ! search engine specific score for PSMs
is_a: MS:1001153 ! search engine specific score
is_a: MS:1001872 ! distinct peptide-level e-value */

#define MS_MS_GF_QVALUE_ID "MS:1002054"
#define MS_MS_GF_QVALUE_NAME "MS-GF:QValue"
/* def: "MS-GF Q-value." [PSI:PI]
xref: value-type:xsd\:double "The allowed value-type for this CV term."
is_a: MS:1001143 ! search engine specific score for PSMs
is_a: MS:1001153 ! search engine specific score
is_a: MS:1001868 ! distinct peptide-level q-value */

#define MS_MS_GF_PEPQVALUE_ID "MS:1002055"
#define MS_MS_GF_PEPQVALUE_NAME "MS-GF:PepQValue"
/* def: "MS-GF peptide-level Q-value." [PSI:PI]
xref: value-type:xsd\:double "The allowed value-type for this CV term."
is_a: MS:1001143 ! search engine specific score for PSMs
is_a: MS:1001153 ! search engine specific score
is_a: MS:1001868 ! distinct peptide-level q-value */

#define MS_MS_GF_PEP_ID "MS:1002056"
#define MS_MS_GF_PEP_NAME "MS-GF:PEP"
/* def: "MS-GF posterior error probability." [PSI:PI]
xref: value-type:xsd\:double "The allowed value-type for this CV term."
is_a: MS:1001143 ! search engine specific score for PSMs
is_a: MS:1001153 ! search engine specific score */

#define MS_MODIFICATION_SPECIFICITY_PROTEIN_N_TERM_ID "MS:1002057"
#define MS_MODIFICATION_SPECIFICITY_PROTEIN_N_TERM_NAME "modification specificity protein N-term"
/* def: "As parameter for search engine: apply the modification only at the N-terminus of a protein." [PSI:PI]
is_a: MS:1001056 ! modification specificity rule */

#define MS_MODIFICATION_SPECIFICITY_PROTEIN_C_TERM_ID "MS:1002058"
#define MS_MODIFICATION_SPECIFICITY_PROTEIN_C_TERM_NAME "modification specificity protein C-term"
/* def: "As parameter for search engine: apply the modification only at the C-terminus of a protein." [PSI:PI]
is_a: MS:1001056 ! modification specificity rule */

#define MS_MICROSOFT_EXCEL_ID "MS:1002059"
#define MS_MICROSOFT_EXCEL_NAME "Microsoft Excel"
/* def: "Microsoft Excel (can be used for spectral counting)." [PSI:PI]
is_a: MS:1001139 ! quantitation software name */

#define MS_DATABASE_UNIPROTKB_TREMBL_ID "MS:1002060"
#define MS_DATABASE_UNIPROTKB_TREMBL_NAME "database UniProtKB_TrEMBL"
/* def: "The name of the UniProtKB/TrEMBL database." [PSI:PI]
is_a: MS:1002126 ! database UniProtKB */

// #define MS_DECOY_DB_FROM_UNIPROTKB_TREMBL_ID "MS:1002061"
// #define MS_DECOY_DB_FROM_UNIPROTKB_TREMBL_NAME "decoy DB from UniProtKB_TrEMBL"
/* def: "OBSOLETE Decoy database from a TrEMBL protein sequence database." [PSI:PI]
comment: This term was made obsolete, because a combination of database name, DB composition , decoy DB type , decoy DB generation algorithm, decoy DB accession regexp and decoy DB details suffices.
is_obsolete: true */

#define MS_METABOLIC_LABELLING__NATURAL_N__MAINLY_14N__ID "MS:1002062"
#define MS_METABOLIC_LABELLING__NATURAL_N__MAINLY_14N__NAME "metabolic labelling: natural N (mainly 14N)"
/* def: "Metabolic labelling: natural N (mainly 14N)." [PSI:PI]
is_a: MS:1001055 ! modification parameters */

#define MS_FINDPAIRS_ID "MS:1002063"
#define MS_FINDPAIRS_NAME "FindPairs"
/* def: "Software e.g. for SILAC and 14N/15N workflow, part of the PeakQuant suite." [http://www.medizinisches-proteom-center.de/software]
is_a: MS:1001139 ! quantitation software name */

#define MS_PEPTIDE_CONSENSUS_RT_ID "MS:1002064"
#define MS_PEPTIDE_CONSENSUS_RT_NAME "peptide consensus RT"
/* def: "Peptide consensus retention time." [PSI:PI]
xref: value-type:xsd\:double "The allowed value-type for this CV term."
is_a: MS:1001805 ! quantification datatype */

#define MS_PEPTIDE_CONSENSUS_M_Z_ID "MS:1002065"
#define MS_PEPTIDE_CONSENSUS_M_Z_NAME "peptide consensus m/z"
/* def: "Peptide consensus mass/charge ratio." [PSI:PI]
xref: value-type:xsd\:double "The allowed value-type for this CV term."
is_a: MS:1001805 ! quantification datatype */

#define MS_RATIO_CALCULATION_METHOD_ID "MS:1002066"
#define MS_RATIO_CALCULATION_METHOD_NAME "ratio calculation method"
/* def: "Method used to calculate the ratio." [PSI:PI]
xref: value-type:xsd\:double "The allowed value-type for this CV term."
is_a: MS:1001805 ! quantification datatype */

#define MS_PROTEIN_VALUE__MEDIAN_OF_PEPTIDE_RATIOS_ID "MS:1002067"
#define MS_PROTEIN_VALUE__MEDIAN_OF_PEPTIDE_RATIOS_NAME "protein value: median of peptide ratios"
/* def: "Protein quantification value calculated as median of peptide ratios." [PSI:PI]
xref: value-type:xsd\:double "The allowed value-type for this CV term."
is_a: MS:1001805 ! quantification datatype */

#define MS_METABOLIC_LABELLING__HEAVY_N__MAINLY_15N__ID "MS:1002068"
#define MS_METABOLIC_LABELLING__HEAVY_N__MAINLY_15N__NAME "metabolic labelling: heavy N (mainly 15N)"
/* def: "Metabolic labelling: heavy N (mainly 15N)." [PSI:PI]
is_a: MS:1001055 ! modification parameters */

#define MS_METABOLIC_LABELLING__LABELLING_PURITY_ID "MS:1002069"
#define MS_METABOLIC_LABELLING__LABELLING_PURITY_NAME "metabolic labelling: labelling purity"
/* def: "Metabolic labelling: Description of labelling purity. Usually the purity of feeding material (e.g. 95%), or the inclusion rate derived from isotopic peak pattern shape." [PSI:PI]
xref: value-type:xsd\:string "The allowed value-type for this CV term."
is_a: MS:1001055 ! modification parameters */

#define MS_T_TEST_ID "MS:1002070"
#define MS_T_TEST_NAME "t-test"
/* def: "Perform a t-test (two groups). Specify in string value, whether paired / unpaired, variance equal / different, one- / two-sided version is performed." [PSI:PI]
xref: value-type:xsd\:string "The allowed value-type for this CV term."
is_a: MS:1001861 ! quantification data processing */

#define MS_ANOVA_TEST_ID "MS:1002071"
#define MS_ANOVA_TEST_NAME "ANOVA-test"
/* def: "Perform an ANOVA-test (more than two groups). Specify in string value, which version is performed." [PSI:PI]
xref: value-type:xsd\:string "The allowed value-type for this CV term."
is_a: MS:1001861 ! quantification data processing */

#define MS_P_VALUE_ID "MS:1002072"
#define MS_P_VALUE_NAME "p-value"
/* def: "P-value as result of one of the processing steps described. Specify in the description, which processing step it was." [PSI:PI]
xref: value-type:xsd\:double "The allowed value-type for this CV term."
is_a: MS:1001805 ! quantification datatype
has_domain: MS:1002305 ! value between 0 and 1 inclusive */

#define MS_MZIDENTML_ID "MS:1002073"
#define MS_MZIDENTML_NAME "mzIdentML"
/* def: "Proteomics Standards Inititative mzIdentML file format. File extension '.mzid'." [PSI:MS]
is_a: MS:1001040 ! intermediate analysis format */

#define MS_QUANTIFICATION_FILE_FORMAT_ID "MS:1002074"
#define MS_QUANTIFICATION_FILE_FORMAT_NAME "quantification file format"
/* def: "File format containing quantification results." [PSI:PI]
is_a: MS:1001459 ! file format */

#define MS_MZQUANTML_ID "MS:1002075"
#define MS_MZQUANTML_NAME "mzQuantML"
/* def: "Proteomics Standards Inititative mzQuantML file format. File extension '.mzq'." [PSI:MS]
is_a: MS:1002074 ! quantification file format */

#define MS_PANALYZER_ID "MS:1002076"
#define MS_PANALYZER_NAME "PAnalyzer"
/* def: "PAnalyzer software for getting protein evidence categories." [http://code.google.com/p/ehu-bio/wiki/PAnalyzer]
is_a: MS:1001456 ! analysis software */

#define MS_IMPACT_ID "MS:1002077"
#define MS_IMPACT_NAME "impact"
/* def: "Bruker Daltonics' impact: ESI Q-TOF, Nanospray, APCI, APPI, GC-APCI, CaptiveSpray." [PSI:MS]
is_a: MS:1001536 ! Bruker Daltonics micrOTOF series */

#define MS_PROTEOMEDISCOVERER_1__STATIC_MODIFICATION_ID "MS:1002078"
#define MS_PROTEOMEDISCOVERER_1__STATIC_MODIFICATION_NAME "ProteomeDiscoverer:1. Static Modification"
/* def: "Determine 1st static post-translational modifications (PTMs)." [PSI:MS]
xref: value-type:xsd\:string "The allowed value-type for this CV term."
is_a: MS:1002101 ! ProteomeDiscoverer input parameter */

#define MS_PROTEOMEDISCOVERER_2__STATIC_MODIFICATION_ID "MS:1002079"
#define MS_PROTEOMEDISCOVERER_2__STATIC_MODIFICATION_NAME "ProteomeDiscoverer:2. Static Modification"
/* def: "Determine 2nd static post-translational modifications (PTMs)." [PSI:MS]
xref: value-type:xsd\:string "The allowed value-type for this CV term."
is_a: MS:1002101 ! ProteomeDiscoverer input parameter */

#define MS_PROTEOMEDISCOVERER_SPECTRUM_SELECTOR_PRECURSOR_CLIPPING_RANGE_BEFORE_ID "MS:1002080"
#define MS_PROTEOMEDISCOVERER_SPECTRUM_SELECTOR_PRECURSOR_CLIPPING_RANGE_BEFORE_NAME "ProteomeDiscoverer:Spectrum Selector:Precursor Clipping Range Before"
/* def: "Precursor clipping range before." [PSI:MS]
xref: value-type:xsd\:double "The allowed value-type for this CV term."
is_a: MS:1002101 ! ProteomeDiscoverer input parameter
relationship: has_units UO:0000221 ! dalton */

#define MS_PROTEOMEDISCOVERER_SPECTRUM_SELECTOR_PRECURSOR_CLIPPING_RANGE_AFTER_ID "MS:1002081"
#define MS_PROTEOMEDISCOVERER_SPECTRUM_SELECTOR_PRECURSOR_CLIPPING_RANGE_AFTER_NAME "ProteomeDiscoverer:Spectrum Selector:Precursor Clipping Range After"
/* def: "Precursor clipping range after." [PSI:MS]
xref: value-type:xsd\:double "The allowed value-type for this CV term."
is_a: MS:1002101 ! ProteomeDiscoverer input parameter
relationship: has_units UO:0000221 ! dalton */

#define MS_FIRST_COLUMN_ELUTION_TIME_ID "MS:1002082"
#define MS_FIRST_COLUMN_ELUTION_TIME_NAME "first column elution time"
/* def: "The time of elution from the first chromatographic column in the chromatographic separation step, relative to the start of chromatography on the first column." [PSI:MS]
xref: value-type:xsd\:float "The allowed value-type for this CV term."
is_a: MS:1000503 ! scan attribute
relationship: has_units UO:0000010 ! second
relationship: has_units UO:0000031 ! minute */

#define MS_SECOND_COLUMN_ELUTION_TIME_ID "MS:1002083"
#define MS_SECOND_COLUMN_ELUTION_TIME_NAME "second column elution time"
/* def: "The time of elution from the second chromatographic column in the chromatographic separation step, relative to the start of the chromatography on the second column." [PSI:MS]
xref: value-type:xsd\:float "The allowed value-type for this CV term."
is_a: MS:1000503 ! scan attribute
relationship: has_units UO:0000010 ! second
relationship: has_units UO:0000031 ! minute */

#define MS_MULTIDIMENSIONAL_CHROMATOGRAPHY_MODULATION_DESCRIPTION_ID "MS:1002084"
#define MS_MULTIDIMENSIONAL_CHROMATOGRAPHY_MODULATION_DESCRIPTION_NAME "multidimensional chromatography modulation description"
/* def: "Multidimensional chromatography modulation description." [PSI:MS]
is_a: MS:1000857 ! run attribute */

#define MS_TWO_DIMENSIONAL_GAS_CHROMATOGRAPHY_WITH_FIXED_MODULATION_TIME_ID "MS:1002085"
#define MS_TWO_DIMENSIONAL_GAS_CHROMATOGRAPHY_WITH_FIXED_MODULATION_TIME_NAME "two-dimensional gas chromatography with fixed modulation time"
/* def: "Two-dimensional gas chromatography where a single modulation time is used throughout the acquisition." [PSI:MS]
is_a: MS:1002084 ! multidimensional chromatography modulation description */

#define MS_TWO_DIMENSIONAL_GAS_CHROMATOGRAPHY_WITH_DISCRETE_MODULATION_TIME_STEPS_ID "MS:1002086"
#define MS_TWO_DIMENSIONAL_GAS_CHROMATOGRAPHY_WITH_DISCRETE_MODULATION_TIME_STEPS_NAME "two-dimensional gas chromatography with discrete modulation time steps"
/* def: "Two-dimensional gas chromatography where the acquisition is divided into steps, each with a different modulation time." [PSI:MS]
is_a: MS:1002084 ! multidimensional chromatography modulation description */

#define MS_TWO_DIMENSIONAL_LIQUID_CHROMATOGRAPHY_WITH_FIXED_MODULATION_TIME_ID "MS:1002087"
#define MS_TWO_DIMENSIONAL_LIQUID_CHROMATOGRAPHY_WITH_FIXED_MODULATION_TIME_NAME "two-dimensional liquid chromatography with fixed modulation time"
/* def: "Two-dimensional liquid chromatography where a single modulation time is used throughout the acquisition." [PSI:MS]
is_a: MS:1002084 ! multidimensional chromatography modulation description */

#define MS_TWO_DIMENSIONAL_LIQUID_CHROMATOGRAPHY_WITH_DISCRETE_MODULATION_TIME_STEPS_ID "MS:1002088"
#define MS_TWO_DIMENSIONAL_LIQUID_CHROMATOGRAPHY_WITH_DISCRETE_MODULATION_TIME_STEPS_NAME "two-dimensional liquid chromatography with discrete modulation time steps"
/* def: "Two-dimensional liquid chromatography where the acquisition is divided into steps, each with a different modulation time." [PSI:MS]
is_a: MS:1002084 ! multidimensional chromatography modulation description */

#define MS_PROTEOMEDISCOVERER_PEPTIDE_WITHOUT_PROTEIN_XCORR_THRESHOLD_ID "MS:1002089"
#define MS_PROTEOMEDISCOVERER_PEPTIDE_WITHOUT_PROTEIN_XCORR_THRESHOLD_NAME "ProteomeDiscoverer:Peptide Without Protein XCorr Threshold"
/* def: "XCorr threshold for storing peptides that do not belong to a protein." [PSI:MS]
xref: value-type:xsd\:float "The allowed value-type for this CV term."
is_a: MS:1002101 ! ProteomeDiscoverer input parameter */

#define MS_CALCULATE_PROBABILITY_SCORES_ID "MS:1002090"
#define MS_CALCULATE_PROBABILITY_SCORES_NAME "Calculate Probability Scores"
/* def: "Flag indicating that a probability score for the assessment that a reported peptide match is a random occurence is calculated." [PSI:MS]
xref: value-type:xsd\:boolean "The allowed value-type for this CV term."
is_a: MS:1002094 ! common search engine input parameter */

#define MS_PROTEOMEDISCOVERER_MAXIMUM_DELTA_CN_ID "MS:1002091"
#define MS_PROTEOMEDISCOVERER_MAXIMUM_DELTA_CN_NAME "ProteomeDiscoverer:Maximum Delta Cn"
/* def: "Delta Cn threshold for filtering out PSM's." [PSI:MS]
xref: value-type:xsd\:float "The allowed value-type for this CV term."
is_a: MS:1002101 ! ProteomeDiscoverer input parameter */

#define MS_PERCOLATOR_VALIDATION_BASED_ON_ID "MS:1002092"
#define MS_PERCOLATOR_VALIDATION_BASED_ON_NAME "Percolator:Validation based on"
/* def: "Algorithm (e.g. q-value or PEP) used for calculation of the validation score using Percolator." [PSI:MS]
xref: value-type:xsd\:string "The allowed value-type for this CV term."
is_a: MS:1002107 ! Percolator input parameter */

#define MS_SEARCH_ENGINE_INPUT_PARAMETER_ID "MS:1002093"
#define MS_SEARCH_ENGINE_INPUT_PARAMETER_NAME "search engine input parameter"
/* def: "Search engine input parameter." [PSI:PI]
is_a: MS:1001249 ! search input details */

#define MS_COMMON_SEARCH_ENGINE_INPUT_PARAMETER_ID "MS:1002094"
#define MS_COMMON_SEARCH_ENGINE_INPUT_PARAMETER_NAME "common search engine input parameter"
/* def: "Search engine input parameter that is shared by more than one search engine." [PSI:PI]
is_a: MS:1002093 ! search engine input parameter */

#define MS_MASCOT_INPUT_PARAMETER_ID "MS:1002095"
#define MS_MASCOT_INPUT_PARAMETER_NAME "Mascot input parameter"
/* def: "Search engine input parameters specific to Mascot." [PSI:PI, source:http://www.matrixscience.com/help/search_field_help.html]
is_a: MS:1001302 ! search engine specific input parameter */

#define MS_SEQUEST_INPUT_PARAMETER_ID "MS:1002096"
#define MS_SEQUEST_INPUT_PARAMETER_NAME "SEQUEST input parameter"
/* def: "Search engine input parameters specific to SEQUEST." [PSI:PI, source:http://fields.scripps.edu/sequest/parameters.html]
is_a: MS:1001302 ! search engine specific input parameter */

#define MS_PHENYX_INPUT_PARAMETER_ID "MS:1002097"
#define MS_PHENYX_INPUT_PARAMETER_NAME "Phenyx input parameter"
/* def: "Search engine input parameters specific to Phenyx." [PSI:PI]
is_a: MS:1001302 ! search engine specific input parameter */

#define MS_PROTEINEXTRACTOR_INPUT_PARAMETER_ID "MS:1002098"
#define MS_PROTEINEXTRACTOR_INPUT_PARAMETER_NAME "ProteinExtractor input parameter"
/* def: "Search engine input parameters specific to ProteinExtractor." [PSI:PI]
is_a: MS:1001302 ! search engine specific input parameter */

#define MS_OMSSA_INPUT_PARAMETER_ID "MS:1002099"
#define MS_OMSSA_INPUT_PARAMETER_NAME "OMSSA input parameter"
/* def: "Search engine input parameters specific to OMSSA." [PSI:PI]
is_a: MS:1001302 ! search engine specific input parameter */

#define MS_PROTEINSCAPE_INPUT_PARAMETER_ID "MS:1002100"
#define MS_PROTEINSCAPE_INPUT_PARAMETER_NAME "ProteinScape input parameter"
/* def: "Search engine input parameters specific to ProteinScape." [PSI:PI]
is_a: MS:1001302 ! search engine specific input parameter */

#define MS_PROTEOMEDISCOVERER_INPUT_PARAMETER_ID "MS:1002101"
#define MS_PROTEOMEDISCOVERER_INPUT_PARAMETER_NAME "ProteomeDiscoverer input parameter"
/* def: "Search engine input parameters specific to ProteomeDiscoverer." [PSI:PI]
is_a: MS:1001302 ! search engine specific input parameter */

#define MS_SOFTWARE_INPUT_PARAMETER_ID "MS:1002103"
#define MS_SOFTWARE_INPUT_PARAMETER_NAME "software input parameter"
/* def: "Software input parameters." [PSI:PI]
is_a: MS:1001249 ! search input details */

#define MS_COMMON_SOFTWARE_INPUT_PARAMETER_ID "MS:1002104"
#define MS_COMMON_SOFTWARE_INPUT_PARAMETER_NAME "common software input parameter"
/* def: "Software input parameter that is shared by more than one software." [PSI:PI]
is_a: MS:1002103 ! software input parameter */

#define MS_SOFTWARE_SPECIFIC_INPUT_PARAMETER_ID "MS:1002105"
#define MS_SOFTWARE_SPECIFIC_INPUT_PARAMETER_NAME "software specific input parameter"
/* def: "Software specific input parameter." [PSI:PI]
is_a: MS:1002103 ! software input parameter */

#define MS_SCAFFOLD_INPUT_PARAMETER_ID "MS:1002106"
#define MS_SCAFFOLD_INPUT_PARAMETER_NAME "Scaffold input parameter"
/* def: "Search engine input parameters specific to Scaffold." [PSI:PI]
is_a: MS:1002105 ! software specific input parameter */

#define MS_PERCOLATOR_INPUT_PARAMETER_ID "MS:1002107"
#define MS_PERCOLATOR_INPUT_PARAMETER_NAME "Percolator input parameter"
/* def: "Search engine input parameters specific to Percolator." [PSI:PI]
is_a: MS:1002105 ! software specific input parameter */

#define MS_HIGHER_SCORE_BETTER_ID "MS:1002108"
#define MS_HIGHER_SCORE_BETTER_NAME "higher score better"
/* def: "Indicates that a higher score is better." [PSI:PI]
relationship: part_of MS:1001153 ! search engine specific score */

#define MS_LOWER_SCORE_BETTER_ID "MS:1002109"
#define MS_LOWER_SCORE_BETTER_NAME "lower score better"
/* def: "Indicates that a lower score is better." [PSI:PI]
relationship: part_of MS:1001153 ! search engine specific score */

#define MS_ASSAY_ATTRIBUTE_ID "MS:1002110"
#define MS_ASSAY_ATTRIBUTE_NAME "assay attribute"
/* def: "Attribute describing an assay." [PSI:PI]
is_a: MS:1001806 ! quantification object attribute */

#define MS_ASSAY_LABEL_ATTRIBUTE_ID "MS:1002111"
#define MS_ASSAY_LABEL_ATTRIBUTE_NAME "assay label attribute"
/* def: "Attribute describing an assay label." [PSI:PI]
is_a: MS:1001806 ! quantification object attribute */

#define MS_PROTEIN_GROUP_LIST_ATTRIBUTE_ID "MS:1002112"
#define MS_PROTEIN_GROUP_LIST_ATTRIBUTE_NAME "protein group list attribute"
/* def: "Attribute describing a protein group list." [PSI:PI]
is_a: MS:1001806 ! quantification object attribute */

#define MS_PROTEIN_GROUP_ATTRIBUTE_ID "MS:1002113"
#define MS_PROTEIN_GROUP_ATTRIBUTE_NAME "protein group attribute"
/* def: "Attribute describing a protein group." [PSI:PI]
is_a: MS:1001806 ! quantification object attribute */

#define MS_PROTEIN_LIST_ATTRIBUTE_ID "MS:1002114"
#define MS_PROTEIN_LIST_ATTRIBUTE_NAME "protein list attribute"
/* def: "Attribute describing a protein list." [PSI:PI]
is_a: MS:1001806 ! quantification object attribute */

#define MS_PEPTIDE_CONSENSUS_LIST_ATTRIBUTE_ID "MS:1002115"
#define MS_PEPTIDE_CONSENSUS_LIST_ATTRIBUTE_NAME "peptide consensus list attribute"
/* def: "Attribute describing a peptide consensus list." [PSI:PI]
is_a: MS:1001806 ! quantification object attribute */

#define MS_PEPTIDE_CONSENSUS_ATTRIBUTE_ID "MS:1002116"
#define MS_PEPTIDE_CONSENSUS_ATTRIBUTE_NAME "peptide consensus attribute"
/* def: "Attribute describing a peptide consensus." [PSI:PI]
is_a: MS:1001806 ! quantification object attribute */

#define MS_SMALL_MOLECULE_LIST_ATTRIBUTE_ID "MS:1002117"
#define MS_SMALL_MOLECULE_LIST_ATTRIBUTE_NAME "small molecule list attribute"
/* def: "Attribute describing a small mollecule list." [PSI:PI]
is_a: MS:1001806 ! quantification object attribute */

#define MS_SMALL_MOLECULE_ATTRIBUTE_ID "MS:1002118"
#define MS_SMALL_MOLECULE_ATTRIBUTE_NAME "small molecule attribute"
/* def: "Attribute describing a small molecule." [PSI:PI]
is_a: MS:1001806 ! quantification object attribute */

#define MS_SMALL_MOLECULE_MODIFICATION_ATTRIBUTE_ID "MS:1002119"
#define MS_SMALL_MOLECULE_MODIFICATION_ATTRIBUTE_NAME "small molecule modification attribute"
/* def: "Attribute describing a small molecule modification." [PSI:PI]
is_a: MS:1001806 ! quantification object attribute */

#define MS_EXPERIMENT_NAME_ID "MS:1002120"
#define MS_EXPERIMENT_NAME_NAME "experiment name"
/* def: "The name for identifying an experiment." [PSI:PI]
is_a: MS:1001806 ! quantification object attribute */

#define MS_SPECTRAL_COUNT_FEATURE_ID "MS:1002121"
#define MS_SPECTRAL_COUNT_FEATURE_NAME "spectral count feature"
/* def: "Dummy decribing a spectral count feature." [PSI:PI]
is_a: MS:1001828 ! feature attribute */

#define MS_COUNTS_REPORTING_ID "MS:1002122"
#define MS_COUNTS_REPORTING_NAME "counts reporting"
/* def: "FeatureList of spectral counts." [PSI:PI]
is_a: MS:1001825 ! feature list attribute */

#define MS_X_TRACKER_ID "MS:1002123"
#define MS_X_TRACKER_NAME "x-Tracker"
/* def: "x-Tracker generic tool for quantitative proteomics." [http://www.x-tracker.info/]
is_a: MS:1001139 ! quantitation software name */

#define MS_PROTEOSUITE_ID "MS:1002124"
#define MS_PROTEOSUITE_NAME "ProteoSuite"
/* def: "ProteoSuite software for the analysis of quantitative proteomics data." [DOI:10.1089/omi.2012.0022, PMID:22804616, http://www.proteosuite.org/]
is_a: MS:1001139 ! quantitation software name */

// #define MS_COMBINED_FDRSCORE_ID "MS:1002125"
// #define MS_COMBINED_FDRSCORE_NAME "combined FDRScore"
/* def: "OBSOLETE FDRScore values specifically obtained for distinct combinations of single, pairs or triplets of search engines making a given PSM, used for integrating results from these distinct pools." [PMID:19253293]
comment: This term was made obsolete because it was split into the more specific terms for PSM-level combined FDRScore (MS:1002356), distinct peptide-level combined FDRScore (MS:1002361), protein-level combined FDRScore (MS:1002366) and protein group-level combined FDRScore (MS:1002375).
xref: value-type:xsd\:double "The allowed value-type for this CV term."
is_a: MS:1001143 ! search engine specific score for PSMs
is_a: MS:1001153 ! search engine specific score
is_obsolete: true */

#define MS_DATABASE_UNIPROTKB_ID "MS:1002126"
#define MS_DATABASE_UNIPROTKB_NAME "database UniProtKB"
/* def: "The name of the UniProtKB knowledgebase." [PSI:PI]
is_a: MS:1001013 ! database name */

#define MS_IDENTIFICATION_FILE_ATTRIBUTE_ID "MS:1002127"
#define MS_IDENTIFICATION_FILE_ATTRIBUTE_NAME "identification file attribute"
/* def: "Attribute describing an identification file." [PSI:PI]
is_a: MS:1001806 ! quantification object attribute */

#define MS_METHOD_FILE_FORMAT_ATTRIBUTE_ID "MS:1002128"
#define MS_METHOD_FILE_FORMAT_ATTRIBUTE_NAME "method file format attribute"
/* def: "Attribute describing a method file format." [PSI:PI]
is_a: MS:1001806 ! quantification object attribute */

#define MS_ITRAQANALYZER_ID "MS:1002129"
#define MS_ITRAQANALYZER_NAME "ITRAQAnalyzer"
/* def: "Software for iTRAQ workflow. Extracts and normalizes iTRAQ information from an MS experiment." [http://www-bs2.informatik.uni-tuebingen.de/services/OpenMS/OpenMS-release/html/TOPP__ITRAQAnalyzer.html]
is_a: MS:1001139 ! quantitation software name
is_a: MS:1000752 ! TOPP software */

#define MS_IDENTIFICATION_FILE_FORMAT_ATTRIBUTE_ID "MS:1002130"
#define MS_IDENTIFICATION_FILE_FORMAT_ATTRIBUTE_NAME "identification file format attribute"
/* def: "Attribute describing an identification file format." [PSI:PI]
is_a: MS:1001806 ! quantification object attribute */

#define MS_TOPP_NOISE_FILTER_ID "MS:1002131"
#define MS_TOPP_NOISE_FILTER_NAME "TOPP noise filter"
/* def: "Noise filter component of the TOPP software." [PSI:PI]
is_a: MS:1000752 ! TOPP software */

#define MS_TOPP_NOISEFILTERGAUSSIAN_ID "MS:1002132"
#define MS_TOPP_NOISEFILTERGAUSSIAN_NAME "TOPP NoiseFilterGaussian"
/* def: "Removes noise from profile spectra by using a gaussian smoothing." [PSI:PI]
is_a: MS:1002131 ! TOPP noise filter */

#define MS_TOPP_NOISEFILTERSGOLAY_ID "MS:1002133"
#define MS_TOPP_NOISEFILTERSGOLAY_NAME "TOPP NoiseFilterSGolay"
/* def: "Removes noise from profile spectra by using a Savitzky-Golay smoothing." [PSI:PI]
is_a: MS:1002131 ! TOPP noise filter */

#define MS_TOPP_PEAK_PICKER_ID "MS:1002134"
#define MS_TOPP_PEAK_PICKER_NAME "TOPP peak picker"
/* def: "Peak picker component of the TOPP software." [PSI:PI]
is_a: MS:1000752 ! TOPP software */

#define MS_TOPP_PEAKPICKERHIRES_ID "MS:1002135"
#define MS_TOPP_PEAKPICKERHIRES_NAME "TOPP PeakPickerHiRes"
/* def: "Finds mass spectrometric peaks in high-resoluted profile mass spectra." [PSI:PI]
is_a: MS:1002134 ! TOPP peak picker */

#define MS_TOPP_PEAKPICKERWAVELET_ID "MS:1002136"
#define MS_TOPP_PEAKPICKERWAVELET_NAME "TOPP PeakPickerWavelet"
/* def: "Finds mass spectrometric peaks with a wavelet algorithm in low-resoluted profile mass spectra." [PSI:PI]
is_a: MS:1002134 ! TOPP peak picker */

#define MS_TOPP_SPECTRA_FILTER_ID "MS:1002137"
#define MS_TOPP_SPECTRA_FILTER_NAME "TOPP spectra filter"
/* def: "Spectra filter component of the TOPP software." [PSI:PI]
is_a: MS:1000752 ! TOPP software */

#define MS_TOPP_SPECTRAFILTERBERNNORM_ID "MS:1002138"
#define MS_TOPP_SPECTRAFILTERBERNNORM_NAME "TOPP SpectraFilterBernNorm"
/* def: "Applies a Bern et al normalization to peak spectra." [PMID:15262780, DOI:10.1093/bioinformatics/bth947]
is_a: MS:1002137 ! TOPP spectra filter */

#define MS_TOPP_SPECTRAFILTERMARKERMOWER_ID "MS:1002139"
#define MS_TOPP_SPECTRAFILTERMARKERMOWER_NAME "TOPP SpectraFilterMarkerMower"
/* def: "Applies a filter to peak spectra for marked peaks." [PSI:PI]
is_a: MS:1002137 ! TOPP spectra filter */

#define MS_TOPP_SPECTRAFILTERNLARGEST_ID "MS:1002140"
#define MS_TOPP_SPECTRAFILTERNLARGEST_NAME "TOPP SpectraFilterNLargest"
/* def: "Retains the n largest peaks of a peak spectra." [PSI:PI]
is_a: MS:1002137 ! TOPP spectra filter */

#define MS_TOPP_SPECTRAFILTERNORMALIZER_ID "MS:1002141"
#define MS_TOPP_SPECTRAFILTERNORMALIZER_NAME "TOPP SpectraFilterNormalizer"
/* def: "Applies a TIC/maximal intensity normalization to peak spectra." [PSI:PI]
is_a: MS:1002137 ! TOPP spectra filter */

#define MS_TOPP_SPECTRAFILTERPARENTPEAKMOWER_ID "MS:1002142"
#define MS_TOPP_SPECTRAFILTERPARENTPEAKMOWER_NAME "TOPP SpectraFilterParentPeakMower"
/* def: "Filters putative unfragmented precursor ions from tandem spectra." [PSI:PI]
is_a: MS:1002137 ! TOPP spectra filter */

#define MS_TOPP_SPECTRAFILTERSCALER_ID "MS:1002143"
#define MS_TOPP_SPECTRAFILTERSCALER_NAME "TOPP SpectraFilterScaler"
/* def: "Applies a filter to peak spectra after intensity scaling according to rank." [PSI:PI]
is_a: MS:1002137 ! TOPP spectra filter */

#define MS_TOPP_SPECTRAFILTERSQRTMOWER_ID "MS:1002144"
#define MS_TOPP_SPECTRAFILTERSQRTMOWER_NAME "TOPP SpectraFilterSqrtMower"
/* def: "Applies a filter to peak spectra after intensity scaling to the squareroot." [PSI:PI]
is_a: MS:1002137 ! TOPP spectra filter */

#define MS_TOPP_SPECTRAFILTERTHRESHOLDMOWER_ID "MS:1002145"
#define MS_TOPP_SPECTRAFILTERTHRESHOLDMOWER_NAME "TOPP SpectraFilterThresholdMower"
/* def: "Applies a filter of peaks below a given threshold to peak spectra." [PSI:PI]
is_a: MS:1002137 ! TOPP spectra filter */

#define MS_TOPP_SPECTRAFILTERWINDOWMOWER_ID "MS:1002146"
#define MS_TOPP_SPECTRAFILTERWINDOWMOWER_NAME "TOPP SpectraFilterWindowMower"
/* def: "Applies a filter of the largest peaks in a sliding window over a peak spectrum." [PSI:PI]
is_a: MS:1002137 ! TOPP spectra filter */

#define MS_TOPP_MAP_ALIGNER_ID "MS:1002147"
#define MS_TOPP_MAP_ALIGNER_NAME "TOPP map aligner"
/* def: "Map aligner component of the TOPP software." [PSI:PI]
is_a: MS:1000752 ! TOPP software */

#define MS_TOPP_MAPALIGNERIDENTIFICATION_ID "MS:1002148"
#define MS_TOPP_MAPALIGNERIDENTIFICATION_NAME "TOPP MapAlignerIdentification"
/* def: "Corrects retention time distortions between maps based on common peptide identifications." [PSI:PI]
is_a: MS:1002147 ! TOPP map aligner */

#define MS_TOPP_MAPALIGNERPOSECLUSTERING_ID "MS:1002149"
#define MS_TOPP_MAPALIGNERPOSECLUSTERING_NAME "TOPP MapAlignerPoseClustering"
/* def: "Corrects retention time distortions between maps using a pose clustering approach." [PSI:PI]
is_a: MS:1002147 ! TOPP map aligner */

#define MS_TOPP_MAPALIGNERSPECTRUM_ID "MS:1002150"
#define MS_TOPP_MAPALIGNERSPECTRUM_NAME "TOPP MapAlignerSpectrum"
/* def: "Corrects retention time distortions between maps by spectrum alignment." [PSI:PI]
is_a: MS:1002147 ! TOPP map aligner */

#define MS_NUMERATOR_DATA_TYPE_ATTRIBUTE_ID "MS:1002151"
#define MS_NUMERATOR_DATA_TYPE_ATTRIBUTE_NAME "numerator data type attribute"
/* def: "Attribute describing the data type of the numerator of a ratio." [PSI:PI]
is_a: MS:1001806 ! quantification object attribute */

#define MS_DENOMINATOR_DATA_TYPE_ATTRIBUTE_ID "MS:1002152"
#define MS_DENOMINATOR_DATA_TYPE_ATTRIBUTE_NAME "denominator data type attribute"
/* def: "Attribute describing the data type of the denominator of a ratio." [PSI:PI]
is_a: MS:1001806 ! quantification object attribute */

#define MS_PROTEIN_LEVEL_PSM_COUNTS_ID "MS:1002153"
#define MS_PROTEIN_LEVEL_PSM_COUNTS_NAME "protein level PSM counts"
/* def: "The number of spectra identified for this protein in spectral counting." [PSI:PI]
xref: value-type:xsd\:int "The allowed value-type for this CV term."
is_a: MS:1001805 ! quantification datatype */

#define MS_TOPP_DTAEXTRACTOR_ID "MS:1002154"
#define MS_TOPP_DTAEXTRACTOR_NAME "TOPP DTAExtractor"
/* def: "Extracts spectra of an MS run file to several files in DTA format." [PSI:PI]
is_a: MS:1000752 ! TOPP software */

#define MS_TOPP_IDMERGER_ID "MS:1002155"
#define MS_TOPP_IDMERGER_NAME "TOPP IDMerger"
/* def: "Merges several protein/peptide identification files into one file." [PSI:PI]
is_a: MS:1000752 ! TOPP software */

#define MS_TOPP_IDFILECONVERTER_ID "MS:1002156"
#define MS_TOPP_IDFILECONVERTER_NAME "TOPP IDFileConverter"
/* def: "Converts identification engine file formats." [PSI:PI]
is_a: MS:1000752 ! TOPP software */

#define MS_TOPP_SPECTRAMERGER_ID "MS:1002157"
#define MS_TOPP_SPECTRAMERGER_NAME "TOPP SpectraMerger"
/* def: "Merges spectra from an LC/MS map, either by precursor or by RT blocks." [PSI:PI]
is_a: MS:1000752 ! TOPP software */

#define MS_TOPP_MZTABEXPORTER_ID "MS:1002158"
#define MS_TOPP_MZTABEXPORTER_NAME "TOPP MzTabExporter"
/* def: "Exports various XML formats to an mzTab file." [PSI:PI]
is_a: MS:1000752 ! TOPP software */

#define MS_TOPP_MASSTRACEEXTRACTOR_ID "MS:1002159"
#define MS_TOPP_MASSTRACEEXTRACTOR_NAME "TOPP MassTraceExtractor"
/* def: "Annotates mass traces in centroided LC/MS maps." [PSI:PI]
is_a: MS:1000752 ! TOPP software */

#define MS_TOPP_PRECURSORMASSCORRECTOR_ID "MS:1002160"
#define MS_TOPP_PRECURSORMASSCORRECTOR_NAME "TOPP PrecursorMassCorrector"
/* def: "Correct the precursor entries of tandem MS scans." [PSI:PI]
is_a: MS:1000752 ! TOPP software */

#define MS_TOPP_HIGHRESPRECURSORMASSCORRECTOR_ID "MS:1002161"
#define MS_TOPP_HIGHRESPRECURSORMASSCORRECTOR_NAME "TOPP HighResPrecursorMassCorrector"
/* def: "Performs precursor mz correction on centroided high resolution data." [PSI:PI]
is_a: MS:1000752 ! TOPP software */

#define MS_TOPP_ADDITIVESERIES_ID "MS:1002162"
#define MS_TOPP_ADDITIVESERIES_NAME "TOPP AdditiveSeries"
/* def: "Computes an additive series to quantify a peptide in a set of samples." [PSI:PI]
is_a: MS:1000752 ! TOPP software */

#define MS_TOPP_DECHARGER_ID "MS:1002163"
#define MS_TOPP_DECHARGER_NAME "TOPP Decharger"
/* def: "Decharges and merges different feature charge variants of the same chemical entity." [PSI:PI]
is_a: MS:1000752 ! TOPP software */

#define MS_TOPP_EICEXTRACTOR_ID "MS:1002164"
#define MS_TOPP_EICEXTRACTOR_NAME "TOPP EICExtractor"
/* def: "Quantifies signals at given positions in (raw or picked) LC/MS maps." [PSI:PI]
is_a: MS:1000752 ! TOPP software */

#define MS_TOPP_FEATURE_FINDER_ID "MS:1002165"
#define MS_TOPP_FEATURE_FINDER_NAME "TOPP feature finder"
/* def: "Feature finder component of the TOPP software." [PSI:PI]
is_a: MS:1000752 ! TOPP software */

#define MS_TOPP_FEATUREFINDERCENTROIDED_ID "MS:1002166"
#define MS_TOPP_FEATUREFINDERCENTROIDED_NAME "TOPP FeatureFinderCentroided"
/* def: "Detects two-dimensional features in centroided LC-MS data." [PSI:PI]
is_a: MS:1002165 ! TOPP feature finder */

#define MS_TOPP_FEATUREFINDERRAW_ID "MS:1002167"
#define MS_TOPP_FEATUREFINDERRAW_NAME "TOPP FeatureFinderRaw"
/* def: "Detects two-dimensional features in uncentroided LC-MS data." [PSI:PI]
is_a: MS:1002165 ! TOPP feature finder */

#define MS_TOPP_FEATUREFINDERISOTOPEWAVELET_ID "MS:1002168"
#define MS_TOPP_FEATUREFINDERISOTOPEWAVELET_NAME "TOPP FeatureFinderIsotopeWavelet"
/* def: "Detects two-dimensional features in uncentroided LC-MS data with a wavelet algorithm." [PSI:PI]
is_a: MS:1002165 ! TOPP feature finder */

#define MS_TOPP_FEATUREFINDERMETABO_ID "MS:1002169"
#define MS_TOPP_FEATUREFINDERMETABO_NAME "TOPP FeatureFinderMetabo"
/* def: "Detects two-dimensional features in centroided LC-MS data of metabolites." [PSI:PI]
is_a: MS:1002165 ! TOPP feature finder */

#define MS_TOPP_FEATUREFINDERMRM_ID "MS:1002170"
#define MS_TOPP_FEATUREFINDERMRM_NAME "TOPP FeatureFinderMRM"
/* def: "Quantifies features LC-MS/MS MRM data." [PSI:PI]
is_a: MS:1002165 ! TOPP feature finder */

#define MS_TOPP_PROTEINQUANTIFIER_ID "MS:1002171"
#define MS_TOPP_PROTEINQUANTIFIER_NAME "TOPP ProteinQuantifier"
/* def: "Computes protein abundances from annotated feature/consensus maps." [PSI:PI]
is_a: MS:1000752 ! TOPP software */

#define MS_TOPP_CONSENSUSMAPNORMALIZER_ID "MS:1002172"
#define MS_TOPP_CONSENSUSMAPNORMALIZER_NAME "TOPP ConsensusMapNormalizer"
/* def: "Normalizes maps of one consensus XML file (after linking)." [PSI:PI]
is_a: MS:1000752 ! TOPP software */

#define MS_TOPP_MAPRTTRANSFORMER_ID "MS:1002173"
#define MS_TOPP_MAPRTTRANSFORMER_NAME "TOPP MapRTTransformer"
/* def: "Applies retention time transformations to maps." [PSI:PI]
is_a: MS:1000752 ! TOPP software */

#define MS_TOPP_FEATURE_LINKER_ID "MS:1002174"
#define MS_TOPP_FEATURE_LINKER_NAME "TOPP feature linker"
/* def: "Feature linker component of the TOPP software." [PSI:PI]
is_a: MS:1000752 ! TOPP software */

#define MS_TOPP_FEATURELINKERLABELED_ID "MS:1002175"
#define MS_TOPP_FEATURELINKERLABELED_NAME "TOPP FeatureLinkerLabeled"
/* def: "Groups corresponding isotope-labeled features in a feature map." [PSI:PI]
is_a: MS:1002174 ! TOPP feature linker */

#define MS_TOPP_FEATURELINKERUNLABELED_ID "MS:1002176"
#define MS_TOPP_FEATURELINKERUNLABELED_NAME "TOPP FeatureLinkerUnlabeled"
/* def: "Groups corresponding features from multiple maps." [PSI:PI]
is_a: MS:1002174 ! TOPP feature linker */

#define MS_TOPP_FEATURELINKERUNLABELEDQT_ID "MS:1002177"
#define MS_TOPP_FEATURELINKERUNLABELEDQT_NAME "TOPP FeatureLinkerUnlabeledQT"
/* def: "Groups corresponding features from multiple maps using a quality threshold clustering approach." [PSI:PI]
is_a: MS:1002174 ! TOPP feature linker */

#define MS_TOPP_COMPNOVO_ID "MS:1002178"
#define MS_TOPP_COMPNOVO_NAME "TOPP CompNovo"
/* def: "Performs a peptide/protein identification with the CompNovo engine." [PSI:PI]
is_a: MS:1000752 ! TOPP software */

#define MS_TOPP_COMPNOVOCID_ID "MS:1002179"
#define MS_TOPP_COMPNOVOCID_NAME "TOPP CompNovoCID"
/* def: "Performs a peptide/protein identification with the CompNovo engine in collission-induced dissociation (CID) mode." [PSI:PI]
is_a: MS:1000752 ! TOPP software */

#define MS_TOPP_SOFTWARE_ADAPTOR_ID "MS:1002180"
#define MS_TOPP_SOFTWARE_ADAPTOR_NAME "TOPP software adaptor"
/* def: "Software adaptor to an external program in the TOPP software." [PSI:PI]
is_a: MS:1000752 ! TOPP software */

#define MS_TOPP_INSPECTADAPTER_ID "MS:1002181"
#define MS_TOPP_INSPECTADAPTER_NAME "TOPP InspectAdapter"
/* def: "Identifies MS/MS spectra using the external program Inspect." [PSI:PI]
is_a: MS:1002180 ! TOPP software adaptor */

#define MS_TOPP_MASCOTADAPTER_ID "MS:1002182"
#define MS_TOPP_MASCOTADAPTER_NAME "TOPP MascotAdapter"
/* def: "Identifies MS/MS spectra using the external program Mascot." [PSI:PI]
is_a: MS:1002180 ! TOPP software adaptor */

#define MS_TOPP_MASCOTADAPTERONLINE_ID "MS:1002183"
#define MS_TOPP_MASCOTADAPTERONLINE_NAME "TOPP MascotAdapterOnline"
/* def: "Identifies MS/MS spectra using the online version of the external program Mascot." [PSI:PI]
is_a: MS:1002180 ! TOPP software adaptor */

#define MS_TOPP_OMSSAADAPTER_ID "MS:1002184"
#define MS_TOPP_OMSSAADAPTER_NAME "TOPP OMSSAAdapter"
/* def: "Identifies MS/MS spectra using the external program OMSSA." [PSI:PI]
is_a: MS:1002180 ! TOPP software adaptor */

#define MS_TOPP_PEPNOVOADAPTER_ID "MS:1002185"
#define MS_TOPP_PEPNOVOADAPTER_NAME "TOPP PepNovoAdapter"
/* def: "Identifies MS/MS spectra using the external program PepNovo." [PSI:PI]
is_a: MS:1002180 ! TOPP software adaptor */

#define MS_TOPP_XTANDEMADAPTER_ID "MS:1002186"
#define MS_TOPP_XTANDEMADAPTER_NAME "TOPP XTandemAdapter"
/* def: "Identifies MS/MS spectra using the external program XTandem." [PSI:PI]
is_a: MS:1002180 ! TOPP software adaptor */

#define MS_TOPP_SPECLIBSEARCHER_ID "MS:1002187"
#define MS_TOPP_SPECLIBSEARCHER_NAME "TOPP SpecLibSearcher"
/* def: "Identifies peptide MS/MS spectra by spectral matching with a searchable spectral library." [PSI:PI]
is_a: MS:1000752 ! TOPP software */

#define MS_TOPP_CONSENSUSID_ID "MS:1002188"
#define MS_TOPP_CONSENSUSID_NAME "TOPP ConsensusID"
/* def: "Computes a consensus identification from peptide identifications of several identification engines." [PSI:PI]
is_a: MS:1000752 ! TOPP software */

#define MS_TOPP_IDCONFLICTRESOLVER_ID "MS:1002189"
#define MS_TOPP_IDCONFLICTRESOLVER_NAME "TOPP IDConflictResolver"
/* def: "Resolves ambiguous annotations of features with peptide identifications." [PSI:PI]
is_a: MS:1000752 ! TOPP software */

#define MS_TOPP_IDFILTER_ID "MS:1002190"
#define MS_TOPP_IDFILTER_NAME "TOPP IDFilter"
/* def: "Filters results from protein or peptide identification engines based on different criteria." [PSI:PI]
is_a: MS:1000752 ! TOPP software */

#define MS_TOPP_IDMAPPER_ID "MS:1002191"
#define MS_TOPP_IDMAPPER_NAME "TOPP IDMapper"
/* def: "Assigns protein/peptide identifications to feature or consensus features." [PSI:PI]
is_a: MS:1000752 ! TOPP software */

#define MS_TOPP_IDPOSTERIORERRORPROBABILITY_ID "MS:1002192"
#define MS_TOPP_IDPOSTERIORERRORPROBABILITY_NAME "TOPP IDPosteriorErrorProbability"
/* def: "Estimates posterior error probabilities using a mixture model." [PSI:PI]
is_a: MS:1000752 ! TOPP software */

#define MS_TOPP_IDRTCALIBRATION_ID "MS:1002193"
#define MS_TOPP_IDRTCALIBRATION_NAME "TOPP IDRTCalibration"
/* def: "Calibrate Retention times of peptide hits to standards." [PSI:PI]
is_a: MS:1000752 ! TOPP software */

#define MS_TOPP_PEPTIDEINDEXER_ID "MS:1002194"
#define MS_TOPP_PEPTIDEINDEXER_NAME "TOPP PeptideIndexer"
/* def: "Refreshes the protein references for all peptide hits." [PSI:PI]
is_a: MS:1000752 ! TOPP software */

#define MS_TOPP_PRECURSORIONSELECTOR_ID "MS:1002195"
#define MS_TOPP_PRECURSORIONSELECTOR_NAME "TOPP PrecursorIonSelector"
/* def: "A tool for precursor ion selection based on identification results." [PSI:PI]
is_a: MS:1000752 ! TOPP software */

#define MS_TOPP_MRMMAPPER_ID "MS:1002196"
#define MS_TOPP_MRMMAPPER_NAME "TOPP MRMMapper"
/* def: "MRMMapper maps measured chromatograms (mzML) and the transitions used (TraML)." [PSI:PI]
is_a: MS:1000752 ! TOPP software */

#define MS_TOPP_OPENSWATH_COMPONENT_ID "MS:1002197"
#define MS_TOPP_OPENSWATH_COMPONENT_NAME "TOPP OpenSwath component"
/* def: "OpenSwath component of the TOPP software." [PSI:PI]
is_a: MS:1000752 ! TOPP software */

#define MS_TOPP_OPENSWATHANALYZER_ID "MS:1002198"
#define MS_TOPP_OPENSWATHANALYZER_NAME "TOPP OpenSwathAnalyzer"
/* def: "Picks peaks and finds features in an SRM experiment." [PSI:PI]
is_a: MS:1002197 ! TOPP OpenSwath component */

#define MS_TOPP_OPENSWATHCHROMATOGRAMEXTRACTOR_ID "MS:1002199"
#define MS_TOPP_OPENSWATHCHROMATOGRAMEXTRACTOR_NAME "TOPP OpenSwathChromatogramExtractor"
/* def: "Extract chromatograms (XIC) from a MS2 map file." [PSI:PI]
is_a: MS:1002197 ! TOPP OpenSwath component */

#define MS_TOPP_OPENSWATHDECOYGENERATOR_ID "MS:1002200"
#define MS_TOPP_OPENSWATHDECOYGENERATOR_NAME "TOPP OpenSwathDecoyGenerator"
/* def: "Generates decoys according to different models for a specific TraML." [PSI:PI]
is_a: MS:1002197 ! TOPP OpenSwath component */

#define MS_TOPP_OPENSWATHFEATUREXMLTOTSV_ID "MS:1002201"
#define MS_TOPP_OPENSWATHFEATUREXMLTOTSV_NAME "TOPP OpenSwathFeatureXMLToTSV"
/* def: "Converts a featureXML to a mProphet tsv (tab separated values)." [PSI:PI]
is_a: MS:1002197 ! TOPP OpenSwath component */

#define MS_TOPP_OPENSWATHRTNORMALIZER_ID "MS:1002202"
#define MS_TOPP_OPENSWATHRTNORMALIZER_NAME "TOPP OpenSwathRTNormalizer"
/* def: "Generates a transformation file for retention time space into normalized space." [PSI:PI]
is_a: MS:1002197 ! TOPP OpenSwath component */

#define MS_TOPP_PROTEININFERENCE_ID "MS:1002203"
#define MS_TOPP_PROTEININFERENCE_NAME "TOPP ProteinInference"
/* def: "Infer proteins from a list of (high-confidence) peptides." [PSI:PI]
is_a: MS:1000752 ! TOPP software */

#define MS_TOPP_FALSEDISCOVERYRATE_ID "MS:1002204"
#define MS_TOPP_FALSEDISCOVERYRATE_NAME "TOPP FalseDiscoveryRate"
/* def: "Estimates the false discovery rate on peptide and protein level using decoy searches." [PSI:PI]
is_a: MS:1000752 ! TOPP software */

#define MS_PROTEOWIZARD_MSCONVERT_ID "MS:1002205"
#define MS_PROTEOWIZARD_MSCONVERT_NAME "ProteoWizard msconvert"
/* def: "Converts, filters, and processes mass spectrometry data in variety of formats." [PSI:MS]
is_a: MS:1000615 ! ProteoWizard software */

#define MS_PROTEOWIZARD_IDCONVERT_ID "MS:1002206"
#define MS_PROTEOWIZARD_IDCONVERT_NAME "ProteoWizard idconvert"
/* def: "Converts, filters, and processes identifications from shotgun proteomics experiments." [PSI:MS]
is_a: MS:1000615 ! ProteoWizard software */

#define MS_PROTEOWIZARD_CHAINSAW_ID "MS:1002207"
#define MS_PROTEOWIZARD_CHAINSAW_NAME "ProteoWizard chainsaw"
/* def: "Filters and processes protein sequence databases." [PSI:MS]
is_a: MS:1000615 ! ProteoWizard software */

#define MS_PROTEOWIZARD_MSACCESS_ID "MS:1002208"
#define MS_PROTEOWIZARD_MSACCESS_NAME "ProteoWizard msaccess"
/* def: "Filters, processes, and displays mass spectrometry data in a variety of ways." [PSI:MS]
is_a: MS:1000615 ! ProteoWizard software */

#define MS_PROTEOWIZARD_SEEMS_ID "MS:1002209"
#define MS_PROTEOWIZARD_SEEMS_NAME "ProteoWizard SeeMS"
/* def: "An interactive GUI application to view and filter mass spectrometry data in a variety of formats." [PSI:MS]
is_a: MS:1000615 ! ProteoWizard software */

#define MS_ISOBARIQ_ID "MS:1002210"
#define MS_ISOBARIQ_NAME "IsobariQ"
/* def: "A quantitative software package designed for analysis of IPTL, TMT and iTRAQ data." [PMID:21067241, DOI:10.1021/pr1009977, http://folk.uio.no/magnusar/isobariq]
is_a: MS:1001139 ! quantitation software name
is_a: MS:1001456 ! analysis software */

#define MS_VARIANCE_STABILIZING_NORMALIZATION_ID "MS:1002211"
#define MS_VARIANCE_STABILIZING_NORMALIZATION_NAME "Variance stabilizing normalization"
/* def: "The model incorporates data calibration (normalization), a model for the dependence of the variance on the mean intensity, and a variance stabilizing data transformation." [PMID:16646781]
is_a: MS:1001861 ! quantification data processing */

#define MS_IPTL_QUANTITATION_ANALYSIS_ID "MS:1002212"
#define MS_IPTL_QUANTITATION_ANALYSIS_NAME "IPTL quantitation analysis"
/* def: "Quantification analysis using a labeling strategy where both peptide termini are labeled so that the peptides from different labeling schema are isobaric." [PSI:PI]
is_a: MS:1002009 ! isobaric label quantitation analysis */

#define MS_PANALYZER_CONCLUSIVE_PROTEIN_ID "MS:1002213"
#define MS_PANALYZER_CONCLUSIVE_PROTEIN_NAME "PAnalyzer:conclusive protein"
/* def: "A protein identified by at least one unique (distinct, discrete) peptide (peptides are considered different only if they can be distinguished by evidence in mass spectrum)." [PSI:PI]
xref: value-type:xsd\:string "The allowed value-type for this CV term."
is_a: MS:1001600 ! protein inference confidence category */

#define MS_PANALYZER_INDISTINGUISHABLE_PROTEIN_ID "MS:1002214"
#define MS_PANALYZER_INDISTINGUISHABLE_PROTEIN_NAME "PAnalyzer:indistinguishable protein"
/* def: "A member of a group of proteins sharing all peptides that are exclusive to the group (peptides are considered different only if they can be distinguished by evidence in mass spectrum)." [PSI:PI]
xref: value-type:xsd\:string "The allowed value-type for this CV term."
is_a: MS:1001600 ! protein inference confidence category */

#define MS_PANALYZER_NON_CONCLUSIVE_PROTEIN_ID "MS:1002215"
#define MS_PANALYZER_NON_CONCLUSIVE_PROTEIN_NAME "PAnalyzer:non-conclusive protein"
/* def: "A protein sharing all its matched peptides with either conclusive or indistinguishable proteins (peptides are considered different only if they can be distinguished by evidence in mass spectrum)." [PSI:PI]
xref: value-type:xsd\:string "The allowed value-type for this CV term."
is_a: MS:1001600 ! protein inference confidence category */

#define MS_PANALYZER_AMBIGUOUS_GROUP_MEMBER_ID "MS:1002216"
#define MS_PANALYZER_AMBIGUOUS_GROUP_MEMBER_NAME "PAnalyzer:ambiguous group member"
/* def: "A protein sharing at least one peptide not matched to either conclusive or indistinguishable proteins (peptides are considered different only if they can be distinguished by evidence in mass spectrum)." [PSI:PI]
xref: value-type:xsd\:string "The allowed value-type for this CV term."
is_a: MS:1001600 ! protein inference confidence category */

#define MS_DECOY_PEPTIDE_ID "MS:1002217"
#define MS_DECOY_PEPTIDE_NAME "decoy peptide"
/* def: "A putative identified peptide issued from a decoy sequence database." [PSI:PI]
xref: value-type:xsd\:boolean "The allowed value-type for this CV term."
is_a: MS:1001105 ! peptide result details */

#define MS_PERCENT_COLLISION_ENERGY_RAMP_START_ID "MS:1002218"
#define MS_PERCENT_COLLISION_ENERGY_RAMP_START_NAME "percent collision energy ramp start"
/* def: "Collision energy at the start of the collision energy ramp in percent, normalized to the mass of the ion." [PSI:PI]
xref: value-type:xsd\:float "The allowed value-type for this CV term."
is_a: MS:1000138 ! percent collision energy
relationship: has_units UO:0000187 ! percent */

#define MS_PERCENT_COLLISION_ENERGY_RAMP_END_ID "MS:1002219"
#define MS_PERCENT_COLLISION_ENERGY_RAMP_END_NAME "percent collision energy ramp end"
/* def: "Collision energy at the end of the collision energy ramp in percent, normalized to the mass of the ion." [PSI:PI]
xref: value-type:xsd\:float "The allowed value-type for this CV term."
is_a: MS:1000138 ! percent collision energy
relationship: has_units UO:0000187 ! percent */

#define MS_MRMAID_ID "MS:1002220"
#define MS_MRMAID_NAME "MRMaid"
/* def: "A web-based SRM assay design tool whose transitions are generated by mining the millions of identified peptide spectra held in the EBI's PRIDE database." [PSI:PI]
is_a: MS:1000871 ! SRM software */

#define MS_MRMAID_PEPTIDE_SCORE_ID "MS:1002221"
#define MS_MRMAID_PEPTIDE_SCORE_NAME "MRMaid:peptide score"
/* def: "Score in MRMaid to indicate the expected performance of the peptide in SRM." [PSI:PI]
is_a: MS:1001143 ! search engine specific score for PSMs
is_a: MS:1001153 ! search engine specific score */

#define MS_SRM_TRANSITION_ATTRIBUTE_ID "MS:1002222"
#define MS_SRM_TRANSITION_ATTRIBUTE_NAME "SRM transition attribute"
/* def: "Attribute associated with a SRM transition." [PSI:MS]
is_a: MS:1000455 ! ion selection attribute
relationship: part_of MS:1000908 ! transition */

#define MS_PRECURSOR_ION_DETECTION_PROBABILITY_ID "MS:1002223"
#define MS_PRECURSOR_ION_DETECTION_PROBABILITY_NAME "precursor ion detection probability"
/* def: "Probability of detecting precursor when parent protein is present." [PSI:PI]
xref: value-type:xsd\:float "The allowed value-type for this CV term."
is_a: MS:1002222 ! SRM transition attribute */

#define MS_PRODUCT_ION_DETECTION_PROBABILITY_ID "MS:1002224"
#define MS_PRODUCT_ION_DETECTION_PROBABILITY_NAME "product ion detection probability"
/* def: "Probability of detecting product ion when precursor ion is present." [PSI:PI]
xref: value-type:xsd\:float "The allowed value-type for this CV term."
is_a: MS:1002222 ! SRM transition attribute */

#define MS_AVERAGE_PRODUCT_ION_INTENSITY_ID "MS:1002225"
#define MS_AVERAGE_PRODUCT_ION_INTENSITY_NAME "average product ion intensity"
/* def: "Average value of product ion intensity in a collection of identified spectra." [PSI:PI]
xref: value-type:xsd\:float "The allowed value-type for this CV term."
is_a: MS:1001226 ! product ion intensity */

#define MS_PRODUCT_ION_INTENSITY_STANDARD_DEVIATION_ID "MS:1002226"
#define MS_PRODUCT_ION_INTENSITY_STANDARD_DEVIATION_NAME "product ion intensity standard deviation"
/* def: "Standard deviation of product ion intensity in a collection of identified spectra." [PSI:PI]
xref: value-type:xsd\:float "The allowed value-type for this CV term."
is_a: MS:1001226 ! product ion intensity */

#define MS_NUMBER_OF_PRODUCT_ION_OBSERVATIONS_ID "MS:1002227"
#define MS_NUMBER_OF_PRODUCT_ION_OBSERVATIONS_NAME "number of product ion observations"
/* def: "The number of times the specific product ion has been observed in a series of SRM experiments." [PSI:PI]
xref: value-type:xsd\:int "The allowed value-type for this CV term."
is_a: MS:1002222 ! SRM transition attribute */

#define MS_NUMBER_OF_PRECURSOR_ION_OBSERVATIONS_ID "MS:1002228"
#define MS_NUMBER_OF_PRECURSOR_ION_OBSERVATIONS_NAME "number of precursor ion observations"
/* def: "The number of times the specific precursor ion has been observed in a series of SRM experiments." [PSI:PI]
xref: value-type:xsd\:int "The allowed value-type for this CV term."
is_a: MS:1002222 ! SRM transition attribute */

#define MS_PROTEOMEDISCOVERER_MASCOT_SIGNIFICANCE_MIDDLE_ID "MS:1002229"
#define MS_PROTEOMEDISCOVERER_MASCOT_SIGNIFICANCE_MIDDLE_NAME "ProteomeDiscoverer:Mascot:Significance Middle"
/* def: "Calculated relaxed significance when performing a decoy search for high-confidence peptides." [PSI:PI]
xref: value-type:xsd\:float "The allowed value-type for this CV term."
is_a: MS:1002101 ! ProteomeDiscoverer input parameter */

#define MS_PROTEOMEDISCOVERER_MASCOT_SIGNIFICANCE_HIGH_ID "MS:1002230"
#define MS_PROTEOMEDISCOVERER_MASCOT_SIGNIFICANCE_HIGH_NAME "ProteomeDiscoverer:Mascot:Significance High"
/* def: "Calculated relaxed significance when performing a decoy search for medium-confidence peptides." [PSI:PI]
xref: value-type:xsd\:float "The allowed value-type for this CV term."
is_a: MS:1002101 ! ProteomeDiscoverer input parameter */

#define MS_PROTEOMEDISCOVERER____DEFAULT_FDR_CALCULATOR_ID "MS:1002232"
#define MS_PROTEOMEDISCOVERER____DEFAULT_FDR_CALCULATOR_NAME "ProteomeDiscoverer:_* default FDR calculator"
/* def: "The default FDR calculator." [PSI:PI]
is_a: MS:1002101 ! ProteomeDiscoverer input parameter
relationship: has_regexp MS:1002231 ! ([A-Fa-f0-9]{8}-([A-Fa-f0-9]{4}-){3}[A-Fa-f0-9]{12}) */

#define MS_PROTEOMEDISCOVERER_SEQUEST_LOW_RESOLUTION_SPECTRA_CONTAINED_ID "MS:1002233"
#define MS_PROTEOMEDISCOVERER_SEQUEST_LOW_RESOLUTION_SPECTRA_CONTAINED_NAME "ProteomeDiscoverer:SEQUEST:Low resolution spectra contained"
/* def: "Flag indicating if low-resolution speatra are taken into consideration." [PSI:PI]
xref: value-type:xsd\:boolean "The allowed value-type for this CV term."
is_a: MS:1002101 ! ProteomeDiscoverer input parameter */

#define MS_SELECTED_PRECURSOR_M_Z_ID "MS:1002234"
#define MS_SELECTED_PRECURSOR_M_Z_NAME "selected precursor m/z"
/* def: "Mass-to-charge ratio of a precursor ion selected for fragmentation." [PSI:PI]
synonym: "selected ion m/z" RELATED []
xref: value-type:xsd\:float "The allowed value-type for this CV term."
is_a: MS:1000455 ! ion selection attribute
relationship: has_units MS:1000040 ! m/z */

#define MS_PROTEOGROUPER_PDH_SCORE_ID "MS:1002235"
#define MS_PROTEOGROUPER_PDH_SCORE_NAME "ProteoGrouper:PDH score"
/* def: "A score assigned to a single protein accession (modelled as ProteinDetectionHypothesis in mzIdentML), based on summed peptide level scores." [PSI:PI]
xref: value-type:xsd\:double "The allowed value-type for this CV term."
is_a: MS:1001116 ! single protein result details */

#define MS_PROTEOGROUPER_PAG_SCORE_ID "MS:1002236"
#define MS_PROTEOGROUPER_PAG_SCORE_NAME "ProteoGrouper:PAG score"
/* def: "A score assigned to a protein group (modelled as ProteinAmbiguityGroup in mzIdentML), based on all summed peptide level scores that have been assigned to the group as unique or razor peptides." [PSI:PI]
xref: value-type:xsd\:double "The allowed value-type for this CV term."
is_a: MS:1001147 ! protein ambiguity group result details */

#define MS_MZIDLIB_ID "MS:1002237"
#define MS_MZIDLIB_NAME "mzidLib"
/* def: "A library of Java routines for manipulating mzIdentML files." [PSI:PI]
is_a: MS:1001456 ! analysis software */

#define MS_MZIDLIB_OMSSA2MZID_ID "MS:1002238"
#define MS_MZIDLIB_OMSSA2MZID_NAME "mzidLib:Omssa2Mzid"
/* def: "A converter for OMSSA OMX to mzIdentML." [PSI:PI]
is_a: MS:1002237 ! mzidLib */

#define MS_MZIDLIB_TANDEM2MZID_ID "MS:1002239"
#define MS_MZIDLIB_TANDEM2MZID_NAME "mzidLib:Tandem2Mzid"
/* def: "A converter for Tandem XML to mzIdentML." [PSI:PI]
is_a: MS:1002237 ! mzidLib */

#define MS_MZIDLIB_CSV2MZID_ID "MS:1002240"
#define MS_MZIDLIB_CSV2MZID_NAME "mzidLib:Csv2Mzid"
/* def: "A converter for CSV files (following OMSSA CSV style) to mzIdentML." [PSI:PI]
is_a: MS:1002237 ! mzidLib */

#define MS_MZIDLIB_PROTEOGROUPER_ID "MS:1002241"
#define MS_MZIDLIB_PROTEOGROUPER_NAME "mzidLib:ProteoGrouper"
/* def: "A generic and parameterizable protein inference algorithm for mzIdentML files." [PSI:PI]
is_a: MS:1002237 ! mzidLib */

#define MS_MZIDLIB_THRESHOLDER_ID "MS:1002242"
#define MS_MZIDLIB_THRESHOLDER_NAME "mzidLib:Thresholder"
/* def: "A routine for keeping only identifications passing a given threshold or setting passThreshold to true or false for SpectrumIdentificationItem or ProteinDetectionHypothesis in mzIdentML files." [PSI:PI]
is_a: MS:1002237 ! mzidLib */

#define MS_MZIDLIB_PERFORM_EMPAI_ON_MZID_ID "MS:1002243"
#define MS_MZIDLIB_PERFORM_EMPAI_ON_MZID_NAME "mzidLib:Perform emPAI on mzid"
/* def: "A routine for adding emPAI quantitative values to an mzIdentML file." [PSI:PI]
is_a: MS:1002237 ! mzidLib */

#define MS_MZIDLIB_FALSEDISCOVERYRATE_ID "MS:1002244"
#define MS_MZIDLIB_FALSEDISCOVERYRATE_NAME "mzidLib:FalseDiscoveryRate"
/* def: "A routine for calculating local FDR, q-value and FDRScore for mzIdentML files, based on a decoy search." [PSI:PI]
is_a: MS:1002237 ! mzidLib */

#define MS_MZIDLIB_MZIDENTML2CSV_ID "MS:1002245"
#define MS_MZIDLIB_MZIDENTML2CSV_NAME "mzidLib:Mzidentml2Csv"
/* def: "A tool for converting mzIdentML files to CSV format." [PSI:PI]
is_a: MS:1002237 ! mzidLib */

#define MS_MZIDLIB_COMBINESEARCHENGINES_ID "MS:1002246"
#define MS_MZIDLIB_COMBINESEARCHENGINES_NAME "mzidLib:CombineSearchEngines"
/* def: "A tool for combining results analysed in parallel in two or three search engines into a single mzIdentML file." [PMID:19253293]
is_a: MS:1002237 ! mzidLib */

#define MS_MZIDLIB_INSERTMETADATAFROMFASTA_ID "MS:1002247"
#define MS_MZIDLIB_INSERTMETADATAFROMFASTA_NAME "mzidLib:InsertMetaDataFromFasta"
/* def: "A tool for adding additional metadata from a FASTA file to DBSequence entries (sequence and description) in mzIdentML files." [PSI:PI]
is_a: MS:1002237 ! mzidLib */

#define MS_SEQUEST_SPSCORE_ID "MS:1002248"
#define MS_SEQUEST_SPSCORE_NAME "SEQUEST:spscore"
/* def: "The SEQUEST result 'SpScore'." [PSI:PI]
xref: value-type:xsd\:double "The allowed value-type for this CV term."
is_a: MS:1001143 ! search engine specific score for PSMs
is_a: MS:1001153 ! search engine specific score */

#define MS_SEQUEST_SPRANK_ID "MS:1002249"
#define MS_SEQUEST_SPRANK_NAME "SEQUEST:sprank"
/* def: "The SEQUEST result 'SpRank'." [PSI:PI]
xref: value-type:xsd\:integer "The allowed value-type for this CV term."
is_a: MS:1001143 ! search engine specific score for PSMs
is_a: MS:1001153 ! search engine specific score */

#define MS_SEQUEST_DELTACNSTAR_ID "MS:1002250"
#define MS_SEQUEST_DELTACNSTAR_NAME "SEQUEST:deltacnstar"
/* def: "The SEQUEST result 'DeltaCnStar'." [PSI:PI]
xref: value-type:xsd\:double "The allowed value-type for this CV term."
is_a: MS:1001143 ! search engine specific score for PSMs
is_a: MS:1001153 ! search engine specific score */

#define MS_COMET_ID "MS:1002251"
#define MS_COMET_NAME "Comet"
/* def: "Comet open-source sequence search engine developed at the University of Washington." [PMID:23148064]
is_a: MS:1001456 ! analysis software */

#define MS_COMET_XCORR_ID "MS:1002252"
#define MS_COMET_XCORR_NAME "Comet:xcorr"
/* def: "The Comet result 'XCorr'." [PSI:PI]
xref: value-type:xsd\:double "The allowed value-type for this CV term."
is_a: MS:1001143 ! search engine specific score for PSMs
is_a: MS:1001153 ! search engine specific score
has_order: MS:1002108 ! higher score better */

#define MS_COMET_DELTACN_ID "MS:1002253"
#define MS_COMET_DELTACN_NAME "Comet:deltacn"
/* def: "The Comet result 'DeltaCn'." [PSI:PI]
xref: value-type:xsd\:double "The allowed value-type for this CV term."
is_a: MS:1001143 ! search engine specific score for PSMs
is_a: MS:1001153 ! search engine specific score */

#define MS_COMET_DELTACNSTAR_ID "MS:1002254"
#define MS_COMET_DELTACNSTAR_NAME "Comet:deltacnstar"
/* def: "The Comet result 'DeltaCnStar'." [PSI:PI]
xref: value-type:xsd\:double "The allowed value-type for this CV term."
is_a: MS:1001143 ! search engine specific score for PSMs
is_a: MS:1001153 ! search engine specific score */

#define MS_COMET_SPSCORE_ID "MS:1002255"
#define MS_COMET_SPSCORE_NAME "Comet:spscore"
/* def: "The Comet result 'SpScore'." [PSI:PI]
xref: value-type:xsd\:double "The allowed value-type for this CV term."
is_a: MS:1001143 ! search engine specific score for PSMs
is_a: MS:1001153 ! search engine specific score */

#define MS_COMET_SPRANK_ID "MS:1002256"
#define MS_COMET_SPRANK_NAME "Comet:sprank"
/* def: "The Comet result 'SpRank'." [PSI:PI]
xref: value-type:xsd\:double "The allowed value-type for this CV term."
is_a: MS:1001143 ! search engine specific score for PSMs
is_a: MS:1001153 ! search engine specific score */

#define MS_COMET_EXPECTATION_VALUE_ID "MS:1002257"
#define MS_COMET_EXPECTATION_VALUE_NAME "Comet:expectation value"
/* def: "The Comet result 'Expectation value'." [PSI:PI]
xref: value-type:xsd\:double "The allowed value-type for this CV term."
is_a: MS:1001153 ! search engine specific score */

#define MS_COMET_MATCHED_IONS_ID "MS:1002258"
#define MS_COMET_MATCHED_IONS_NAME "Comet:matched ions"
/* def: "The Comet result 'Matched Ions'." [PSI:PI]
xref: value-type:xsd\:integer "The allowed value-type for this CV term."
is_a: MS:1001143 ! search engine specific score for PSMs
is_a: MS:1001153 ! search engine specific score */

#define MS_COMET_TOTAL_IONS_ID "MS:1002259"
#define MS_COMET_TOTAL_IONS_NAME "Comet:total ions"
/* def: "The Comet result 'Total Ions'." [PSI:PI]
xref: value-type:xsd\:integer "The allowed value-type for this CV term."
is_a: MS:1001143 ! search engine specific score for PSMs
is_a: MS:1001153 ! search engine specific score */

#define MS_PSM_FDR_THRESHOLD_ID "MS:1002260"
#define MS_PSM_FDR_THRESHOLD_NAME "PSM:FDR threshold"
/* def: "False-discovery rate threshold for peptide-spectrum matches." [PSI:PI]
xref: value-type:xsd\:double "The allowed value-type for this CV term."
is_a: MS:1001060 ! quality estimation method details */

#define MS_BYONIC_ID "MS:1002261"
#define MS_BYONIC_NAME "Byonic"
/* def: "Byonic search engine from Protein Metrics." [PMID:23255153]
is_a: MS:1001456 ! analysis software */

#define MS_BYONIC_SCORE_ID "MS:1002262"
#define MS_BYONIC_SCORE_NAME "Byonic:Score"
/* def: "The Byonic score is the primary indicator of PSM correctness. The Byonic score reflects the absolute quality of the peptide-spectrum match, not the relative quality compared to other candidate peptides. Byonic scores range from 0 to about 1000, with 300 a good score, 400 a very good score, and PSMs with scores over 500 almost sure to be correct." [PSI:PI]
xref: value-type:xsd\:double "The allowed value-type for this CV term."
is_a: MS:1001143 ! search engine specific score for PSMs
is_a: MS:1001153 ! search engine specific score
has_order: MS:1002108 ! higher score better */

#define MS_BYONIC_DELTA_SCORE_ID "MS:1002263"
#define MS_BYONIC_DELTA_SCORE_NAME "Byonic:Delta Score"
/* def: "The drop in Byonic score from the top-scoring peptide to the next peptide with distinct sequence. In this computation, the same peptide with different modifications is not considered distinct." [PSI:PI]
xref: value-type:xsd\:double "The allowed value-type for this CV term."
is_a: MS:1001143 ! search engine specific score for PSMs
is_a: MS:1001153 ! search engine specific score
has_order: MS:1002108 ! higher score better */

#define MS_BYONIC_DELTAMOD_SCORE_ID "MS:1002264"
#define MS_BYONIC_DELTAMOD_SCORE_NAME "Byonic:DeltaMod Score"
/* def: "The drop in Byonic score from the top-scoring peptide to the next peptide different in any way, including placement of modifications. DeltaMod gives an indication of whether modifications are confidently localized; DeltaMod over 10.0 means that there is high likelihood that all modification placements are correct." [PSI:PI]
xref: value-type:xsd\:double "The allowed value-type for this CV term."
is_a: MS:1001143 ! search engine specific score for PSMs
is_a: MS:1001153 ! search engine specific score
has_order: MS:1002108 ! higher score better */

#define MS_BYONIC_PEP_ID "MS:1002265"
#define MS_BYONIC_PEP_NAME "Byonic:PEP"
/* def: "Byonic posterior error probability." [PSI:PI]
xref: value-type:xsd\:double "The allowed value-type for this CV term."
is_a: MS:1001143 ! search engine specific score for PSMs
is_a: MS:1001153 ! search engine specific score
has_order: MS:1002109 ! lower score better */

#define MS_BYONIC_PEPTIDE_LOGPROB_ID "MS:1002266"
#define MS_BYONIC_PEPTIDE_LOGPROB_NAME "Byonic:Peptide LogProb"
/* def: "The log p-value of the PSM. This is the log of the probability that the PSM with such a score and delta would arise by chance in a search of this size (size of the protein database, as expanded by the modification rules). A log p-value of -3.0 should happen by chance on only one of a thousand spectra. Caveat: it is very hard to compute a p-value that works for all searches and all spectra, so read Byonic p-values with a certain amount of skepticism." [PSI:PI]
xref: value-type:xsd\:double "The allowed value-type for this CV term."
is_a: MS:1001143 ! search engine specific score for PSMs
is_a: MS:1001153 ! search engine specific score
has_order: MS:1002109 ! lower score better */

#define MS_BYONIC_PROTEIN_LOGPROB_ID "MS:1002267"
#define MS_BYONIC_PROTEIN_LOGPROB_NAME "Byonic:Protein LogProb"
/* def: "The log p-value of the protein." [PSI:PI]
xref: value-type:xsd\:double "The allowed value-type for this CV term."
is_a: MS:1001116 ! single protein result details
is_a: MS:1001153 ! search engine specific score
has_order: MS:1002109 ! lower score better */

#define MS_BYONIC_BEST_LOGPROB_ID "MS:1002268"
#define MS_BYONIC_BEST_LOGPROB_NAME "Byonic:Best LogProb"
/* def: "Best (most negative) log p-value of an individual PSM." [PSI:PI]
xref: value-type:xsd\:double "The allowed value-type for this CV term."
is_a: MS:1001116 ! single protein result details
is_a: MS:1001153 ! search engine specific score
has_order: MS:1002109 ! lower score better */

#define MS_BYONIC_BEST_SCORE_ID "MS:1002269"
#define MS_BYONIC_BEST_SCORE_NAME "Byonic:Best Score"
/* def: "Best (largest) Byonic score of a PSM." [PSI:PI]
xref: value-type:xsd\:double "The allowed value-type for this CV term."
is_a: MS:1001116 ! single protein result details
is_a: MS:1001153 ! search engine specific score
has_order: MS:1002108 ! higher score better */

#define MS_CHROMATOGRAPHY_SEPARATION_ID "MS:1002270"
#define MS_CHROMATOGRAPHY_SEPARATION_NAME "chromatography separation"
/* def: "A technique by which molecules are separated by chemical and physical properties such as hydrophobicity or vapor pressure." [PSI:MS]
is_a: MS:1000857 ! run attribute */

#define MS_LIQUID_CHROMATOGRAPHY_SEPARATION_ID "MS:1002271"
#define MS_LIQUID_CHROMATOGRAPHY_SEPARATION_NAME "liquid chromatography separation"
/* def: "Liquid chromatography (LC) is a separation technique in which the mobile phase is a liquid." [PSI:MS]
is_a: MS:1002270 ! chromatography separation */

#define MS_GAS_CHROMATOGRAPHY_SEPARATION_ID "MS:1002272"
#define MS_GAS_CHROMATOGRAPHY_SEPARATION_NAME "gas chromatography separation"
/* def: "Gas chromatography (GC) is a separation technique in which the mobile phase is a gas." [PSI:MS]
is_a: MS:1002270 ! chromatography separation */

#define MS_DETECTOR_POTENTIAL_ID "MS:1002273"
#define MS_DETECTOR_POTENTIAL_NAME "detector potential"
/* def: "Detector potential difference in volts." [PSI:MS]
xref: value-type:xsd\:float "The allowed value-type for this CV term."
is_a: MS:1000481 ! detector attribute
relationship: has_units UO:0000218 ! volt */

#define MS_SQ_DETECTOR_2_ID "MS:1002274"
#define MS_SQ_DETECTOR_2_NAME "SQ Detector 2	"
/* def: "Waters quadrupole based SQ Detector 2." [PSI:MS]
is_a: MS:1000126 ! Waters instrument model */

#define MS_XEVO_G2_S_TOF_ID "MS:1002275"
#define MS_XEVO_G2_S_TOF_NAME "Xevo G2-S Tof"
/* def: "Waters oa-ToF based Xevo G2-S Tof." [PSI:MS]
is_a: MS:1000126 ! Waters instrument model */

#define MS_XEVO_G2_S_QTOF_ID "MS:1002276"
#define MS_XEVO_G2_S_QTOF_NAME "Xevo G2-S QTof"
/* def: "Waters oa-ToF based Xevo G2-S QTof." [PSI:MS]
is_a: MS:1000126 ! Waters instrument model */

#define MS_AUTOSPEC_PREMIER_ID "MS:1002277"
#define MS_AUTOSPEC_PREMIER_NAME "AutoSpec Premier"
/* def: "Waters AutoSpec Premier." [PSI:MS]
is_a: MS:1000126 ! Waters instrument model */

#define MS_PEGASUS_III_ID "MS:1002278"
#define MS_PEGASUS_III_NAME "Pegasus III"
/* def: "LECO nominal mass resolution time-of-flight GC mass spectrometer." [PSI:MS]
is_a: MS:1001800 ! LECO instrument model */

#define MS_MAXIS_4G_ID "MS:1002279"
#define MS_MAXIS_4G_NAME "maXis 4G"
/* def: "Bruker Daltonics' maXis 4G: ESI Q-TOF, Nanospray, APCI, APPI, GC-APCI, CaptiveSpray." [PSI:MS]
is_a: MS:1001547 ! Bruker Daltonics maXis series */

#define MS_COMPACT_ID "MS:1002280"
#define MS_COMPACT_NAME "compact"
/* def: "Bruker Daltonics' compact: ESI Q-TOF, Nanospray, APCI, APPI, GC-APCI, CaptiveSpray." [PSI:MS]
is_a: MS:1001536 ! Bruker Daltonics micrOTOF series */

#define MS_SRM_FEATURE_LEVEL_QUANTITATION_ID "MS:1002281"
#define MS_SRM_FEATURE_LEVEL_QUANTITATION_NAME "SRM feature level quantitation"
/* def: "Selected Reaction Monitoring feature level quantitation." [PSI:PI]
is_a: MS:1001838 ! SRM quantitation analysis */

#define MS_SRM_PEPTIDE_LEVEL_QUANTITATION_ID "MS:1002282"
#define MS_SRM_PEPTIDE_LEVEL_QUANTITATION_NAME "SRM peptide level quantitation"
/* def: "Selected Reaction Monitoring peptide level quantitation." [PSI:PI]
is_a: MS:1001838 ! SRM quantitation analysis */

#define MS_SRM_PROTEIN_LEVEL_QUANTITATION_ID "MS:1002283"
#define MS_SRM_PROTEIN_LEVEL_QUANTITATION_NAME "SRM protein level quantitation"
/* def: "Selected Reaction Monitoring protein level quantitation." [PSI:PI]
is_a: MS:1001838 ! SRM quantitation analysis */

#define MS_SRM_PROTEINGROUP_LEVEL_QUANTITATION_ID "MS:1002284"
#define MS_SRM_PROTEINGROUP_LEVEL_QUANTITATION_NAME "SRM proteingroup level quantitation"
/* def: "Selected Reaction Monitoring proteingroup level quantitation." [PSI:PI]
is_a: MS:1001838 ! SRM quantitation analysis */

#define MS_TRANS_PROTEOMIC_PIPELINE_ID "MS:1002285"
#define MS_TRANS_PROTEOMIC_PIPELINE_NAME "Trans-Proteomic Pipeline"
/* def: "A suite of open source tools for the processing of MS/MS proteomics data developed by the Seattle Proteome Center at the Institute for Systems Biology." [PSI:PI]
synonym: "TPP" EXACT []
is_a: MS:1001456 ! analysis software */

#define MS_TRANS_PROTEOMIC_PIPELINE_SOFTWARE_ID "MS:1002286"
#define MS_TRANS_PROTEOMIC_PIPELINE_SOFTWARE_NAME "Trans-Proteomic Pipeline software"
/* def: "A software program that is a component of the Trans-Proteomic Pipeline." [PSI:PI]
is_a: MS:1001456 ! analysis software
relationship: part_of MS:1002285 ! Trans-Proteomic Pipeline */

#define MS_PEPTIDEPROPHET_ID "MS:1002287"
#define MS_PEPTIDEPROPHET_NAME "PeptideProphet"
/* def: "A program in the TPP that calculates PSM probabilities for MS/MS proteomics data searched with any of the supported sequence or spectral library search engines via the pepXML format." [PMID:12403597, PMID:23176103]
is_a: MS:1002286 ! Trans-Proteomic Pipeline software */

#define MS_IPROPHET_ID "MS:1002288"
#define MS_IPROPHET_NAME "iProphet"
/* def: "A program in the TPP that calculates distinct peptide probabilities based on several lines of corroborating evidence including search results from multiple search engines via the pepXML format." [PMID:21876204]
is_a: MS:1002286 ! Trans-Proteomic Pipeline software */

#define MS_PROTEINPROPHET_ID "MS:1002289"
#define MS_PROTEINPROPHET_NAME "ProteinProphet"
/* def: "A program in the TPP that calculates protein-level probabilities based on input PSM or peptide-level probabilities from PeptideProphet or iProphet. The output is written in the protXML format." [PMID:14632076]
is_a: MS:1002286 ! Trans-Proteomic Pipeline software */

#define MS_ASAPRATIO_ID "MS:1002289"
#define MS_ASAPRATIO_NAME "ASAPRatio"
/* def: "A program in the TPP that calculates PSM, peptide, and protein-level abundances based on 2-channel isotope-labeled data such as ICAT, SILAC, etc." [PSI:PI]
is_a: MS:1002286 ! Trans-Proteomic Pipeline software */

#define MS_XPRESS_ID "MS:1002290"
#define MS_XPRESS_NAME "XPRESS"
/* def: "A program in the TPP that calculates PSM-level abundances based on 2-channel isotope-labeled data such as ICAT, SILAC, etc." [PSI:PI]
is_a: MS:1002286 ! Trans-Proteomic Pipeline software */

#define MS_LIBRA_ID "MS:1002291"
#define MS_LIBRA_NAME "Libra"
/* def: "A program in the TPP that calculates PSM, peptide, and protein-level abundances based on N-channel isobaric label peptide data such as iTRAQ, TMT, etc." [PSI:PI]
is_a: MS:1002286 ! Trans-Proteomic Pipeline software */

#define MS_PTMPROPHET_ID "MS:1002292"
#define MS_PTMPROPHET_NAME "PTMProphet"
/* def: "A program in the TPP that calculates PTM localization probabilities by re-analyzing the peaks that are available to distinguish between possible modification sites." [PSI:PI]
is_a: MS:1002286 ! Trans-Proteomic Pipeline software */

#define MS_BRUKER_DALTONICS_SCION_SERIES_ID "MS:1002293"
#define MS_BRUKER_DALTONICS_SCION_SERIES_NAME "Bruker Daltonics SCION series"
/* def: "Bruker Daltonics' SCION series." [PSI:MS]
is_a: MS:1000122 ! Bruker Daltonics instrument model */

#define MS_BRUKER_DALTONICS_EVOQ_SERIES_ID "MS:1002294"
#define MS_BRUKER_DALTONICS_EVOQ_SERIES_NAME "Bruker Daltonics EVOQ series"
/* def: "Bruker Daltonics' EVOQ series." [PSI:MS]
is_a: MS:1000122 ! Bruker Daltonics instrument model */

#define MS_SCION_SQ_ID "MS:1002295"
#define MS_SCION_SQ_NAME "SCION SQ"
/* def: "Bruker Daltonics' SCION SQ: GC-single quadrupole." [PSI:MS]
is_a: MS:1002293 ! Bruker Daltonics SCION series */

#define MS_SCION_TQ_ID "MS:1002296"
#define MS_SCION_TQ_NAME "SCION TQ"
/* def: "Bruker Daltonics' SCION TQ: GC-triple quadrupole." [PSI:MS]
is_a: MS:1002293 ! Bruker Daltonics SCION series */

#define MS_EVOQ_ELITE_ID "MS:1002297"
#define MS_EVOQ_ELITE_NAME "EVOQ Elite"
/* def: "Bruker Daltonics' EVOQ Elite: LC-triple quadrupole." [PSI:MS]
is_a: MS:1002294 ! Bruker Daltonics EVOQ series */

#define MS_EVOQ_QUBE_ID "MS:1002298"
#define MS_EVOQ_QUBE_NAME "EVOQ Qube"
/* def: "Bruker Daltonics' EVOQ Qube: LC-triple quadrupole." [PSI:MS]
is_a: MS:1002294 ! Bruker Daltonics EVOQ series */

#define MS_MICROTOF_Q_III_ID "MS:1002299"
#define MS_MICROTOF_Q_III_NAME "micrOTOF-Q III"
/* def: "Bruker Daltonics' micrOTOF-Q III: ESI Q-TOF, Nanospray, APCI, APPI, GC-APCI, CaptiveSpray." [PSI:MS]
is_a: MS:1001536 ! Bruker Daltonics micrOTOF series */

#define MS_AMAZON_SPEED_ETD_ID "MS:1002300"
#define MS_AMAZON_SPEED_ETD_NAME "amaZon Speed ETD"
/* def: "Bruker Daltonics' amaZon Speed ETD: ESI quadrupole ion trap, Nanospray, APCI, APPI, ETD, PTR, GC-APCI, CaptiveSpray." [PSI:MS]
is_a: MS:1001545 ! Bruker Daltonics amaZon series */

#define MS_AMAZON_SPEED_ID "MS:1002301"
#define MS_AMAZON_SPEED_NAME "amaZon Speed"
/* def: "Bruker Daltonics' amaZon ETD: ESI quadrupole ion trap, Nanospray, APCI, APPI, GC-APCI, CaptiveSpray." [PSI:MS]
is_a: MS:1001545 ! Bruker Daltonics amaZon series */

#define MS_BRUKER_CONTAINER_FILE_ID "MS:1002302"
#define MS_BRUKER_CONTAINER_FILE_NAME "Bruker Container file"
/* def: "Bruker Container raw file format." [PSI:MS]
is_a: MS:1000560 ! mass spectrometer file format */

#define MS_BRUKER_CONTAINER_NATIVEID_FORMAT_ID "MS:1002303"
#define MS_BRUKER_CONTAINER_NATIVEID_FORMAT_NAME "Bruker Container nativeID format"
/* def: "Native identifier (UUID)." [PSI:MS]
xref: value-type:xsd\:string "The allowed value-type for this CV term."
is_a: MS:1000767 ! native spectrum identifier format */

#define MS_DOMAIN_RANGE_ID "MS:1002304"
#define MS_DOMAIN_RANGE_NAME "domain range"
/* def: "Domain range of a numerical value." [PSI:PI]
relationship: part_of MS:0000000 ! Proteomics Standards Initiative Mass Spectrometry Vocabularies */

#define MS_VALUE_BETWEEN_0_AND_1_INCLUSIVE_ID "MS:1002305"
#define MS_VALUE_BETWEEN_0_AND_1_INCLUSIVE_NAME "value between 0 and 1 inclusive"
/* def: "Value range for probabilities." [PSI:PI]
is_a: MS:1002304 ! domain range */

#define MS_VALUE_GREATER_THAN_ZERO_ID "MS:1002306"
#define MS_VALUE_GREATER_THAN_ZERO_NAME "value greater than zero"
/* def: "Positive value range." [PSI:PI]
is_a: MS:1002304 ! domain range */

#define MS_FRAGMENTATION_ION_TYPE_ID "MS:1002307"
#define MS_FRAGMENTATION_ION_TYPE_NAME "fragmentation ion type"
/* def: "Type of fragment ion based on where the backbone breaks, such as a y ion or a c ion." [PSI:PI]
is_a: MS:1001221 ! fragmentation information */

#define MS_FLUORESCENCE_DETECTOR_ID "MS:1002308"
#define MS_FLUORESCENCE_DETECTOR_NAME "fluorescence detector"
/* def: "A detector using a fluorescent signal after excitation with light." [PSI:MS]
is_a: MS:1000026 ! detector type */

#define MS_BYONIC__PEPTIDE_ABSLOGPROB_ID "MS:1002309"
#define MS_BYONIC__PEPTIDE_ABSLOGPROB_NAME "Byonic: Peptide AbsLogProb"
/* def: "The absolute value of the log-base10 of the Byonic posterior error probability (PEP) of the PSM." [PSI:PI]
xref: value-type:xsd\:double "The allowed value-type for this CV term."
is_a: MS:1001143 ! search engine specific score for PSMs
is_a: MS:1001153 ! search engine specific score
has_order: MS:1002108 ! higher score better */

#define MS_BYONIC__PROTEIN_ABSLOGPROB_ID "MS:1002310"
#define MS_BYONIC__PROTEIN_ABSLOGPROB_NAME "Byonic: Protein AbsLogProb"
/* def: "The absolute value of the log-base10 of the Byonic posterior error probability (PEP) of the protein." [PSI:PI]
xref: value-type:xsd\:double "The allowed value-type for this CV term."
is_a: MS:1001116 ! single protein result details
is_a: MS:1001153 ! search engine specific score
has_order: MS:1002108 ! higher score better */

#define MS_BYONIC__PEPTIDE_ABSLOGPROB2D_ID "MS:1002311"
#define MS_BYONIC__PEPTIDE_ABSLOGPROB2D_NAME "Byonic: Peptide AbsLogProb2D"
/* def: "The absolute value of the log-base10 Byonic two-dimensional posterior error probability (PEP) of the PSM. The two-dimensional PEP takes into account protein ranking information as well as PSM information." [PSI:PI]
xref: value-type:xsd\:double "The allowed value-type for this CV term."
is_a: MS:1001143 ! search engine specific score for PSMs
is_a: MS:1001153 ! search engine specific score
has_order: MS:1002108 ! higher score better */

#define MS_MS_NUMPRESS_LINEAR_PREDICTION_COMPRESSION_ID "MS:1002312"
#define MS_MS_NUMPRESS_LINEAR_PREDICTION_COMPRESSION_NAME "MS-Numpress linear prediction compression"
/* def: "Compression using MS-Numpress linear prediction compression." [https://github.com/fickludd/ms-numpress]
is_a: MS:1000572 ! binary data compression type */

#define MS_MS_NUMPRESS_POSITIVE_INTEGER_COMPRESSION_ID "MS:1002313"
#define MS_MS_NUMPRESS_POSITIVE_INTEGER_COMPRESSION_NAME "MS-Numpress positive integer compression"
/* def: "Compression using MS-Numpress positive integer compression." [https://github.com/fickludd/ms-numpress]
is_a: MS:1000572 ! binary data compression type */

#define MS_MS_NUMPRESS_SHORT_LOGGED_FLOAT_COMPRESSION_ID "MS:1002314"
#define MS_MS_NUMPRESS_SHORT_LOGGED_FLOAT_COMPRESSION_NAME "MS-Numpress short logged float compression"
/* def: "Compression using MS-Numpress short logged float compression." [https://github.com/fickludd/ms-numpress]
is_a: MS:1000572 ! binary data compression type */

#define MS_CONSENSUS_RESULT_ID "MS:1002315"
#define MS_CONSENSUS_RESULT_NAME "consensus result"
/* def: "Indicates a consensus result from several search engine runs." [PSI:PI]
is_a: MS:1001405 ! spectrum identification result details */

#define MS_PROTEOMEDISCOVERER_AMANDA_HIGH_CONFIDENCE_THRESHOLD_ID "MS:1002316"
#define MS_PROTEOMEDISCOVERER_AMANDA_HIGH_CONFIDENCE_THRESHOLD_NAME "ProteomeDiscoverer:Amanda:high confidence threshold"
/* def: "Strict confidence probability score." [PSI:PI]
xref: value-type:xsd\:int "The allowed value-type for this CV term."
is_a: MS:1002101 ! ProteomeDiscoverer input parameter */

#define MS_PROTEOMEDISCOVERER_AMANDA_MIDDLE_CONFIDENCE_THRESHOLD_ID "MS:1002317"
#define MS_PROTEOMEDISCOVERER_AMANDA_MIDDLE_CONFIDENCE_THRESHOLD_NAME "ProteomeDiscoverer:Amanda:middle confidence threshold"
/* def: "Relaxed confidence probability score." [PSI:PI]
xref: value-type:xsd\:int "The allowed value-type for this CV term."
is_a: MS:1002101 ! ProteomeDiscoverer input parameter */

#define MS_PROTEOMEDISCOVERER_AUTOMATIC_WORKLOAD_ID "MS:1002318"
#define MS_PROTEOMEDISCOVERER_AUTOMATIC_WORKLOAD_NAME "ProteomeDiscoverer:automatic workload"
/* def: "Flag indicating automatic estimation of the workload level." [PSI:PI]
xref: value-type:xsd\:boolean "The allowed value-type for this CV term."
is_a: MS:1002101 ! ProteomeDiscoverer input parameter */

#define MS_AMANDA_AMANDASCORE_ID "MS:1002319"
#define MS_AMANDA_AMANDASCORE_NAME "Amanda:AmandaScore"
/* def: "The Amanda score of the scoring function for a PSM." [PSI:PI]
xref: value-type:xsd\:double "The allowed value-type for this CV term."
is_a: MS:1001143 ! search engine specific score for PSMs
is_a: MS:1001153 ! search engine specific score */

#define MS_PROTEOMEDISCOVERER_MAX_DIFFERENTIAL_MODIFICATIONS_ID "MS:1002320"
#define MS_PROTEOMEDISCOVERER_MAX_DIFFERENTIAL_MODIFICATIONS_NAME "ProteomeDiscoverer:max differential modifications"
/* def: "Maximum dynamic modifications per PSM." [PSI:PI]
xref: value-type:xsd\:int "The allowed value-type for this CV term."
is_a: MS:1002101 ! ProteomeDiscoverer input parameter */

#define MS_PROTEOMEDISCOVERER_MAX_EQUAL_MODIFICATIONS_ID "MS:1002321"
#define MS_PROTEOMEDISCOVERER_MAX_EQUAL_MODIFICATIONS_NAME "ProteomeDiscoverer:max equal modifications"
/* def: "Maximum equal modifications per PSM." [PSI:PI]
xref: value-type:xsd\:int "The allowed value-type for this CV term."
is_a: MS:1002101 ! ProteomeDiscoverer input parameter */

#define MS_PROTEOMEDISCOVERER_MIN_PEPTIDE_LENGTH_ID "MS:1002322"
#define MS_PROTEOMEDISCOVERER_MIN_PEPTIDE_LENGTH_NAME "ProteomeDiscoverer:min peptide length"
/* def: "Minimum peptide length." [PSI:PI]
xref: value-type:xsd\:int "The allowed value-type for this CV term."
is_a: MS:1002101 ! ProteomeDiscoverer input parameter */

#define MS_PROTEOMEDISCOVERER_MAX_PEPTIDE_LENGTH_ID "MS:1002323"
#define MS_PROTEOMEDISCOVERER_MAX_PEPTIDE_LENGTH_NAME "ProteomeDiscoverer:max peptide length"
/* def: "Maximum peptide length." [PSI:PI]
xref: value-type:xsd\:int "The allowed value-type for this CV term."
is_a: MS:1002101 ! ProteomeDiscoverer input parameter */

#define MS_PROTEOMEDISCOVERER_MAX_NUMBER_NEUTRAL_LOSS_ID "MS:1002324"
#define MS_PROTEOMEDISCOVERER_MAX_NUMBER_NEUTRAL_LOSS_NAME "ProteomeDiscoverer:max number neutral loss"
/* def: "Maximum number of same neutral losses." [PSI:PI]
xref: value-type:xsd\:int "The allowed value-type for this CV term."
is_a: MS:1002101 ! ProteomeDiscoverer input parameter */

#define MS_PROTEOMEDISCOVERER_MAX_NUMBER_NEUTRAL_LOSS_MODIFICATIONS_ID "MS:1002325"
#define MS_PROTEOMEDISCOVERER_MAX_NUMBER_NEUTRAL_LOSS_MODIFICATIONS_NAME "ProteomeDiscoverer:max number neutral loss modifications"
/* def: "Max number of same neutral losses of modifications." [PSI:PI]
xref: value-type:xsd\:int "The allowed value-type for this CV term."
is_a: MS:1002101 ! ProteomeDiscoverer input parameter */

#define MS_PROTEOMEDISCOVERER_USE_FLANKING_IONS_ID "MS:1002326"
#define MS_PROTEOMEDISCOVERER_USE_FLANKING_IONS_NAME "ProteomeDiscoverer:use flanking ions"
/* def: "Flag for usage of flanking ions." [PSI:PI]
xref: value-type:xsd\:boolean "The allowed value-type for this CV term."
is_a: MS:1002101 ! ProteomeDiscoverer input parameter */

#define MS_PROTEOMEDISCOVERER_MAX_NUMBER_OF_SAME_MODIFS_ID "MS:1002327"
#define MS_PROTEOMEDISCOVERER_MAX_NUMBER_OF_SAME_MODIFS_NAME "ProteomeDiscoverer:max number of same modifs"
/* def: "The maximum number of possible equal modifications per PSM." [PSI:PI]
xref: value-type:xsd\:int "The allowed value-type for this CV term."
is_a: MS:1002101 ! ProteomeDiscoverer input parameter */

#define MS_PROTEOMEDISCOVERER_PERFORM_DEISOTOPING_ID "MS:1002328"
#define MS_PROTEOMEDISCOVERER_PERFORM_DEISOTOPING_NAME "ProteomeDiscoverer:perform deisotoping"
/* def: "defines whether a simple deisotoping shall be performed." [PSI:PI]
xref: value-type:xsd\:boolean "The allowed value-type for this CV term."
is_a: MS:1002101 ! ProteomeDiscoverer input parameter */

#define MS_PROTEOMEDISCOVERER_ION_SETTINGS_ID "MS:1002329"
#define MS_PROTEOMEDISCOVERER_ION_SETTINGS_NAME "ProteomeDiscoverer:ion settings"
/* def: "Specifies the fragment ions and neutral losses that are calculated." [PSI:PI]
xref: value-type:xsd\:string "The allowed value-type for this CV term."
is_a: MS:1002101 ! ProteomeDiscoverer input parameter */

#define MS_PROTEOMEDISCOVERER_3__STATIC_MODIFICATION_ID "MS:1002330"
#define MS_PROTEOMEDISCOVERER_3__STATIC_MODIFICATION_NAME "ProteomeDiscoverer:3. Static Modification"
/* def: "Determine 3rd static (fixed) post-translational modifications (PTMs)." [PSI:PI]
xref: value-type:xsd\:string "The allowed value-type for this CV term."
is_a: MS:1002101 ! ProteomeDiscoverer input parameter */

#define MS_PROTEOMEDISCOVERER_5__DYNAMIC_MODIFICATION_ID "MS:1002331"
#define MS_PROTEOMEDISCOVERER_5__DYNAMIC_MODIFICATION_NAME "ProteomeDiscoverer:5. Dynamic Modification"
/* def: "Determine 5th dynamic (variable) post-translational modifications (PTMs)." [PSI:PI]
xref: value-type:xsd\:string "The allowed value-type for this CV term."
is_a: MS:1002101 ! ProteomeDiscoverer input parameter */

#define MS_LAB_HEAD_ID "MS:1002332"
#define MS_LAB_HEAD_NAME "lab head"
/* def: "The scientist responsible for personnel, grants, and instrumentation in a functional laboratory group." [PSI:PI]
is_a: MS:1002033 ! contact role */

#define MS_CONVERSION_SOFTWARE_ID "MS:1002333"
#define MS_CONVERSION_SOFTWARE_NAME "conversion software"
/* def: "Computer software primarily designed to convert data represented in one format to another format, sometimes with minor data alterations in the process." [PSI:PI]
is_a: MS:1001457 ! data processing software */

#define MS_PROCON_ID "MS:1002334"
#define MS_PROCON_NAME "ProCon"
/* def: "Java software designed to convert one of several proteomics identification results formats into mzIdentML or PRIDE XML." [PSI:PI, http://www.medizinisches-proteom-center.de/procon]
is_a: MS:1002333 ! conversion software
is_a: MS:1001457 ! data processing software */

#define MS_PRIDE_CONVERTER2_ID "MS:1002335"
#define MS_PRIDE_CONVERTER2_NAME "PRIDE Converter2"
/* def: "Java software designed to convert one of several proteomics identification results formats into PRIDE XML." [PMID:22949509]
is_a: MS:1002333 ! conversion software
is_a: MS:1001457 ! data processing software */

#define MS_AMANDA_ID "MS:1002336"
#define MS_AMANDA_NAME "Amanda"
/* def: "Amanda scoring system for PSM identification." [PSI:PI]
is_a: MS:1001456 ! analysis software */

#define MS_ANDROMEDA_ID "MS:1002337"
#define MS_ANDROMEDA_NAME "Andromeda"
/* def: "Andromeda is a peptide search engine." [PSI:PI]
is_a: MS:1001456 ! analysis software */

#define MS_ANDROMEDA_SCORE_ID "MS:1002338"
#define MS_ANDROMEDA_SCORE_NAME "Andromeda:score"
/* def: "The probability based score of the Andromeda search engine." [PSI:PI]
xref: value-type:xsd\:double "The allowed value-type for this CV term."
is_a: MS:1001143 ! search engine specific score for PSMs
is_a: MS:1001153 ! search engine specific score
has_order: MS:1002108 ! higher score better */

#define MS_SITE_GLOBAL_FDR_ID "MS:1002339"
#define MS_SITE_GLOBAL_FDR_NAME "site:global FDR"
/* def: "Estimation of global false discovery rate of peptides with a posttranslational modification." [PSI:PI]
xref: value-type:xsd\:double "The allowed value-type for this CV term."
is_a: MS:1001405 ! spectrum identification result details */

#define MS_PROTEOMEXCHANGE_PROJECT_TAG_ID "MS:1002340"
#define MS_PROTEOMEXCHANGE_PROJECT_TAG_NAME "ProteomeXchange project tag"
/* def: "Tag that can be added to a ProteomeXchange dataset, to enable the grouping of datasets. One tag can be used for indicating that a given dataset is part of a bigger project, like e.g. the Human Proteome Project." [PSI:PI]
xref: value-type:xsd\:string "The allowed value-type for this CV term."
is_a: MS:1000878 ! external reference identifier */

#define MS_SECOND_PASS_PEPTIDE_IDENTIFICATION_ID "MS:1002341"
#define MS_SECOND_PASS_PEPTIDE_IDENTIFICATION_NAME "second-pass peptide identification"
/* def: "A putative identified peptide found in a second-pass search of protein sequences selected from a first-pass search." [PSI:PI]
xref: value-type:xsd\:boolean "The allowed value-type for this CV term."
is_a: MS:1001105 ! peptide result details */

#define MS_MZMINE_ID "MS:1002342"
#define MS_MZMINE_NAME "MZmine"
/* def: "A framework for differential analysis of mass spectrometry data." [PMID:16403790, PMID:20650010]
is_a: MS:1001456 ! analysis software
is_a: MS:1001457 ! data processing software */

#define MS_ION_STABILITY_TYPE_ID "MS:1002343"
#define MS_ION_STABILITY_TYPE_NAME "ion stability type"
/* def: "Stability type of the ion." [PSI:PI]
is_a: MS:1000365 ! ion? */

#define MS_MALTCMS_ID "MS:1002344"
#define MS_MALTCMS_NAME "Maltcms"
/* def: "Modular Application Toolkit for Chromatography Mass-Spectrometry is an application framework mainly for developers." [PSI:PI, http://maltcms.sf.net]
is_a: MS:1001456 ! analysis software
is_a: MS:1001457 ! data processing software */

#define MS_PSM_LEVEL_RESULT_DETAILS_ID "MS:1002345"
#define MS_PSM_LEVEL_RESULT_DETAILS_NAME "PSM-level result details"
/* def: "Peptide spectrum match level information." [PSI:PI]
is_a: MS:1001405 ! spectrum identification result details */

#define MS_PROTEIN_GROUP_LEVEL_RESULT_DETAILS_ID "MS:1002346"
#define MS_PROTEIN_GROUP_LEVEL_RESULT_DETAILS_NAME "protein group-level result details"
/* def: "Protein group level information." [PSI:PI]
is_a: MS:1001405 ! spectrum identification result details */

#define MS_PSM_LEVEL_IDENTIFICATION_CONFIDENCE_METRIC_ID "MS:1002347"
#define MS_PSM_LEVEL_IDENTIFICATION_CONFIDENCE_METRIC_NAME "PSM-level identification confidence metric"
/* def: "Identification confidence metric for a peptide spectrum match." [PSI:PI]
is_a: MS:1002345 ! PSM-level result details */

#define MS_PROTEIN_GROUP_LEVEL_IDENTIFICATION_CONFIDENCE_METRIC_ID "MS:1002348"
#define MS_PROTEIN_GROUP_LEVEL_IDENTIFICATION_CONFIDENCE_METRIC_NAME "protein group-level identification confidence metric"
/* def: "Identification confidence metric for a protein group." [PSI:PI]
is_a: MS:1002346 ! protein group-level result details */

#define MS_VALUE_GREATER_THAN_ZERO_BUT_LESS_THAN_OR_EQUAL_TO_ONE_ID "MS:1002349"
#define MS_VALUE_GREATER_THAN_ZERO_BUT_LESS_THAN_OR_EQUAL_TO_ONE_NAME "value greater than zero but less than or equal to one"
/* def: "Positive value range less than or equal to 1." [PSI:PI]
is_a: MS:1002304 ! domain range */

#define MS_PSM_LEVEL_GLOBAL_FDR_ID "MS:1002350"
#define MS_PSM_LEVEL_GLOBAL_FDR_NAME "PSM-level global FDR"
/* def: "Estimation of the global false discovery rate of peptide spectrum matches." [PSI:PI]
xref: value-type:xsd\:double "The allowed value-type for this CV term."
is_a: MS:1002345 ! PSM-level result details
has_domain: MS:1002305 ! value between 0 and 1 inclusive */

#define MS_PSM_LEVEL_LOCAL_FDR_ID "MS:1002351"
#define MS_PSM_LEVEL_LOCAL_FDR_NAME "PSM-level local FDR"
/* def: "Estimation of the local false discovery rate of peptide spectrum matches." [PSI:PI]
xref: value-type:xsd\:double "The allowed value-type for this CV term."
is_a: MS:1002345 ! PSM-level result details
has_domain: MS:1002305 ! value between 0 and 1 inclusive */

#define MS_PSM_LEVEL_P_VALUE_ID "MS:1002352"
#define MS_PSM_LEVEL_P_VALUE_NAME "PSM-level p-value"
/* def: "Estimation of the p-value for peptide spectrum matches." [PSI:PI]
xref: value-type:xsd\:double "The allowed value-type for this CV term."
is_a: MS:1002347 ! PSM-level identification confidence metric
has_domain: MS:1002305 ! value between 0 and 1 inclusive */

#define MS_PSM_LEVEL_E_VALUE_ID "MS:1002353"
#define MS_PSM_LEVEL_E_VALUE_NAME "PSM-level e-value"
/* def: "Estimation of the e-value for peptide spectrum matches." [PSI:PI]
xref: value-type:xsd\:double "The allowed value-type for this CV term."
is_a: MS:1002347 ! PSM-level identification confidence metric
has_domain: MS:1002306 ! value greater than zero */

#define MS_PSM_LEVEL_Q_VALUE_ID "MS:1002354"
#define MS_PSM_LEVEL_Q_VALUE_NAME "PSM-level q-value"
/* def: "Estimation of the q-value for peptide spectrum matches." [PSI:PI]
xref: value-type:xsd\:double "The allowed value-type for this CV term."
is_a: MS:1002347 ! PSM-level identification confidence metric
relationship: has_units UO:0000166 ! parts per notation unit
relationship: has_units UO:0000187 ! percent
has_domain: MS:1002305 ! value between 0 and 1 inclusive */

#define MS_PSM_LEVEL_FDRSCORE_ID "MS:1002355"
#define MS_PSM_LEVEL_FDRSCORE_NAME "PSM-level FDRScore"
/* def: "FDRScore for peptide spectrum matches." [PSI:PI]
xref: value-type:xsd\:double "The allowed value-type for this CV term."
is_a: MS:1001143 ! search engine specific score for PSMs
is_a: MS:1001153 ! search engine specific score
has_domain: MS:1002349 ! value greater than zero but less than or equal to 1 */

#define MS_PSM_LEVEL_COMBINED_FDRSCORE_ID "MS:1002356"
#define MS_PSM_LEVEL_COMBINED_FDRSCORE_NAME "PSM-level combined FDRScore"
/* def: "Combined FDRScore for peptide spectrum matches specifically obtained for distinct combinations of single, pairs or triplets of search engines making a given PSM, used for integrating results from these distinct pools." [PMID:19253293]
xref: value-type:xsd\:double "The allowed value-type for this CV term."
is_a: MS:1001143 ! search engine specific score for PSMs
is_a: MS:1001153 ! search engine specific score
has_domain: MS:1002349 ! value greater than zero but less than or equal to 1 */

#define MS_PSM_LEVEL_PROBABILITY_ID "MS:1002357"
#define MS_PSM_LEVEL_PROBABILITY_NAME "PSM-level probability"
/* def: "Probability that the reported peptide ion is truly responsible for some or all of the components of the specified mass spectrum." [PSI:PI]
xref: value-type:xsd\:double "The allowed value-type for this CV term."
is_a: MS:1002347 ! PSM-level identification confidence metric
has_domain: MS:1002305 ! value between 0 and 1 inclusive */

#define MS_SEARCH_ENGINE_SPECIFIC_SCORE_FOR_DISTINCT_PEPTIDES_ID "MS:1002358"
#define MS_SEARCH_ENGINE_SPECIFIC_SCORE_FOR_DISTINCT_PEPTIDES_NAME "search engine specific score for distinct peptides"
/* def: "Search engine specific distinct peptide score." [PSI:PI]
is_a: MS:1001105 ! peptide result details */

#define MS_DISTINCT_PEPTIDE_LEVEL_LOCAL_FDR_ID "MS:1002359"
#define MS_DISTINCT_PEPTIDE_LEVEL_LOCAL_FDR_NAME "distinct peptide-level local FDR"
/* def: "Estimation of the local false discovery rate for distinct peptides once redundant identifications of the same peptide have been removed (id est multiple PSMs have been collapsed to one entry)." [PSI:PI]
xref: value-type:xsd\:double "The allowed value-type for this CV term."
is_a: MS:1001105 ! peptide result details
has_domain: MS:1002305 ! value between 0 and 1 inclusive */

#define MS_DISTINCT_PEPTIDE_LEVEL_FDRSCORE_ID "MS:1002360"
#define MS_DISTINCT_PEPTIDE_LEVEL_FDRSCORE_NAME "distinct peptide-level FDRScore"
/* def: "FDRScore for distinct peptides once redundant identifications of the same peptide have been removed (id est multiple PSMs have been collapsed to one entry)." [PSI:PI]
xref: value-type:xsd\:double "The allowed value-type for this CV term."
is_a: MS:1002358 ! search engine specific score for distinct peptides
is_a: MS:1001153 ! search engine specific score
has_domain: MS:1002349 ! value greater than zero but less than or equal to one */

#define MS_DISTINCT_PEPTIDE_LEVEL_COMBINED_FDRSCORE_ID "MS:1002361"
#define MS_DISTINCT_PEPTIDE_LEVEL_COMBINED_FDRSCORE_NAME "distinct peptide-level combined FDRScore"
/* def: "Combined FDRScore for peptides once redundant identifications of the same peptide have been removed (id est multiple PSMs have been collapsed to one entry) specifically obtained for distinct combinations of single, pairs or triplets of search engines making a given peptide, used for integrating results from these distinct pools." [PMID:19253293]
xref: value-type:xsd\:double "The allowed value-type for this CV term."
is_a: MS:1002358 ! search engine specific score for distinct peptides
is_a: MS:1001153 ! search engine specific score
has_domain: MS:1002349 ! value greater than zero but less than or equal to one */

#define MS_DISTINCT_PEPTIDE_LEVEL_PROBABILITY_ID "MS:1002362"
#define MS_DISTINCT_PEPTIDE_LEVEL_PROBABILITY_NAME "distinct peptide-level probability"
/* def: "Probability that the reported distinct peptide sequence (irrespective of mass modifications) has been correctly identified via the referenced PSMs." [PSI:PI]
xref: value-type:xsd\:double "The allowed value-type for this CV term."
is_a: MS:1001192 ! peptide identification confidence metric
has_domain: MS:1002305 ! value between 0 and 1 inclusive */

#define MS_SEARCH_ENGINE_SPECIFIC_SCORE_FOR_PROTEINS_ID "MS:1002363"
#define MS_SEARCH_ENGINE_SPECIFIC_SCORE_FOR_PROTEINS_NAME "search engine specific score for proteins"
/* def: "Search engine specific protein scores." [PSI:PI]
is_a: MS:1001116 ! single protein result details */

#define MS_PROTEIN_LEVEL_LOCAL_FDR_ID "MS:1002364"
#define MS_PROTEIN_LEVEL_LOCAL_FDR_NAME "protein-level local FDR"
/* def: "Estimation of the local false discovery rate of proteins." [PSI:PI]
xref: value-type:xsd\:double "The allowed value-type for this CV term."
is_a: MS:1001116 ! single protein result details */

#define MS_FDRSCORE_FOR_PROTEINS_ID "MS:1002365"
#define MS_FDRSCORE_FOR_PROTEINS_NAME "FDRScore for proteins"
/* def: "FDRScore for proteins specifically obtained for distinct combinations of single, pairs or triplets of search engines making a given PSM, used for integrating results from these distinct pools." [PMID:19253293]
xref: value-type:xsd\:double "The allowed value-type for this CV term."
is_a: MS:1002363 ! search engine specific score for proteins
is_a: MS:1001153 ! search engine specific score
has_domain: MS:1002349 ! value greater than zero but less than or equal to one */

#define MS_COMBINED_FDRSCORE_FOR_PROTEINS_ID "MS:1002366"
#define MS_COMBINED_FDRSCORE_FOR_PROTEINS_NAME "combined FDRScore for proteins"
/* def: "Combined FDRScore for proteins." [PSI:PI]
xref: value-type:xsd\:double "The allowed value-type for this CV term."
is_a: MS:1002363 ! search engine specific score for proteins
is_a: MS:1001153 ! search engine specific score
has_domain: MS:1002349 ! value greater than zero but less than or equal to one */

#define MS_PROBABILITY_FOR_PROTEINS_ID "MS:1002367"
#define MS_PROBABILITY_FOR_PROTEINS_NAME "probability for proteins"
/* def: "Probability that a specific protein sequence has been correctly identified from the PSM and distinct peptide evidence, and based on the available protein sequences presented to the analysis software." [PSI:PI]
xref: value-type:xsd\:double "The allowed value-type for this CV term."
is_a: MS:1001192 ! protein identification confidence metric
has_domain: MS:1002305 ! value between 0 and 1 inclusive */

#define MS_SEARCH_ENGINE_SPECIFIC_SCORE_FOR_PROTEIN_GROUPS_ID "MS:1002368"
#define MS_SEARCH_ENGINE_SPECIFIC_SCORE_FOR_PROTEIN_GROUPS_NAME "search engine specific score for protein groups"
/* def: "Search engine specific protein group scores." [PSI:PI]
is_a: MS:1002346 ! protein group-level result details */

#define MS_PROTEIN_GROUP_LEVEL_GLOBAL_FDR_ID "MS:1002369"
#define MS_PROTEIN_GROUP_LEVEL_GLOBAL_FDR_NAME "protein group-level global FDR"
/* def: "Estimation of the global false discovery rate of protein groups." [PSI:PI]
xref: value-type:xsd\:double "The allowed value-type for this CV term."
is_a: MS:1002346 ! protein group-level result details */

#define MS_PROTEIN_GROUP_LEVEL_LOCAL_FDR_ID "MS:1002370"
#define MS_PROTEIN_GROUP_LEVEL_LOCAL_FDR_NAME "protein group-level local FDR"
/* def: "Estimation of the local false discovery rate of protein groups." [PSI:PI]
xref: value-type:xsd\:double "The allowed value-type for this CV term."
is_a: MS:1002346 ! protein group-level result details */

#define MS_PROTEIN_GROUP_LEVEL_P_VALUE_ID "MS:1002371"
#define MS_PROTEIN_GROUP_LEVEL_P_VALUE_NAME "protein group-level p-value"
/* def: "Estimation of the p-value for protein groups." [PSI:PI]
xref: value-type:xsd\:double "The allowed value-type for this CV term."
is_a: MS:1002348 ! protein group-level identification confidence metric
has_domain: MS:1002305 ! value between 0 and 1 inclusive */

#define MS_PROTEIN_GROUP_LEVEL_E_VALUE_ID "MS:1002372"
#define MS_PROTEIN_GROUP_LEVEL_E_VALUE_NAME "protein group-level e-value"
/* def: "Estimation of the e-value for protein groups." [PSI:PI]
xref: value-type:xsd\:double "The allowed value-type for this CV term."
is_a: MS:1002348 ! protein group-level identification confidence metric
has_domain: MS:1002306 ! value greater than zero */

#define MS_PROTEIN_GROUP_LEVEL_Q_VALUE_ID "MS:1002373"
#define MS_PROTEIN_GROUP_LEVEL_Q_VALUE_NAME "protein group-level q-value"
/* def: "Estimation of the q-value for protein groups." [PSI:PI]
xref: value-type:xsd\:double "The allowed value-type for this CV term."
is_a: MS:1002348 ! protein group-level identification confidence metric
relationship: has_units UO:0000166 ! parts per notation unit
relationship: has_units UO:0000187 ! percent
has_domain: MS:1002305 ! value between 0 and 1 inclusive */

#define MS_PROTEIN_GROUP_LEVEL_FDRSCORE_ID "MS:1002374"
#define MS_PROTEIN_GROUP_LEVEL_FDRSCORE_NAME "protein group-level FDRScore"
/* def: "FDRScore for protein groups." [PSI:PI]
xref: value-type:xsd\:double "The allowed value-type for this CV term."
is_a: MS:1002368 ! search engine specific score for protein groups
is_a: MS:1001153 ! search engine specific score
has_domain: MS:1002349 ! value greater than zero but less than or equal to one */

#define MS_PROTEIN_GROUP_LEVEL_COMBINED_FDRSCORE_ID "MS:1002375"
#define MS_PROTEIN_GROUP_LEVEL_COMBINED_FDRSCORE_NAME "protein group-level combined FDRScore"
/* def: "Combined FDRScore for proteins specifically obtained for distinct combinations of single, pairs or triplets of search engines making a given PSM, used for integrating results from these distinct pools." [PMID:19253293]
xref: value-type:xsd\:double "The allowed value-type for this CV term."
is_a: MS:1002368 ! search engine specific score for protein groups
is_a: MS:1001153 ! search engine specific score
has_domain: MS:1002349 ! value greater than zero but less than or equal to one */

#define MS_PROTEIN_GROUP_LEVEL_PROBABILITY_ID "MS:1002376"
#define MS_PROTEIN_GROUP_LEVEL_PROBABILITY_NAME "protein group-level probability"
/* def: "Probability that at least one of the members of a group of protein sequences has been correctly identified from the PSM and distinct peptide evidence, and based on the available protein sequences presented to the analysis software." [PSI:PI]
xref: value-type:xsd\:double "The allowed value-type for this CV term."
is_a: MS:1002368 ! search engine specific score for protein groups
has_domain: MS:1002305 ! value between 0 and 1 inclusive */

#define MS_PROTEOMEDISCOVERER_RELAXED_SCORE_THRESHOLD_ID "MS:1002377"
#define MS_PROTEOMEDISCOVERER_RELAXED_SCORE_THRESHOLD_NAME "ProteomeDiscoverer:Relaxed Score Threshold"
/* def: "Specifies the threshold value for relaxed scoring." [PSI:PI]
xref: value-type:xsd\:float "The allowed value-type for this CV term."
is_a: MS:1002101 ! ProteomeDiscoverer input parameter */

#define MS_PROTEOMEDISCOVERER_STRICT_SCORE_THRESHOLD_ID "MS:1002378"
#define MS_PROTEOMEDISCOVERER_STRICT_SCORE_THRESHOLD_NAME "ProteomeDiscoverer:Strict Score Threshold"
/* def: "Specifies the threshold value for strict scoring." [PSI:PI]
xref: value-type:xsd\:float "The allowed value-type for this CV term."
is_a: MS:1002101 ! ProteomeDiscoverer input parameter */

#define MS_PROTEOMEDISCOVERER_PEPTIDE_WITHOUT_PROTEIN_CUT_OFF_SCORE_ID "MS:1002379"
#define MS_PROTEOMEDISCOVERER_PEPTIDE_WITHOUT_PROTEIN_CUT_OFF_SCORE_NAME "ProteomeDiscoverer:Peptide Without Protein Cut Off Score"
/* def: "Cut off score for storing peptides that do not belong to a protein." [PSI:PI]
xref: value-type:xsd\:float "The allowed value-type for this CV term."
is_a: MS:1002101 ! ProteomeDiscoverer input parameter */

#define MS_FALSE_LOCALIZATION_RATE_ID "MS:1002380"
#define MS_FALSE_LOCALIZATION_RATE_NAME "false localization rate"
/* def: "Estimation of the false localization rate for modification site assignment." [PSI:PI]
xref: value-type:xsd\:double "The allowed value-type for this CV term."
is_a: MS:1001405 ! spectrum identification result details */

#define MS_MALDI_SOLUTIONS_LC_MALDI_ID "MS:1002381"
#define MS_MALDI_SOLUTIONS_LC_MALDI_NAME "MALDI Solutions LC-MALDI"
/* def: "Software for automated LC-MALDI analysis and reporting." [PSI:PI]
is_a: MS:1001455 ! acquisition software
is_a: MS:1001456 ! analysis software
is_a: MS:1001457 ! data processing software
is_a: MS:1001557 ! Shimadzu Corporation software */

#define MS_SHIMADZU_MALDI_7090_ID "MS:1002382"
#define MS_SHIMADZU_MALDI_7090_NAME "Shimadzu MALDI-7090"
/* def: "Shimadzu MALDI-7090: MALDI-TOF-TOF." [PSI:PI]
is_a: MS:1000602 ! Shimadzu MALDI-TOF instrument model */
