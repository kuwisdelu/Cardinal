
#define UO_UNIT_ID "UO:0000000"
#define UO_UNIT_NAME "unit"
/* def: "A unit of measurement is a standardized quantity of a physical quality." [Wikipedia:Wikipedia "http://www.wikipedia.org/"]		
created_by: george gkoutos */

#define UO_LENGTH_UNIT_ID "UO:0000001"
#define UO_LENGTH_UNIT_NAME "length unit"
/* def: "A unit which is a standard measure of the distance between two points." [Wikipedia:Wikipedia "http://www.wikipedia.org/"]
subset: unit_group_slim
is_a: UO:0000000 ! unit
relationship: unit_of PATO:0001708
created_by: george gkoutos */

#define UO_MASS_UNIT_ID "UO:0000002"
#define UO_MASS_UNIT_NAME "mass unit"
/* def: "A unit which is a standard measure of the amount of matter/energy of a physical object." [Wikipedia:Wikipedia "http://www.wikipedia.org/"]
subset: unit_group_slim
is_a: UO:0000000 ! unit
relationship: unit_of PATO:0000125
relationship: unit_of PATO:0000128
created_by: george gkoutos */

#define UO_TIME_UNIT_ID "UO:0000003"
#define UO_TIME_UNIT_NAME "time unit"
/* alt_id: UO:0000149
def: "A unit which is a standard measure of the dimension in which events occur in sequence." [Wikipedia:Wikipedia "http://www.wikipedia.org/"]
subset: unit_group_slim
synonym: "time derived unit" EXACT []
is_a: UO:0000000 ! unit
relationship: unit_of PATO:0000165
relationship: unit_of PATO:0001309
created_by: george gkoutos */

#define UO_ELECTRIC_CURRENT_UNIT_ID "UO:0000004"
#define UO_ELECTRIC_CURRENT_UNIT_NAME "electric current unit"
/* def: "A unit which is a standard measure of the flow of electric charge." [Wikipedia:Wikipedia "http://www.wikipedia.org/"]
subset: unit_group_slim
is_a: UO:0000000 ! unit
created_by: george gkoutos */

#define UO_TEMPERATURE_UNIT_ID "UO:0000005"
#define UO_TEMPERATURE_UNIT_NAME "temperature unit"
/* alt_id: UO:0000126
def: "A unit which is a standard measure of the average kinetic energy of the particles in a sample of matter." [Wikipedia:Wikipedia "http://www.wikipedia.org/"]
subset: unit_group_slim
synonym: "temperature derived unit" EXACT []
is_a: UO:0000000 ! unit
relationship: unit_of PATO:0000146
created_by: george gkoutos */

#define UO_SUBSTANCE_UNIT_ID "UO:0000006"
#define UO_SUBSTANCE_UNIT_NAME "substance unit"
/* def: "A unit which is a standardised quantity of an element or compound with uniform composition." [Wikipedia:Wikipedia "http://www.wikipedia.org/"]
subset: unit_group_slim
is_a: UO:0000000 ! unit
created_by: george gkoutos */

#define UO_LUMINOUS_INTENSITY_UNIT_ID "UO:0000007"
#define UO_LUMINOUS_INTENSITY_UNIT_NAME "luminous intensity unit"
/* def: "A unit which is a standard measure of the wavelength-weighted power emitted by a light source in a particular direction." [Wikipedia:Wikipedia "http://www.wikipedia.org/"]
subset: unit_group_slim
is_a: UO:0000157 ! light unit
created_by: george gkoutos */

#define UO_METER_ID "UO:0000008"
#define UO_METER_NAME "meter"
/* def: "A length unit which is equal to the length of the path traveled by light in vacuum during a time interval of 1/299 792 458 of a second." [NIST:NIST "http://physics.nist.gov/cuu/Units/"]
subset: unit_slim
synonym: "m" EXACT []
is_a: UO:0000001 ! length unit
is_a: UO:0000045 ! base unit
created_by: george gkoutos */

#define UO_KILOGRAM_ID "UO:0000009"
#define UO_KILOGRAM_NAME "kilogram"
/* def: "A mass unit which is equal to the mass of the International Prototype Kilogram kept by the BIPM at Svres, France." [NIST:NIST "http://physics.nist.gov/cuu/Units/"]
subset: unit_slim
synonym: "kg" EXACT []
is_a: UO:0000002 ! mass unit
is_a: UO:0000045 ! base unit
created_by: george gkoutos */

#define UO_SECOND_ID "UO:0000010"
#define UO_SECOND_NAME "second"
/* def: "A time unit which is equal to the duration of 9 192 631 770 periods of the radiation corresponding to the transition between the two hyperfine levels of the ground state of the caesium 133 atom." [NIST:NIST "http://physics.nist.gov/cuu/Units/"]
subset: unit_slim
synonym: "s" EXACT []
is_a: UO:0000003 ! time unit
is_a: UO:0000045 ! base unit
created_by: george gkoutos */

#define UO_AMPERE_ID "UO:0000011"
#define UO_AMPERE_NAME "ampere"
/* def: "An electric current unit which is equal to the constant current which, if maintained in two straight parallel conductors of infinite length, of negligible circular cross-section, and placed 1 m apart in vacuum, would produce between these conductors a force equal to 2 x 10^[-7] newton per meter of length." [NIST:NIST "http://physics.nist.gov/cuu/Units/"]
subset: unit_slim
synonym: "A" EXACT []
is_a: UO:0000004 ! electric current unit
is_a: UO:0000045 ! base unit
created_by: george gkoutos */

#define UO_KELVIN_ID "UO:0000012"
#define UO_KELVIN_NAME "kelvin"
/* def: "A thermodynamic temperature unit which is equal to the fraction 1/273.16 of the thermodynamic temperature of the triple point of water." [NIST:NIST "http://physics.nist.gov/cuu/Units/"]
subset: unit_slim
synonym: "K" EXACT []
is_a: UO:0000005 ! temperature unit
is_a: UO:0000045 ! base unit
created_by: george gkoutos */

#define UO_MOLE_ID "UO:0000013"
#define UO_MOLE_NAME "mole"
/* def: "A substance unit which is equal to the amount of substance of a molecular system which contains as many elementary entities as there are atoms in 0.012 kilogram of carbon 12." [NIST:NIST "http://physics.nist.gov/cuu/Units/"]
subset: unit_slim
synonym: "mol" EXACT []
is_a: UO:0000006 ! substance unit
is_a: UO:0000045 ! base unit
created_by: george gkoutos */

#define UO_CANDELA_ID "UO:0000014"
#define UO_CANDELA_NAME "candela"
/* def: "A luminous intensity unit which equal to the luminous intensity, in a given direction, of a source that emits monochromatic radiation of frequency 540 x 1012 hertz and that has a radiant intensity in that direction of 1/683 watt per steradian." [NIST:NIST "http://physics.nist.gov/cuu/Units/"]
subset: unit_slim
synonym: "cd" EXACT []
is_a: UO:0000007 ! luminous intensity unit
is_a: UO:0000045 ! base unit
created_by: george gkoutos */

#define UO_CENTIMETER_ID "UO:0000015"
#define UO_CENTIMETER_NAME "centimeter"
/* def: "A length unit which is equal to one hundredth of a meter or 10^[-2] m." [NIST:NIST "http://physics.nist.gov/cuu/Units/"]
subset: unit_slim
synonym: "cm" EXACT []
is_a: UO:0000001 ! length unit
created_by: george gkoutos */

#define UO_MILLIMETER_ID "UO:0000016"
#define UO_MILLIMETER_NAME "millimeter"
/* def: "A length unit which is equal to one thousandth of a meter or 10^[-3] m." [NIST:NIST "http://physics.nist.gov/cuu/Units/"]
subset: unit_slim
synonym: "mm" EXACT []
is_a: UO:0000001 ! length unit
created_by: george gkoutos */

#define UO_MICROMETER_ID "UO:0000017"
#define UO_MICROMETER_NAME "micrometer"
/* def: "A length unit which is equal to one millionth of a meter or 10^[-6] m." [NIST:NIST "http://physics.nist.gov/cuu/Units/"]
subset: unit_slim
synonym: "micron" EXACT []
synonym: "um" EXACT []
is_a: UO:0000001 ! length unit
created_by: george gkoutos */

#define UO_NANOMETER_ID "UO:0000018"
#define UO_NANOMETER_NAME "nanometer"
/* def: "A length unit which is equal to one thousandth of one millionth of a meter or 10^[-9] m." [NIST:NIST "http://physics.nist.gov/cuu/Units/"]
subset: unit_slim
synonym: "nm" EXACT []
is_a: UO:0000001 ! length unit
created_by: george gkoutos */

#define UO_ANGSTROM_ID "UO:0000019"
#define UO_ANGSTROM_NAME "angstrom"
/* def: "A length unit which is equal to 10 [-10] m." [NIST:NIST "http://physics.nist.gov/cuu/Units/"]
subset: unit_slim
synonym: "A" EXACT []
is_a: UO:0000001 ! length unit
created_by: george gkoutos */

#define UO_PICOMETER_ID "UO:0000020"
#define UO_PICOMETER_NAME "picometer"
/* def: "A length unit which is equal to 10^[-12] m." [NIST:NIST "http://physics.nist.gov/cuu/Units/"]
subset: unit_slim
synonym: "pm" EXACT []
is_a: UO:0000001 ! length unit
created_by: george gkoutos */

#define UO_GRAM_ID "UO:0000021"
#define UO_GRAM_NAME "gram"
/* def: "A mass unit which is equal to one thousandth of a kilogram or 10^[-3] kg." [NIST:NIST "http://physics.nist.gov/cuu/Units/"]
subset: unit_slim
synonym: "g" EXACT []
is_a: UO:0000002 ! mass unit
created_by: george gkoutos */

#define UO_MILLIGRAM_ID "UO:0000022"
#define UO_MILLIGRAM_NAME "milligram"
/* def: "A mass unit which is equal to one thousandth of a gram or 10^[-3] g." [UOC:GVG]
subset: unit_slim
synonym: "mg" EXACT []
is_a: UO:0000002 ! mass unit
created_by: george gkoutos */

#define UO_MICROGRAM_ID "UO:0000023"
#define UO_MICROGRAM_NAME "microgram"
/* def: "A mass unit which is equal to one millionth of a gram or 10^[-6] g." [UOC:GVG]
subset: unit_slim
synonym: "ug" EXACT []
is_a: UO:0000002 ! mass unit
created_by: george gkoutos */

#define UO_NANOGRAM_ID "UO:0000024"
#define UO_NANOGRAM_NAME "nanogram"
/* def: "A mass unit which is equal to one thousandth of one millionth of a gram or 10^[-9] g." [UOC:GVG]
subset: unit_slim
synonym: "ng" EXACT []
is_a: UO:0000002 ! mass unit
created_by: george gkoutos */

#define UO_PICOGRAM_ID "UO:0000025"
#define UO_PICOGRAM_NAME "picogram"
/* def: "A mass unit which is equal to 10^[-12] g." [UOC:GVG]
subset: unit_slim
synonym: "pg" EXACT []
is_a: UO:0000002 ! mass unit
created_by: george gkoutos */

#define UO_FEMTOGRAM_ID "UO:0000026"
#define UO_FEMTOGRAM_NAME "femtogram"
/* def: "A mass unit which is equal to 10^[-15] g." [NIST:NIST "http://physics.nist.gov/cuu/Units/"]
subset: unit_slim
synonym: "fg" EXACT []
is_a: UO:0000002 ! mass unit
created_by: george gkoutos */

#define UO_DEGREE_CELSIUS_ID "UO:0000027"
#define UO_DEGREE_CELSIUS_NAME "degree celsius"
/* def: "A temperature unit which is equal to one Kelvin degree. However, they have their zeros at different points. The Centigrade scale has its zero at 273.15 K." [NIST:NIST "http://physics.nist.gov/cuu/Units/"]
subset: unit_slim
synonym: "C" EXACT []
is_a: UO:0000005 ! temperature unit
created_by: george gkoutos */

#define UO_MILLISECOND_ID "UO:0000028"
#define UO_MILLISECOND_NAME "millisecond"
/* def: "A time unit which is equal to one thousandth of a second or 10^[-3] s." [NIST:NIST "http://physics.nist.gov/cuu/Units/"]
subset: unit_slim
synonym: "ms" EXACT []
is_a: UO:0000003 ! time unit
created_by: george gkoutos */

#define UO_MICROSECOND_ID "UO:0000029"
#define UO_MICROSECOND_NAME "microsecond"
/* def: "A time unit which is equal to one millionth of a second or 10^[-6] s." [NIST:NIST "http://physics.nist.gov/cuu/Units/"]
subset: unit_slim
synonym: "us" EXACT []
is_a: UO:0000003 ! time unit
created_by: george gkoutos */

#define UO_PICOSECOND_ID "UO:0000030"
#define UO_PICOSECOND_NAME "picosecond"
/* def: "A time unit which is equal to 10^[-12] s." [NIST:NIST "http://physics.nist.gov/cuu/Units/"]
subset: unit_slim
synonym: "ps" EXACT []
is_a: UO:0000003 ! time unit
created_by: george gkoutos */

#define UO_MINUTE_ID "UO:0000031"
#define UO_MINUTE_NAME "minute"
/* def: "A time unit which is equal to 60 seconds." [Wikipedia:Wikipedia "http://www.wikipedia.org/"]
subset: unit_slim
synonym: "min" EXACT []
is_a: UO:0000003 ! time unit
created_by: george gkoutos */

#define UO_HOUR_ID "UO:0000032"
#define UO_HOUR_NAME "hour"
/* def: "A time unit which is equal to 3600 seconds or 60 minutes." [Wikipedia:Wikipedia "http://www.wikipedia.org/"]
subset: unit_slim
synonym: "h" EXACT []
is_a: UO:0000003 ! time unit
created_by: george gkoutos */

#define UO_DAY_ID "UO:0000033"
#define UO_DAY_NAME "day"
/* def: "A time unit which is equal to 24 hours." [Wikipedia:Wikipedia "http://www.wikipedia.org/"]
subset: unit_slim
is_a: UO:0000003 ! time unit
created_by: george gkoutos */

#define UO_WEEK_ID "UO:0000034"
#define UO_WEEK_NAME "week"
/* def: "A time unit which is equal to 7 days." [Wikipedia:Wikipedia "http://www.wikipedia.org/"]
subset: unit_slim
is_a: UO:0000003 ! time unit
created_by: george gkoutos */

#define UO_MONTH_ID "UO:0000035"
#define UO_MONTH_NAME "month"
/* def: "A time unit which is approximately equal to the length of time of one of cycle of the moon's phases which in science is taken to be equal to 30 days." [Wikipedia:Wikipedia "http://www.wikipedia.org/"]
subset: unit_slim
is_a: UO:0000003 ! time unit
created_by: george gkoutos */

#define UO_YEAR_ID "UO:0000036"
#define UO_YEAR_NAME "year"
/* def: "A time unit which is equal to 12 months which is science is taken to be equal to 365.25 days." [Wikipedia:Wikipedia "http://www.wikipedia.org/"]
subset: unit_slim
is_a: UO:0000003 ! time unit
created_by: george gkoutos */

#define UO_MILLIAMPERE_ID "UO:0000037"
#define UO_MILLIAMPERE_NAME "milliampere"
/* def: "An electric current unit current which is equal to one thousandth of an ampere or 10^[-3] A." [UOC:GVG]
subset: unit_slim
synonym: "mA" EXACT []
is_a: UO:0000004 ! electric current unit
created_by: george gkoutos */

#define UO_MICROAMPERE_ID "UO:0000038"
#define UO_MICROAMPERE_NAME "microampere"
/* def: "An electric current unit current which is equal to one millionth of an ampere or 10^[-6] A." [UOC:GVG]
subset: unit_slim
synonym: "uA" EXACT []
is_a: UO:0000004 ! electric current unit
created_by: george gkoutos */

#define UO_MICROMOLE_ID "UO:0000039"
#define UO_MICROMOLE_NAME "micromole"
/* def: "A substance unit equal to a millionth of a mol or 10^[-6] mol." [NIST:NIST "http://physics.nist.gov/cuu/Units/"]
subset: unit_slim
synonym: "umol" EXACT []
is_a: UO:0000006 ! substance unit
created_by: george gkoutos */

#define UO_MILLIMOLE_ID "UO:0000040"
#define UO_MILLIMOLE_NAME "millimole"
/* def: "A substance unit equal to a thousandth of a mol or 10^[-3] mol." [NIST:NIST "http://physics.nist.gov/cuu/Units/"]
subset: unit_slim
synonym: "mmol" EXACT []
is_a: UO:0000006 ! substance unit
created_by: george gkoutos */

#define UO_NANOMOLE_ID "UO:0000041"
#define UO_NANOMOLE_NAME "nanomole"
/* def: "A substance unit equal to one thousandth of one millionth of a mole or 10^[-9] mol." [NIST:NIST "http://physics.nist.gov/cuu/Units/"]
subset: unit_slim
synonym: "nmol" EXACT []
is_a: UO:0000006 ! substance unit
created_by: george gkoutos */

#define UO_PICOMOLE_ID "UO:0000042"
#define UO_PICOMOLE_NAME "picomole"
/* def: "A substance unit equal to 10^[-12] mol." [NIST:NIST "http://physics.nist.gov/cuu/Units/"]
subset: unit_slim
synonym: "pmol" EXACT []
is_a: UO:0000006 ! substance unit
created_by: george gkoutos */

#define UO_FEMTOMOLE_ID "UO:0000043"
#define UO_FEMTOMOLE_NAME "femtomole"
/* def: "A substance unit equal to 10^[-15] mol." [NIST:NIST "http://physics.nist.gov/cuu/Units/"]
subset: unit_slim
synonym: "fmol" EXACT []
is_a: UO:0000006 ! substance unit
created_by: george gkoutos */

#define UO_ATTOMOLE_ID "UO:0000044"
#define UO_ATTOMOLE_NAME "attomole"
/* def: "A substance unit equal to 10^[-18] mol." [NIST:NIST "http://physics.nist.gov/cuu/Units/"]
subset: unit_slim
synonym: "amol" EXACT []
is_a: UO:0000006 ! substance unit
created_by: george gkoutos */

#define UO_BASE_UNIT_ID "UO:0000045"
#define UO_BASE_UNIT_NAME "base unit"
/* def: "A unit which is one of a particular measure to which all measures of that type can be related." [NIST:NIST "http://physics.nist.gov/cuu/Units/"]
subset: unit_group_slim
is_a: UO:0000000 ! unit
created_by: george gkoutos */

#define UO_DERIVED_UNIT_ID "UO:0000046		"
#define UO_DERIVED_UNIT_NAME "derived unit		"
/* def: "A unit which is derived from base units." [Wikipedia:Wikipedia "http://www.wikipedia.org/"]		
is_obsolete: true */

#define UO_AREA_UNIT_ID "UO:0000047"
#define UO_AREA_UNIT_NAME "area unit"
/* def: "A unit which is a standard measure of the amount of a 2-dimensional flat surface." [UOC:GVG]
subset: unit_group_slim
is_a: UO:0000000 ! unit
relationship: unit_of PATO:0001709
created_by: george gkoutos */

#define UO_ACCELERATION_UNIT_ID "UO:0000048"
#define UO_ACCELERATION_UNIT_NAME "acceleration unit"
/* def: "A unit which is a standard measure of the rate of change of velocity in either speed or direction." [Wikipedia:Wikipedia "http://www.wikipedia.org/"]
subset: unit_group_slim
is_a: UO:0000000 ! unit
relationship: unit_of PATO:0001028
created_by: george gkoutos */

#define UO_ANGULAR_VELOCITY_UNIT_ID "UO:0000049"
#define UO_ANGULAR_VELOCITY_UNIT_NAME "angular velocity unit"
/* def: "A unit which is a standard measure of the rate of angular movement about an axis; the angle rotated in a given time." [Wikipedia:Wikipedia "http://www.wikipedia.org/"]
subset: unit_group_slim
is_a: UO:0000000 ! unit
relationship: unit_of PATO:0001413
created_by: george gkoutos */

#define UO_ANGULAR_ACCELERATION_UNIT_ID "UO:0000050"
#define UO_ANGULAR_ACCELERATION_UNIT_NAME "angular acceleration unit"
/* def: "A unit which is a standard measure of the rate of change of angular velocity." [Wikipedia:Wikipedia "http://www.wikipedia.org/"]
subset: unit_group_slim
is_a: UO:0000000 ! unit
relationship: unit_of PATO:0001350
created_by: george gkoutos */

#define UO_CONCENTRATION_UNIT_ID "UO:0000051"
#define UO_CONCENTRATION_UNIT_NAME "concentration unit"
/* def: "A unit which represents a standard measurement of how much of a given substance there is mixed with another substance." [UOC:GVG]
subset: unit_group_slim
is_a: UO:0000000 ! unit
relationship: unit_of PATO:0000033
created_by: george gkoutos */

#define UO_MASS_DENSITY_UNIT_ID "UO:0000052"
#define UO_MASS_DENSITY_UNIT_NAME "mass density unit"
/* def: "A density unit which is a standard measure of the mass of a substance in a given volume." [UOC:GVG]
subset: unit_group_slim
synonym: "mass per unit volume" EXACT []
is_a: UO:0000182 ! density unit
relationship: unit_of PATO:0001353
created_by: george gkoutos */

#define UO_LUMINANCE_UNIT_ID "UO:0000053"
#define UO_LUMINANCE_UNIT_NAME "luminance unit"
/* def: "A unit which is a standard measure of the luminous intensity impinging on a given area." [Wikipedia:Wikipedia "http://www.wikipedia.org/"]
subset: unit_group_slim
is_a: UO:0000157 ! light unit
relationship: unit_of PATO:0001718
created_by: george gkoutos */

#define UO_AREA_DENSITY_UNIT_ID "UO:0000054"
#define UO_AREA_DENSITY_UNIT_NAME "area density unit"
/* def: "A density unit which is a standard measure of the mass exerting an influence on a given area." [Wikipedia:Wikipedia "http://www.wikipedia.org/"]
subset: unit_group_slim
synonym: "mass per unit area unit" EXACT []
is_a: UO:0000182 ! density unit
relationship: unit_of PATO:0001351
created_by: george gkoutos */

#define UO_MOLAR_MASS_UNIT_ID "UO:0000055"
#define UO_MOLAR_MASS_UNIT_NAME "molar mass unit"
/* def: "A unit which is a standard measure of the mass of a homogeneous substance containing 6.02 x 1023 atoms or molecules." [Wikipedia:Wikipedia "http://www.wikipedia.org/"]
subset: unit_group_slim
is_a: UO:0000002 ! mass unit
relationship: unit_of PATO:0001681
created_by: george gkoutos */

#define UO_MOLAR_VOLUME_UNIT_ID "UO:0000056"
#define UO_MOLAR_VOLUME_UNIT_NAME "molar volume unit"
/* def: "A unit which is a standard measure of the volume of a homogeneous substance containing 6.02 x 1023 atoms or molecules." [Wikipedia:Wikipedia "http://www.wikipedia.org/"]
subset: unit_group_slim
is_a: UO:0000095 ! volume unit
relationship: unit_of PATO:0001680
created_by: george gkoutos */

#define UO_MOMENTUM_UNIT_ID "UO:0000057"
#define UO_MOMENTUM_UNIT_NAME "momentum unit"
/* def: "A unit which is a standard measure of the quantity of motion measured by the product of mass and velocity." [Wikipedia:Wikipedia "http://www.wikipedia.org/"]
subset: unit_group_slim
is_a: UO:0000000 ! unit
relationship: unit_of PATO:0001022
relationship: unit_of PATO:0001023
created_by: george gkoutos */

#define UO_ROTATIONAL_FREQUENCY_UNIT_ID "UO:0000058"
#define UO_ROTATIONAL_FREQUENCY_UNIT_NAME "rotational frequency unit"
/* def: "A unit which is a standard measure of the number of rotations in a given time." [NIST:NIST "http://physics.nist.gov/cuu/Units/"]
subset: unit_group_slim
is_a: UO:0000105 ! frequency unit
created_by: george gkoutos */

#define UO_SPECIFIC_VOLUME_UNIT_ID "UO:0000059"
#define UO_SPECIFIC_VOLUME_UNIT_NAME "specific volume unit"
/* def: "A unit which is a standard measure of the volume of a given mass of substance (the reciprocal of density)." [Wikipedia:Wikipedia "http://www.wikipedia.org/"]
subset: unit_group_slim
is_a: UO:0000095 ! volume unit
created_by: george gkoutos */

#define UO_SPEED_VELOCITY_UNIT_ID "UO:0000060"
#define UO_SPEED_VELOCITY_UNIT_NAME "speed/velocity unit"
/* def: "A unit which is a standard measure of the rate of movement. Speed is measured in the same physical units of measurement as velocity, but does not contain the element of direction that velocity has. Speed is thus the magnitude component of velocity." [Wikipedia:Wikipedia "http://www.wikipedia.org/"]
subset: unit_group_slim
is_a: UO:0000000 ! unit
relationship: unit_of PATO:0000008
created_by: george gkoutos */

#define UO_UNIT_OF_MOLARITY_ID "UO:0000061"
#define UO_UNIT_OF_MOLARITY_NAME "unit of molarity"
/* def: "A concentration unit which is a standard measure of the number of moles of a given substance per liter of solution." [UOC:GVG]
subset: unit_group_slim
is_a: UO:0000051 ! concentration unit
created_by: george gkoutos */

#define UO_MOLAR_ID "UO:0000062"
#define UO_MOLAR_NAME "molar"
/* def: "A unit of concentration which expresses a concentration of 1 mole of solute per liter of solution (mol/L)." [UOC:GVG]
subset: unit_slim
synonym: "M" EXACT []
is_a: UO:0000061 ! unit of molarity
created_by: george gkoutos */

#define UO_MILLIMOLAR_ID "UO:0000063"
#define UO_MILLIMOLAR_NAME "millimolar"
/* def: "A unit of molarity which is equal to one thousandth of a molar or 10^[-3] M." [UOC:GVG]
subset: unit_slim
synonym: "mM" EXACT []
is_a: UO:0000061 ! unit of molarity
created_by: george gkoutos */

#define UO_MICROMOLAR_ID "UO:0000064"
#define UO_MICROMOLAR_NAME "micromolar"
/* def: "A unit of molarity which is equal to one millionth of a molar or 10^[-6] M." [UOC:GVG]
subset: unit_slim
synonym: "uM" EXACT []
is_a: UO:0000061 ! unit of molarity
created_by: george gkoutos */

#define UO_NANOMOLAR_ID "UO:0000065"
#define UO_NANOMOLAR_NAME "nanomolar"
/* def: "A unit of molarity which is equal to one thousandth of one millionth of a molar or 10^[-9] M." [UOC:GVG "http://www.wikipedia.org/"]
subset: unit_slim
synonym: "nM" EXACT []
is_a: UO:0000061 ! unit of molarity
created_by: george gkoutos */

#define UO_PICOMOLAR_ID "UO:0000066"
#define UO_PICOMOLAR_NAME "picomolar"
/* def: "A unit of molarity which is equal to 10^[-12] M." [UOC:GVG]
subset: unit_slim
synonym: "pM" EXACT []
is_a: UO:0000061 ! unit of molarity
created_by: george gkoutos */

#define UO_UNIT_OF_MOLALITY_ID "UO:0000067"
#define UO_UNIT_OF_MOLALITY_NAME "unit of molality"
/* def: "A concentration unit which is a standard measure of the number of moles of a given substance per kilogram of solvent." [UOC:GVG]
subset: unit_group_slim
is_a: UO:0000051 ! concentration unit
created_by: george gkoutos */

#define UO_MOLAL_ID "UO:0000068"
#define UO_MOLAL_NAME "molal"
/* def: "A unit of concentration which expresses a concentration of a solution of 1 mole per kilogram of solvent (mol/kg)." [UOC:GVG]
subset: unit_slim
synonym: "m" EXACT []
is_a: UO:0000067 ! unit of molality
created_by: george gkoutos */

#define UO_MILLIMOLAL_ID "UO:0000069"
#define UO_MILLIMOLAL_NAME "millimolal"
/* def: "A molality unit which is equal to one thousandth of a molal or 10^[-3] m." [UOC:GVG]
subset: unit_slim
synonym: "mm" EXACT []
is_a: UO:0000067 ! unit of molality
created_by: george gkoutos */

#define UO_MICROMOLAL_ID "UO:0000070"
#define UO_MICROMOLAL_NAME "micromolal"
/* def: "A molality unit which is equal to one millionth of a molal or 10^[-6] m." [UOC:GVG]
subset: unit_slim
synonym: "um" EXACT []
is_a: UO:0000067 ! unit of molality
created_by: george gkoutos */

#define UO_NANOMOLAL_ID "UO:0000071"
#define UO_NANOMOLAL_NAME "nanomolal"
/* def: "A molality unit which is equal to one thousandth of one millionth of a molal or 10^[-9] m." [UOC:GVG]
subset: unit_slim
synonym: "nm" EXACT []
is_a: UO:0000067 ! unit of molality
created_by: george gkoutos */

#define UO_PICOMOLAL_ID "UO:0000072"
#define UO_PICOMOLAL_NAME "picomolal"
/* def: "A molality unit which is equal to 10^[-12] m." [UOC:GVG]
subset: unit_slim
synonym: "pm" EXACT []
is_a: UO:0000067 ! unit of molality
created_by: george gkoutos */

#define UO_FEMTOMOLAR_ID "UO:0000073"
#define UO_FEMTOMOLAR_NAME "femtomolar"
/* def: "A unit of molarity which is equal to 10^[-15] M." [UOC:GVG]
subset: unit_slim
synonym: "fM" EXACT []
is_a: UO:0000061 ! unit of molarity
created_by: george gkoutos */

#define UO_UNIT_OF_NORMALITY_ID "UO:0000074"
#define UO_UNIT_OF_NORMALITY_NAME "unit of normality"
/* def: "A unit of concentration which highlights the chemical nature of salts." [Wikipedia:Wikipedia "http://www.wikipedia.org/"]
subset: unit_group_slim
is_a: UO:0000051 ! concentration unit
created_by: george gkoutos */

#define UO_NORMAL_ID "UO:0000075"
#define UO_NORMAL_NAME "normal"
/* def: "A unit of concentration which is one gram equivalent of a solute per liter of solution. A gram equivalent weight or equivalent is a measure of the reactive capacity of a given molecule." [Wikipedia:Wikipedia "http://www.wikipedia.org/"]
subset: unit_slim
synonym: "N" EXACT []
is_a: UO:0000074 ! unit of normality
created_by: george gkoutos */

#define UO_MOLE_FRACTION_ID "UO:0000076"
#define UO_MOLE_FRACTION_NAME "mole fraction"
/* def: "A concentration unit which denotes the number of moles of solute as a proportion of the total number of moles in a solution." [Wikipedia:Wikipedia "http://www.wikipedia.org/"]
subset: unit_slim
synonym: "(x)" EXACT []
synonym: "chi" EXACT []
is_a: UO:0000051 ! concentration unit
is_a: UO:0000191 ! fraction
created_by: george gkoutos */

#define UO_METER_PER_SECOND_PER_SECOND_ID "UO:0000077"
#define UO_METER_PER_SECOND_PER_SECOND_NAME "meter per second per second"
/* def: "An acceleration unit which is equal to the acceleration an object changing its velocity by 1meter/s over a time period that equals one second." [NIST:NIST "http://physics.nist.gov/cuu/Units/"]
subset: unit_slim
synonym: "m/s^[2]" EXACT []
is_a: UO:0000048 ! acceleration unit
created_by: george gkoutos */

#define UO_RADIAN_PER_SECOND_PER_SECOND_ID "UO:0000078"
#define UO_RADIAN_PER_SECOND_PER_SECOND_NAME "radian per second per second"
/* def: "An angular unit acceleration which is equal to the angular acceleration of an object changing its angular velocity by 1rad/s over a time period that equals one second." [NIST:NIST "http://physics.nist.gov/cuu/Units/"]
subset: unit_slim
synonym: "alpha" EXACT []
synonym: "rad/s^[2]" EXACT []
is_a: UO:0000050 ! angular acceleration unit
created_by: george gkoutos */

#define UO_RADIAN_PER_SECOND_ID "UO:0000079"
#define UO_RADIAN_PER_SECOND_NAME "radian per second"
/* def: "An angular unit velocity which is equal to about 9.54930 rpm (revolutions per minute)." [NIST:NIST "http://physics.nist.gov/cuu/Units/"]
subset: unit_slim
synonym: "rad/s" EXACT []
is_a: UO:0000049 ! angular velocity unit
created_by: george gkoutos */

#define UO_SQUARE_METER_ID "UO:0000080"
#define UO_SQUARE_METER_NAME "square meter"
/* def: "An area unit which is equal to an area enclosed by a square with sides each 1 meter long." [NIST:NIST "http://physics.nist.gov/cuu/Units/"]
subset: unit_slim
synonym: "m^[2]" EXACT []
is_a: UO:0000047 ! area unit
created_by: george gkoutos */

#define UO_SQUARE_CENTIMETER_ID "UO:0000081"
#define UO_SQUARE_CENTIMETER_NAME "square centimeter"
/* def: "An area unit which is equal to one thousand of square meter or 10^[-3] m^[2]." [NIST:NIST "http://physics.nist.gov/cuu/Units/"]
subset: unit_slim
synonym: "cm^[2]" EXACT []
is_a: UO:0000047 ! area unit
created_by: george gkoutos */

#define UO_SQUARE_MILLIMETER_ID "UO:0000082"
#define UO_SQUARE_MILLIMETER_NAME "square millimeter"
/* def: "An area unit which is equal to one millionth of a square meter or 10^[-6] m^[2]." [NIST:NIST "http://physics.nist.gov/cuu/Units/"]
subset: unit_slim
synonym: "mm^[2]" EXACT []
is_a: UO:0000047 ! area unit
created_by: george gkoutos */

#define UO_KILOGRAM_PER_CUBIC_METER_ID "UO:0000083"
#define UO_KILOGRAM_PER_CUBIC_METER_NAME "kilogram per cubic meter"
/* def: "A mass unit density which is equal to mass of an object in kilograms divided by the volume in cubic meters." [UOC:GVG]
subset: unit_slim
synonym: "kg/m^[3]" EXACT []
is_a: UO:0000052 ! mass density unit
created_by: george gkoutos */

#define UO_GRAM_PER_CUBIC_CENTIMETER_ID "UO:0000084"
#define UO_GRAM_PER_CUBIC_CENTIMETER_NAME "gram per cubic centimeter"
/* def: "A mass unit density which is equal to mass of an object in grams divided by the volume in cubic centimeters." [UOC:GVG]
subset: unit_slim
synonym: "g/cm^[3]" EXACT []
is_a: UO:0000052 ! mass density unit
created_by: george gkoutos */

#define UO_CANDELA_PER_SQUARE_METER_ID "UO:0000085"
#define UO_CANDELA_PER_SQUARE_METER_NAME "candela per square meter"
/* def: "A luminance unit which is equal to a luminous intensity of one candela radiating from a surface whose area is one square meter." [NIST:NIST "http://physics.nist.gov/cuu/Units/"]
subset: unit_slim
synonym: "cd/m^[2]" EXACT []
is_a: UO:0000053 ! luminance unit
created_by: george gkoutos */

#define UO_KILOGRAM_PER_SQUARE_METER_ID "UO:0000086"
#define UO_KILOGRAM_PER_SQUARE_METER_NAME "kilogram per square meter"
/* def: "An area density unit which is equal to the mass of an object in kilograms divided by the surface area in meters squared." [NIST:NIST "http://physics.nist.gov/cuu/Units/"]
subset: unit_slim
synonym: "Body Mass Index (BMI)" EXACT []
synonym: "kg/m^[2]" EXACT []
is_a: UO:0000054 ! area density unit
created_by: george gkoutos */

#define UO_KILOGRAM_PER_MOLE_ID "UO:0000087"
#define UO_KILOGRAM_PER_MOLE_NAME "kilogram per mole"
/* def: "A molar mass unit which is equal to one kilogram of mass of one mole of chemical element or chemical compound." [NIST:NIST "http://physics.nist.gov/cuu/Units/"]
subset: unit_slim
synonym: "kg/mol" EXACT []
is_a: UO:0000055 ! molar mass unit
created_by: george gkoutos */

#define UO_GRAM_PER_MOLE_ID "UO:0000088"
#define UO_GRAM_PER_MOLE_NAME "gram per mole"
/* def: "A molar mass unit which is equal to one gram of mass of one mole of chemical element or chemical compound." [NIST:NIST "http://physics.nist.gov/cuu/Units/"]
subset: unit_slim
synonym: "g/mol" EXACT []
is_a: UO:0000055 ! molar mass unit
created_by: george gkoutos */

#define UO_CUBIC_METER_PER_MOLE_ID "UO:0000089"
#define UO_CUBIC_METER_PER_MOLE_NAME "cubic meter per mole"
/* def: "A molar volume unit which is equal to 1 cubic meter occupied by one mole of a substance in the form of a solid, liquid, or gas." [NIST:NIST "http://physics.nist.gov/cuu/Units/"]
subset: unit_slim
synonym: "m^[3]/mol" EXACT []
is_a: UO:0000056 ! molar volume unit
created_by: george gkoutos */

#define UO_CUBIC_CENTIMETER_PER_MOLE_ID "UO:0000090"
#define UO_CUBIC_CENTIMETER_PER_MOLE_NAME "cubic centimeter per mole"
/* def: "A molar volume unit which is equal to 1 cubic centimeter occupied by one mole of a substance in the form of a solid, liquid, or gas." [NIST:NIST "http://physics.nist.gov/cuu/Units/"]
subset: unit_slim
synonym: "cm^[3]/mol" EXACT []
is_a: UO:0000056 ! molar volume unit
created_by: george gkoutos */

#define UO_KILOGRAM_METER_PER_SECOND_ID "UO:0000091"
#define UO_KILOGRAM_METER_PER_SECOND_NAME "kilogram meter per second"
/* def: "A momentum unit which is equal to the momentum of a one kilogram mass object with a speed of one meter per second." [NIST:NIST "http://physics.nist.gov/cuu/Units/"]
subset: unit_slim
synonym: "kg.m/s" EXACT []
is_a: UO:0000057 ! momentum unit
created_by: george gkoutos */

#define UO_TURNS_PER_SECOND_ID "UO:0000092"
#define UO_TURNS_PER_SECOND_NAME "turns per second"
/* def: "A rotational frequency unit which is equal to the number complete turn in a period of time that equals to 1 second." [NIST:NIST "http://physics.nist.gov/cuu/Units/"]
subset: unit_slim
synonym: "1/s" EXACT []
synonym: "one turn per second" NARROW []
is_a: UO:0000058 ! rotational frequency unit
created_by: george gkoutos */

#define UO_CUBIC_METER_PER_KILOGRAM_ID "UO:0000093"
#define UO_CUBIC_METER_PER_KILOGRAM_NAME "cubic meter per kilogram"
/* def: "A specific volume unit which is equal to one cubic meter volume occupied by one kilogram of a particular substance." [NIST:NIST "http://physics.nist.gov/cuu/Units/"]
subset: unit_slim
synonym: "m^[3]/kg" EXACT []
is_a: UO:0000059 ! specific volume unit
created_by: george gkoutos */

#define UO_METER_PER_SECOND_ID "UO:0000094"
#define UO_METER_PER_SECOND_NAME "meter per second"
/* def: "A speed/velocity unit which is equal to the speed of an object traveling 1 meter distance in one second." [NIST:NIST "http://physics.nist.gov/cuu/Units/"]
subset: unit_slim
synonym: "m/s" EXACT []
is_a: UO:0000060 ! speed/velocity unit
created_by: george gkoutos */

#define UO_VOLUME_UNIT_ID "UO:0000095"
#define UO_VOLUME_UNIT_NAME "volume unit"
/* def: "A unit which is a standard measure of the amount of space occupied by any substance, whether solid, liquid, or gas." [NIST:NIST "http://physics.nist.gov/cuu/Units/"]
subset: unit_group_slim
is_a: UO:0000000 ! unit
relationship: unit_of PATO:0001710
created_by: george gkoutos */

#define UO_CUBIC_METER_ID "UO:0000096"
#define UO_CUBIC_METER_NAME "cubic meter"
/* def: "A volume unit which is equal to the volume of a cube with edges one meter in length. One cubic meter equals to 1000 liters." [NIST:NIST "http://physics.nist.gov/cuu/Units/"]
subset: unit_slim
synonym: "m^[3]" EXACT []
is_a: UO:0000095 ! volume unit
created_by: george gkoutos */

#define UO_CUBIC_CENTIMETER_ID "UO:0000097"
#define UO_CUBIC_CENTIMETER_NAME "cubic centimeter"
/* def: "A volume unit which is equal to one millionth of a cubic meter or 10^[-9] m^[3], or to 1 ml." [NIST:NIST "http://physics.nist.gov/cuu/Units/"]
subset: unit_slim
synonym: "cc" EXACT []
synonym: "cm^3" EXACT []
is_a: UO:0000095 ! volume unit
created_by: george gkoutos */

#define UO_MILLILITER_ID "UO:0000098"
#define UO_MILLILITER_NAME "milliliter"
/* def: "A volume unit which is equal to one thousandth of a liter or 10^[-3] L, or to 1 cubic centimeter." [NIST:NIST "http://physics.nist.gov/cuu/Units/"]
subset: unit_slim
synonym: "ml" EXACT []
is_a: UO:0000095 ! volume unit
created_by: george gkoutos */

#define UO_LITER_ID "UO:0000099"
#define UO_LITER_NAME "liter"
/* def: "A volume unit which is equal to one thousandth of a cubic meter or 10^[-3] m^[3], or to 1 decimeter." [NIST:NIST "http://physics.nist.gov/cuu/Units/"]
subset: unit_slim
synonym: "l" EXACT []
synonym: "L" EXACT []
is_a: UO:0000095 ! volume unit
created_by: george gkoutos */

#define UO_CUBIC_DECIMETER_ID "UO:0000100"
#define UO_CUBIC_DECIMETER_NAME "cubic decimeter"
/* def: "A volume unit which is equal to one thousand of a cubic meter or 10^[-3] m^[3], or to 1 L." [NIST:NIST "http://physics.nist.gov/cuu/Units/"]
subset: unit_slim
synonym: "dm^[3]" EXACT []
is_a: UO:0000095 ! volume unit
created_by: george gkoutos */

#define UO_MICROLITER_ID "UO:0000101"
#define UO_MICROLITER_NAME "microliter"
/* def: "A volume unit which is equal to one millionth of a liter or 10^[-6] L." [NIST:NIST "http://physics.nist.gov/cuu/Units/"]
subset: unit_slim
synonym: "ul" EXACT []
is_a: UO:0000095 ! volume unit
created_by: george gkoutos */

#define UO_NANOLITER_ID "UO:0000102"
#define UO_NANOLITER_NAME "nanoliter"
/* def: "A volume unit which is equal to one thousandth of one millionth of a liter or 10^[-9] L." [NIST:NIST "http://physics.nist.gov/cuu/Units/"]
subset: unit_slim
synonym: "nl" EXACT []
is_a: UO:0000095 ! volume unit
created_by: george gkoutos */

#define UO_PICOLITER_ID "UO:0000103"
#define UO_PICOLITER_NAME "picoliter"
/* def: "A volume unit which is equal to 10^[-12] L." [NIST:NIST "http://physics.nist.gov/cuu/Units/"]
subset: unit_slim
synonym: "pl" EXACT []
is_a: UO:0000095 ! volume unit
created_by: george gkoutos */

#define UO_FEMTOLITER_ID "UO:0000104"
#define UO_FEMTOLITER_NAME "femtoliter"
/* def: "A volume unit which is equal to 10^[-15] L." [NIST:NIST "http://physics.nist.gov/cuu/Units/"]
subset: unit_slim
synonym: "fl" EXACT []
is_a: UO:0000095 ! volume unit
created_by: george gkoutos */

#define UO_FREQUENCY_UNIT_ID "UO:0000105"
#define UO_FREQUENCY_UNIT_NAME "frequency unit"
/* def: "A unit which is a standard measure of the number of repetitive actions in a particular time." [NIST:NIST "http://physics.nist.gov/cuu/Units/"]
subset: unit_group_slim
is_a: UO:0000000 ! unit
relationship: unit_of PATO:0000044
created_by: george gkoutos */

#define UO_HERTZ_ID "UO:0000106"
#define UO_HERTZ_NAME "hertz"
/* def: "A frequency unit which is equal to 1 complete cycle of a recurring phenomenon in 1 second." [NIST:NIST "http://physics.nist.gov/cuu/Units/"]
subset: unit_slim
synonym: "Hz" EXACT []
synonym: "s^1" EXACT []
is_a: UO:0000105 ! frequency unit
created_by: george gkoutos */

#define UO_FORCE_UNIT_ID "UO:0000107"
#define UO_FORCE_UNIT_NAME "force unit"
/* def: "A unit which is a standard measure of the force is applied when a mass is accelerated." [NIST:NIST "http://physics.nist.gov/cuu/Units/"]
subset: unit_group_slim
is_a: UO:0000000 ! unit
relationship: unit_of PATO:0001035
created_by: george gkoutos */

#define UO_NEWTON_ID "UO:0000108"
#define UO_NEWTON_NAME "newton"
/* def: "A force unit which is equal to the force required to cause an acceleration of 1m/s2 of a mass of 1 Kg in the direction of the force." [NIST:NIST "http://physics.nist.gov/cuu/Units/"]
subset: unit_slim
synonym: "N" EXACT []
is_a: UO:0000107 ! force unit
created_by: george gkoutos */

#define UO_PRESSURE_UNIT_ID "UO:0000109"
#define UO_PRESSURE_UNIT_NAME "pressure unit"
/* def: "A unit which is a standard measure of the force applied to a given area." [NIST:NIST "http://physics.nist.gov/cuu/Units/"]
subset: unit_group_slim
is_a: UO:0000000 ! unit
relationship: unit_of PATO:0001025
created_by: george gkoutos */

#define UO_PASCAL_ID "UO:0000110"
#define UO_PASCAL_NAME "pascal"
/* def: "A pressure unit which is equal to the pressure or stress on a surface caused by a force of 1 newton spread over a surface of 1 m^[2]." [NIST:NIST "http://physics.nist.gov/cuu/Units/"]
subset: unit_slim
synonym: "Pa" EXACT []
is_a: UO:0000109 ! pressure unit
created_by: george gkoutos */

#define UO_ENERGY_UNIT_ID "UO:0000111"
#define UO_ENERGY_UNIT_NAME "energy unit"
/* def: "A unit which is a standard measure of the work done by a certain force (gravitational, electric, magnetic, force of inertia, etc)." [NIST:NIST "http://physics.nist.gov/cuu/Units/"]
subset: unit_group_slim
is_a: UO:0000000 ! unit
relationship: unit_of PATO:0001021
relationship: unit_of PATO:0001026
created_by: george gkoutos */

#define UO_JOULE_ID "UO:0000112"
#define UO_JOULE_NAME "joule"
/* def: "An energy unit which is equal to the energy required when a force of 1 newton moves an object 1 meter in the direction of the force." [NIST:NIST "http://physics.nist.gov/cuu/Units/"]
subset: unit_slim
synonym: "J" EXACT []
is_a: UO:0000111 ! energy unit
created_by: george gkoutos */

#define UO_POWER_UNIT_ID "UO:0000113"
#define UO_POWER_UNIT_NAME "power unit"
/* def: "A unit which is a standard measure power or the rate of doing work." [NIST:NIST "http://physics.nist.gov/cuu/Units/"]
subset: unit_group_slim
is_a: UO:0000000 ! unit
relationship: unit_of PATO:0001024
created_by: george gkoutos */

#define UO_WATT_ID "UO:0000114"
#define UO_WATT_NAME "watt"
/* def: "A power unit which is equal to the power used when work is done at the rate of 1 joule per second." [NIST:NIST "http://physics.nist.gov/cuu/Units/"]
subset: unit_slim
synonym: "W" EXACT []
is_a: UO:0000113 ! power unit
created_by: george gkoutos */

#define UO_ILLUMINANCE_UNIT_ID "UO:0000115"
#define UO_ILLUMINANCE_UNIT_NAME "illuminance unit"
/* def: "A unit which is a standard measure of the luminous flux incident on a unit area." [Wikipedia:Wikipedia "http://www.wikipedia.org/"]
subset: unit_group_slim
is_a: UO:0000157 ! light unit
created_by: george gkoutos */

#define UO_LUX_ID "UO:0000116"
#define UO_LUX_NAME "lux"
/* def: "An illuminance unit which is equal to the illuminance produced by 1 lumen evenly spread over an area 1 m^[2]." [NIST:NIST "http://physics.nist.gov/cuu/Units/"]
subset: unit_slim
synonym: "lx" EXACT []
is_a: UO:0000115 ! illuminance unit
created_by: george gkoutos */

#define UO_LUMINOUS_FLUX_UNIT_ID "UO:0000117"
#define UO_LUMINOUS_FLUX_UNIT_NAME "luminous flux unit"
/* def: "A unit which is a standard measure of the flow of radiant energy." [Wikipedia:Wikipedia "http://www.wikipedia.org/"]
subset: unit_group_slim
is_a: UO:0000157 ! light unit
relationship: unit_of PATO:0001296
created_by: george gkoutos */

#define UO_LUMEN_ID "UO:0000118"
#define UO_LUMEN_NAME "lumen"
/* def: "A luminous flux unit which is equal to the luminous flux emitted into 1 steradian by a point source of 1 candela." [NIST:NIST "http://physics.nist.gov/cuu/Units/"]
subset: unit_slim
synonym: "lm" EXACT []
is_a: UO:0000117 ! luminous flux unit
created_by: george gkoutos */

#define UO_CATALYTIC_ACTIVITY_UNIT_ID "UO:0000119"
#define UO_CATALYTIC_ACTIVITY_UNIT_NAME "catalytic activity unit"
/* def: "A unit which is a standard measure of the amount of the action of a catalyst." [NIST:NIST "http://physics.nist.gov/cuu/Units/"]
subset: unit_group_slim
is_a: UO:0000000 ! unit
relationship: unit_of PATO:0001414
created_by: george gkoutos */

#define UO_KATAL_ID "UO:0000120"
#define UO_KATAL_NAME "katal"
/* def: "A catalytic unit activity which is equal to the activity of a catalyst in moles per second, such as the amount of an enzyme needed to transform one mole of substrate per second." [NIST:NIST "http://physics.nist.gov/cuu/Units/"]
subset: unit_slim
synonym: "kat" EXACT []
is_a: UO:0000119 ! catalytic activity unit
created_by: george gkoutos */

#define UO_ANGLE_UNIT_ID "UO:0000121"
#define UO_ANGLE_UNIT_NAME "angle unit"
/* def: "A unit which is a standard measure of the figure or space formed by the junction of two lines or planes." [Wikipedia:Wikipedia "http://www.wikipedia.org/"]
subset: unit_group_slim
is_a: UO:0000000 ! unit
relationship: unit_of PATO:0000133
created_by: george gkoutos */

#define UO_PLANE_ANGLE_UNIT_ID "UO:0000122"
#define UO_PLANE_ANGLE_UNIT_NAME "plane angle unit"
/* def: "A unit which is a standard measure of the angle formed by two straight lines in the same plane." [Wikipedia:Wikipedia "http://www.wikipedia.org/"]
subset: unit_group_slim
is_a: UO:0000121 ! angle unit
created_by: george gkoutos */

#define UO_RADIAN_ID "UO:0000123"
#define UO_RADIAN_NAME "radian"
/* def: "A plane angle unit which is equal to the angle subtended at the center of a circle by an arc equal in length to the radius of the circle, approximately 57 degrees 17 minutes and 44.6 seconds." [NIST:NIST "http://physics.nist.gov/cuu/Units/"]
subset: unit_slim
synonym: "rad" EXACT []
is_a: UO:0000122 ! plane angle unit
created_by: george gkoutos */

#define UO_SOLID_ANGLE_UNIT_ID "UO:0000124"
#define UO_SOLID_ANGLE_UNIT_NAME "solid angle unit"
/* def: "A unit which is a standard measure of the angle formed by three or more planes intersecting at a common point." [Wikipedia:Wikipedia "http://www.wikipedia.org/"]
subset: unit_group_slim
is_a: UO:0000121 ! angle unit
created_by: george gkoutos */

#define UO_STERADIAN_ID "UO:0000125"
#define UO_STERADIAN_NAME "steradian"
/* def: "A solid angle unit which is equal to the solid angle subtended at the center of a sphere by an area on the surface of the sphere that is equal to the radius squared." [NIST:NIST "http://physics.nist.gov/cuu/Units/"]
subset: unit_slim
synonym: "sr" EXACT []
is_a: UO:0000124 ! solid angle unit
created_by: george gkoutos */

#define UO_RADIATION_UNIT_ID "UO:0000127"
#define UO_RADIATION_UNIT_NAME "radiation unit"
/* def: "A unit which is a standard measure of the amount of radiation emitted by a given radiation source as well as the amount of radiation absorbed or deposited in a specific material by a radiation source." [OCRBS:OCRBS "http://www.orcbs.msu.edu/"]
subset: unit_group_slim
is_a: UO:0000000 ! unit
created_by: george gkoutos */

#define UO_ACTIVITY__OF_A_RADIONUCLIDE__UNIT_ID "UO:0000128"
#define UO_ACTIVITY__OF_A_RADIONUCLIDE__UNIT_NAME "activity (of a radionuclide) unit"
/* def: "A unit which is a standard measure of the transformation (disintegration) rate of a radioactive substance." [DEFRA:DEFRA "http://www.defra.gov.uk/"]
subset: unit_group_slim
is_a: UO:0000127 ! radiation unit
relationship: unit_of PATO:0001740
created_by: george gkoutos */

#define UO_ABSORBED_DOSE_UNIT_ID "UO:0000129"
#define UO_ABSORBED_DOSE_UNIT_NAME "absorbed dose unit"
/* def: "A unit which is a standard measure of the energy imparted by ionizing radiation to unit mass of matter such as tissue." [DEFRA:DEFRA "http://www.defra.gov.uk/"]
subset: unit_group_slim
is_a: UO:0000127 ! radiation unit
relationship: unit_of PATO:0001745
created_by: george gkoutos */

#define UO_DOSE_EQUIVALENT_UNIT_ID "UO:0000130"
#define UO_DOSE_EQUIVALENT_UNIT_NAME "dose equivalent unit"
/* def: "A unit which is a standard measure of the expression of dose in terms of its biological effect." [ORCBS:ORCBS "http://www.orcbs.msu.edu/"]
subset: unit_group_slim
is_a: UO:0000127 ! radiation unit
relationship: unit_of PATO:0001746
created_by: george gkoutos */

#define UO_EXPOSURE_UNIT_ID "UO:0000131"
#define UO_EXPOSURE_UNIT_NAME "exposure unit"
/* def: "A unit which is a standard measure of the quantity that expresses the ability of radiation to ionize air and thereby create electric charges which can be collected and measured." [ORCBS:ORCBS "http://www.orcbs.msu.edu/"]
subset: unit_group_slim
is_a: UO:0000127 ! radiation unit
relationship: unit_of PATO:0001744
created_by: george gkoutos */

#define UO_BECQUEREL_ID "UO:0000132"
#define UO_BECQUEREL_NAME "becquerel"
/* def: "An activity (of a radionuclide) unit which is equal to the activity of a quantity of radioactive material in which one nucleus decays per second or there is one atom disintegration per second (dps)." [NIST:NIST "http://physics.nist.gov/cuu/Units/"]
subset: unit_slim
synonym: "Bq" EXACT []
is_a: UO:0000128 ! activity (of a radionuclide) unit
created_by: george gkoutos */

#define UO_CURIE_ID "UO:0000133"
#define UO_CURIE_NAME "curie"
/* def: "An activity (of a radionuclide) unit which is equal to the activity of a quantity of radioactive material in which there are 3.7 x 10^[10] atom disintegration per second (dps)." [ORCBS:ORCBS "http://www.orcbs.msu.edu/"]
subset: unit_slim
synonym: "Ci" EXACT []
is_a: UO:0000128 ! activity (of a radionuclide) unit
created_by: george gkoutos */

#define UO_GRAY_ID "UO:0000134"
#define UO_GRAY_NAME "gray"
/* def: "An absorbed dose unit which is equal to the absorption of one joule of radiation energy by one kilogram of matter." [NIST:NIST "http://physics.nist.gov/cuu/Units/"]
subset: unit_slim
synonym: "Gy" EXACT []
is_a: UO:0000129 ! absorbed dose unit
created_by: george gkoutos */

#define UO_RAD_ID "UO:0000135"
#define UO_RAD_NAME "rad"
/* def: "An absorbed dose unit which is equal to 0.01 gray (Gy)." [Wikipedia:Wikipedia "http://www.wikipedia.org/"]
subset: unit_slim
is_a: UO:0000129 ! absorbed dose unit
created_by: george gkoutos */

#define UO_ROENTGEN_ID "UO:0000136"
#define UO_ROENTGEN_NAME "roentgen"
/* def: "An exposure unit which is equal to the amount of radiation required to liberate positive and negative charges of one electrostatic unit of charge in 1 cm^[3] of air at standard temperature and pressure (STP). This corresponds to the generation of approximately 2.0810^[9] ion pairs." [Wikipedia:Wikipedia "http://www.wikipedia.org/"]
subset: unit_slim
synonym: "R" EXACT []
is_a: UO:0000131 ! exposure unit
created_by: george gkoutos */

#define UO_SIEVERT_ID "UO:0000137"
#define UO_SIEVERT_NAME "sievert"
/* def: "A dose equivalent unit which is equal to the absorption of one joule of radiation energy by one kilogram of matter." [NIST:NIST "http://physics.nist.gov/cuu/Units/"]
subset: unit_slim
synonym: "Sv" EXACT []
is_a: UO:0000130 ! dose equivalent unit
created_by: george gkoutos */

#define UO_MILLISIEVERT_ID "UO:0000138"
#define UO_MILLISIEVERT_NAME "millisievert"
/* def: "A dose equivalent unit which is equal to one thousandth of a sievert or 10^[-3] Sv." [NIST:NIST "http://physics.nist.gov/cuu/Units/"]
subset: unit_slim
synonym: "mSv" EXACT []
is_a: UO:0000130 ! dose equivalent unit
created_by: george gkoutos */

#define UO_MICROSIEVERT_ID "UO:0000139"
#define UO_MICROSIEVERT_NAME "microsievert"
/* def: "A dose equivalent unit which is equal to one millionth of a sievert or 10^[-6] Sv." [NIST:NIST "http://physics.nist.gov/cuu/Units/"]
subset: unit_slim
synonym: "uSv" EXACT []
is_a: UO:0000130 ! dose equivalent unit
created_by: george gkoutos */

#define UO_ROENTGEN_EQUIVALENT_MAN_ID "UO:0000140"
#define UO_ROENTGEN_EQUIVALENT_MAN_NAME "Roentgen equivalent man"
/* def: "A dose equivalent unit which when multiplied by hundred is equal to one sievert or 1 Sv. 1 Sv is equal to 100 rem." [Wikipedia:Wikipedia "http://www.wikipedia.org/"]
comment: Rem = absorbed dose (rad) x quality factor (Q). Q is unique to the type of incident radiation.
subset: unit_slim
synonym: "rem" EXACT []
is_a: UO:0000130 ! dose equivalent unit
created_by: george gkoutos */

#define UO_MICROGRAY_ID "UO:0000141"
#define UO_MICROGRAY_NAME "microgray"
/* def: "An absorbed dose unit which is equal to one millionth of a gray or 10^[-6] Gy." [NIST:NIST "http://physics.nist.gov/cuu/Units/"]
subset: unit_slim
synonym: "uGy" EXACT []
is_a: UO:0000129 ! absorbed dose unit
created_by: george gkoutos */

#define UO_MILLIGRAY_ID "UO:0000142"
#define UO_MILLIGRAY_NAME "milligray"
/* def: "An absorbed dose unit which is equal to one thousandth of a gray or 10^[-3] Gy." [NIST:NIST "http://physics.nist.gov/cuu/Units/"]
subset: unit_slim
synonym: "mGy" EXACT []
is_a: UO:0000129 ! absorbed dose unit
created_by: george gkoutos */

#define UO_NANOGRAY_ID "UO:0000143"
#define UO_NANOGRAY_NAME "nanogray"
/* def: "An absorbed dose unit which is equal to one thousandth of a millionth of a gray or 10^[-9] Gy." [NIST:NIST "http://physics.nist.gov/cuu/Units/"]
subset: unit_slim
synonym: "nGy" EXACT []
is_a: UO:0000129 ! absorbed dose unit
created_by: george gkoutos */

#define UO_NANOSIEVERT_ID "UO:0000144"
#define UO_NANOSIEVERT_NAME "nanosievert"
/* def: "A dose equivalent unit which is equal to one thousandth of a millionth of a sievert or 10^[-9] Sv." [NIST:NIST "http://physics.nist.gov/cuu/Units/"]
subset: unit_slim
synonym: "nSv" EXACT []
is_a: UO:0000130 ! dose equivalent unit
created_by: george gkoutos */

#define UO_MILLICURIE_ID "UO:0000145"
#define UO_MILLICURIE_NAME "millicurie"
/* def: "An activity (of a radionuclide) unit which is equal to one thousandth of a curie or 10^[-3] Ci." [ORCBS:ORCBS "http://www.orcbs.msu.edu/"]
subset: unit_slim
synonym: "mCi" EXACT []
is_a: UO:0000128 ! activity (of a radionuclide) unit
created_by: george gkoutos */

#define UO_MICROCURIE_ID "UO:0000146"
#define UO_MICROCURIE_NAME "microcurie"
/* def: "An activity (of a radionuclide) unit which is equal to one millionth of a curie or 10^[-6] Ci." [ORCBS:ORCBS "http://www.orcbs.msu.edu/"]
subset: unit_slim
synonym: "uCi" EXACT []
is_a: UO:0000128 ! activity (of a radionuclide) unit
created_by: george gkoutos */

#define UO_DISINTEGRATIONS_PER_MINUTE_ID "UO:0000147"
#define UO_DISINTEGRATIONS_PER_MINUTE_NAME "disintegrations per minute"
/* def: "An activity (of a radionuclide) unit which is equal to the activity of a quantity of radioactive material in which one nucleus decays per minute or there is one atom disintegration per minute." [ORCBS:ORCBS "http://www.orcbs.msu.edu/"]
subset: unit_slim
synonym: "dpm" EXACT []
is_a: UO:0000128 ! activity (of a radionuclide) unit
created_by: george gkoutos */

#define UO_COUNTS_PER_MINUTE_ID "UO:0000148"
#define UO_COUNTS_PER_MINUTE_NAME "counts per minute"
/* def: "An activity (of a radionuclide) unit which is equal to the number of light emissions produced by ionizing radiation in one minute." [ORCBS:ORCBS "http://www.orcbs.msu.edu/"]
subset: unit_slim
synonym: "cpm" EXACT []
is_a: UO:0000128 ! activity (of a radionuclide) unit
created_by: george gkoutos */

#define UO_NANOSECOND_ID "UO:0000150"
#define UO_NANOSECOND_NAME "nanosecond"
/* def: "A time unit which is equal to one thousandth of one millionth of a second or 10^[-9] s." [NIST:NIST "http://physics.nist.gov/cuu/Units/"]
subset: unit_slim
synonym: "ns" EXACT []
is_a: UO:0000003 ! time unit
created_by: george gkoutos */

#define UO_CENTURY_ID "UO:0000151"
#define UO_CENTURY_NAME "century"
/* def: "A time unit which is equal to 100 years." [Wikipedia:Wikipedia "http://www.wikipedia.org/"]
subset: unit_slim
is_a: UO:0000003 ! time unit
created_by: george gkoutos */

#define UO_HALF_LIFE_ID "UO:0000152"
#define UO_HALF_LIFE_NAME "half life"
/* def: "A time unit which represents the period over which the activity or concentration of a specified chemical or element falls to half its original activity or concentration." [MGED:MGED "http://mged.sourceforge.net/ontologies/MGEDontology.php"]
comment: Typically applied to the half life of radioactive atoms but also applicable to any other situation where the population is of molecules of diminishing concentration or activity.
subset: unit_slim
is_a: UO:0000003 ! time unit
created_by: george gkoutos */

#define UO_FOOT_CANDLE_ID "UO:0000153"
#define UO_FOOT_CANDLE_NAME "foot candle"
/* def: "An illuminance unit which is equal to the illuminance produced by 1 lumen evenly spread over an area 1 foot^[2]. One footcandle is equal to 10.76 lux." [Wikipedia:Wikipedia "http://www.wikipedia.org/"]
subset: unit_slim
synonym: "ft-c" EXACT []
is_a: UO:0000115 ! illuminance unit
created_by: george gkoutos */

#define UO_IRRADIANCE_UNIT_ID "UO:0000154"
#define UO_IRRADIANCE_UNIT_NAME "irradiance unit"
/* def: "A unit which is a standard measure of the power of electromagnetic radiation at a surface, per unit area." [Wikipedia:Wikipedia "http://www.wikipedia.org/"]
subset: unit_group_slim
is_a: UO:0000157 ! light unit
created_by: george gkoutos */

#define UO_WATT_PER_SQUARE_METER_ID "UO:0000155"
#define UO_WATT_PER_SQUARE_METER_NAME "watt per square meter"
/* def: "An irradiance unit which is equal to 1 watt of radiant power incident per one square meter surface area." [NIST:NIST "http://physics.nist.gov/cuu/Units/"]
subset: unit_slim
synonym: "W/m^[2]" EXACT []
is_a: UO:0000154 ! irradiance unit
created_by: george gkoutos */

#define UO_EINSTEIN_PER_SQUARE_METER_PER_SECOND_ID "UO:0000156"
#define UO_EINSTEIN_PER_SQUARE_METER_PER_SECOND_NAME "einstein per square meter per second"
/* def: "An irradiance unit which is equal to one einstein per square meter per second. One einstein is one mole of photons, regardless of their frequency. Therefore, the number of photons in an einstein is Avogadro's number." [Wikipedia:Wikipedia "http://www.wikipedia.org/"]
subset: unit_slim
synonym: "einstein/sm^[2]" EXACT []
synonym: "mole per second and square meter mol/sm^2" EXACT []
is_a: UO:0000154 ! irradiance unit
created_by: george gkoutos */

#define UO_LIGHT_UNIT_ID "UO:0000157"
#define UO_LIGHT_UNIT_NAME "light unit"
/* def: "A unit which is a standard measure of the intensity of light." [NIST:NIST "http://physics.nist.gov/cuu/Units/"]
subset: unit_group_slim
is_a: UO:0000000 ! unit
created_by: george gkoutos */

#define UO_WATT_PER_STERADIAN_PER_SQUARE_METER_ID "UO:0000158"
#define UO_WATT_PER_STERADIAN_PER_SQUARE_METER_NAME "watt per steradian per square meter"
/* def: "A radiance unit which is equal to one watt of radiant power incident per steradian solid angle per one square meter projected area of the source, as viewed from the given direction." [NIST:NIST "http://physics.nist.gov/cuu/Units/"]
subset: unit_slim
synonym: "W/sr m^[2]" EXACT []
is_a: UO:0000161 ! radiance unit
created_by: george gkoutos */

#define UO_RADIANT_INTENSITY_UNIT_ID "UO:0000159"
#define UO_RADIANT_INTENSITY_UNIT_NAME "radiant intensity unit"
/* def: "A unit which is a standard measure of the intensity of electromagnetic radiation." [Wikipedia:Wikipedia "http://www.wikipedia.org/"]
subset: unit_group_slim
is_a: UO:0000157 ! light unit
relationship: unit_of PATO:0001717
created_by: george gkoutos */

#define UO_MICROEINSTEIN_PER_SQUARE_METER_PER_SECOND_ID "UO:0000160"
#define UO_MICROEINSTEIN_PER_SQUARE_METER_PER_SECOND_NAME "microeinstein per square meter per second"
/* def: "An irradiance unit which is equal to one microeinstein per square meter per second or 10^[-6] microeinstein/sm^[2]." [Wikipedia:Wikipedia "http://www.wikipedia.org/"]
subset: unit_slim
synonym: "micromole per second and square meter mmol/sm^2" EXACT []
synonym: "umicroeinstein/sm^[2]" EXACT []
is_a: UO:0000154 ! irradiance unit
created_by: george gkoutos */

#define UO_RADIANCE_UNIT_ID "UO:0000161"
#define UO_RADIANCE_UNIT_NAME "radiance unit"
/* def: "A unit which is a standard measure of the power of electromagnetic radiation through space or through a material medium in the form of electromagnetic waves." [Wikipedia:Wikipedia "http://www.wikipedia.org/"]
subset: unit_group_slim
is_a: UO:0000157 ! light unit
relationship: unit_of PATO:0001299
created_by: george gkoutos */

#define UO_WATT_PER_STERADIAN_ID "UO:0000162"
#define UO_WATT_PER_STERADIAN_NAME "watt per steradian"
/* def: "A radiant intensity unit which is equal to one kilogram meter squared per second cubed per steradian." [NIST:NIST "http://physics.nist.gov/cuu/Units/"]
subset: unit_slim
synonym: "W/sr" EXACT []
is_a: UO:0000159 ! radiant intensity unit
created_by: george gkoutos */

#define UO_MASS_PERCENTAGE_ID "UO:0000163"
#define UO_MASS_PERCENTAGE_NAME "mass percentage"
/* def: "A dimensionless concentration unit which denotes the mass of a substance in a mixture as a percentage of the mass of the entire mixture." [Wikipedia:Wikipedia "http://www.wikipedia.org/"]
subset: unit_slim
synonym: "w/w" EXACT []
synonym: "weight-weight percentage" EXACT []
is_a: UO:0000051 ! concentration unit
is_a: UO:0000187 ! percent
created_by: george gkoutos */

#define UO_MASS_VOLUME_PERCENTAGE_ID "UO:0000164"
#define UO_MASS_VOLUME_PERCENTAGE_NAME "mass volume percentage"
/* def: "A dimensionless concentration unit which denotes the mass of the substance in a mixture as a percentage of the volume of the entire mixture." [UOC:GVG]
subset: unit_slim
synonym: "(w/v)" EXACT []
synonym: "weight-volume percentage" EXACT []
is_a: UO:0000051 ! concentration unit
is_a: UO:0000187 ! percent
created_by: george gkoutos */

#define UO_VOLUME_PERCENTAGE_ID "UO:0000165"
#define UO_VOLUME_PERCENTAGE_NAME "volume percentage"
/* def: "A dimensionless concentration unit which denotes the volume of the solute in mL per 100 mL of the resulting solution." [UOC:GVG]
subset: unit_slim
synonym: "% (v/v)" EXACT []
is_a: UO:0000187 ! percent
is_a: UO:0000205 ! volume per unit volume
created_by: george gkoutos */

#define UO_PARTS_PER_NOTATION_UNIT_ID "UO:0000166"
#define UO_PARTS_PER_NOTATION_UNIT_NAME "parts per notation unit"
/* def: "A dimensionless concentration notation which describes the amount of one substance in another. It is the ratio of the amount of the substance of interest to the amount of that substance plus the amount of the substance." [Wikipedia:Wikipedia "http://www.wikipedia.org/"]
subset: unit_group_slim
is_a: UO:0000186 ! dimensionless unit
created_by: george gkoutos */

#define UO_PARTS_PER_HUNDRED_ID "UO:0000167"
#define UO_PARTS_PER_HUNDRED_NAME "parts per hundred"
/* def: "A dimensionless concentration notation which denotes the amount of a given substance in a total amount of 100 regardless of the units of measure as long as they are the same." [UOC:GVG]
subset: unit_slim
synonym: "10^[-2]" EXACT []
synonym: "pph" EXACT []
is_a: UO:0000166 ! parts per notation unit
created_by: george gkoutos */

#define UO_PARTS_PER_THOUSAND_ID "UO:0000168"
#define UO_PARTS_PER_THOUSAND_NAME "parts per thousand"
/* def: "A dimensionless concentration notation which denotes the amount of a given substance in a total amount of 1000 regardless of the units of measure as long as they are the same." [UOC:GVG]
subset: unit_slim
synonym: "10^[-3]" EXACT []
synonym: "ppth" EXACT []
is_a: UO:0000166 ! parts per notation unit
created_by: george gkoutos */

#define UO_PARTS_PER_MILLION_ID "UO:0000169"
#define UO_PARTS_PER_MILLION_NAME "parts per million"
/* def: "A dimensionless concentration notation which denotes the amount of a given substance in a total amount of 1,000,000 regardless of the units of measure used as long as they are the same or 1 part in 10^[6]." [UOC:GVG]
subset: unit_slim
synonym: "10^[-6]" EXACT []
synonym: "ppm" EXACT []
is_a: UO:0000166 ! parts per notation unit
created_by: george gkoutos */

#define UO_PARTS_PER_BILLION_ID "UO:0000170"
#define UO_PARTS_PER_BILLION_NAME "parts per billion"
/* def: "A dimensionless concentration notation which denotes the amount of a given substance in a total amount of 1,000,000,000 regardless of the units of measure as long as they are the same or 1 part in 10^[9]." [UOC:GVG]
subset: unit_slim
synonym: "10^[-9]" EXACT []
synonym: "ppb" EXACT []
is_a: UO:0000166 ! parts per notation unit
created_by: george gkoutos */

#define UO_PARTS_PER_TRILLION_ID "UO:0000171"
#define UO_PARTS_PER_TRILLION_NAME "parts per trillion"
/* def: "A dimensionless concentration notation which denotes the amount of a given substance in a total amount of 1,000,000,000 regardless of the units of measure used as long as they are the same or 1 part in 10^[12]." [UOC:GVG]
subset: unit_slim
synonym: "10^[-12]" EXACT []
synonym: "ppt" EXACT []
is_a: UO:0000166 ! parts per notation unit
created_by: george gkoutos */

#define UO_PARTS_PER_QUADRILLION_ID "UO:0000172"
#define UO_PARTS_PER_QUADRILLION_NAME "parts per quadrillion"
/* def: "A dimensionless concentration notation which denotes the amount of a given substance in a total amount of 1,000,000,000,000 regardless of the units of measure used as long as they are the same or 1 part in 10^[15]." [UOC:GVG]
subset: unit_slim
synonym: "10^[-15]" EXACT []
synonym: "ppq" EXACT []
is_a: UO:0000166 ! parts per notation unit
created_by: george gkoutos */

#define UO_GRAM_PER_MILLILITER_ID "UO:0000173"
#define UO_GRAM_PER_MILLILITER_NAME "gram per milliliter"
/* def: "A mass unit density which is equal to mass of an object in grams divided by the volume in milliliter." [UOC:GVG]
subset: unit_slim
synonym: "g/ml" EXACT []
is_a: UO:0000052 ! mass density unit
created_by: george gkoutos */

#define UO_KILOGRAM_PER_LITER_ID "UO:0000174"
#define UO_KILOGRAM_PER_LITER_NAME "kilogram per liter"
/* def: "A mass unit density which is equal to mass of an object in kilograms divided by the volume in liters." [UOC:GVG]
subset: unit_slim
synonym: "kg/L" EXACT []
is_a: UO:0000052 ! mass density unit
created_by: george gkoutos */

#define UO_GRAM_PER_LITER_ID "UO:0000175"
#define UO_GRAM_PER_LITER_NAME "gram per liter"
/* def: "A mass unit density which is equal to mass of an object in grams divided by the volume in liters." [UOC:GVG]
subset: unit_slim
synonym: "g/L" EXACT []
is_a: UO:0000052 ! mass density unit
created_by: george gkoutos */

#define UO_MILLIGRAM_PER_MILLILITER_ID "UO:0000176"
#define UO_MILLIGRAM_PER_MILLILITER_NAME "milligram per milliliter"
/* def: "A mass unit density which is equal to mass of an object in milligrams divided by the volume in milliliters." [UOC:GVG]
subset: unit_slim
synonym: "mg/ml" EXACT []
is_a: UO:0000052 ! mass density unit
created_by: george gkoutos */

#define UO_UNIT_PER_VOLUME_UNIT_ID "UO:0000177"
#define UO_UNIT_PER_VOLUME_UNIT_NAME "unit per volume unit"
/* def: "A concentration unit which is a standard measure of the number of units, as an agreed arbitrary amount, of a given substance per a specific volume of solution." [Webmd:Webmd "http://www.webmd.com/hw/health_guide_atoz/aa74537.asp"]
subset: unit_group_slim
is_a: UO:0000051 ! concentration unit
created_by: george gkoutos */

#define UO_UNIT_PER_MILLILITER_ID "UO:0000178"
#define UO_UNIT_PER_MILLILITER_NAME "unit per milliliter"
/* def: "A unit per milliliter unit which is equal to one unit of an agreed arbitrary amount per one milliliter." [Webmd:Webmd "http://www.webmd.com/hw/health_guide_atoz/aa74537.asp"]
subset: unit_slim
synonym: "U/ml" EXACT []
is_a: UO:0000177 ! unit per volume unit
created_by: george gkoutos */

#define UO_UNIT_PER_LITER_ID "UO:0000179"
#define UO_UNIT_PER_LITER_NAME "unit per liter"
/* def: "A unit per milliliter unit which is equal to one unit of an agreed arbitrary amount per one liter." [UOC:GVG]
subset: unit_slim
synonym: "U/l" EXACT []
is_a: UO:0000177 ! unit per volume unit
created_by: george gkoutos */

#define UO_MASS_PER_UNIT_VOLUME_ID "UO:0000180"
#define UO_MASS_PER_UNIT_VOLUME_NAME "mass per unit volume"
/* def: "A concentration unit which is a standard measure of the mass of a substance in a given volume (density)." [Wikipedia:Wikipedia "http://www.wikipedia.org/"]
comment: For units and further information look under the mass density unit. For a sample of a specific pure elemental substance, the concentration is directly proportional to the density. However, concentration is not proportional to density in general.
subset: unit_slim
is_a: UO:0000051 ! concentration unit
created_by: george gkoutos */

#define UO_ENZYME_UNIT_ID "UO:0000181"
#define UO_ENZYME_UNIT_NAME "enzyme unit"
/* def: "A catalytic unit activity which is equal to the amount of the enzyme that catalyzes the conversion of 1 micro mole of substrate per minute." [Wikipedia:Wikipedia "http://www.wikipedia.org/"]
subset: unit_slim
synonym: "U" EXACT []
is_a: UO:0000119 ! catalytic activity unit
created_by: george gkoutos */

#define UO_DENSITY_UNIT_ID "UO:0000182"
#define UO_DENSITY_UNIT_NAME "density unit"
/* def: "A unit which is a standard measure of the influence exerted by some mass." [Wikipedia:Wikipedia "http://www.wikipedia.org/"]
subset: unit_group_slim
is_a: UO:0000000 ! unit
relationship: unit_of PATO:0001019
created_by: george gkoutos */

#define UO_LINEAR_DENSITY_UNIT_ID "UO:0000183"
#define UO_LINEAR_DENSITY_UNIT_NAME "linear density unit"
/* def: "A density unit which is a standard measure of the mass exerting an influence on a one-dimensional object." [Wikipedia:Wikipedia "http://www.wikipedia.org/"]
subset: unit_group_slim
is_a: UO:0000182 ! density unit
relationship: unit_of PATO:0001352
created_by: george gkoutos */

#define UO_KILOGRAM_PER_METER_ID "UO:0000184"
#define UO_KILOGRAM_PER_METER_NAME "kilogram per meter"
/* def: "An area density unit which is equal to the mass of an object in kilograms divided by one meter." [NIST:NIST "http://physics.nist.gov/cuu/Units/"]
subset: unit_slim
synonym: "kg/m" EXACT []
is_a: UO:0000183 ! linear density unit
created_by: george gkoutos */

#define UO_DEGREE_ID "UO:0000185"
#define UO_DEGREE_NAME "degree"
/* def: "A plane angle unit which is equal to 1/360 of a full rotation or 1.7453310^[-2] rad." [Wikipedia:Wikipedia "http://www.wikipedia.org/"]
subset: unit_slim
is_a: UO:0000122 ! plane angle unit
created_by: george gkoutos */

#define UO_DIMENSIONLESS_UNIT_ID "UO:0000186"
#define UO_DIMENSIONLESS_UNIT_NAME "dimensionless unit"
/* def: "A unit which is a standard measure of physical quantity consisting of only a numerical number without any units." [Wikipedia:Wikipedia "http://www.wikipedia.org/"]
subset: unit_group_slim
is_a: UO:0000000 ! unit
created_by: george gkoutos */

#define UO_PERCENT_ID "UO:0000187"
#define UO_PERCENT_NAME "percent"
/* def: "A dimensionless ratio unit which denotes numbers as fractions of 100." [Wikipedia:Wikipedia "http://www.wikipedia.org/"]
subset: unit_slim
synonym: "%" EXACT []
is_a: UO:0000190 ! ratio
created_by: george gkoutos */

#define UO_PI_ID "UO:0000188"
#define UO_PI_NAME "pi"
/* def: "A dimensionless unit which denoted an irrational real number, approximately equal to 3.14159 which is the ratio of a circle's circumference to its diameter in Euclidean geometry." [Wikipedia:Wikipedia "http://www.wikipedia.org/"]
subset: unit_slim
is_a: UO:0000186 ! dimensionless unit
created_by: george gkoutos */

#define UO_COUNT_UNIT_ID "UO:0000189"
#define UO_COUNT_UNIT_NAME "count unit"
/* def: "A dimensionless unit which denotes a simple count of things." [MGED:MGED "http://mged.sourceforge.net/ontologies/MGEDontology.php"]
subset: unit_group_slim
synonym: "count" EXACT []
is_a: UO:0000186 ! dimensionless unit
created_by: george gkoutos */

#define UO_RATIO_ID "UO:0000190"
#define UO_RATIO_NAME "ratio"
/* def: "A dimensionless unit which denotes an amount or magnitude of one quantity relative to another." [Wikipedia:Wikipedia "http://www.wikipedia.org/"]
subset: unit_group_slim
is_a: UO:0000186 ! dimensionless unit
created_by: george gkoutos */

#define UO_FRACTION_ID "UO:0000191"
#define UO_FRACTION_NAME "fraction"
/* def: "A dimensionless ratio unit which relates the part (the numerator) to the whole (the denominator)." [Wikipedia:Wikipedia "http://www.wikipedia.org/"]
subset: unit_slim
is_a: UO:0000190 ! ratio
created_by: george gkoutos */

#define UO_MOLECULE_COUNT_ID "UO:0000192"
#define UO_MOLECULE_COUNT_NAME "molecule count"
/* def: "A dimensionless count unit which denotes the number of molecules." [MGED:MGED "http://mged.sourceforge.net/ontologies/MGEDontology.php"]
subset: unit_slim
is_a: UO:0000189 ! count unit
created_by: george gkoutos */

#define UO_PURITY_PERCENTAGE_ID "UO:0000193"
#define UO_PURITY_PERCENTAGE_NAME "purity percentage"
/* def: "A dimensionless percent unit which denotes the homogeneity of a biomaterial." [MGED:MGED "http://mged.sourceforge.net/ontologies/MGEDontology.php"]
comment: An example of a biomaterial could be an e.g. tumor biopsy.
subset: unit_slim
is_a: UO:0000187 ! percent
created_by: george gkoutos */

#define UO_CONFLUENCE_PERCENTAGE_ID "UO:0000194"
#define UO_CONFLUENCE_PERCENTAGE_NAME "confluence percentage"
/* def: "A dimensionless percent unit which denotes the density of an attached or monolayer culture (e.g., cell culture)." [MGED:MGED "http://mged.sourceforge.net/ontologies/MGEDontology.php"]
subset: unit_slim
is_a: UO:0000187 ! percent
created_by: george gkoutos */

#define UO_DEGREE_FAHRENHEIT_ID "UO:0000195"
#define UO_DEGREE_FAHRENHEIT_NAME "degree fahrenheit"
/* def: "A temperature unit which is equal to 5/9ths of a kelvin. Negative 40 degrees Fahrenheit is equal to negative 40 degrees Celsius." [Wikipedia:Wikipedia "http://www.wikipedia.org/"]
comment: In this scale, the freezing point of water is 32 degrees Fahrenheit and the boiling point is 212 degrees, placing the boiling and freezing points of water 180 degrees apart. -40 degrees Fahrenheit is equal to -40 degrees Celsius.
subset: unit_slim
synonym: "F" EXACT []
is_a: UO:0000005 ! temperature unit
created_by: george gkoutos */

#define UO_PH_ID "UO:0000196"
#define UO_PH_NAME "pH"
/* def: "A dimensionless concentration notation which denotes the acidity of a solution in terms of activity of hydrogen ions (H+)." [Wikipedia:Wikipedia "http://www.wikipedia.org/"]
subset: unit_slim
is_a: UO:0000051 ! concentration unit
is_a: UO:0000186 ! dimensionless unit
relationship: unit_of PATO:0001428
created_by: george gkoutos */

#define UO_LITER_PER_KILOGRAM_ID "UO:0000197"
#define UO_LITER_PER_KILOGRAM_NAME "liter per kilogram"
/* def: "A specific volume unit which is equal to one liter volume occupied by one kilogram of a particular substance." [NIST:NIST "http://physics.nist.gov/cuu/Units/"]
subset: unit_slim
synonym: "l/kg" EXACT []
is_a: UO:0000059 ! specific volume unit
created_by: george gkoutos */

#define UO_MILLILITER_PER_KILOGRAM_ID "UO:0000198"
#define UO_MILLILITER_PER_KILOGRAM_NAME "milliliter per kilogram"
/* def: "A specific volume unit which is equal to a thousandth of a liter per kilogram or 10^[-3] l/kg." [NIST:NIST "http://physics.nist.gov/cuu/Units/"]
subset: unit_slim
synonym: "ml/kg" EXACT []
is_a: UO:0000059 ! specific volume unit
created_by: george gkoutos */

#define UO_MICROLITER_PER_KILOGRAM_ID "UO:0000199"
#define UO_MICROLITER_PER_KILOGRAM_NAME "microliter per kilogram"
/* def: "A specific volume unit which is equal to one millionth of a liter per kilogram or 10^[-6] l/kg." [NIST:NIST "http://physics.nist.gov/cuu/Units/"]
subset: unit_slim
synonym: "ul/kg" EXACT []
is_a: UO:0000059 ! specific volume unit
created_by: george gkoutos */

#define UO_CELL_CONCENTRATION_UNIT_ID "UO:0000200"
#define UO_CELL_CONCENTRATION_UNIT_NAME "cell concentration unit"
/* def: "A concentration unit which denotes the average cell number in a given volume." [Bioedonline:Bioedonline "http://www.bioedonline.org/"]
subset: unit_group_slim
is_a: UO:0000051 ! concentration unit
created_by: george gkoutos */

#define UO_CELLS_PER_MILLILITER_ID "UO:0000201"
#define UO_CELLS_PER_MILLILITER_NAME "cells per milliliter"
/* def: "A unit of cell concentration which is equal to one cell in a volume of 1 milliliter." [Bioedonline:Bioedonline "http://www.bioedonline.org/"]
subset: unit_slim
synonym: "cells per ml" EXACT []
is_a: UO:0000200 ! cell concentration unit
created_by: george gkoutos */

#define UO_CATALYTIC__ACTIVITY__CONCENTRATION_UNIT_ID "UO:0000202"
#define UO_CATALYTIC__ACTIVITY__CONCENTRATION_UNIT_NAME "catalytic (activity) concentration unit"
/* def: "A concentration unit which is a standard measure of the amount of the action of a catalyst in a given volume." [UOC:GVG]
subset: unit_group_slim
is_a: UO:0000051 ! concentration unit
relationship: unit_of PATO:0001674
created_by: george gkoutos */

#define UO_KATAL_PER_CUBIC_METER_ID "UO:0000203"
#define UO_KATAL_PER_CUBIC_METER_NAME "katal per cubic meter"
/* def: "A catalytic (activity) concentration unit which is equal to 1 katal activity of a catalyst in a given volume of one cubic meter." [NIST:NIST "http://physics.nist.gov/cuu/Units/"]
subset: unit_slim
synonym: "kat/m^[3]" EXACT []
is_a: UO:0000202 ! catalytic (activity) concentration unit
created_by: george gkoutos */

#define UO_KATAL_PER_LITER_ID "UO:0000204"
#define UO_KATAL_PER_LITER_NAME "katal per liter"
/* def: "A catalytic (activity) concentration unit which is equal to 1 katal activity of a catalyst in a given volume of one thousandth of a cubic meter." [NIST:NIST "http://physics.nist.gov/cuu/Units/"]
subset: unit_slim
synonym: "kat/l" EXACT []
is_a: UO:0000202 ! catalytic (activity) concentration unit
created_by: george gkoutos */

#define UO_VOLUME_PER_UNIT_VOLUME_ID "UO:0000205"
#define UO_VOLUME_PER_UNIT_VOLUME_NAME "volume per unit volume"
/* def: "A dimensionless concentration unit which denotes the given volume of the solute in the total volume of the resulting solution." [NIST:NIST "http://physics.nist.gov/cuu/Units/"]
subset: unit_group_slim
is_a: UO:0000051 ! concentration unit
created_by: george gkoutos */

#define UO_MILLILITER_PER_CUBIC_METER_ID "UO:0000206"
#define UO_MILLILITER_PER_CUBIC_METER_NAME "milliliter per cubic meter"
/* def: "A volume per unit volume unit which is equal to one millionth of a liter of solute in one cubic meter of solution." [NIST:NIST "http://physics.nist.gov/cuu/Units/"]
subset: unit_slim
synonym: "ml/m^[3]" EXACT []
is_a: UO:0000205 ! volume per unit volume
created_by: george gkoutos */

#define UO_MILLILITER_PER_LITER_ID "UO:0000207"
#define UO_MILLILITER_PER_LITER_NAME "milliliter per liter"
/* def: "A volume per unit volume unit which is equal to one millionth of a liter of solute in one liter of solution." [NIST:NIST "http://physics.nist.gov/cuu/Units/"]
subset: unit_slim
synonym: "ml/l" EXACT []
is_a: UO:0000205 ! volume per unit volume
created_by: george gkoutos */

#define UO_GRAM_PER_DECILITER_ID "UO:0000208"
#define UO_GRAM_PER_DECILITER_NAME "gram per deciliter"
/* def: "A mass density unit which is equal to mass of an object in grams divided by the volume in deciliters." [UOC:GVG]
subset: unit_slim
synonym: "g/dl" EXACT []
is_a: UO:0000052 ! mass density unit
created_by: george gkoutos */

#define UO_DECILITER_ID "UO:0000209"
#define UO_DECILITER_NAME "deciliter"
/* def: "A volume unit which is equal to one tenth of a liter or 10^[-1] L." [NIST:NIST "http://physics.nist.gov/cuu/Units/"]
subset: unit_slim
synonym: "dl" EXACT []
is_a: UO:0000095 ! volume unit
created_by: george gkoutos */

#define UO_COLONY_FORMING_UNIT_ID "UO:0000210"
#define UO_COLONY_FORMING_UNIT_NAME "colony forming unit"
/* def: "A dimensionless count unit which a measure of viable bacterial numbers." [Wikipedia:Wikipedia "http://www.wikipedia.org/"]
subset: unit_slim
synonym: "cfu" EXACT []
is_a: UO:0000189 ! count unit
created_by: george gkoutos */

#define UO_PLAQUE_FORMING_UNIT_ID "UO:0000211"
#define UO_PLAQUE_FORMING_UNIT_NAME "plaque forming unit"
/* def: "A dimensionless count unit which a measure of plague forming units in a given volume." [Wikipedia:Wikipedia "http://www.wikipedia.org/"]
subset: unit_slim
synonym: "pfu" EXACT []
is_a: UO:0000189 ! count unit
created_by: george gkoutos */

#define UO_COLONY_FORMING_UNIT_PER_VOLUME_ID "UO:0000212"
#define UO_COLONY_FORMING_UNIT_PER_VOLUME_NAME "colony forming unit per volume"
/* def: "A concentration unit which a measure of viable bacterial numbers in a given volume." [Wikipedia:Wikipedia "http://www.wikipedia.org/"]
subset: unit_group_slim
is_a: UO:0000051 ! concentration unit
created_by: george gkoutos */

#define UO_COLONY_FORMING_UNIT_PER_MILLILITER_ID "UO:0000213"
#define UO_COLONY_FORMING_UNIT_PER_MILLILITER_NAME "colony forming unit per milliliter"
/* def: "A colony forming unit which a measure of viable bacterial numbers in one milliliter." [Wikipedia:Wikipedia "http://www.wikipedia.org/"]
subset: unit_slim
synonym: "cfu/ml" EXACT []
is_a: UO:0000212 ! colony forming unit per volume
created_by: george gkoutos */

#define UO_PLAQUE_FORMING_UNIT_PER_VOLUME_ID "UO:0000214"
#define UO_PLAQUE_FORMING_UNIT_PER_VOLUME_NAME "plaque forming unit per volume"
/* def: "A concentration unit which a measure of plague forming units in a given volume." [UOC:GVG]
subset: unit_group_slim
is_a: UO:0000051 ! concentration unit
created_by: george gkoutos */

#define UO_PLAQUE_FORMING_UNIT_PER_MILLILITER_ID "UO:0000215"
#define UO_PLAQUE_FORMING_UNIT_PER_MILLILITER_NAME "plaque forming unit per milliliter"
/* def: "A concentration unit which a measure of plague forming units in one milliliter." [UOC:GVG]
subset: unit_slim
synonym: "pfu/ml" EXACT []
is_a: UO:0000214 ! plaque forming unit per volume
created_by: george gkoutos */

#define UO_DISINTEGRATIONS_PER_SECOND_ID "UO:0000216"
#define UO_DISINTEGRATIONS_PER_SECOND_NAME "disintegrations per second"
/* def: "An activity (of a radionuclide) unit which is equal to the activity of a quantity of radioactive material in which one nucleus decays per second or there is one atom disintegration per second." [ORCBS:ORCBS "http://www.orcbs.msu.edu/"]
subset: unit_slim
synonym: "dps" EXACT []
is_a: UO:0000128 ! activity (of a radionuclide) unit
created_by: george gkoutos */

#define UO_ELECTRIC_POTENTIAL_DIFFERENCE_UNIT_ID "UO:0000217"
#define UO_ELECTRIC_POTENTIAL_DIFFERENCE_UNIT_NAME "electric potential difference unit"
/* def: "A unit which is a standard measure of the work done per unit charge as a charge is moved between two points in an electric field." [Wikipedia:Wikipedia "http://www.wikipedia.org/"]
subset: unit_group_slim
is_a: UO:0000000 ! unit
relationship: unit_of PATO:0001464
created_by: george gkoutos */

#define UO_VOLT_ID "UO:0000218"
#define UO_VOLT_NAME "volt"
/* def: "An electric potential difference unit which is equal to the work per unit charge. One volt is the potential difference required to move one coulomb of charge between two points in a circuit while using one joule of energy." [Wikipedia:Wikipedia "http://www.wikipedia.org/"]
subset: unit_slim
synonym: "V" EXACT []
is_a: UO:0000217 ! electric potential difference unit
created_by: george gkoutos */

#define UO_ELECTRIC_CHARGE_ID "UO:0000219"
#define UO_ELECTRIC_CHARGE_NAME "electric charge"
/* def: "A unit which is a standard measure of the quantity of unbalanced electricity in a body (either positive or negative) and construed as an excess or deficiency of electrons." [WordNet:WordNet "http://wordnet.princeton.edu/"]
subset: unit_group_slim
is_a: UO:0000000 ! unit
created_by: george gkoutos */

#define UO_COULOMB_ID "UO:0000220"
#define UO_COULOMB_NAME "coulomb"
/* def: "An electrical charge unit which is equal to the amount of charge transferred by a current of 1 ampere in 1 second." [WordNet:WordNet "http://wordnet.princeton.edu/"]
subset: unit_slim
synonym: "C" EXACT []
is_a: UO:0000219 ! electric charge
created_by: george gkoutos */

#define UO_DALTON_ID "UO:0000221"
#define UO_DALTON_NAME "dalton"
/* def: "An independently to the base SI units defined mass unit which is equal to one twelfth of the mass of an unbound atom of the carbon-12 nuclide, at rest and in its ground state." [Wikipedia:Wikipedia "http://www.wikipedia.org/"]
subset: unit_slim
synonym: "Da" EXACT []
synonym: "u" EXACT []
synonym: "unified atomic mass unit" EXACT []
is_a: UO:0000002 ! mass unit
created_by: george gkoutos */

#define UO_KILODALTON_ID "UO:0000222"
#define UO_KILODALTON_NAME "kilodalton"
/* def: "A mass unit which is equal to one thousand daltons." [Wikipedia:Wikipedia "http://www.wikipedia.org/"]
subset: unit_slim
synonym: "kDa" EXACT []
is_a: UO:0000002 ! mass unit
created_by: george gkoutos */

#define UO_WATT_HOUR_ID "UO:0000223"
#define UO_WATT_HOUR_NAME "watt-hour"
/* def: "An energy unit which is equal to the amount of electrical energy equivalent to a one-watt load drawing power for one hour." [Wikipedia:Wikipedia "http://www.wikipedia.org/"]
subset: unit_slim
synonym: "Wh" EXACT []
is_a: UO:0000111 ! energy unit
created_by: george gkoutos */

#define UO_KILOWATT_HOUR_ID "UO:0000224"
#define UO_KILOWATT_HOUR_NAME "kilowatt-hour"
/* def: "An energy unit which is equal to 1,000 watt-hours." [Wikipedia:Wikipedia "http://www.wikipedia.org/"]
subset: unit_slim
is_a: UO:0000111 ! energy unit
created_by: george gkoutos */

#define UO_MAGNETIC_FLUX_UNIT_ID "UO:0000225"
#define UO_MAGNETIC_FLUX_UNIT_NAME "magnetic flux unit"
/* def: "A unit which is a standard measure of quantity of magnetism, taking account of the strength and the extent of a magnetic field." [Wikipedia:Wikipedia "http://www.wikipedia.org/"]
subset: unit_group_slim
is_a: UO:0000000 ! unit
relationship: unit_of PATO:0001682
created_by: george gkoutos */

#define UO_WEBER_ID "UO:0000226"
#define UO_WEBER_NAME "weber"
/* def: "A magnetic flux unit which is equal to the amount of flux that when linked with a single turn of wire for an interval of one second will induce an electromotive force of one volt." [ScienceLobby:ScienceLobby "www.sciencelobby.com/dictionary/w.html"]
subset: unit_slim
synonym: "V s" EXACT []
synonym: "volt-second" RELATED []
synonym: "Wb" EXACT []
is_a: UO:0000225 ! magnetic flux unit
created_by: george gkoutos */

#define UO_MAGNETIC_FLUX_DENSITY_UNIT_ID "UO:0000227"
#define UO_MAGNETIC_FLUX_DENSITY_UNIT_NAME "magnetic flux density unit"
/* def: "A unit which is a standard measure of the strength of a magnetic field." [allnet:allnet "www.all.net/books/standards/remnants/glo.html"]
subset: unit_group_slim
synonym: "B" EXACT []
is_a: UO:0000000 ! unit
created_by: george gkoutos */

#define UO_TESLA_ID "UO:0000228"
#define UO_TESLA_NAME "tesla"
/* def: "A magnetic flux density unit which is equal to one weber per square meter." [WordNet:WordNet "http://wordnet.princeton.edu/"]
subset: unit_slim
synonym: "T" EXACT []
synonym: "Wb/m2" EXACT []
is_a: UO:0000227 ! magnetic flux density unit
created_by: george gkoutos */

#define UO_VOLT_HOUR_ID "UO:0000229"
#define UO_VOLT_HOUR_NAME "volt-hour"
/* def: "A magnetic flux unit which is equal to 3600 Wb." [UOC:GVG]
subset: unit_slim
synonym: "Vh" EXACT []
is_a: UO:0000225 ! magnetic flux unit
created_by: george gkoutos */

#define UO_KILOVOLT_HOUR_ID "UO:0000230"
#define UO_KILOVOLT_HOUR_NAME "kilovolt-hour"
/* def: "A magnetic flux unit which is equal to one thousand volt-hours." [UOC:GVG]
subset: unit_slim
synonym: "kVh" EXACT []
is_a: UO:0000225 ! magnetic flux unit
created_by: george gkoutos */

#define UO_INFORMATION_UNIT_ID "UO:0000231"
#define UO_INFORMATION_UNIT_NAME "information unit"
/* def: "A unit which is a standard measure of the amount of information." [Wikipedia:Wikipedia "http://www.wikipedia.org/"]
subset: unit_group_slim
is_a: UO:0000000 ! unit
created_by: george gkoutos */

#define UO_BIT_ID "UO:0000232"
#define UO_BIT_NAME "bit"
/* def: "An information unit which refers to a digit in the binary numeral system, which consists of base 2 digits (ie there are only 2 possible values: 0 or 1)." [Wikipedia:Wikipedia "http://www.wikipedia.org/"]
subset: unit_slim
is_a: UO:0000231 ! information unit
created_by: george gkoutos */

#define UO_BYTE_ID "UO:0000233"
#define UO_BYTE_NAME "byte"
/* def: "An information unit which is equal to 8 bits." [Wikipedia:Wikipedia "http://www.wikipedia.org/"]
subset: unit_slim
synonym: "B" EXACT []
is_a: UO:0000231 ! information unit
created_by: george gkoutos */

#define UO_KILOBYTE_ID "UO:0000234"
#define UO_KILOBYTE_NAME "kilobyte"
/* def: "An information unit which is equal to 1000 bytes." [Wikipedia:Wikipedia "http://www.wikipedia.org/"]
subset: unit_slim
synonym: "kB" EXACT []
is_a: UO:0000231 ! information unit
created_by: george gkoutos */

#define UO_MEGABYTE_ID "UO:0000235"
#define UO_MEGABYTE_NAME "megabyte"
/* def: "An information unit which is equal to 1000 kB." [Wikipedia:Wikipedia "http://www.wikipedia.org/"]
subset: unit_slim
synonym: "MB" EXACT []
is_a: UO:0000231 ! information unit
created_by: george gkoutos */

#define UO_IMAGE_RESOLUTION_UNIT_ID "UO:0000236"
#define UO_IMAGE_RESOLUTION_UNIT_NAME "image resolution unit"
/* def: "An information unit which is a standard measure of the detail an image holds." [Wikipedia:Wikipedia "http://www.wikipedia.org/"]
subset: unit_group_slim
is_a: UO:0000231 ! information unit
created_by: george gkoutos */

#define UO_CHROMA_SAMPLING_UNIT_ID "UO:0000237"
#define UO_CHROMA_SAMPLING_UNIT_NAME "chroma sampling unit"
/* def: "An image resolution unit which is a standard measure of the amount of spatial detail in an image." [Wikipedia:Wikipedia "http://www.wikipedia.org/"]
subset: unit_slim
is_a: UO:0000236 ! image resolution unit
created_by: george gkoutos */

#define UO_DYNAMIC_RANGE_UNIT_ID "UO:0000238"
#define UO_DYNAMIC_RANGE_UNIT_NAME "dynamic range unit"
/* def: "An image resolution unit which is a standard measure of the amount of contrast available in a pixel." [Wikipedia:Wikipedia "http://www.wikipedia.org/"]
subset: unit_slim
is_a: UO:0000236 ! image resolution unit
created_by: george gkoutos */

#define UO_SPATIAL_RESOLUTION_UNIT_ID "UO:0000239"
#define UO_SPATIAL_RESOLUTION_UNIT_NAME "spatial resolution unit"
/* def: "An image resolution unit which is a standard measure of the way luminance and chrominance may be sampled at different levels." [Wikipedia:Wikipedia "http://www.wikipedia.org/"]
subset: unit_group_slim
is_a: UO:0000236 ! image resolution unit
created_by: george gkoutos */

#define UO_DOTS_PER_INCH_ID "UO:0000240"
#define UO_DOTS_PER_INCH_NAME "dots per inch"
/* def: "A spatial resolution unit which is a standard measure of the printing resolution, in particular the number of individual dots of ink a printer or toner can produce within a linear one-inch space." [Wikipedia:Wikipedia "http://www.wikipedia.org/"]
subset: unit_slim
synonym: "dpi" EXACT []
is_a: UO:0000239 ! spatial resolution unit
created_by: george gkoutos */

#define UO_MICRON_PIXEL_ID "UO:0000241"
#define UO_MICRON_PIXEL_NAME "micron pixel"
/* def: "A spatial resolution unit which is equal to a pixel size of one micrometer." [Wikipedia:Wikipedia "http://www.wikipedia.org/"]
subset: unit_slim
synonym: "micrometer pixel" EXACT []
is_a: UO:0000239 ! spatial resolution unit
created_by: george gkoutos */

#define UO_PIXELS_PER_INCH_ID "UO:0000242"
#define UO_PIXELS_PER_INCH_NAME "pixels per inch"
/* def: "A spatial resolution unit which is a standard measure of the resolution of a computer display, related to the size of the display in inches and the total number of pixels in the horizontal and vertical directions." [Wikipedia:Wikipedia "http://www.wikipedia.org/"]
subset: unit_slim
synonym: "pixel density" EXACT []
synonym: "ppi" EXACT []
is_a: UO:0000239 ! spatial resolution unit
created_by: george gkoutos */

#define UO_PIXELS_PER_MILLIMETER_ID "UO:0000243"
#define UO_PIXELS_PER_MILLIMETER_NAME "pixels per millimeter"
/* def: "A spatial resolution unit which is a standard measure of the number of pixels in one millimeter length or width of a digital image divided by the physical length or width of a printed image." [Wikipedia:Wikipedia "http://www.wikipedia.org/"]
subset: unit_slim
is_a: UO:0000239 ! spatial resolution unit
created_by: george gkoutos */

#define UO_BASE_PAIR_ID "UO:0000244"
#define UO_BASE_PAIR_NAME "base pair"
/* def: "A count unit which contains one nucleotide." [UO:GVG]
subset: unit_slim
synonym: "bp" EXACT []
is_a: UO:0000189 ! count unit
created_by: george gkoutos */

#define UO_KIBIBYTE_ID "UO:0000245"
#define UO_KIBIBYTE_NAME "kibibyte"
/* def: "An information unit which is equal to 1024 B." [NIST:NIST "http://physics.nist.gov/cuu/Units/"]
subset: unit_slim
synonym: "KiB" EXACT []
is_a: UO:0000231 ! information unit
created_by: george gkoutos */

#define UO_MEBIBYTE_ID "UO:0000246"
#define UO_MEBIBYTE_NAME "mebibyte"
/* def: "An information unit which is equal to 1024 KiB." [NIST:NIST "http://physics.nist.gov/cuu/Units/"]
subset: unit_slim
synonym: "MiB" EXACT []
is_a: UO:0000231 ! information unit
created_by: george gkoutos */

#define UO_MILLIVOLT_ID "UO:0000247"
#define UO_MILLIVOLT_NAME "millivolt"
/* def: "An electric potential difference unit which is equal to one thousandth of a volt or 10^[-3] V." [UOC:GVG]
subset: unit_slim
synonym: "mV" EXACT []
is_a: UO:0000217 ! electric potential difference unit
created_by: george gkoutos */

#define UO_KILOVOLT_ID "UO:0000248"
#define UO_KILOVOLT_NAME "kilovolt"
/* def: "An electric potential difference unit which is equal to one thousand volts or 10^[3] V." [UOC:GVG]
synonym: "kV" EXACT []
is_a: UO:0000217 ! electric potential difference unit
created_by: george gkoutos */

#define UO_MICROVOLT_ID "UO:0000249"
#define UO_MICROVOLT_NAME "microvolt"
/* def: "An electric potential difference unit which is equal to one millionth of a volt or 10^[-6] V." [UOC:GVG]
subset: unit_slim
synonym: "uV" EXACT []
is_a: UO:0000217 ! electric potential difference unit
created_by: george gkoutos */

#define UO_NANOVOLT_ID "UO:0000250"
#define UO_NANOVOLT_NAME "nanovolt"
/* def: "An electric potential difference unit which is equal to one billionth of a volt or 10^[-12] V." [UOC:GVG]
subset: unit_slim
synonym: "nV" EXACT []
is_a: UO:0000217 ! electric potential difference unit
created_by: george gkoutos */

#define UO_PICOVOLT_ID "UO:0000251"
#define UO_PICOVOLT_NAME "picovolt"
/* def: "An electric potential difference unit which is equal to one trillionth of a volt or 10^[-12] V." [UOC:GVG]
subset: unit_slim
synonym: "pV" EXACT []
is_a: UO:0000217 ! electric potential difference unit
created_by: george gkoutos */

#define UO_MEGAVOLT_ID "UO:0000252"
#define UO_MEGAVOLT_NAME "megavolt"
/* def: "An electric potential difference unit which is equal to one million volts or 10^[6] V." [UOC:GVG]
subset: unit_slim
synonym: "MV" EXACT []
is_a: UO:0000217 ! electric potential difference unit
created_by: george gkoutos */

#define UO_SURFACE_TENSION_UNIT_ID "UO:0000253"
#define UO_SURFACE_TENSION_UNIT_NAME "surface tension unit"
/* def: "A unit which is a standard measure of the ability of a liguid to attraction of molecules at its surface as a result of unbalanced molecular cohesive forces." [NIST:NIST "http://physics.nist.gov/cuu/Units/"]
subset: unit_group_slim
is_a: UO:0000000 ! unit
relationship: unit_of PATO:0001461
created_by: george gkoutos */

#define UO_NEWTON_PER_METER_ID "UO:0000254"
#define UO_NEWTON_PER_METER_NAME "newton per meter"
/* def: "A surface tension unit which is equal to one newton per meter." [NIST:NIST "http://physics.nist.gov/cuu/Units/"]
subset: unit_slim
synonym: "N/m" EXACT []
is_a: UO:0000253 ! surface tension unit
created_by: george gkoutos */

#define UO_DYNE_PER_CM_ID "UO:0000255"
#define UO_DYNE_PER_CM_NAME "dyne per cm"
/* def: "A surface tension unit which is equal to one dyne per centimeter." [Wikipedia:Wikipedia "http://www.wikipedia.org"]
subset: unit_slim
synonym: "dyn/cm" EXACT []
is_a: UO:0000253 ! surface tension unit
created_by: george gkoutos */

#define UO_VISCOSITY_UNIT_ID "UO:0000256"
#define UO_VISCOSITY_UNIT_NAME "viscosity unit"
/* def: "A unit which is a standard measure of the internal resistance of fluids to flow." [UOC:GVG]
subset: unit_group_slim
is_a: UO:0000000 ! unit
relationship: unit_of PATO:0000992
created_by: george gkoutos */

#define UO_PASCAL_SECOND_ID "UO:0000257"
#define UO_PASCAL_SECOND_NAME "pascal second"
/* def: "A viscosity unit which is equal to one pascale per second." [NIST:NIST "http://physics.nist.gov/cuu/Units/"]
subset: unit_slim
synonym: "Pa  s" EXACT []
is_a: UO:0000256 ! viscosity unit
created_by: george gkoutos */

#define UO_POISE_ID "UO:0000258"
#define UO_POISE_NAME "poise"
/* def: "A viscosity unit which is equal to one dyne second per square centimeter." [Wikipedia:Wikipedia "http://www.wikipedia.org"]
subset: unit_slim
synonym: "dyne s/cm^2" EXACT []
synonym: "P" EXACT []
is_a: UO:0000256 ! viscosity unit
created_by: george gkoutos */

#define UO_DECIBEL_ID "UO:0000259"
#define UO_DECIBEL_NAME "decibel"
/* def: "An ratio unit which is an indicator of sound power per unit area." [techtarget:techtarget "http://searchsmb.techtarget.com/"]
subset: unit_slim
synonym: "dB" EXACT []
is_a: UO:0000190 ! ratio
relationship: unit_of PATO:0001521
created_by: george gkoutos */

#define UO_EFFECTIVE_DOSE_UNIT_ID "UO:0000260"
#define UO_EFFECTIVE_DOSE_UNIT_NAME "effective dose unit"
/* def: "A unit which is a standard measure of the estimate of the stochastic effect that a non-uniform radiation dose has on a human." [Wikipedia:Wikipedia "http://www.wikipedia.org"]
subset: unit_slim
is_a: UO:0000127 ! radiation unit
relationship: unit_of PATO:0001747
created_by: george gkoutos */

#define UO_CONDUCTION_UNIT_ID "UO:0000261"
#define UO_CONDUCTION_UNIT_NAME "conduction unit"
/* def: "A unit which represents a standard measurement of the transmission of an entity through a medium." [UOC:GVG]
subset: unit_group_slim
is_a: UO:0000000 ! unit
created_by: george gkoutos */

#define UO_ELECTRICAL_CONDUCTION_UNIT_ID "UO:0000262"
#define UO_ELECTRICAL_CONDUCTION_UNIT_NAME "electrical conduction unit"
/* def: "A unit which represents a standard measurement of the movement of electrically charged particles through a transmission medium (electrical conductor)." [UOC:GVG]
subset: unit_group_slim
is_a: UO:0000261 ! conduction unit
relationship: unit_of PATO:0001757
created_by: george gkoutos */

#define UO_HEAT_CONDUCTION_UNIT_ID "UO:0000263"
#define UO_HEAT_CONDUCTION_UNIT_NAME "heat conduction unit"
/* def: "A unit which represents a standard measurement of the spontaneous transfer of thermal energy through matter, from a region of higher temperature to a region of lower temperature." [Wikipedia:Wikipedia "http://www.wikipedia.org/"]
subset: unit_group_slim
is_a: UO:0000261 ! conduction unit
relationship: unit_of PATO:0001756
created_by: george gkoutos */

#define UO_SIEMENS_ID "UO:0000264"
#define UO_SIEMENS_NAME "siemens"
/* def: "An electrical conduction unit which is equal to A/V." [NIST:NIST "http://physics.nist.gov/cuu/Units/"]
subset: unit_slim
synonym: "A V^-1" EXACT []
synonym: "mho " RELATED []
synonym: "S" RELATED []
is_a: UO:0000262 ! electrical conduction unit
created_by: george gkoutos */

#define UO_WATT_PER_METER_KELVIN_ID "UO:0000265"
#define UO_WATT_PER_METER_KELVIN_NAME "watt per meter kelvin"
/* def: "An heat conduction unit which is equal to one watt divided by meter kelvin." [NIST:NIST "http://physics.nist.gov/cuu/Units/"]
subset: unit_slim
synonym: "W/m K" EXACT []
is_a: UO:0000263 ! heat conduction unit
created_by: george gkoutos */

#define UO_ELECTRONVOLT_ID "UO:0000266"
#define UO_ELECTRONVOLT_NAME "electronvolt"
/* def: "A non-SI unit of energy (eV) defined as the energy acquired by a single unbound electron when it passes through an electrostatic potential difference of one volt. An electronvolt is equal to 1.602 176 53(14) x 10^-19 J." [Wikipedia:Wikipedia "http://www.wikipedia.org/"]
subset: unit_slim
synonym: "electron volt" EXACT []
synonym: "eV" EXACT []
is_a: UO:0000111 ! energy unit
created_by: george gkoutos */

#define UO_ELECTRIC_FIELD_STRENGTH_UNIT_ID "UO:0000267"
#define UO_ELECTRIC_FIELD_STRENGTH_UNIT_NAME "electric field strength unit"
/* def: "The electric field strength is a unit which is a measure of the potential difference between two points some distance apart." [Wikipedia:http\://en.wikipedia.org/wiki/Electric_field]
subset: unit_group_slim
synonym: "E-field strength " EXACT []
is_a: UO:0000000 ! unit
created_by: george gkoutos
creation_date: 2009-03-03T12:23:16Z */

#define UO_VOLT_PER_METER_ID "UO:0000268"
#define UO_VOLT_PER_METER_NAME "volt per meter"
/* def: "The volt per meter is a unit of electric field strength equal to the a potential difference of 1 volt existing between two points that are 1 meter apart." [Wikipedia:http\://en.wikipedia.org/wiki/Electric_field]
subset: unit_slim
synonym: "V/m" EXACT []
is_a: UO:0000267 ! electric field strength unit
created_by: george gkoutos
creation_date: 2009-03-03T12:28:17Z */

#define UO_ABSORBANCE_UNIT_ID "UO:0000269"
#define UO_ABSORBANCE_UNIT_NAME "absorbance unit"
/* def: "A dimensionless logarithmic unit assigned to a measure of absorbance of light through a partially absorbing substance, defined as -log10(I/I_0) where I = transmitted light and I_0 = incident light." [Wikipedia:http\://en.wikipedia.org/wiki/Absorbance "http://en.wikipedia.org/wiki/Absorbance"]
subset: unit_slim
synonym: "AU" EXACT []
is_a: UO:0000186 ! dimensionless unit
created_by: george gkoutos
creation_date: 2009-07-14T12:33:48Z */

#define UO_VOLUMETRIC_FLOW_RATE_UNIT_ID "UO:0000270"
#define UO_VOLUMETRIC_FLOW_RATE_UNIT_NAME "volumetric flow rate unit"
/* def: "A unit which is a standard measure of the volume of fluid which passes through a given surface per unit time ." [Wikipedia:Wikipedia "http://www.wikipedia.org/"]
subset: unit_group_slim
is_a: UO:0000000 ! unit
created_by: george gkoutos */

#define UO_MICROLITERS_PER_MINUTE_ID "UO:0000271"
#define UO_MICROLITERS_PER_MINUTE_NAME "microliters per minute"
/* def: "A volumetric flow rate unit which is equal to one microliter volume through a given surface in one minute." [UOC:GVG]
subset: unit_slim
synonym: "uL/min" EXACT []
is_a: UO:0000270 ! volumetric flow rate unit
relationship: unit_of PATO:0001574
created_by: george gkoutos */

#define UO_MILLIMETRES_OF_MERCURY_ID "UO:0000272"
#define UO_MILLIMETRES_OF_MERCURY_NAME "millimetres of mercury"
/* def: "A unit of pressure equal to the amount of fluid pressure one millimeter deep in mercury at zero degrees centigrade on Earth." [url:en.wiktionary.org/wiki/mmHg]
subset: unit_slim
synonym: "mmHg" EXACT []
is_a: UO:0000109 ! pressure unit
created_by: george gkoutos */

#define UO_MILLIGRAM_PER_LITER_ID "UO:0000273"
#define UO_MILLIGRAM_PER_LITER_NAME "milligram per liter"
/* def: "A mass unit density which is equal to mass of an object in milligrams divided by the volume in liters." [UOC:GVG]
synonym: "mg/L" EXACT []
is_a: UO:0000052 ! mass density unit
created_by: george
creation_date: 2011-03-21T10:35:01Z */

#define UO_MICROGRAM_PER_MILLILITER_ID "UO:0000274"
#define UO_MICROGRAM_PER_MILLILITER_NAME "microgram per milliliter"
/* def: "A mass unit density which is equal to mass of an object in micrograms divided by the volume in millliters." [UOC:GVG]
synonym: "ug/ml" EXACT []
is_a: UO:0000052 ! mass density unit
created_by: George Gkoutos
creation_date: 2011-03-21T10:40:21Z */

#define UO_NANOGRAM_PER_MILLILITER_ID "UO:0000275"
#define UO_NANOGRAM_PER_MILLILITER_NAME "nanogram per milliliter"
/* def: "A mass unit density which is equal to mass of an object in nanograms divided by the volume in milliliters." [UOC:GVG]
synonym: "ng/ml" EXACT []
is_a: UO:0000052 ! mass density unit
created_by: George Gkoutos
creation_date: 2011-03-21T10:55:12Z */

#define UO_AMOUNT_PER_CONTAINER_ID "UO:0000276"
#define UO_AMOUNT_PER_CONTAINER_NAME "amount per container"
/* def: "A concentration unit which is a standard measure of the amount of a substance in a given container." [UOC:GVG]
subset: unit_group_slim
is_a: UO:0000051 ! concentration unit
created_by: george gkoutos */

#define UO_UG_DISK_ID "UO:0000277"
#define UO_UG_DISK_NAME "ug/disk"
/* def: "A unit which is equal to one microgram per disk, where a disk is some physical surface/container upon which the substance is deposited." [UOC:MD]
subset: unit_group_slim
is_a: UO:0000276 ! amount per container
created_by: george gkoutos */

#define UO_NMOLE_DISK_ID "UO:0000278"
#define UO_NMOLE_DISK_NAME "nmole/disk"
/* def: "A unit which is equal to one nanomole per disk, where a disk is some physical surface/container upon which the substance is deposited." [UOC:MD]
subset: unit_group_slim
is_a: UO:0000276 ! amount per container
created_by: george gkoutos */

#define UO_MILLIUNITS_PER_MILLILITER_ID "UO:0000279"
#define UO_MILLIUNITS_PER_MILLILITER_NAME "milliunits per milliliter"
/* def: "A unit per milliliter unit which is equal to one thousandth of a unit of an agreed arbitrary amount per one milliliter." [UOC:GVG]
subset: unit_slim
synonym: "mU/ml" EXACT []
is_a: UO:0000177 ! unit per volume unit
created_by: george gkoutos */

#define UO_RATE_UNIT_ID "UO:0000280"
#define UO_RATE_UNIT_NAME "rate unit"
/* def: "A unit which represents a standard measurement occurrence of a process per unit time." [UOC:GVG]
subset: unit_group_slim
is_a: UO:0000000 ! unit
created_by: george gkoutos */

#define UO_COUNT_PER_NANOMOLAR_SECOND_ID "UO:0000281"
#define UO_COUNT_PER_NANOMOLAR_SECOND_NAME "count per nanomolar second"
/* def: "A rate unit which is equal to one over one nanomolar second." [UOC:GVG]
subset: unit_slim
synonym: "nM^-1 s^-1" EXACT []
is_a: UO:0000280 ! rate unit
created_by: george gkoutos */

#define UO_COUNT_PER_MOLAR_SECOND_ID "UO:0000282"
#define UO_COUNT_PER_MOLAR_SECOND_NAME "count per molar second"
/* def: "A rate unit which is equal to one over one molar second." [UOC:GVG]
subset: unit_slim
synonym: "M^-1 s^-1" EXACT []
is_a: UO:0000280 ! rate unit
created_by: george gkoutos */

#define UO_KILOGRAM_PER_HECTARE_ID "UO:0000283"
#define UO_KILOGRAM_PER_HECTARE_NAME "kilogram per hectare"
/* def: "An area density unit which is equal to the mass of an object in kilograms divided by the surface area in hectares.." [UO:GVG]
is_a: UO:0000054 ! area density unit
created_by: George Gkoutos
creation_date: 2011-10-12T11:17:08Z */

#define UO_COUNT_PER_NANOMOLAR_ID "UO:0000284"
#define UO_COUNT_PER_NANOMOLAR_NAME "count per nanomolar"
/* def: "A rate unit which is equal to one over one nanomolar." [UO:GVG]
subset: unit_slim
synonym: "nM^-1" EXACT []
synonym: "1/nM" EXACT []
is_a: UO:0000280 ! rate unit */

#define UO_COUNT_PER_MOLAR_ID "UO:0000285"
#define UO_COUNT_PER_MOLAR_NAME "count per molar"
/* def: "A rate unit which is equal to one over one molar." [UO:GVG]
subset: unit_slim
synonym: "M^-1" EXACT []
synonym: "1/M" EXACT []
is_a: UO:0000280 ! rate unit
 */

#define UO_MICROGRAM_PER_LITER_ID "UO:0000301"
#define UO_MICROGRAM_PER_LITER_NAME "microgram per liter"
/* def: "A mass unit density which is equal to mass of an object in micrograms divided by the volume in liters." [UOC:GVG]
subset: unit_slim
synonym: "ug/L" EXACT []
is_a: UO:0000052 ! mass density unit
created_by: george gkoutos */
