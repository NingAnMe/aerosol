/************************************************************************
*                       mapiL1Bgeo utilities header file                *
*                                                                       *
*                            L1B and GEOLOCATION                        *
*			                                                *
*                       HDF Object Identifier Constants                 *
*************************************************************************

************************************************************************
* !C-INC
*
* !Purpose:	Utilities header file containing constants for MOD 02
*		product HDF object names.
*
* !Description: The Header file mapiL1Bgeo.h is part of a larger software
*              system called the MODIS Applications Programming Interface (API)
*              Utility, abbreviated M-API.  The M-API Utility consists of
*              subroutines which allow MODIS Science Team-supplied software
*              to read in Level 1B radiance bands and write out output
*              products and metadata to HDF files.  The functionality of the
*              M-API is defined in the MODIS API User's Guide, Version 2.3,
*              dated 6/20/98.
*
*              The mapiL1Bgeo.h file contains the macros, for the specific
*              names of data objects contained in the following MODIS
*              and product HDF files:
*
*              L1B EV 1km                   vers 2.1.1       Dec 22, 1998
*              L1B EV 500m                  vers 2.1.1       Dec 22, 1998
*              L1B EV 250m                  vers 2.1.1       Dec 22, 1998
*              L1B OBC (partial coverage)   vers 2.1         Jul  9, 1998
*              MOD03.ASCII.fs               vers 2.4         Nov 17, 1998
*              MOD03CP.ASCII.fs             vers 1.1         Mar 20, 1998
*
*
* !Input Parameters: 
*
*     none.
*
* !Output Parameters: 
*
*     none.
*
*
* !Revision History: 
*
*	Revision 01.00 1995/11/28
*	JJ Blanchette
*	Prototype
*	Initial introduction of L1B object names 
*
*       Revision 01.00 1995/12/20
*       Qi Huang
*       Original development
*       Version 1.3 11/2/95 Product Format
*
*	Revision 01.01 1996/01/30
*	JJ Blanchette
*	Concatenation of L1B and geolocation object name files.
*
*	Revision 01.02 1996/02/22
*	JJ Blanchette
*	Revised to 2 February 1996 V1 L1B Prod. Spec. Draft
*
*	Revision 01.03 1996/04/03
*	Qi Huang
*	Revised to 29 February 1996 V1 L1B Prod. Spec..
*       
*       Revision 01.03 1996/06/07
*       Frederick J. Shaw
*       Revised to file spec GEOLOCATION May 6, 1996
*
*	Revision 01.04 1998/06/16
*	Jayshree Murthy
*	Revised to V2 L1B file specs., December 9 1997
*	Revised to V2 Geolocation file spec., March 12 1997
*
*	Revision 02.00 1999/03/10
*	J J Blanchette
*	Revised to V2.1.1 L1B file specs., December 22 1998
* 	Revised to V2 Geolocation file spec., November 17 1998
*       Added V2 Geolocation point residuals file spec., March 20, 1998
*
*	Revision 02.01 1999/12/09
*	Arvind Solanki
*	Fixed MOD03CP macros per DDTs report MODxl01407. Added macros as per
*       CCRs 473, 491, 498, 501, 508, and 509.
*
* !Team-unique Header: 
*      HDF portions developed at the National Center for Supercomputing
*      Applications at the University of Illinois at Urbana-Champaign.
*
* !References and Credits: 
*      This software is developed by the MODIS Science Data Support
*      Team for the National Aeronautics and Space Administration,
*      Goddard Space Flight Center, under contract NAS5-32373.
*
*
* !Design Notes: 
*
* !END
********************************************************************
*/

#ifndef MAPIL1BGEO
#define MAPIL1BGEO

/*              MOD PR02-L1B                    */
/*  MODIS MCST Level 1B data, December 22, 1998 */
/*              Version 2.1.1                   */


/*  Product type identifier  */
#define M02_PROD_ID		"MOD02"


/*  Product type identifiers to conform to V2 ESDTs   */
#define M02_PROD_1KM            "MOD021KM"
#define M02_PROD_HKM            "MOD02HKM"
#define M02_PROD_QKM            "MOD02QKM"
#define M02_PROD_OBC            "MOD02OBC"


/*   MODIS Level 1B Product Granule Metadata   */

/*  MODIS Level 1B Product-Specific Attributes stored as   */
/*      Additional Attributes in Inventory Metadata        */
#define M02AVG_BB_TEMP		"AveragedBlackBodyTemperature"
#define M02AVG_MIR_TEMP		"AveragedMirrorTemperature"
#define M02AVG_FPA1_TEMP	"AveragedFocalPlane1Temperature"
#define M02AVG_FPA2_TEMP	"AveragedFocalPlane2Temperature"
#define M02AVG_FPA3_TEMP	"AveragedFocalPlane3Temperature"
#define M02AVG_FPA4_TEMP	"AveragedFocalPlane4Temperature"
#define M02CAL_QUAL		"CalibrationQuality"
#define M02MISSION_PHASE	"MissionPhase"
#define M02NADIR_PNTNG		"NadirPointing" 


/*  MODIS Level 1B Non-core Granule Metadata Stored as  */
/*           Native HDF Global Attributes               */
#define M02NUMBER_OF_SCANS	  "Number of Scans"
#define M02NUMBER_OF_DAY_SCANS	  "Number of Day mode scans"
#define M02NUMBER_OF_NIGHT_SCANS  "Number of Night mode scans"
#define M02PARTIAL_SCANS	  "Incomplete Scans"
#define M02MAX_EARTH_FRAMES	  "Max Earth View Frames"  
#define M02VLD_EV_OBSERV          "%Valid EV Observations"
#define M02SAT_EV_OBSERV          "%Saturated EV Observations"
#define M02SAT_EV_OBSERVS         M02SAT_EV_OBSERV
#define M02PPROC_IND_BAD_DAT      "Post Processing Indicates Bad data"
#define M02ELEC_RED_VECTOR        "Electronics Redundancy Vector"
#define M02ELEC_CONFIG_CHANGE     "Electronics Configuration Change"
#define M02REFL_LUT_LCGE_DATE     "Reflective LUT Serial Number and Date of Last Change"
#define M02EMIS_LUT_LCGE_DATE     "Emissive LUT Serial Number and Date of Last Change"
#define M02QA_LUT_LCGE_DATE       "QA LUT Serial Number and Date of Last Change"
#define M02FOCAL_PLANE_SET_STATE  "Focal Plane Set Point State"


/*  MODIS Level 1B QA Granule Metadata global attributes  */
#define M02DOORS_CONFIG		"Doors and Screens Configuration"
#define M02VIS_NIR_QI		"Managers VIS/NIR Quality Index"
#define M02SWIR_QI		"Managers SWIR Quality Index"
#define M02MWIR_LWIR_QI		"Managers MWIR/LWIR Quality Index"
#define M02BAD_REFL_BANDS	"Reflective Bands With Bad Data"
#define M02BAD_EMIS_BANDS	"Emissive Bands With Bad Data"
/*  #define M02FLAGS_OFF		"All L1B Error Flags Off"  */
#define M02BB_THERM_NOISE	"Noise in Black Body Thermistors"
#define M02BB_AVG_TEMP_NOISE	"Noise in Average BB Temperature"
#define M02LWIR_FPA_TEMP_NOISE	"Noise in LWIR FPA Temperature"
#define M02MWIR_FPA_TEMP_NOISE	"Noise in MWIR FPA Temperature"
#define M02SCMIR_THERM1_NOISE	"Noise in Scan Mirror Thermistor #1"
#define M02SCMIR_THERM2_NOISE	"Noise in Scan Mirror Thermistor #2"
#define M02SCMIR_AVG_TEMP_NOISE	"Noise in Scan Mirror Thermistor Average"
#define M02INST_TEMP_NOISE	"Noise in Instrument Temperature"
/*  #define M02MWIR_ADC_TEMP_NOISE	"Noise in MWIR ADC Temperature"
    #define M02LWIR_ADC_TEMP_NOISE	"Noise in LWIR ADC Temperature"  */
#define M02CAVITY_TEMP_NOISE	"Noise in Cavity Temperature"
#define M02RESPONSE_CHANGE      "Discontinuities in linear gain b1 across granules"
#define M02NIR_FPA_TEMP_NOISE	"Noise in Temperature of NIR FPA"
#define M02VIS_FPA_TEMP_NOISE	"Noise in Temperature of Vis FPA"
/*  #define M02VIS_ADC_TEMP_NOISE	"Noise in VIS ADC Temperature"
    #define M02NIR_ADC_TEMP_NOISE	"Noise in NIR 1km ADC Temperature"
    #define M02BAND1_ADC_TEMP_NOISE	"Noise in band 1 ADC Temperature"
    #define M02BAND2_ADC_TEMP_NOISE	"Noise in band 2 ADC Temperature"  
    #define M02SWIR_ADC_TEMP_NOISE	"Noise in SWIR Bands ADC Temperature" */
#define M02DEAD_DETECTORS	"Dead Detector List"
#define M02NOISY_DETECTORS	"Noisy Detector List"

#define M02L1A_EV_ALL_SCAN_DATA_MISSING   "% L1A EV All Scan Data are Missing"
#define M02L1A_EV_DN_MISSING_WITHIN_SCAN  "% L1A EV DN Missing Within Scan"
#define M02DEAD_DETECTOR_EV_DATA          "% Dead Detector EV Data"
#define M02SECTOR_ROT_EV_DATA             "% Sector Rotation EV Data"
#define M02SAT_EV_DATA                    "% Saturated EV Data"
#define M02EV_DATA_WHERE_CANT_COMP_BG_DN  "% EV Data Where Cannot Compute BG DN"
#define M02TEB_EV_DATA_W_MOON_IN_SV       "% TEB EV Data With Moon in SVP"
#define M02RSB_EV_DATA_W_DN_BELOW_SCALE   "% RSB EV Data With dn** Below Scale"
#define M02EV_DATA_NOT_CALIB              "% EV Data Not Calibrated"
#define M02OUT_OF_BOUNDS                  "% Out of Bounds"
#define M02MGRS_CHOICE_CALIB_QUALITY      "Manager's Choice Calibration Quality"
#define M02BIT_QA_FLAGS_LAST_VAL          "Bit QA flags Last Value"
#define M02BIT_QA_FLAGS_CHANGE            "Bit QA Flags Change"

#define M02EARTH_SUN_DIST                 "Earth-Sun Distance"
#define M02DETECTOR_QUAL_FLAG             "Detector Quality Flag"


/*  Detector Quality Flag Bitmask  */
#define M02NOISY_DETECTOR             1  /*  Noisy Detector  */
#define M02DEAD_DETECTOR              2  /*  Dead Detector  */
#define M02OUT_OF_FAMILY_GAIN         4  /*  Out-of-Family Gain  */
#define M02DYNAMIC_RANGE              8  /*  Dynamic Range  */
#define M02HIGH_CALIB_FIT_RESIDUALS   32
#define M02ELECTRICAL_CROSSTALK       64
/*  Detector DN saturates on illuminated, unscreened SD  */
/*                (for Reflective Bands)                 */
#define M02REFB_DET_SATURATES         16
/*  Detector DN saturates on blackbody when T_BB = 300K  */
/*                (for Emissive Bands)                   */
#define M02EMISB_DET_SATURATES        16


/*  MODIS Level 1B QA Granule Metadata SDSs  */
#define M02NOISY_THERMISTORS	"Noise in Thermal Detectors"
#define M02THERMISTOR_CHANGE    "Change in relative responses of thermal detectors"
#define M02THERM_DCREST_CHANGE  "DC Restore Change for Thermal Bands"
#define M02QKM_DCREST_CHANGE    "DC Restore Change for Reflective 250m Bands"
#define M02HKM_DCREST_CHANGE    "DC Restore Change for Reflective 500m Bands"
#define M021KM_DCREST_CHANGE    "DC Restore Change for Reflective 1km Bands"

/*  Swath Vgroup  */
#define M02SWATH		 "MODIS_SWATH_Type_L1B"
#define M02_SWATH_NAME           M02SWATH

/*  Level 1B Swath Metadata Written as Vdata  */
#define M02SWATH_MD		"Level 1B Swath Metadata"


/*  Field Names   */
#define M02SW_SCAN_NO   	  "Scan Number"	          /* I32 */
#define M02SW_COMPL_SCAN_FLG      "Complete Scan Flag"    /* I32 */
#define M02SW_SCAN_TYPE		  "Scan Type"      	  /* TXT */
#define M02SW_MIR_SIDE            "Mirror Side"           /* I32 */
#define M02SW_EV_SEC_ST_TIME      "EV Sector Start Time"
#define M02SW_EV_START_TIME	  M02SW_EV_SEC_ST_TIME    /* R64 */
#define M02SW_EV_FRAMES 	  "EV_Frames"	          /* I32 */
#define M02SW_NADIR_FRAME_NUM     "Nadir_Frame_Number"    /* I32 */
#define M02SW_LAT_NADIR FRAME     "Latitude of Nadir Frame"      /* R32 */
#define M02SW_LONG_NADIR FRAME    "Longitude of Nadir Frame"     /* R32 */
#define M02SW_SOL_AZIMNADIR_FRAME "Solar Azimuth of Nadir Frame" /* R32 */
#define M02SW_SOL_ZENNADIR_FRAME  "Solar Zenith of Nadir Frame"  /* R32 */
#define M02SW_NUM_BB_OUTLIERS	  "No. OBC BB thermistor outliers" /* I32 */
#define M02SW_BIT_QA_FLG          "Bit QA Flags"		/* UI32 */
#define M02SW_SECTOR_ROT_ANGLE    "Sector Rotation Angle" 	/* R32 */


/*  Field Names replaced with Bit QA Flag bit masks  */
#define M02SW_MOON_OBS          1       /* Moon in SV Port */
#define M02SW_SC_MANEUVER       2       /* Spacecraft Maneuver */
#define M02SW_MANEUVER          2       /* On-Orbit Manuever */
#define M02SW_SECTOR_ROTATION   4       /* Sector Rotation */
#define M02SW_NEG_RADIANCE      8       /* Negative Radiance */
#define M02SW_BEYOND_NOISE_LVL  8       /* Beyond Noise Level*/
#define M02SW_PC_ECAL_ON        16      /* PC Ecal on */
#define M02SW_PV_ECAL_ON        32      /* PV Ecal on */
#define M02SW_SD_DOOR_OPEN      64      /* SD Door Open */
#define M02SW_SD_SCREEN_DOWN    128     /* SD Screen Down */
#define M02NAD_CLOSED           256     /* NAD closed */   
#define M02SW_SDSM_ON           512     /* SDSM On */
#define M02SW_OUTGASSING        1024    /* Radcooler Heaters On */
#define M02DAY_MODE_BANDS_TELEM_AT_NIGHT    2048
					/* Day mode bands telemetered at night*/
#define M02SW_LIN_EMIS CALIB    4096    /* Linear Emissive Calibration */
#define M02SW_DC_RESTORE_CHG    8192    /* DC Restore Change */
/* #define M02SW_BB_CAVITY_TEMP    16384   * BB/Cavity Temperature Differential  -- UNUSED */
#define M02SW_BB_HEATER_ON      32768   /* BB Heater On */
#define M02SW_MISS_PREV_GRAN    65536   /* Missing Previous Granule */
#define M02SW_MISS_SUBS_GRAN    131072	/* Missing Subsequent Granule */
#define M02SRCA_MEAS            786432 	/* SRCA measures (two-bits) */      
#define M02SRCA_GRATING_MEAS    2       /* Spectral Calibration Mode */  
#define M02SRCA_LAMP_MEAS       0       /* Radiometric Calibration Mode */
#define M02SRCA_SLIT_MEAS       1       /* Spatial Calibration Mode */
#define M02SRCA_OFF             3       /* SRCA Off */
#define M02MOON_IN_RSB_KOBOX	1048576 /* Moon in keep out box 4 any RSB band*/
#define M02MOON_IN_TEB_KOBOX	2097152 /* Moon in keep out box 4 any TEB band*/
#define M02SV_ALL_BAD		4194304 /* All SV data are bad for any RSB */
#define M02BB_ALL_BAD		8388608 /* All BB data are bad for any RSB */


/*  L1B SDS dimension names  */
#define M02_40NSCANS		"40*nscans"
#define M02_20NSCANS		"20*nscans"
#define M02_10NSCANS		"10*nscans"
#define M02NSCANS               "2*nscans"
#define M02_NSCANS		"nscans"
#define M02_40NREFSBSCANS	M02_40NSCANS
#define M02_20NREFSBSCANS	M02_20NSCANS
#define M02_10NREFSBSCANS	M02_10NSCANS
#define M02BAND_250M		"Band_250M"
#define M02BAND_500M		"Band_500M"
#define M02BAND_1KM_REFSB	"Band_1KM_RefSB"
#define M02BAND_1KM_EMIS	"Band_1KM_Emissive"
#define M02_BAND_1KM_EMISSIVE   M02BAND_1KM_EMIS
#define M02MAX_EV_FRAMES        "Max_EV_frames"
#define M02MAX_EV_FRAMES_5      "Max_EV_frames/5+1"
#define M02_4EV_FRAMES		"4*Max_EV_frames"
#define M02_2EV_FRAMES		"2*Max_EV_frames"
#define M02_EV_FRAMES		M02MAX_EV_FRAMES


/*  OBC L1B SDS dimension names	 */
#define M02_4BB_FRAMES		"4*BB_frames"
#define M02_2BB_FRAMES		"2*BB_frames"
#define M02_BB_FRAMES		"BB_frames"
#define M02_4SD_FRAMES		"4*SD_frames"
#define M02_2SD_FRAMES		"2*SD_frames"
#define M02_SD_FRAMES		"SD_frames"
#define M02_4SRCA_FRAMES	"4*SRCA_frames"
#define M02_2SRCA_FRAMES	"2*SRCA_frames"
#define M02_SRCA_FRAMES		"SRCA_frames"
#define M02_4SV_FRAMES		"4*SV_frames"
#define M02_2SV_FRAMES		"2*SV_frames"
#define M02_SV_FRAMES		"SV_frames"


/*  L1B SDS local attributes for Instrument Data   */
#define M02BAND_NAMES		"band_names"
#define M02RAD_SCALES		"radiance_scales"
#define M02RAD_OFFSETS		"radiance_offsets"
#define M02RAD_UNITS		"radiance_units"
#define M02REFL_SCALES		"reflectance_scales"
#define M02REFL_OFFSETS		"reflectance_offsets"
#define M02REFL_UNITS		"reflectance_units"
#define M02CNTS_SCALES		"corrected_counts_scales"
#define M02CNTS_OFFSETS		"corrected_counts_offsets"
#define M02CNTS_UNITS		"corrected_counts_units"



/*  Instrument and Uncertainty Data Stored as Scientific Data Sets   */

/*  Black Body 250M Reflected Solar Bands Raw DN SDS  */
#define M02BB_250        	"BB_250m"

/*  Earth View 250M Reflected Solar Bands Scaled Integer SDS  */
#define M02EARTH_RAD_250	"EV_250_RefSB"
#define M02EARTH_REFL_250	M02EARTH_RAD_250

/*  Earth View 250M Reflected Solar Bands Scaled Integer  */
/*	            Uncertainty SDS                       */
#define M02EARTH_RAD_250_UNCERT	"EV_250_RefSB_Uncert_Indexes"
#define M02EARTH_REFL_250_UNCERT  M02EARTH_RAD_250_UNCERT	              

/*  Solar Diffuser 250M Reflected Solar Bands Raw DN SDS  */
#define M02DIFFUSER_250		"SD_250m"

/*  SRCA 250M Reflected Solar Bands Raw DN SDS  */
#define M02SRCA_250      	"SRCA_250m"

/*  Space View 250M Reflected Solar Bands Raw DN SDS  */
#define M02SPACE_250		"SV_250m" 

/*  Black Body 500M Reflected Solar Bands Raw DN SDS  */
#define M02BB_500        	"BB_500m"

/*  Earth View 500M Reflected Solar Bands Scaled Integer SDS  */
#define M02EARTH_RAD_500	"EV_500_RefSB"
#define M02EARTH_REFL_500	M02EARTH_RAD_500

/*  Earth View 500M Reflected Solar Bands   */
/*     Scaled Integer Uncertainty SDS       */
#define M02EARTH_RAD_500_UNCERT	"EV_500_RefSB_Uncert_Indexes"
#define M02EARTH_REFL_500_UNCERT                M02EARTH_RAD_500_UNCERT

/*  Solar Diffuser 500M Reflected Solar Bands Raw DN SDS  */
#define M02DIFFUSER_500		"SD_500m"

/*  SRCA 500M Reflected Solar Bands Raw DN SDS  */
#define M02SRCA_500      	"SRCA_500m"

/*  Space View 500M Reflected Solar Bands Raw DN SDS  */
#define M02SPACE_500		"SV_500m"

/*  Black Body 1000M Day Bands Raw DN SDS  */
#define M02BB_1000        	"BB_1km_day"

/*  Black Body 1000M Night Bands Raw DN SDS  */
#define M02BB_EMIS_1000        	"BB_1km_night"

/*  Earth View 1000M Reflective Bands Scaled Integer SDS  */
#define M02EARTH_RAD_1000	"EV_1KM_RefSB"
#define M02EARTH_REFL_1000	M02EARTH_RAD_1000
#define M02EV_1KM_REFSB         M02EARTH_RAD_1000

/*  Earth View 1000M Reflective Bands   */
/*    Scaled Integer Uncertainty SDS    */
#define M02EARTH_RAD_1000_UNCERT "EV_1KM_RefSB_Uncert_Indexes"
#define M02EARTH_REFL_1000_UNCERT  M02EARTH_RAD_1000_UNCERT
#define M02EV_1KM_REFSB_UNCERT_IDX M02EARTH_RAD_1000_UNCERT

/*  Earth View 1000M Emissive Bands Scaled Integer SDS  */
#define M02EARTH_EMIS_RAD_1000	"EV_1KM_Emissive"
#define M02EV_1KM_EMISSIVE      M02EARTH_EMIS_RAD_1000

/*  Earth View 1000M Emissive Bands Scaled  */
/*     Integer Radiance Uncertainty SDS     */
#define M02EARTH_EMIS_RAD_1000_UNCERT     "EV_1KM_Emissive_Uncert_Indexes"
#define M02EV_1KM_EMISSIVE_UNCERT_IDX       M02EARTH_EMIS_RAD_1000_UNCERT 

/*  Solar Diffuser 1000M Day Bands Raw DN SDS  */
#define M02DIFFUSER_1000	"SD_1km_day"

/*  Solar Diffuser 1000M Night Bands Raw DN SDS  */
#define M02DIFFUSER_EMIS_1000	"SD_1km_night"

/*  SRCA 1000M Day Bands Raw DN SDS  */
#define M02SRCA_1000      	"SRCA_1km_day"

/*  SRCA 1000M Night Bands Raw DN SDS  */
#define M02SRCA_EMIS_1000      	"SRCA_1km_night"

/*  Space View 1000M Day Bands Raw DN SDS  */
#define M02SPACE_1000		                "SV_1km_day"

/*  Space View 1000M Night Bands Raw DN SDS  */
#define M02SPACE_EMIS_1000	                "SV_1km_night"

/*  250m Earth View data, aggregated to 1km resolution  */
#define M02EV_250_AGGR1KM_REFS                  "EV_250_Aggr1km_RefSB"

/*  250m Earth View data uncertainty index values,  */
/*         aggregated to 1km resolution             */
#define M02EV_250_AGGR1KM_REFSB_UNCERT_IDX "EV_250_Aggr1km_RefSB_Uncert_Indexes"

/*  Number of samples used in 250m EV data to aggregate to 1km  */
#define M02EV_250_AGGR1KM_REFSB_SAMPLES_USED "EV_250_Aggr1km_RefSB_Samples_Used"

/*  500m Earth View data, aggregated to 1km resolution  */
#define M02EV_500_AGGR1KM_REFSB                "EV_500_Aggr1km_RefSB"

/*  500m Earth View data uncertainty index values,  */
/*           aggregated to 1km resolution */
#define M02EV_500_AGGR1KM_REFSB_UNCERT_IDX "EV_500_Aggr1km_RefSB_Uncert_Indexes"

/*     Number of samples used in the 500m Earth View data    */
/*                    to aggregate to 1km                    */
#define M02EV_500_AGGR1KM_REFSB_SAMPLES_USED "EV_500_Aggr1km_RefSB_Samples_Used"

/*      250m Earth View data, aggregated to 500m resolution    */
#define M02EV_250_AGGR500M_REFS                  "EV_250_Aggr500_RefSB"

/*      250m Earth View data uncertainty index values,    */
/*            aggregated to 500m resolution               */
#define M02EV_250_AGGR500M_REFSB_UNCERT_IDX "EV_250_Aggr500_RefSB_Uncert_Indexes"

/*     Number of samples used in the 250m Earth View data    */
/*                   to aggregate to 500m                    */
#define M02EV_250_AGGR500M_REFSB_SAMPLES_USED "EV_250_Aggr500_RefSB_Samples_Used"


/*      MODIS Level 1B Band-Subsetting SDSs           */
#define M02_250M_BAND_NUMS	"Band_250M"
#define M02_500M_BAND_NUMS	"Band_500M"
#define M02_1000M_REF_BAND_NUMS	"Band_1KM_RefSB"
#define M02_1000M_EM_BAND_NUMS	"Band_1KM_Emissive"


/* 	L1B Engineering Data Stored in the OBC file as SDSs    */
#define M02FPA_AEM_CONFIG         "fpa_aem_config"  
#define M02SCIENCE_STATE          "science_state"   

/*	FPA DCR offset Data	*/
#define M02FPA_DCR_OFFSET         "fpa_dcr_offset" 

/*	Raw Mirror Encder Data	  */
#define M02RAW_MIR_ENC            "raw_mir_enc"     

/*	Current/Prior HK Telemtry	*/
#define M02RAW_HK_TELEM           "raw_hk_telem"     
#define M02HK_TELEM		  M02RAW_HK_TELEM

/*	Science Engineering Data	*/
#define M02RAW_SCI_ENG            "raw_sci_eng"     
#define M02SCI_ENG		  M02RAW_SCI_ENG

/*	Parameter Table		*/
#define M02RAW_PARAM              "raw_param"         
#define M02PARM_TABLE		  M02RAW_PARAM

#define M02PV_MEAS                "raw_pv_gains"         

/*	View Sector Start	*/
#define M02VIEW_START		"raw_vs_def"
#define M02RAW_VS_START         M02VIEW_START

/*	Raw S/C Ancillary Data	*/
#define M02RAW_SC_ANCIL         "raw_sc_ancil"     
#define M02SC_ANCIL		 M02RAW_SC_ANCIL

#define M02SW_TOT_FRAMES	"Frame count array"
#define M02SW_SD_START_TIME	"SD start time"
#define MO2SD_SEC_ST_TIME        M02SW_SD_START_TIME
#define M02SRCA_SEC_ST_TIME     "SRCA start time"  
#define M02BB_SEC_ST_TIME       "BB start time"
#define M02SW_SV_START_TIME	"SV start time"
#define M02SV_SEC_ST_TIME        M02SW_SV_START_TIME
#define M02SW_SCAN_START	"EV start time"
#define M02SW_SRCA_ON           "SRCA calibration mode"
#define M02SW_L1_SCAN_QUALITY   "Scan quality array"


/*	L1B SDS local attributes for Geolocation Data      */
#define M02GEO_LINES		"line_numbers"
#define M02GEO_FRAMES		"frame_numbers"

/*      Level 1B Geolocation SDSs     */
#define M02LONGITUDE            "Longitude"
#define M02LATITUDE             "Latitude"
#define M02_HEIGHT              "Height"
#define M02_SENSORZENITH        "SensorZenith" 
#define M02_SENSORAZIMUTH       "SensorAzimuth" 
#define M02_RANGE               "Range" 
#define M02_SOLARZENITH         "SolarZenith" 
#define M02SW_SD_SUN_ZENITH      M02_SOLARZENITH
#define M02_SOLARAZIMUTH        "SolarAzimuth"
#define M02SW_SD_SUN_AZIMUTH     M02_SOLARAZIMUTH
#define M02_GFLAGS              "gflags" 


/************************************************************************
*	        	MODIS GEOLOCATION V2 PRODUCT FORMAT             *
*		MODIS SDST, 11/17/1998, Geolocation data, Version 2.4	*
*************************************************************************/

/*	Product type identifier		*/
#define M03_PROD_ID  		"MOD03"

/*      GEO SwathName      */
#define M03_SWATH_NAME          "MODIS_Swath_Type_GEO"

/*      MODIS Geolocation Product-Specific Attributes stored as     */
/*		Additional Attributes in Inventory Metadata	    */
#define M03SCI_STATE		"SCI_STATE"
#define M03SCI_ABNORM		"SCI_ABNORM"

/*     MODIS Geolocation Specific Granule Metadata                  */
/*     These fields will be stored as individual HDF attributes.    */
#define M03NUMBER_OF_SCANS    "Number of Scans"
#define M03MAX_EARTH_FRAMES   "Max Earth Frames"
#define M03MAX_SD_FRAMES      "Max SD Frames" 
#define M03MAX_SV_FRAMES      "Max SV Frames"
#define M03INCOMP_SCANS       "Incomplete Scans"
#define M03MISSING_PACKETS    "Missing Packets"
#define M03BAD_PKTS           "Packets with bad CRC"
#define M03DISC_PKTS          "Discarded Packets"
#define M03BAD_PACKETS         M03BAD_PKTS
#define M03DISCARD_PACKETS     M03DISC_PKTS
#define M03EPHEMERIS_ATT_SRCE "Ephemeris/Attitude Source"
#define M03PARAM_VERS         "Parameter Version"
#define M03GEO_EST_ERROR      "GEO_EST_RMS_ERROR"
#define M03CUM_GFLAGS	      "Cumulated gflags"



/*      Dimension Names:      */

/*      Number of elements in a cartesian vector     */
#define M03VECDIM            "vecdim"

/*      Number of MODIS bands      */                 
#define M03NUMBANDS           "numbands"                

/*      Number of MODIS scans      */ 
#define M03NSCANS             "nscans"                   

/*      Number of data quality values per scan      */ 
#define M03NUMQUAL             "numqual" 

/*      Maximum number of Earth view frames per scan    */ 
#define M03MFRAMES             "mframes" 

/*      Number of MODIS scans times 10      */ 
#define M03NSCANS_10           "nscans*10" 

/*      Mirror encoder data values per scan      */ 
#define M03NUMENC              "numenc" 

/*      SDS attribute names         */
#define M03ROLL_ELEM           "roll_element" 
#define M03PITCH_ELEM          "pitch_element"
#define M03YAW_ELEM            "yaw_element"   


/*      Processing and Geometric Parameter SDS      */

/*      Focal length for detectors SDS     */
#define	M03FOCAL_LENGTH		"Focal_length"	

/*     Scan IFOV Offsets of band trailing edges     */
/*        with respect to the Optical Center        */
#define M03BAND_POSITION       "band_position"    

/*      Nominal detector spacing in the cross-scan direction   */
#define M03DETECTOR_SPACE      "detector_space"    

/*      Offsets of detector positions from nominal     */
/*            locations on the focal plane             */
#define M03DETECTOR_OFFSETS    "detector_offsets"    

/*  Offsets of the first sample for a band to time of 1km frame.   */
#define	M03T_OFFSET		"T_offset"	

/* Number of samples per frame for each band  */
#define M03NUM_SAMPLES          "num_samples"



/*****************************************************************
 *            Scan-level Metadata                                *
 *****************************************************************/

/*      Scan number in granule        */
#define M03SCAN_NO          "Scan number"           

/*      Number of Earth view frames in scan         */
#define M03EV_FRAMES        "EV frames"             

/*      Number of solar diffuser frames in scan     */
#define M03SD_FRAMES        "SD frames"             

/*      Number of space view frames in scan         */
#define M03SV_FRAMES        "SV frames"       

/*      Earth view start time (TAI)       */
#define M03EV_START_TIME    "EV start time"     

/*      Solar diffuser view start time (TAI)        */
#define M03SD_START_TIME    "SD start time"      

/*      Space view start time (TAI)           */
#define M03SV_START_TIME    "SV start time"     

/*      Sun vector zenith angle in SD frame      */
/*          (with respect to SD Z axis)          */
#define M03SD_SUN_ZENITH            "SD Sun zenith"         

/*      Sun vector azimuth angle in SD frame      */
/*      (CW rotation about Z axis from Y axis)    */
#define M03SD_SUN_AZIMUTH           "SD Sun azimuth"       

/*      Mirror side           */
#define M03MIR_SIDE                  "Mirror side"           

/*      Moon unit vector in instrument frame        */
#define M03MOON_VECTOR              "Moon Vector"

/*      Unit Sun vector in ECR frame at scan center time SDS    */
#define	M03SUN_REF		"sun_ref"	

/*      Number of mirror encoder samples for this scan SDS     */
#define	M03NUM_IMPULSE		"num_impulse"	

/*      Mirror angles from encoder data SDS      */
#define	M03IMPULSE_ENC		"impulse_enc"	

/*      Mirror encoder sample times from start of scan SDS     */
#define	M03IMPULSE_TIME		"impulse_time"	

/*      L1A scan quality flags (defined by Level 1A)    */
#define M03L1_SCAN_QUALITY          "L1 scan quality"

/*      Geolocation scan quality flags     */   
#define M03GEO_SCAN_QUALITY        "Geo scan quality"



/*************************************************************
 *            Scan-Level  Navigation Metadata                *
 *************************************************************/

/*      Earth view center frame time (TAI)          */
#define M03EV_CENTER_TIME              "EV center time"   

/*      ECR orbit position at scan center time SDS    */
#define	M03ORB_POS		"orb_pos"	

/*      ECR orbit velocity at scan center time SDS    */
#define	M03ORB_VEL		"orb_vel"	

/*      ECR-to-instrument frame transformation matrix     */
/*               at scan center time SDS                  */
#define	M03T_INST2ECR		"T_inst2ECR"	

/*      Spacecraft attitude at scan center time expressed     */
/*           in the Orbital Reference frame.                  */
#define M03ATTITUDE_ANGLES      "attitude_angles"



/***********************************************************
 *        Spatial Element Geolocation Data                 *
 ***********************************************************/

/*      Geodetic longitude SDS      */
#define	M03LONGITUDE		"Longitude"	

/*      Geodetic latitude SDS     */
#define	M03LATITUDE 		"Latitude"	

/*      Height above ellipsoid SDS     */
#define	M03HEIGHT		"Height"	

/*      Sensor zenith SDS      */
#define	M03SENSOR_ZEN		"SensorZenith"		

/*      Sensor azimuth SDS     */
#define	M03SENSOR_AZ		"SensorAzimuth"		

/*      Range (pixel to sensor) SDS     */
#define	M03RANGE		"Range"			

/*      Solar zenith SDS     */
#define	M03SOLAR_ZENITH		"SolarZenith"		

/*      Solar azimuth SDS      */
#define	M03SOLAR_AZIMUTH	"SolarAzimuth"		

/*      Land/SeaMask  SDS      */
#define	M03LAND_SEAMASK         "Land/SeaMask"		

/*      Geolocation flags SDS     */
#define	M03GFLAGS 		"gflags"		



/************************************************************************
*	       	MODIS GEOLOCATION V2 CONTROL POINT PRODUCT FORMAT       *
*		MODIS SDST, 3/20/1998, Geolocation data, Version 2.1	*
*************************************************************************/

/*	Product type identifier		*/
#define M03CP_PROD_ID  		"MOD03CP"

/*      Control Point Specific Metadata      */
#define M03CSCID		"Spacecraft ID"
#define M03CPROD_DATE		"Production Date"
#define M03C_GRANULENUMBER	"GRANULENUMBER"
#define M03C_RANGE_BEG_DATE	"RANGEBEGINNINGDATE"
#define M03C_RANGE_BEG_TIME	"RANGEBEGINNINGTIME"
#define M03C_RANGE_END_DATE	"RANGEENDINGDATE"
#define M03C_RANGE_END_TIME	"RANGEENDINGTIME"
#define M03CSC_START_POS	"s/c start position"
#define M03CCS_END_POS  	"s/c end position"
#define M03C_ORBIT_NUM  	"ORBITNUMBER"
#define M03CPARAM_VERS   	"Parameter Version"
#define M03CGEO_PARAM_VERS   	"Geo Parameter Version"
#define M03CBAND_NUM  		"Band_number"
#define M03CCLOUD_SEL   	"Cloud_select"
#define M03CSNOW_SEL   		"Snow_select"
#define M03CICE_SEL   		"Ice_select"
#define M03CLAND_SEL   		"land_select"
#define M03C1ISLE_SEL   	"single_island_select"
#define M03C3ISLE_SEL   	"triplet_select"
#define M03CNUM_RECORDS   	"Number of Records"

/*      Control Point match Vdata      */
#define M03CP_MATCHES   	"Control Point Matches"

/*     Vdata Field Names      */
#define M03CP_LOCX   		"Control Point Location x"
#define M03CP_LOCY    		"Control Point Location y"
#define M03CP_LOCZ   		"Control Point Location z"
#define M03CP_OBSX   		"Observed Control Point x"
#define M03CP_OBSY   		"Observed Control Point y"
#define M03CP_OBSZ   		"Observed Control Point z"
#define M03CP_SCPOSX  		"S/C position x"
#define M03CP_SCPOSY  		"S/C position y"
#define M03CP_SCPOSZ  		"S/C position z"
#define M03CP_SCVELX  		"S/C velocity x"
#define M03CP_SCVELY  		"S/C velocity y"
#define M03CP_SCVELZ	  	"S/C velocity z"
#define M03CP_SCROLL  		"S/C attitude roll"
#define M03CP_SCPITCH	 	"S/C attitude pitch"
#define M03CP_SCYAW	    	"S/C attitude yaw"
#define M03CP_OBST   		"Time of observation"
#define M03CP_VIEWX   		"Control Point view vector x"
#define M03CP_VIEWY   		"Control Point view vector y"
#define M03CP_VIEWZ   		"Control Point view vector z"
#define M03CP_OBS_LINE   	"Observation line number"
#define M03CP_OBS_FRAME   	"Observation frame number"
#define M03CP_DEM_ERROR   	"DEM uncertainty"
#define M03CP_MEAS_ERROR   	"Measurement uncertainty"
#define M03CP_ID   		"Control Point ID"
#define M03CP_SCAN_NUM   	"Scan Number"
#define M03CP_TYPE   		"Control Point Type"
#define M03CP_BAND   		"MODIS band used"
#define M03CP_MIR_SIDE  	"Mirror Side"
#define M03CP_ERROR_FLAG   	"Error Flag"
#define M03CP_SPARE1		"Spare1"
#define M03CP_SPARE2		"Spare2"






#endif
