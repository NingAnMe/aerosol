/************************************************************************
*               mapiatmos.h utilities header file                       *
*                             						*
*                           MODATMOS	          		        * 
*                                                                       *
*               HDF Object Identifier Constants                         *
*************************************************************************/

/************************************************************************
* !C-INC
*
* !Purpose:  Utilities header file containing constants for MOD 
*           ATMOSPHERE discipline HDF object names.
*
* !Description:  
*
*    The Header file mapiatmos.h is part of a larger software system 
*    called the MODIS Application Program Interface (API) Utility, 
*    abbreviated M-API.  The M-API Utility consists of subroutines which
*    allow MODIS Science Team supplied software to read and write data 
*    and metadata from/to HDF files.  The functionality of the M-API is 
*    defined in the MODIS API Specification.
*
*    The mapiatmos.h file contains macros for the the specific names of
*    data objects contained in the following MODIS product HDF files:
*
*           MOD.AM1.V1.aersea.L2   Version 1        May 15, 1996 
*           MOD.AM1.V1.aerland.L2  Version 1        May 15, 1996 
*           MODANC_ATMOS_L2        Version 1       July 17, 1996
*           MOD05.HDF              Version 1      April 18, 1996
*           MOD.AM1.V1.cld_opt.L2  Version 1      April 18, 1996 
*           MOD.AM1.V1.cld_top.L2  Version 1        May 15, 1996
*           MOD.AM1.V1.profiles.L2 Version 1        May 15, 1996 
*           MOD.AM1.V1.cldmask.L2  Version 1      April 25, 1996
*	    MOD35_V2.CDL                          Sept. 11, 1999
*
* !Input Parameters:None
*
* !Output Parameters:None
*
* !Revision History:
*   $Log: mapiatmos.h,v $
 * Revision 2.9  1999/12/20  15:39:11  solanki
 * Reinserted Byte segment along with Common SDS names. Made macro
 * prefix MA instead of M35L2_.
 *
 * Revision 2.9  1999/12/20  15:39:11  solanki
 * Reinserted Byte segment along with Common SDS names. Made macro
 * prefix MA instead of M35L2_.
 *
 * Revision 2.8  1999/12/14  22:54:05  solanki
 * Updated to match latest filespecs - MOD35_V2.CDL.
 *
 * Revision 2.7  1999/04/23  21:17:28  jayshree
 * reorganizing mapi RCS
 *
 * Revision 2.2  1996/07/26  13:12:27  fshaw
 * added file spec MODANC_ATMOS_L2
 *
 * Revision 2.1  1996/07/02  14:12:13  fshaw
 * updated file to reflect new file specs recently
 * baselined.
 * see MODIS product HDF files for list of
 * file specs
 *
 * Revision 1.2  1996/04/22  14:25:14  fshaw
 * updated using file spec MDO35, April 10, 1996
 *
 * Revision 1.1  1996/04/17  19:20:30  fshaw
 * Initial revision
 *
*
* !Team-unique Header:
*
*    This software is developed by the MODIS Science Data Support
*    Team for the National Aeronautics and Space Administration,
*    Goddard Space Flight Center, under contract NAS5-32373.
*
* !References and Credits:
*
*    Written by    Vicky Lin        01/26/96
*    Research and Data Systems Corporation
*    SAIC/GSC MODIS SCIENCE DATA SUPPORT OFFICE
*    7501 FORBES BLVD, SEABROOK MD 20706
*    vlin@ltpmail.gsfc.nasa.gov
*
*    Modified by                             
*               Frederick J. Shaw   
*               General Science Corp.
*               MODIS SCIENCE DATA SUPPORT OFFICE
*               7501 Forbes Blvd. Seabrook MD 20706
*               fshaw@ltpmail.gsfc.nasa.gov
*
*   11/4/99   Updated to match latest filespecs - MOD35_V2.CDL.
*             Arvind Solanki
*
*
* !Design Notes:
*
*!END
********************************************************************/

#ifndef MAPIATMOS
#define MAPIATMOS

/**------------------  MOD ATMOS  ------------------**/
/**             Product type identifier             **/


#define M04L2_PROD_ID            "MOD04_L2"
#define M04SL2_PROD_ID           "MOD04S_L2"
#define M04LL2_PROD_ID           "MOD04L_L2"
#define MODANC_PROD_ID           "MODANC_ATMOS_L2" 
#define M05L2_PROD_ID            "MOD05_L2"
#define M06L2_PROD_ID            "MOD06_L2"
#define M07L2_PROD_ID            "MOD07_L2"
#define M08L2_PROD_ID            "MOD08_L2"
#define M30L2_PROD_ID            "MOD30_L2"
#define M35L2_PROD_ID            "MOD35_L2"
#define M38L2_PROD_ID            "MOD38_L2"

/**------------------  MOD ATMOS  ------------------**/
/**              Common Global Metadata             **/
/**                    Level 2                      **/

#define MAPIXELS_PER_SCAN               "1-km_pixels"
#define MA1KM_ROWS                      "1-km_rows"
#define MALINES_PER_GRANULE             "1-km_Scan_Lines_Per_Granule"
#define MANUMBER_OF_INSTRUMENT_SCANS    "Number_of_Instrument_Scans"
#define MASPSO_PARAMETER                "SPSO_parameter"        
#define MASAT_ZEN_ANG_PROCESS_THRESHOLD "Sat_Zen_Ang_Process_Threshold"
#define MA1ST_GRIB_ID_Title             "FIRST_GRIB_ID_Title"
#define MANUMBER_OF_1KM_FRAMES          "Maximum_Number_of_1km_Frames"
#define MA_TITLE                        "title"
#define MA_HIST                         "history"

/**------------------  MOD ATMOS  ------------------**/
/**                   Common SDSs                   **/

/*  */
#define MACORNER_LAT            "Latitude"

/*  */
#define MACORNER_LON            "Longitude"

/* Scanline number through center of 5x5 pixel array */
#define MASCANLINE_NO           "Scanline_Number"

/* Satellite zenith angle at midpoint of 5x5 array */
#define MAZENITH_SAT            "Sensor_Zenith"

/*  */
#define MA_AZIMUTH_SAT          "Sensor_Azimuth"

/* Solar zenith angle at midpoint of 5x5 array */
#define MAZENITH_SOLAR          "Solar_Zenith"

/*  */
#define MA_AZIMUTH_SOLAR        "Solar_Azimuth"

/*  Index indicating the surface geography type as 
    either Water(0) or Land(1) */
#define MAGEO_FLAG              "Land_Sea_Flag"

/* Surface temperature at midpoint of 5x5 pixel array */
#define MATEMP_SFC              "Sfc_Temp"

/* Surface pressure at midpoint of 5x5 pixel array */
#define MAPRES_SFC              "Sfc_Pres"

/* Estimated tropopause height */
#define MATROPOPAUSE            "Height_Tropopause"

/* */
#define MASCAN_START_TIME       "Scan_Start_Time"


/**------------------  MOD ATMOS  ------------------**/
/**             Common Local Metadata               **/

#define MALONG_NAME             "long_name"
#define MASAMPLING              "sampling_factor"
#define MASCALE		       	"scale_factor"
#define MAOFFSET                "add_offset"
#define MAUNIT                  "units"
#define MARANGE                 "valid_range"
#define MA_FILLVALUE            "_FillValue"
#define MA_PARAM_TYPE           "Parameter_Type"
#define MACELLS_ACROSS_SAMP     "Cell_Across_Swath_Sampling"
#define MACELLS_ALONG_SAMP      "Cell_Along_Swath_Sampling"
#define MA_GEO_PTR              "Geolocation_Pointer"


/**------------------  MOD ATMOS  ------------------**/
/**             Common SDS Dimension Names          **/

#define MACELLS_ACROSS          "Cell_Across_Swath_1km"
#define MACELLS_ACROSS_5KM      "Cell_Across_Swath_5km"
#define MACELLS_ALONG           "Cell_Along_Swath_1km"
#define MACELLS_ALONG_5KM       "Cell_Along_Swath_5km"
#define MAPIXELS                "Pixels Per Scan Line"
#define MASCANLINES             "Number of Scan Lines"
#define MABYTE_SEGMENT          "Byte_Segment"
#define MA_QA_DIMS              "QA_Dimensions"


/****************************************************************
*                       MOD04L_L2                               * 
*                  MOD.AM1.V1.aerland.L2                        * 
*          MODIS Level 2 Aerosol over Land Products             *
*               Version 1       May 15, 1996                    *
*****************************************************************/

/**-----  MOD04L_L2 SDS Dimension Names  -------**/

#define M04BANDS                "Number of Bands"

/**---------  MOD04L_L2 SDS Names  -------------**/

/* Observed land reflectances averaged on 10x10 1-km pixel array   */
#define M04LAND_REFLS              "Avg_Refl"

/* Land aerosol optical thickness (AOT) for continental model      */
#define M04LAND_OPT_THICK       "Opt_Thickness_M1"

/* Standard deviation of observed land reflectances                 */
#define M04LAND_REFLS_DEV       "Std_Dev_Refl"

/* Land AOT for corrected model                                     */
#define M04LAND_OPT_THICK_COR   "Opt_Thickness_M2"

/* Three-dimensional array of the standard deviation of corrected
   aerosol optical thickness due to spatial variability in band
   radiance.                                                        */
#define M04STD_DEV_OPT_THICKNESS       "Std_Dev_Opt_Thickness"

/* Aerosol path radiance ratio (continental model) of 
   red to blue channel (band 3/band 1)                              */
#define M04LAND_RADIANCE_RATIO  "Aerosol_Path_Rad_Ratio"

/* Relative contribution of smoke/sulfate particles to dust 
   in the computation of the aerosol optical depth                   */
#define M04LAND_CONTRIBUTION    "Relative_Contribution"

/* Number of Clear Land Pixels in Band 3                             */
#define M04LAND_PIXELS_B3       "Number_of_Pixels_B3"

/* Number of Clear Land Pixels in Band 1 */
#define M04LAND_PIXELS_B1       "Number_of_Pixels_B1"

/* Identification of retrieval procedure */
#define M04LAND_PROC_ID         "Procedure_ID"

/* Aerosol type in one of four categories: 
   continental, dust, sulfate, and smoke */
#define M04LAND_AERO_TYPE       "Aerosol_Type"

/* Aerosol land error flag */
#define M04LAND_ERROR           "Error_Flag"

/****************************************************************
*                       MOD04S_L2                               *
*                  MOD.AM1.V1.aersea.L2                         *
*          MODIS Level 2 Aerosol over Ocean Products            *
*               Version 1       May 15, 1996                    *
*****************************************************************/

/**-----  MOD04S_L2 SDS Dimension Names  -------**/
#define M04POSSIBLE_SOLUTIONS    "Possible Solutions"

/**---------  MOD04S_L2 SDS Names  -------------**/

/* Ocean AOT at 0.55 micron on 10x10 1-km pixel array          */
#define M04OCEAN_OPT_THICK      "Opt_Thickness"

/* Small-particle ocean AOT at 0.55 micron on 10x10 pixel array   */
#define M04OCEAN_OPT_THICK_S    "Opt_Thickness_Small"

/* Large-particle ocean AOT at 0.55 micron on 10x10 pixel array        */
#define M04OCEAN_OPT_THICK_L    "Opt_Thickness_Large"

/* Minimum error function derived between computed radiances and
   derived radiances                                                   */
#define M04ERROR_EPSILON        "Error_epsilon" 

/* Weight factor for combining large and small aerosol modes
   during retrieval.  This parameter minimizes the least-squares
   error summed over spectral bands                                    */
#define M04OCEAN_ERROR          "Error_Min_Factor"

/* Solution number from 1 to 36                                          */
#define M04OCEAN_SOLUTION       "Solution_Number"

/* Observed ocean reflectances averaged on 10x10 1-km pixel array        */
#define M04OCEAN_REFLS           "Avg_Refl"

/* Look-Up Table of Aerosol Model Parameters and Values Vdata            */
#define M04AEROSOL_LUT          "LUT_Data"

/**------------  Vdata Field names:  ---------------**/

/* small mode aerosol mean radius */
#define M04LUT_RGSS             "RGSS"

/* large mode aerosol mean radius */
#define M04LUT_RGSB             "RGSB"

/* standard deviation of small mode radius */
#define M04LUT_SIGMAS           "SIGMAS"

/* standard deviation of large mode radius */
#define M04LUT_SIGMAB           "SIGMAB"

/* CCN */
#define M04LUT_CCNS             "CNNS"
 
/* small mode extinction coefficient for 5 wavelengths */
#define M04LUT_EXTS             "EXTS"

/* large mode extinction coefficient for 5 wavelengths */
#define M04LUT_EXTB             "EXTB"

/* moments order 1-4 of small mode particle radius */
#define M04LUT_MOMENTS          "MOMENTS"

/* moments order 1-4 of large mode particle radius */
#define M04LUT_MOMENTB          "MOMENTB"

/* small mode backscatter ratio for 5 wavelengths */
#define M04LUT_BACKSCTS         "BACKSCTS"

/* large mode backscatter ratio for 5 wavelengths */
#define M04LUT_BACKSCTB         "BACKSCTB"

/* small mode asymmetry factor for 5 wavelengths */
#define M04LUT_ASSYMS           "ASSYMS"

/* large mode asymmetry factor for 5 wavelengths */
#define M04LUT_ASSYMB           "ASSYMB"

/* small mode albedo for 5 wavelengths */
#define M04LUT_ALBEDOS          "ALBEDOS"

/* large mode albedo for 5 wavelengths */
#define M04LUT_ALBEDOB          "ALBEDOB"

/**************************************************************************
*          MODANC_ATMOS_L2                                                *
*          MODIS Level 2 Atmosphere Ancillary Data                        *
*          Version 1 July 17, 1996                                         *    
***************************************************************************/

/**----- MODANC SDS Dimension Names  --------------------------------**/

#define MANCEV_SAMP                             "EV_samples"
#define MANC10SCANS                             "10*scans"
#define MANCTEMP_LVL_DIM                        "temperature_level"
#define MANCMIX_RATIO_LVL_DIM                   "mixing_ratio_level"

/**--------- MODANC SDS Names  --------------------------------------**/

/* Temperature profile heights in hPa.                                     */
#define MANCTEMP_LVL                        "temperature_level"

/* Mixing ratio heights in hPa                                             */
#define MANCMIX_RATIO_LVL                   "mixing_ratio_level"

/* Temperature profile in degree K                                         */
#define MANCTEMP                            "temperature"

/* Mixing_ratio profile in g/Kg                                            */
#define MANCMIXING_RATIO                    "mixing_ratio"

/* Surface temperature in degree K                                         */
#define MANCSURF_TEMP                       "surface_temperature"

/* Surface pressure reduced to mean sea level in hPa.                       */
#define MANCSURF_PRES                       "surface_pressure"

/* Total column precipitable water in kg per square meter                   */
#define MANCPRECIP_WATER                    "precipitable_water"

/* Eastward component of wind 10 meters above surfacein meters per second   */
#define MANCSURF_WIND_UCOMP                 "surface_wind_ucomponent"

/* Northward component of wind 10 meters above surface in meters per second */
#define MANCSURF_WIND_VCOMP                 "surface_wind_vcomponent"

/* Fractional sea ice concentration */
#define MANCSEA_ICE_CONC                 "sea_ice_concentration"

/* TOVS total column ozone in Dobson units                                  */
#define MANCTOTAL_OZONE                  "total_ozone"

/* Reynolds sea surface temperature in degree K                               */
#define MANCSEA_SURF_TEMP                "sea_surface_temperature"

/****************************************************************
*                       MOD 05_L2                               * 
*          MODIS Level 2 Near Infrared Water Vapor Product      *
*               Version 1          April 25, 1996               *
*****************************************************************/

/**-----------  MOD05 SDS Names  -----------**/

/* Total column water vapor amounts over clear land,
    and cloud scenes over land and ocean                      */
#define M05WATER_VAPOR          "Column_Water_Vapor"

/* Index indicating cloud(0), no cloud(1), 
   or cloud/no cloud determination not made(-1)               */
#define M05CLOUD_QUAL           "Cloud_Qualifier"

/* Index indicating the surface geography type as
   either Water(0) or Land(1).                                */
#define M05LAND_SEA_FLAG         "Land_Sea_Flag"


/****************************************************************
*                       MOD 06_L2                               * 
*                 MODIS Level 2 Cloud Products                  *
*               Version 1         May 15, 1996                  *
*****************************************************************/

/**-------  MOD06 SDS Dimension Names  -----                  **/
#define M06CHANNEL_IND          "Number of Channel Indices"
#define M06CHANNEL_DIFF         "Number of Channel Differences"
#define M06PIXELS_PER_SCAN_LINE  "Pixels Per Scan Line" 
#define M06NUMBER_OF_SCAN_LINES  "Number Of Scan Lines" 


/**-----------  MOD06 SDS Names  -----------**/

/* Year and Julian day (YYDDD) at the time of the first
   observation in 5x5 array.                                  */
#define M06DAY                  "Day"

/* Local time in hours, minutes, and seconds of the first
   observation in 5x5 array.                                  */
#define M06LOCAL_TIME           "Local_Time"

/* Scanline number through center of 5x5 pixel array.           */
#define M06SCANLINE_NUM         "Scanline_Num"

/*  Frame number of center pixel in 5x5 array.                  */
#define M06SCANLINE_ELEMENT     "Scanline_Element"

/* Solar zenith angle at midpoint of 5x5 array.                 */
#define M06SOLAR_ZENITH_ANGLE   "Solar_Zenith_Angle"

/* Brightness temperatures for IR channels 27 - 36 
                           at 5x5 1-km pixel resolution        */
#define M06BRIGHT_TEMP          "Brightness_Temp"

/* Sufficient number of cloudy pixels (0) or too few cloudy pixels (1)
   to be able to process 5x5 pixel array                       */
#define M06PROCESS_FLAG         "Processing_Flag"

/* Spectral cloud forcing for IR channels 29, and 31 - 36       */
#define M06CLOUD_FORCING        "Spec_Cloud_Forcing"

/* value to indicate the method of cloud height determination */
#define M06METHOD               "Cloud_H_Method"

/* Cloud top effective emissivity                              */
#define M06EMISSIVITY_CT        "Cloudtop_Eff_Emi"

/* Cloud top pressure */
#define M06PRES_CT              "Cloudtop_Pres"

/* Cloud top temperature */
#define M06TEMP_CT              "Cloudtop_Temp"

/* Cloud fraction at 5x5 1-km pixel resolution */
#define M06FRACTION             "Cloud_Fraction"

/* Separate cloud top pressure estimates from five radiances ratios */
#define M06PRES_CT_RATIO        "Cloudtop_Pres_From_Ratios"

/* Cloud top pressure from IR window */
#define M06PRES_CT_IR           "Cloudtop_Pres_IR"

/* Estimated tropopause height                                */
#define M06TROPOPAUSE_HEIGHT    "Tropopause_Height"

/* Surface type index */
#define M06SFC_TYPE             "Sfc_Type"

/* Radiance variance for channels 29, 31, and 32 */
#define M06RADIANCE             "Radiance_Var"

/* Brightness temperature differences between 
                      IR channels 29, 31, and 32 */
#define M06BRIGHT_TEMP_DIFF     "Brightness_Temp_Diff"

/* Cloud thermodynamic phase derived from infrared retrieval algorithm */
#define M06PHASE_IR		"Cloud_Phase_IR"

/* Effective particle radius at 1-km resolution */
#define M06EFF_RADIUS		"Eff_Particle_Rad"

/* Cloud optical thickness at 1-km pixel resolution */
#define M06CLOUD_OPT_THICK	"Cloud_Opt_Thickness"

/* Cloud thermodynamic phase derived from 
   visible/SW infrared retrieval algorithm */
#define M06PHASE_VIS		"Cloud_Phase_VIS"

/* Statistics at 1-km pixel resolution */
#define M06STATISTICS		"Statistics"

/****************************************************************
*                 MODIS Level 2 Profiles                        *
*                 Includes (MOD07, MOD08, MOD30, MOD38)         *
*                 Version 1     May 15, 1996     		*
*****************************************************************/

/*--------------  MPROF SDS Dimension Names  -------------------*/

#define MPROFCHANNELS		"Number Of Channels"
#define MPROFNUM_VERT_LVLS	"Number Of Vertical Levels"
#define MPROFNUM_PRESS_LVLS	"Number Of Pressure Levels"

/*--------------  MPROF SDS Names        -----------------------*/

/* GMT Time of observation in milliseconds */
#define MPROFGMT                   "Obs_Time"

/* Frame number of center pixel in 5x5 array */
#define MPROFPIXEL_NO              "Pixel_Number"

/* Total Colume Ozone at 5x5 1-km pixel resolution */
#define MPROFTOTAL_OZONE		"Total_Ozone"

/* Total Totals Atmospheric Stability Index */
#define MPROFTOTALS		"Total_Totals"

/* Lifted Index Atmospheric Stability Index */
#define MPROFLIFTED_INDEX		"Lifted_Index"

/* K Index Atmospheric Stability Index */
#define MPROFK_INDEX		"K_Index"

/* Brightness temperatures for IR channels 20, 22-25, and 27-36 */
#define MPROFBRIGHT_TEMP		"Brightness_Temp"

/* Guess temperature profile for 20 vertical levels */
#define MRPOFTEMP_PROF		"Guess_Temp_Profile"

/* Guess dewpoint temperature profile for 15 vertical levels */
#define MPROFDEWP_TEMP_PROF	"Guess_DewP_Profile"

/* Retrieved temperature profile for 20 vertical levels */
#define MPROFRETR_TEMP_PROF	"Retr_Temp_Profile"

/* Retrieved dewpoint temperature profile for 15 vertical levels */
#define MPROFRETR_DEWP_TEMP_PROF	"Retr_DewP_Profile"

/* Index of pressure levels for the 15 vertical levels              */
#define MPROFPRESS_LEVEL		"Index_Of_Pressure_Levels"


/****************************************************************
*                       MOD_35_L2                               *
*               Level 2 MODIS Classification Masks at 1 km      *
*               and 250 m spatial resolutions                   *
*               Version 2            June 15, 1999              *
*****************************************************************/

/* Bit field mask containing the results of visible and
   infrared radiance cloud/no cloud tests */
#define M35CLOUD_MASK		"Cloud_Mask"
#define M35QA                   "Quality_Assurance"
#define M35BAND_NUMBER          "Band_Number"

#endif
