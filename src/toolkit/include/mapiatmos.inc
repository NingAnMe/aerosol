CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C               mapiatmos.inc utilities header file                  C
C								     C
C                           MODATMOS                                 C 
C                                                                    C
C               HDF Object Identifier Constants                      C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

C---------------------------------------------------------------------
C !F77
C
C !Purpose:  Utilities header file containing constants for MOD 
C           ATMOSPHERE discipline HDF object names.
C
C !Description:  
C
C    The Header file mapiatmos.inc is part of a larger software system 
C    called the MODIS Application Program Interface (API) Utility,
C    abbreviated M-API.  The M-API Utility consists of subroutines which
C    allow MODIS Science Team supplied software to read and write data 
C    and metadata from/to HDF files.  The functionality of the M-API is 
C    defined in the MODIS API Specification.
C
C    The mapiatmos.inc file contains macros for the the specific names 
C    of data objects contained in the following MODIS product HDF files:
C
C           MOD.AM1.V1.aersea.L2   Version 1        May 15, 1996
C           MOD.AM1.V1.aerland.L2  Version 1        May 15, 1996
C           MODANC_ATMOS_L2        Version 1       July 17, 1996
C           MOD05.HDF              Version 1      April 18, 1996
C           MOD.AM1.V1.cld_opt.L2  Version 1      April 18, 1996
C           MOD.AM1.V1.cld_top.L2  Version 1        May 15, 1996
C           MOD.AM1.V1.profiles.L2 Version 1        May 15, 1996 
C           MOD.AM1.V1.cldmask.L2  Version 1      April 25, 1996
C	    MOD35_V2.CDL                          Sept. 11, 1999
C
C !Input Parameters:None
C
C !Output Parameters:None
C
C !Revision History:
C   $Log: mapiatmos.inc,v $
# Revision 2.7  2000/01/26  21:48:57  jjb
# Removed syntax errors and typos found in unit test.
#
# Revision 2.7  2000/01/26  21:48:57  jjb
# Removed syntax errors and typos found in unit test.
#
C Revision 2.6  1999/12/20  15:39:11  solanki
C Reinserted Byte segment along with Common SDS names. Made macro
C prefix MA instead of M35L2_.
C
C Revision 2.5  1999/12/14  23:34:06  solanki
C Updated to match latest filespecs - MOD35_V2.CDL.
C 
C
C !Team-unique Header:
C
C    This software is developed by the MODIS Science Data Support
C    Team for the National Aeronautics and Space Administration,
C    Goddard Space Flight Center, under contract NAS5-32373.
C
C !References and Credits:
C
C    Written by    Vicky Lin        01/26/96
C    Research and Data Systems Corporation
C    SAIC/GSC MODIS SCIENCE DATA SUPPORT OFFICE
C    7501 FORBES BLVD, SEABROOK MD 20706
C
C    vlin@ltpmail.gsfc.nasa.gov
C
C    Modified by Frederick J. Shaw  04/22/96
C               MODIS SCIENCE DATA SUPPORT OFFICE
C               7501 Forbes Blvd. Seabrook MD 2070
C               fshaw@ltpmail.gsfc.nasa.gov
C               updated file using V1 file specifications.
C
C   11/4/99   Updated to match latest filespecs - MOD35_V2.CDL.    
C             Arvind Solanki
C
C !END
C-----------------------------------------------------------------------

C------------------------  MOD ATMOS  --------------------------*
C--                Product type identifiers                   --* 

      character*(*) M04L2_PROD_ID, M04SL2_PROD_ID, M04LL2_PROD_ID,
     &              MODANC_PROD_ID,M05L2_PROD_ID, M06L2_PROD_ID, 
     &              M07L2_PROD_ID, M08L2_PROD_ID, M30L2_PROD_ID,
     &              M35L2_PROD_ID, M38L2_PROD_ID

      parameter (M04L2_PROD_ID  = 'MOD04_L2')
      parameter (M04SL2_PROD_ID = 'MOD04S_L2')
      parameter (M04LL2_PROD_ID = 'MOD04L_L2')
      parameter (MODANC_PROD_ID = 'MODANC_ATMOS_L2')
      parameter (M05L2_PROD_ID  = 'MOD05_L2')
      parameter (M06L2_PROD_ID  = 'MOD06_L2')
      parameter (M07L2_PROD_ID  = 'MOD07_L2')
      parameter (M08L2_PROD_ID  = 'MOD08_L2')
      parameter (M30L2_PROD_ID  = 'MOD30_L2')
      parameter (M35L2_PROD_ID  = 'MOD35_L2')
      parameter (M38L2_PROD_ID  = 'MOD38_L2')

C-------------------------  MOD ATMOS  -------------------------*
C--                   Common Global Metadata                    *
C-------------------------  Level 2    -------------------------*

      character*(*) MAPIXELS_PER_SCAN, MALINES_PER_GRANULE,
     &              MANUMBER_OF_INSTRUMENT_SCANS, 
     &              MASPSO_PARAMETER, MA1KM_ROWS,
     &              MASAT_ZEN_ANG_PROCESS_THRESHOLD,
     &              MA1ST_GRIB_ID_TITLE, MANUMBER_OF_1KM_FRAMES,
     &              MA_TITLE, MA_HIST 

      parameter (MAPIXELS_PER_SCAN     = '1-km_pixels')
      parameter (MA1KM_ROWS            = '1-km_rows')
      parameter (MALINES_PER_GRANULE   = '1-km_Scan_Lines_Per_Granule')
      parameter (MASPSO_PARAMETER      = 'SPSO_parameter')
      parameter (MANUMBER_OF_INSTRUMENT_SCANS =
     &          'Number_of_Instrument_Scans')
      parameter (MASAT_ZEN_ANG_PROCESS_THRESHOLD = 
     &          'Sat_Zen_Ang_Process_Threshold')
      parameter (MA1ST_GRIB_ID_TITLE    =  'FIRST_GRIB_ID_Title')
      parameter (MANUMBER_OF_1KM_FRAMES = 
     &          'Maximum_Number_of_1km_Frames')
      parameter (MA_TITLE = 'title')
      parameter (MA_HIST  = 'history')


C------------------------  MOD ATMOS  --------------------------*
C--                       Common SDSs                         --*

      character*(*) MACORNER_LAT, MACORNER_LON, MASCANLINE_NO,
     &              MAZENITH_SAT, MAZENITH_SOLAR, 
     &              MAGEO_FLAG, MATEMP_SFC, MAPRES_SFC, MATROPOPAUSE,
     &              MA_AZIMUTH_SAT, MA_AZIMUTH_SOLAR, 
     &              MASCAN_START_TIME

C ****  
      parameter (MACORNER_LAT = 'Latitude')

C **** 
      parameter (MACORNER_LON = 'Longitude')

C Scanline number through center of 5x5 pixel array 
      parameter (MASCANLINE_NO = 'Scanline_Number')

C Satellite zenith angle at midpoint of 5x5 array 
      parameter (MAZENITH_SAT = 'Sensor_Zenith')

C ****
      parameter (MA_AZIMUTH_SAT = 'Sensor_Azimuth')

C Solar zenith angle at midpoint of 5x5 array 
      parameter (MAZENITH_SOLAR = 'Solar_Zenith')

C ****
      parameter (MA_AZIMUTH_SOLAR = 'Solar_Azimuth')

C Indicating the surface geography type as either Water(0) or Land(1) 
      parameter (MAGEO_FLAG = 'Land_Sea_Flag')

C Surface temperature at midpoint of 5x5 pixel array 
      parameter (MATEMP_SFC = 'Sfc_Temp')

C Surface pressure at midpoint of 5x5 pixel array 
      parameter (MAPRES_SFC = 'Sfc_Pres')

C Estimated tropopause height 
      parameter (MATROPOPAUSE = 'Height_Tropopause')

C ****
      parameter (MASCAN_START_TIME = 'Scan_Start_Time')


C------------------------  MOD ATMOS  --------------------------*
C--                 Common Local Metadata                     --*

      character*(*) MALONG_NAME, MASAMPLING, MASCALE, MAOFFSET, MAUNIT, 
     &              MARANGE, MA_FILLVALUE, MA_PARAM_TYPE, 
     &              MACELLS_ACROSS_SAMP, MACELLS_ALONG_SAMP, MA_GEO_PTR 

      parameter (MALONG_NAME   =  'long_name')
      parameter (MASAMPLING    =  'sampling_factor')
      parameter (MASCALE       =  'scale_factor')
      parameter (MAOFFSET      =  'add_offset')
      parameter (MAUNIT        =  'units')
      parameter (MARANGE       =  'valid_range')
      parameter (MA_FILLVALUE  =  '_FillValue')
      parameter (MA_PARAM_TYPE =  'Parameter_Type')
      parameter (MACELLS_ACROSS_SAMP =  'Cell_Across_Swath_Sampling')
      parameter (MACELLS_ALONG_SAMP  =  'Cell_Along_Swath_Sampling')
      parameter (MA_GEO_PTR    =  'Geolocation_Pointer')


C------------------------  MOD ATMOS  --------------------------*
C--                 Common SDS Dimension Names                --*

      character*(*) MACELLS_ACROSS, MACELLS_ACROSS_5KM, MACELLS_ALONG, 
     &              MACELLS_ALONG_5KM, MAPIXELS, MASCANLINES, 
     &              MA_QA_DIMS, MABYTE_SEGMENT

      parameter (MACELLS_ACROSS     = 'Cell_Across_Swath_1km')
      parameter (MACELLS_ACROSS_5KM = 'Cell_Across_Swath_5km')
      parameter (MACELLS_ALONG      = 'Cell_Along_Swath_1km')
      parameter (MACELLS_ALONG_5KM  = 'Cell_Along_Swath_5km')
      parameter (MAPIXELS           = 'Pixels Per Scan Line')
      parameter (MASCANLINES        = 'Number of Scan Lines')
      parameter (MA_QA_DIMS         = 'QA_Dimension')
      parameter (MABYTE_SEGMENT     = 'Byte_Segment')

C---------------------------------------------------------------+
C                       MOD04L_L2                               | 
C                 MOD.AM1.V1.aerland.L2                         |
C          MODIS Level 2 Aerosol over Land Products             |
C               Version 1       May 15, 1996                    |
C---------------------------------------------------------------+

C--------------  MOD04L_L2 SDS Dimension Names  -------------------*

      character*(*) M04BANDS

      parameter (M04BANDS              = 'Number of Bands')


C--------------  MOD04L_L2  SDS Names  ------------------------*

      character*(*) M04LAND_REFLS, M04LAND_OPT_THICK,
     &              M04LAND_REFLS_DEV, M04LAND_OPT_THICK_COR,
     &              M04LAND_RADIANCE_RATIO,
     &              M04LAND_CONTRIBUTION, M04LAND_PIXELS_B3,
     &              M04LAND_PIXELS_B1, M04LAND_PROC_ID, M04LAND_ERROR,
     &              M04LAND_AERO_TYPE, M04OCEAN_OPT_THICK, 
     &              M04OCEAN_OPT_THICK_S, M04OCEAN_OPT_THICK_L, 
     &              M04OCEAN_ERROR, M04OCEAN_SOLUTION, M04OCEAN_REFLS,  
     &              M04ERROR_EPSILON, M04OCEAN_VDATA,
     &              M04STD_DEV_OPT_THICKNESS 

C Observed land reflectances averaged on 10x10 1-km pixel array 
      parameter (M04LAND_REFLS = 'Avg_Refl')

C Land aerosol optical thickness (AOT) for continental model 
      parameter (M04LAND_OPT_THICK = 'Opt_Thickness_M1')

C Standard deviation of observed land reflectances 
      parameter (M04LAND_REFLS_DEV = 'Std_Dev_Refl')

C Land AOT for corrected model 
      parameter (M04LAND_OPT_THICK_COR = 'Opt_Thickness_M2')

C Three-dimensional array of the standard deviation of corrected
C aerosol optical thickness due to spatial variability in band
C radiance.                                                          
      parameter (M04STD_DEV_OPT_THICKNESS =  'Std_Dev_Opt_Thickness')

C Aerosol path radiance ratio (continental model) of red to blue channel 
C (band 3/band 1) 
      parameter (M04LAND_RADIANCE_RATIO = 'Aerosol_Path_Rad_Ratio')

C Relative contribution of smoke/sulfate particles to dust in the 
C computation of the aerosol optical depth 
      parameter (M04LAND_CONTRIBUTION = 'Relative_Contribution')

C Number of Clear Land Pixels in Band 3 
      parameter (M04LAND_PIXELS_B3 = 'Number_of_Pixels_B3')

C Number of Clear Land Pixels in Band 1 
      parameter (M04LAND_PIXELS_B1 = 'Number_of_Pixels_B1')

C Identification of retrieval procedure 
      parameter (M04LAND_PROC_ID = 'Procedure_ID')

C Aerosol type in one of four categories: 
C continental, dust, sulfate, and smoke 
      parameter (M04LAND_AERO_TYPE = 'Aerosol_Type')

C Aerosol land error flag 
      parameter (M04LAND_ERROR = 'Error_Flag')

C---------------------------------------------------------------+
C                       MOD04S_L2                               |
C                 MOD.AM1.V1.aersea.L2                          |
C          MODIS Level 2 Aerosol over Ocean Products            |
C               Version 1       May 15, 1996                    |
C---------------------------------------------------------------+

C--------------  MOD04S_L2 SDS Dimension Names  -------------------*

      character*(*) M04POSSIBLE_SOLUTIONS

      parameter (M04POSSIBLE_SOLUTIONS = 'Possible Solutions')

C--------------  MOD04S_L2  SDS Names  ------------------------*

C Ocean AOT at 0.55 micron on 10x10 1-km pixel array 
      parameter (M04OCEAN_OPT_THICK = 'Opt_Thickness')

C Small-particle ocean AOT at 0.55 micron on 10x10 pixel array 
      parameter (M04OCEAN_OPT_THICK_S = 'Opt_Thickness_Small')

C Large-particle ocean AOT at 0.55 micron on 10x10 pixel array 
      parameter (M04OCEAN_OPT_THICK_L = 'Opt_Thickness_Large')

C  Minimum error function derived between computed radiances and
C  derived radiances                                                       
      parameter (M04ERROR_EPSILON  = 'Error_epsilon') 

C Weight factor for combining large and small aerosol modes during 
C retrieval.  This parameter minimizes the least-squares error summed 
C over spectral bands 
      parameter (M04OCEAN_ERROR = 'Error_Min_Factor')

C Solution number from 1 to 36 
      parameter (M04OCEAN_SOLUTION = 'Solution_Number')

C Observed ocean reflectances averaged on 10x10 1-km pixel array 
      parameter (M04OCEAN_REFLS = 'Avg_Refl')

C Look-Up Table of Aerosol Model Parameters and Values Vdata 
      parameter (M04OCEAN_VDATA = 'LUT_Data')


C--------------  Vdata Field names:  -----------------*

      character*(*) M04LUT_RGSS, M04LUT_RGSB, M04LUT_SIGMAS, 
     &              M04LUT_SIGMAB, M04LUT_CCNS, M04LUT_EXTS, 
     &              M04LUT_EXTB, M04LUT_MOMENTS, M04LUT_MOMENTB, 
     &              M04LUT_BACKSCTS, M04LUT_BACKSCTB, M04LUT_ASSYMS, 
     &              M04LUT_ASSYMB, M04LUT_ALBEDOS, M04LUT_ALBEDOB

C small mode aerosol mean radius 
      parameter (M04LUT_RGSS = 'RGSS')

C large mode aerosol mean radius 
      parameter (M04LUT_RGSB = 'RGSB')

C standard deviation of small mode radius 
      parameter (M04LUT_SIGMAS = 'SIGMAS')

C standard deviation of large mode radius 
      parameter (M04LUT_SIGMAB = 'SIGMAB')

C CCN 
      parameter (M04LUT_CCNS = 'CNNS')
 
C small mode extinction coefficient for 5 wavelengths 
      parameter (M04LUT_EXTS = 'EXTS')

C large mode extinction coefficient for 5 wavelengths 
      parameter (M04LUT_EXTB = 'EXTB')

C moments order 1-4 of small mode particle radius 
      parameter (M04LUT_MOMENTS = 'MOMENTS')

C moments order 1-4 of large mode particle radius 
      parameter (M04LUT_MOMENTB = 'MOMENTB')

C small mode backscatter ratio for 5 wavelengths 
      parameter (M04LUT_BACKSCTS = 'BACKSCTS')

C large mode backscatter ratio for 5 wavelengths 
      parameter (M04LUT_BACKSCTB = 'BACKSCTB')

C small mode asymmetry factor for 5 wavelengths 
      parameter (M04LUT_ASSYMS = 'ASSYMS')

C large mode asymmetry factor for 5 wavelengths 
      parameter (M04LUT_ASSYMB = 'ASSYMB')

C small mode albedo for 5 wavelengths 
      parameter (M04LUT_ALBEDOS = 'ALBEDOS')

C large mode albedo for 5 wavelengths 
      parameter (M04LUT_ALBEDOB = 'ALBEDOB')

C---------------------------------------------------------------+
C          MODANC_ATMOS_L2                                      |
C          MODIS Level 2 Atmosphere Ancillary Data              | 
C          Version 1 July 17, 1996                               | 
C---------------------------------------------------------------+

C-------------- MODANC SDS Dimension Names  -------------------*

      character*(*) MANCEV_SAMP, MANC10SCANS,            
     &              MANCTEMP_LVL_DIM, MANCMIX_RATIO_LVL_DIM 

      parameter (MANCEV_SAMP                = 'EV_samples')
      parameter (MANC10SCANS                = '10*scans')
      parameter (MANCTEMP_LVL_DIM           =   'temperature_level')
      parameter (MANCMIX_RATIO_LVL_DIM      =   'mixing_ratio_level')



C------------------- MODANC SDS Names  ------------------------*

      character*(*) MANCTEMP_LVL, MANCMIX_RATIO_LVL,
     &              MANCTEMP, MANCMIXING_RATIO,
     &              MANCSURF_TEMP, MANCPRECIP_WATER,
     &              MANCSURF_WIND_UCOMP, MANCSURF_WIND_VCOMP,
     &              MANCSEA_ICE_CONC, MANCTOTAL_OZONE,
     &              MANCSEA_SURF_TEMP, MANCSURF_PRES


C Temperature profile heights in hPa
      parameter (MANCTEMP_LVL       =   'temperature_level')

C  Mixing ratio heights in hPa 
      parameter (MANCMIX_RATIO_LVL  =   'mixing_ratio_level')

C  Temperature profile in degree K 
      parameter (MANCTEMP           =   'temperature')

C  Mixing_ratio profile in g/Kg 
      parameter (MANCMIXING_RATIO   =   'mixing_ratio')

C  Surface temperature in degree K 
      parameter (MANCSURF_TEMP      =   'surface_temperature')

C  Surface pressure reduced to mean sea level in hPa. 
      parameter (MANCSURF_PRES      =     'surface_pressure')

C  Total column precipitable water in kg per square meter
      parameter (MANCPRECIP_WATER     =   'precipitable_water')

C Eastward component of wind 10 meters above surface in meters per second 
      parameter (MANCSURF_WIND_UCOMP  =  'surface_wind_ucomponent')

C Northward component of wind 10 meters above surface in meters per second 
      parameter (MANCSURF_WIND_VCOMP  =  'surface_wind_vcomponent')

C Fractional sea ice concentration 
      parameter (MANCSEA_ICE_CONC     =   'sea_ice_concentration')

C TOVS total column ozone in Dobson units                 
      parameter (MANCTOTAL_OZONE        =   'total_ozone')

C Reynolds sea surface temperture in degree K 
      parameter (MANCSEA_SURF_TEMP      =   'sea_surface_temperature')

C---------------------------------------------------------------+
C                       MOD 05_L2                               | 
C          MODIS Level 2 Near Infrared Water Vapor Product      |
C               Version 1         April 18, 1996                |
C---------------------------------------------------------------+

C-------------------  MOD05 SDS Names  ------------------------*

      character*(*) M05WATER_VAPOR, M05CLOUD_QUAL

C Total column water vapor amounts over clear land, and cloud scenes 
C over land and ocean 
      parameter (M05WATER_VAPOR = 'Column_Water_Vapor')

C Index indicating cloud(0), no cloud(1), 
C or cloud/no cloud determination not made(-1) 
      parameter (M05CLOUD_QUAL = 'Cloud_Qualifier')

C  Index indicating the surface geography type as
C  either Water(0) or Land(1).                                  
      character*(*) M05LAND_SEA_FLAG  
      parameter (M05LAND_SEA_FLAG   =  'Land_Sea_Flag')

C---------------------------------------------------------------+
C                       MOD 06_L2                               | 
C                 MODIS Level 2 Cloud Products                  |
C               Version 1       May 15, 1996                    |
C---------------------------------------------------------------+

C----------------  MOD06 SDS Dimension Names  -----------------*

      character*(*) M06CHANNEL_IND, M06CHANNEL_DIFF,
     &              M06PIXELS_PER_SCAN_LINE,
     &              M06NUMBER_OF_SCAN_LINES 

      parameter (M06CHANNEL_IND          = 'Number of Channel Indices')
      parameter (M06CHANNEL_DIFF     = 'Number of Channel Differences')
      parameter (M06PIXELS_PER_SCAN_LINE  =  'Pixels Per Scan Line')
      parameter (M06NUMBER_OF_SCAN_LINES  =  'Number Of Scan Lines')


C--------------------  MOD06 SDS Names  -----------------------*

      character*(*) M06BRIGHT_TEMP, M06PROCESS_FLAG, M06CLOUD_FORCING, 
     &              M06METHOD, M06EMISSIVITY_CT, M06PRES_CT, M06TEMP_CT,
     &              M06FRACTION, M06PRES_CT_RATIO, M06PRES_CT_IR, 
     &              M06SFC_TYPE, M06RADIANCE, M06BRIGHT_TEMP_DIFF, 
     &              M06PHASE_IR, M06EFF_RADIUS, M06CLOUD_OPT_THICK, 
     &              M06PHASE_VIS, M06STATISTICS, M06DAY, M06LOCAL_TIME, 
     &              M06SCANLINE_NUM, M06SCANLINE_ELEMENT,
     &              M06SOLAR_ZENITH_ANGLE, M06TROPOPAUSE_HEIGHT 

C Year and Julian day (YYDDD) at the time of the first
C  observation in 5x5 array.                            
      parameter (M06DAY   =         'Day')

C Local time in hours, minutes, and seconds of the first
C  observation in 5x5 array.                               
      parameter (M06LOCAL_TIME  =   'Local_Time')

C Scanline number through center of 5x5 pixel array.     
      parameter (M06SCANLINE_NUM =  'Scanline_Num')

C  Frame number of center pixel in 5x5 array.
      parameter (M06SCANLINE_ELEMENT  =   'Scanline_Element')

C Solar zenith angle at midpoint of 5x5 array.
      parameter (M06SOLAR_ZENITH_ANGLE   =  'Solar_Zenith_Angle')

C Brightness temperatures for IR channels 27 - 36 
      parameter (M06BRIGHT_TEMP = 'Brightness_Temp')

C Sufficient number of cloudy pixels (0) or too few cloudy pixels (1)
      parameter (M06PROCESS_FLAG = 'Processing_Flag')

C Spectral cloud forcing for IR channels 29, and 31 - 36 
      parameter (M06CLOUD_FORCING = 'Spec_Cloud_Forcing')

C value to indicate the method of cloud height determination 
      parameter (M06METHOD        = 'Cloud_H_Method')

C Cloud top effective emissivity 
      parameter (M06EMISSIVITY_CT = 'Cloudtop_Eff_Emi')

C Cloud top pressure 
      parameter (M06PRES_CT       = 'Cloudtop_Pres')

C Cloud top temperature 
      parameter (M06TEMP_CT       = 'Cloudtop_Temp')

C Cloud fraction at 5x5 1-km pixel resolution 
      parameter (M06FRACTION           = 'Cloud_Fraction')

C Separate cloud top pressure estimates from five radiances ratios 
      parameter (M06PRES_CT_RATIO      = 'Cloudtop_Pres_From_Ratios')

C Cloud top pressure from IR window 
      parameter (M06PRES_CT_IR         = 'Cloudtop_Pres_IR')

C Estimated tropopause height 
      parameter (M06TROPOPAUSE_HEIGHT =  'Tropopause_Height')

C Surface type index 
      parameter (M06SFC_TYPE          = 'Sfc_Type')

C Radiance variance for channels 29, 31, and 32 
      parameter (M06RADIANCE          = 'Radiance_Var')

C Brightness temperature differences between IR channels 29, 31, and 32 
      parameter (M06BRIGHT_TEMP_DIFF = 'Brightness_Temp_Diff')

C Cloud thermodynamic phase derived from infrared retrieval algorithm 
      parameter (M06PHASE_IR         = 'Cloud_Phase_IR')

C Effective particle radius at 1-km resolution 
      parameter (M06EFF_RADIUS       = 'Eff_Particle_Rad')

C Cloud optical thickness at 1-km pixel resolution 
      parameter (M06CLOUD_OPT_THICK = 'Cloud_Opt_Thickness')

C Cloud thermodynamic phase derived from visible/SW IR retrieval algo.
      parameter (M06PHASE_VIS = 'Cloud_Phase_VIS')

C Statistics at 1-km pixel resolution 
      parameter (M06STATISTICS = 'Statistics')

C---------------------------------------------------------------+
C                MODIS Level 2 Profiles                         | 
C                Includes (MOD07, MOD08, MOD30, MOD38)          |
C               Version 1       May 15, 1996                    |
C---------------------------------------------------------------+

C----------------  MPROF SDS Dimension Names  -----------------*

      character*(*) MPROFCHANNELS, MPROFNUM_VERT_LVLS,
     &              MPROFNUM_PRESS_LVLS

      parameter (MPROFCHANNELS = 'Number Of Channels')
      parameter (MPROFNUM_VERT_LVLS = 'Number Of Vertical Levels')
      parameter (MPROFNUM_PRESS_LVLS
     &             = 'Number Of Pressure Levels')

C------------------MPROF SDS Names  -----------------------*



      character*(*) MPROFTOTALS, 
     &              MPROFBRIGHT_TEMP, MPROFTEMP_PROF,
     &               MPROFDEWP_TEMP_PROF,
     &              MPROFRETR_TEMP_PROF, MPROFRETR_DEWP_TEMP_PROF,
     &              MPROFPRESS_LEVEL, MPROFTOTAL_OZONE, 
     &              MPROFLIFTED_INDEX, MPROFK_INDEX, 
     &              MPROFPIXEL_NO, MPROFGMT

C GMT Time of observation in milliseconds
      parameter (MPROFGMT = 'Obs_Time')

C Frame number of center pixel in 5x5 array
      parameter (MPROFPIXEL_NO = 'Pixel_Number')

C Total Colume Ozone at 5x5 1-km pixel resolution 
      parameter (MPROFTOTAL_OZONE = 'Total_Ozone')

C Total Totals Atmospheric Stability Index 
      parameter (MPROFTOTALS = 'Total_Totals')

C Lifted Index Atmospheric Stability Index 
      parameter (MPROFLIFTED_INDEX = 'Lifted_Index')

C K Index Atmospheric Stability Index 
      parameter (MPROFK_INDEX = 'K_Index')

C Brightness temperatures for IR channels 20, 22-25, and 27-36 
      parameter (MPROFBRIGHT_TEMP = 'Brightness_Temp')

C Guess temperature profile for 20 vertical levels 
      parameter (MPROFTEMP_PROF   = 'Guess_Temp_Profile')

C Guess dewpoint temperature profile for 15 vertical levels 
      parameter (MPROFDEWP_TEMP_PROF = 'Guess_DewP_Profile')

C Retrieved temperature profile for 20 vertical levels 
      parameter (MPROFRETR_TEMP_PROF = 'Retr_Temp_Profile')

C Retrieved dewpoint temperature profile for 15 vertical levels 
      parameter (MPROFRETR_DEWP_TEMP_PROF = 'Retr_DewP_Profile')

C Index of pressure levels for the 15 vertical levels 
      parameter (MPROFPRESS_LEVEL    = 'Index_Of_Pressure_Levels')


C---------------------------------------------------------------+
C                       MOD_35_L2                               | 
C               Level 2 MODIS Classification Masks at 1 km      |
C               and 250 m spatial resolutions                   |
C               Version 2            June 15, 1999              |
C---------------------------------------------------------------+

      character  M35CLOUD_MASK, M35QA, M35BAND_NUMBER 

C Bit field mask containing the results of visible and infrared 
C radiance cloud/no cloud tests 
      parameter (M35CLOUD_MASK  = 'Cloud_Mask')
      parameter (M35QA          = 'Quality_Assurance')
      parameter (M35BAND_NUMBER = 'Band_Number')
