C************************************************************************
C                       mapiL1Bgeo utilities header file
C
C                            L1B and GEOLOCATION
C 
C                       HDF Object Identifier Constants
C************************************************************************

C**********************************************************************
C !F77-inc
C
C !Purpose:      Utilities header file containing constants for MOD 02
C               product HDF object names.
C
C !Description: The Header file mapiL1Bgeo.h is part of a larger software
C              system called the MODIS Applications Programming Interface (API)
C              Utility, abbreviated M-API.  The M-API Utility consists of
C              subroutines which allow MODIS Science Team-supplied software
C              to read in Level 1B radiance bands and write out output
C              products and metadata to HDF files.  The functionality of the
C              M-API is defined in the MODIS API User's Guide, Version 2.3,
C              dated 6/20/98.
C
C              The mapiL1Bgeo.h file contains the macros, for the specific
C              names of data objects contained in the following MODIS and 
C              product HDF files:
C
C              L1B EV 1km                   vers 2.1.1       Dec 22, 1998
C              L1B EV 500m                  vers 2.1.1       Dec 22, 1998
C              L1B EV 250m                  vers 2.1.1       Dec 22, 1998
C              L1B OBC (partial coverage)   vers 2.1         Jul  9, 1998
C              MOD03.ASCII.fs	            vers 2.4         Nov 17, 1998
C              MOD03CP.ASCII.fs             vers 1.1         Mar 20, 1998
C
C
C !Input Parameters: 
C
C     none.
C
C !Output Parameters: 
C
C     none.
C
C
C !Revision History: 
C
C     Revision 01.00 1995/11/28
C     JJ Blanchette
C     Prototype
C     Initial introduction of L1B object names 
C
C                    Revision 01.00 1995/12/20
C                Qi Huang
C                Original development
C                Version 1.3 11/2/95 Product Format
C
C     Revision 01.01 1996/01/30
C     JJ Blanchette
C     Concatenation of L1B and geolocation object name files.
C
C     Revision 01.03 1996/04/03
C     Qi Huang
C     Revised to 29 February 1996 V1 L1B prod. Spec..
C
C       Revision 01.03 1996/06/07
C       Frederick J. Shaw
C       Revised to file spec GEOLOCATION May 6, 1996
C
C       Revision 01.04 1998/06/16
C       Jayshree Murthy
C       Revised to V2 L1B file specs., December 9 1997
C       Revised to V2 Geolocation file spec., March 12 1997
C
C       Revision 02.00 1999/03/10
C       J J Blanchette
C       Revised to V2.1.1 L1B file specs., December 22 1998
C       Revised to V2 Geolocation file spec., November 17 1998
C       Added V2 Geolocation point residuals file spec., March 20, 1998
C
C       Revision 02.01 1999/12/09
C       Arvind Solanki
C       Fixed MOD03CP macros per DDTs report MODxl01407. Added macros as per
C       CCRs 473, 491, 498, 501, 508, and 509.
C
C
C !Team-unique Header: 
C      HDF portions developed at the National Center for Supercomputing
C      Applications at the University of Illinois at Urbana-Champaign.
C
C !References and Credits: 
C      This software is developed by the MODIS Science Data Support
C      Team for the National Aeronautics and Space Administration,
C      Goddard Space Flight Center, under contract NAS5-32373.
C
C !Design Notes: 
C
C
C !END
C**********************************************************************


!C**********************************************************************
C                         MOD PR02-L1B                            
C                         MODIS MCST Level 1B data, 22 December 1998
C                         Version 2.1.1                           
!C**********************************************************************
C     Product type identifier       
        
      CHARACTER*(*)M02_PROD_ID
      PARAMETER (M02_PROD_ID  ='MOD02')

      CHARACTER*(*)M02_PROD_1KM
      PARAMETER (M02_PROD_1KM ='MOD021KM')

      CHARACTER*(*)M02_PROD_HKM
      PARAMETER (M02_PROD_HKM ='MOD02HKM')

      CHARACTER*(*)M02_PROD_QKM
      PARAMETER (M02_PROD_QKM ='MOD02QKM')

      CHARACTER*(*)M02_PROD_OBC
      PARAMETER (M02_PROD_OBC ='MOD02OBC')


C     MODIS Level 1B Product Granule Metadata                    */


C     MODIS Level 1B Product-Specific Attributes stored as   
C     Additional Attributes in Inventory Metadata
      CHARACTER*(*)M02AVG_BB_TEMP
      PARAMETER (M02AVG_BB_TEMP = 'AveragedBlackBodyTemperature')
      CHARACTER*(*)M02AVG_MIR_TEMP
      PARAMETER (M02AVG_MIR_TEMP = 'AveragedMirrorTemperature')
      CHARACTER*(*)M02AVG_FPA1_TEMP
      PARAMETER (M02AVG_FPA1_TEMP = 'AveragedFocalPlane1Temperature')
      CHARACTER*(*)M02AVG_FPA2_TEMP
      PARAMETER (M02AVG_FPA2_TEMP = 'AveragedFocalPlane2Temperature')
      CHARACTER*(*)M02AVG_FPA3_TEMP
      PARAMETER (M02AVG_FPA3_TEMP = 'AveragedFocalPlane3Temperature')
      CHARACTER*(*)M02AVG_FPA4_TEMP
      PARAMETER (M02AVG_FPA4_TEMP = 'AveragedFocalPlane4Temperature')
      CHARACTER*(*)M02CAL_QUAL
      PARAMETER (M02CAL_QUAL = 'CalibrationQuality')
      CHARACTER*(*)M02MISSION_PHASE
      PARAMETER (M02MISSION_PHASE = 'MissionPhase')
      CHARACTER*(*)M02NADIR_PNTNG
      PARAMETER (M02NADIR_PNTNG = 'NadirPointing')

C     MODIS Level 1B Non-core Granule Metadata Stored as     
C     Native HDF Global Attributes               

      CHARACTER*(*)M02NUMBER_OF_SCANS
      PARAMETER (M02NUMBER_OF_SCANS  = 'Number of Scans')
      CHARACTER*(*)M02NUMBER_OF_DAY_SCANS
      PARAMETER (M02NUMBER_OF_DAY_SCANS  = 'Number of Day mode scans')
      CHARACTER*(*)M02NUMBER_OF_NIGHT_SCANS
      PARAMETER (M02NUMBER_OF_NIGHT_SCANS =
     &  'Number of Night mode scans')
      CHARACTER*(*)M02PARTIAL_SCANS 
      PARAMETER (M02PARTIAL_SCANS = 'Incomplete Scans')
      CHARACTER*(*)M02MAX_EARTH_FRAMES
      PARAMETER (M02MAX_EARTH_FRAMES  = 'Max Earth View Frames')
      CHARACTER*(*) M02VLD_EV_OBSERV
      PARAMETER (M02VLD_EV_OBSERV  = '%Valid EV Observations')
      CHARACTER*(*) M02SAT_EV_OBSERVS     
      PARAMETER (M02SAT_EV_OBSERVS =  '%Saturated EV Observations')
      CHARACTER*(*) M02PPROC_IND_BAD_DAT
      PARAMETER (M02PPROC_IND_BAD_DAT =
     & 'Post Processing Indicates Bad data')
      CHARACTER*(*) M02ELEC_RED_VECTOR
      PARAMETER (M02ELEC_RED_VECTOR = 
     & 'Electronics Redundancy Vector')
      CHARACTER*(*) M02ELEC_CONFIG_CHANGE
      PARAMETER (M02ELEC_CONFIG_CHANGE = 
     & 'Electronics Configuration Change')
      CHARACTER*(*) M02REFL_LUT_LCGE_DATE
      PARAMETER (M02REFL_LUT_LCGE_DATE =
     & 'Reflective LUT Serial Number and Date of Last Change')
      CHARACTER*(*) M02EMIS_LUT_LCGE_DATE
      PARAMETER (M02EMIS_LUT_LCGE_DATE =
     & 'Emissive LUT Serial Number and Date of Last Change')
      CHARACTER*(*) M02QA_LUT_LCGE_DATE
      PARAMETER (M02QA_LUT_LCGE_DATE =
     & 'QA LUT Serial Number and Date of Last Change')
      CHARACTER*(*) M02FOCAL_PLANE_SET_STATE
      PARAMETER (M02FOCAL_PLANE_SET_STATE =
     & 'Focal Plane Set Point State')
      

C     MODIS Level 1B QA Granule Metadata global attributes                    */

      CHARACTER*(*)M02DOORS_CONFIG
      PARAMETER (M02DOORS_CONFIG  = 'Doors and Screens Configuration')
      CHARACTER*(*)M02VIS_NIR_QI
      PARAMETER (M02VIS_NIR_QI ='Managers VIS/NIR Quality Index')
      CHARACTER*(*)M02SWIR_QI
      PARAMETER (M02SWIR_QI  = 'Managers SWIR Quality Index')
      CHARACTER*(*)M02MWIR_LWIR_QI
      PARAMETER (M02MWIR_LWIR_QI ='Managers MWIR/LWIR Quality Index')
      CHARACTER*(*)M02BAD_REFL_BANDS
      PARAMETER (M02BAD_REFL_BANDS  = 'Reflective Bands With Bad Data')
      CHARACTER*(*)M02BAD_EMIS_BANDS
      PARAMETER (M02BAD_EMIS_BANDS  = 'Emissive Bands With Bad Data')
      CHARACTER*(*)M02FLAGS_OFF
      PARAMETER (M02FLAGS_OFF = 'All L1B Error Flags Off')
      CHARACTER*(*)M02BB_THERM_NOISE
      PARAMETER (M02BB_THERM_NOISE = 'Noise in Black Body Thermistors')
      CHARACTER*(*)M02BB_AVG_TEMP_NOISE
      PARAMETER (M02BB_AVG_TEMP_NOISE =
     & 'Noise in Average BB Temperature')
      CHARACTER*(*)M02LWIR_FPA_TEMP_NOISE
      PARAMETER (M02LWIR_FPA_TEMP_NOISE  = 
     & 'Noise in LWIR FPA Temperature')
      CHARACTER*(*)M02MWIR_FPA_TEMP_NOISE
      PARAMETER (M02MWIR_FPA_TEMP_NOISE = 
     & 'Noise in MWIR FPA Temperature')
      CHARACTER*(*)M02SCMIR_THERM1_NOISE
      PARAMETER (M02SCMIR_THERM1_NOISE = 
     & 'Noise in Scan Mirror Thermistor #1')
      CHARACTER*(*)M02SCMIR_THERM2_NOISE
      PARAMETER (M02SCMIR_THERM2_NOISE = 
     & 'Noise in Scan Mirror Thermistor #2')
      CHARACTER*(*)M02SCMIR_AVG_TEMP_NOISE
      PARAMETER (M02SCMIR_AVG_TEMP_NOISE = 
     & 'Noise in Scan Mirror Thermistor Average')
      CHARACTER*(*)M02INST_TEMP_NOISE
      PARAMETER (M02INST_TEMP_NOISE = 
     & 'Noise in Instrument Temperature')
C       CHARACTER*(*)M02MWIR_ADC_TEMP_NOISE
C       PARAMETER (M02MWIR_ADC_TEMP_NOISE = 
C      & 'Noise in MWIR ADC Temperature')
C       CHARACTER*(*)M02LWIR_ADC_TEMP_NOISE
C       PARAMETER (M02LWIR_ADC_TEMP_NOISE = 
C      & 'Noise in LWIR ADC Temperature')
      CHARACTER*(*)M02CAVITY_TEMP_NOISE
      PARAMETER (M02CAVITY_TEMP_NOISE = 'Noise in Cavity Temperature')
      CHARACTER*(*)M02RESPONSE_CHANGE
      PARAMETER (M02RESPONSE_CHANGE = 
     & 'Discontinuities in linear gain b1 across granules')
      CHARACTER*(*)M02NIR_FPA_TEMP_NOISE
      PARAMETER (M02NIR_FPA_TEMP_NOISE = 
     &  'Noise in Temperature of NIR FPA')
      CHARACTER*(*)M02VIS_FPA_TEMP_NOISE
      PARAMETER (M02VIS_FPA_TEMP_NOISE = 
     &  'Noise in Temperature of Vis FPA')
C       CHARACTER*(*)M02VIS_ADC_TEMP_NOISE
C       PARAMETER (M02VIS_ADC_TEMP_NOISE = 
C      & 'Noise in VIS ADC Temperature')
C       CHARACTER*(*)M02NIR_ADC_TEMP_NOISE
C       PARAMETER (M02NIR_ADC_TEMP_NOISE = 
C      & 'Noise in NIR 1km ADC Temperature')
C       CHARACTER*(*)M02BAND1_ADC_TEMP_NOISE
C       PARAMETER (M02BAND1_ADC_TEMP_NOISE = 
C      & 'Noise in band 1 ADC Temperature')
C      CHARACTER*(*)M02BAND2_ADC_TEMP_NOISE
C       PARAMETER (M02BAND2_ADC_TEMP_NOISE = 
C      & 'Noise in band 2 ADC Temperature')
C       CHARACTER*(*)M02SWIR_ADC_TEMP_NOISE
C       PARAMETER (M02SWIR_ADC_TEMP_NOISE = 
C      & 'Noise in SWIR Bands ADC Temperature')
      CHARACTER*(*)M02DEAD_DETECTORS
      PARAMETER (M02DEAD_DETECTORS  = 'Dead Detector List')
      CHARACTER*(*)M02NOISY_DETECTORS
      PARAMETER (M02NOISY_DETECTORS  ='Noisy Detector List')

      CHARACTER*(*)M02L1A_EV_ALL_SCAN_DATA_MISSING
      PARAMETER (M02L1A_EV_ALL_SCAN_DATA_MISSING = 
     & '% L1A EV All Scan Data are Missing')
      CHARACTER*(*) M02L1A_EV_DN_MISSING_WITHIN_SCAN
      PARAMETER (M02L1A_EV_DN_MISSING_WITHIN_SCAN = 
     & '% L1A EV DN Missing Within Scan')
      CHARACTER*(*) M02DEAD_DETECTOR_EV_DATA
      PARAMETER (M02DEAD_DETECTOR_EV_DATA =  '% Dead Detector EV Data')
      CHARACTER*(*) M02SECTOR_ROT_EV_DATA
      PARAMETER (M02SECTOR_ROT_EV_DATA = '% Sector Rotation EV Data')
      CHARACTER*(*) M02SAT_EV_DATA
      PARAMETER (M02SAT_EV_DATA = '% Saturated EV Data')
      CHARACTER*(*) M02EV_DATA_WHERE_CANT_COMP_BG_DN
      PARAMETER (M02EV_DATA_WHERE_CANT_COMP_BG_DN = 
     & '% EV Data Where Cannot Compute BG DN')
      CHARACTER*(*) M02TEB_EV_DATA_W_MOON_IN_SV
      PARAMETER (M02TEB_EV_DATA_W_MOON_IN_SV = 
     & '% TEB EV Data With Moon in SVP')
      CHARACTER*(*) M02RSB_EV_DATA_W_DN_BELOW_SCALE
      PARAMETER (M02RSB_EV_DATA_W_DN_BELOW_SCALE = 
     & '% RSB EV Data With dn** Below Scale')
      CHARACTER*(*) M02EV_DATA_NOT_CALIB
      PARAMETER (M02EV_DATA_NOT_CALIB = '% EV Data Not Calibrated')
      CHARACTER*(*) M02OUT_OF_BOUNDS
      PARAMETER (M02OUT_OF_BOUNDS = '% Out of Bounds')
      CHARACTER*(*) M02MGRS_CHOICE_CALIB_QUALITY
      PARAMETER (M02MGRS_CHOICE_CALIB_QUALITY = 
     & 'Manager''s Choice Calibration Quality')
      CHARACTER*(*) M02BIT_QA_FLAGS_LAST_VAL
      PARAMETER (M02BIT_QA_FLAGS_LAST_VAL = 'Bit QA flags Last Value')
      CHARACTER*(*) M02BIT_QA_FLAGS_CHANGE
      PARAMETER (M02BIT_QA_FLAGS_CHANGE = 'Bit QA Flags Change')

      CHARACTER*(*) M02EARTH_SUN_DIST
      PARAMETER (M02EARTH_SUN_DIST = 'Earth-Sun Distance')
      CHARACTER*(*) M02DETECTOR_QUAL_FLAG
      PARAMETER (M02DETECTOR_QUAL_FLAG = 'Detector Quality Flag')

C  Detector Quality Flag Bitmask
      INTEGER M02NOISY_DETECTOR
      PARAMETER (M02NOISY_DETECTOR = 1)
      INTEGER M02DEAD_DETECTOR
      PARAMETER (M02DEAD_DETECTOR = 2)
      INTEGER M02OUT_OF_FAMILY_GAIN
      PARAMETER (M02OUT_OF_FAMILY_GAIN = 4)
      INTEGER M02DYNAMIC_RANGE
      PARAMETER (M02DYNAMIC_RANGE = 8)
      INTEGER M02HIGH_CALIB_FIT_RESIDUALS
      PARAMETER (M02HIGH_CALIB_FIT_RESIDUALS = 32)
      INTEGER M02ELECTRICAL_CROSSTALK
      PARAMETER (M02ELECTRICAL_CROSSTALK = 64)
C Detector DN saturates on illuminated, unscreened SD 
C             (for Reflective Bands)
      INTEGER M02REFB_DET_SATURATES
      PARAMETER (M02REFB_DET_SATURATES = 16)
C Detector DN saturates on blackbody when T_BB = 300K
      INTEGER M02EMISB_DET_SATURATES
      PARAMETER (M02EMISB_DET_SATURATES = 16)

C     MODIS Level 1B QA Granule Metadata SDSs                                 */
      CHARACTER*(*)M02NOISY_THERMISTORS
      PARAMETER (M02NOISY_THERMISTORS  ='Noise in Thermal Detectors')
      CHARACTER*(*)M02THERMISTOR_CHANGE
      PARAMETER (M02THERMISTOR_CHANGE  =
     & 'Change in relative responses of thermal detectors')
      CHARACTER*(*)M02THERM_DCREST_CHANGE
      PARAMETER (M02THERM_DCREST_CHANGE  =
     & 'DC Restore Change for Thermal Bands')
      CHARACTER*(*)M02QKM_DCREST_CHANGE
      PARAMETER (M02QKM_DCREST_CHANGE  =
     & 'DC Restore Change for Reflective 250m Bands')
      CHARACTER*(*)M02HKM_DCREST_CHANGE
      PARAMETER (M02HKM_DCREST_CHANGE  =
     & 'DC Restore Change for Reflective 500m Bands')
      CHARACTER*(*)M021KM_DCREST_CHANGE
      PARAMETER (M021KM_DCREST_CHANGE  =
     & 'DC Restore Change for Reflective 1km Bands')


C     Swath Vgroup                                    
        
      CHARACTER*(*)M02SWATH
      PARAMETER (M02SWATH  ='MODIS_SWATH_Type_L1B')
      CHARACTER*(*)M02_SWATH_NAME
      PARAMETER (M02_SWATH_NAME = M02SWATH)


C     Level 1B Swath Metadata Written as Vdata     
      CHARACTER*(*)M02SWATH_MD
      PARAMETER (M02SWATH_MD  ='Level 1B Swath Metadata')

C     Field Names
C     I32
      CHARACTER*(*)M02SW_SCAN_NO
      PARAMETER (M02SW_SCAN_NO  ='Scan Number')      
C     I32
      CHARACTER*(*)M02SW_COMPL_SCAN_FLG 
      PARAMETER ( M02SW_COMPL_SCAN_FLG = 'Complete Scan Flag')
C     TXT
      CHARACTER*(*)M02SW_SCAN_TYPE
      PARAMETER (M02SW_SCAN_TYPE  ='Scan Type') 
C     I32
      CHARACTER*(*)M02SW_MIR_SIDE
      PARAMETER (M02SW_MIR_SIDE  ='Mirror Side')      
C     R64
      CHARACTER*(*)M02SW_EV_START_TIME
      PARAMETER (M02SW_EV_START_TIME  ='EV Sector Start Time') 
C     I32
      CHARACTER*(*)M02SW_EV_FRAMES
      PARAMETER (M02SW_EV_FRAMES  ='EV_Frames')      
C     I32
      CHARACTER  M02SW_NADIR_FRAME_NUM  
      PARAMETER ( M02SW_NADIR_FRAME_NUM = 'Nadir_Frame_Number')
C     R32
      CHARACTER M02SW_LAT_NADIR FRAME
      PARAMETER (M02SW_LAT_NADIR FRAME = 'Latitude of Nadir Frame')
C     R32
      CHARACTER M02SW_LONG_NADIR FRAME    
      PARAMETER (M02SW_LONG_NADIR FRAME = 'Longitude of Nadir Frame')
C     R32
      CHARACTER M02SW_SOL_AZIMNADIR_FRAME 
      PARAMETER (M02SW_SOL_AZIMNADIR_FRAME =
     & 'Solar Azimuth of Nadir Frame')
C     R32
      CHARACTER*(*) M02SW_SOL_ZENNADIR_FRAME 
      PARAMETER (M02SW_SOL_ZENNADIR_FRAME =
     & 'Solar Zenith of Nadir Frame')
C     I32
      CHARACTER*(*)M02SW_NUM_BB_OUTLIERS
      PARAMETER (M02SW_NUM_BB_OUTLIERS  = 
     & 'No. OBC BB thermistor outliers') 
C     UI32
      CHARACTER*(*) M02SW_BIT_QA_FLG        
      PARAMETER (M02SW_BIT_QA_FLG =    'Bit QA Flags')
C     R32
      CHARACTER*(*) M02SW_SECTOR_ROT_ANGLE   
      PARAMETER (M02SW_SECTOR_ROT_ANGLE =  'Sector Rotation Angle') 

C     Field Names replaced with Bit QA Flag bit masks
C     Moon within defined limits of SVP
      INTEGER M02SW_MOON_OBS
      PARAMETER (M02SW_MOON_OBS  = 1)
C     On-Orbit Spacecraft Manuever
      INTEGER M02SW_MANEUVER
      PARAMETER (M02SW_MANEUVER  = 2) 
      INTEGER M02SW_SC_MANEUVER       
      PARAMETER (M02SW_SC_MANEUVER = 2)
C     Sector Rotation
      INTEGER M02SW_SECTOR_ROTATION    
      PARAMETER (M02SW_SECTOR_ROTATION = 4)
C     Negative Radiance Beyond Noise Level
      INTEGER M02SW_NEG_RADIANCE       
      PARAMETER (M02SW_NEG_RADIANCE = 8)
      INTEGER M02SW_BEYOND_NOISE_LVL    
      PARAMETER (M02SW_BEYOND_NOISE_LVL = 8) 
C     PC Ecal on
      INTEGER M02SW_PC_ECAL_ON 
      PARAMETER (M02SW_PC_ECAL_ON  = 16)
C     PV Ecal on
      INTEGER M02SW_PV_ECAL_ON
      PARAMETER ( M02SW_PV_ECAL_ON = 32)
C     SD Door Open
      INTEGER M02SW_SD_DOOR_OPEN  
      PARAMETER (M02SW_SD_DOOR_OPEN  = 64)
C     SD Screen Down
      INTEGER M02SW_SD_SCREEN_DOWN  
      PARAMETER (M02SW_SD_SCREEN_DOWN = 128)
C     NAD Closed
      INTEGER M02NAD_CLOSED
      PARAMETER (M02NAD_CLOSED = 256)
C     SDSM On
      INTEGER M02SW_SDSM_ON      
      PARAMETER (M02SW_SDSM_ON  =  512)
C     Radcooler Heaters On
      INTEGER M02SW_OUTGASSING   
      PARAMETER (M02SW_OUTGASSING = 1024)
C     Day mode bands telemetered at night
      INTEGER M02DAY_MODE_BANDS_TELEM_AT_NIGHT 
      PARAMETER (M02DAY_MODE_BANDS_TELEM_AT_NIGHT = 2048)
C     Linear Emissive Calibration
      INTEGER M02SW_LIN_EMIS CALIB  
      PARAMETER (M02SW_LIN_EMIS CALIB  = 4096)
C     DC Restore Change
      INTEGER M02SW_DC_RESTORE_CHG  
      PARAMETER (M02SW_DC_RESTORE_CHG =  8192)
C       BB/Cavity Temperature Differential    --- UNUSED
C       INTEGER M02SW_BB_CAVITY_TEMP     
C       PARAMETER (M02SW_BB_CAVITY_TEMP = 16384)
C     BB Heater On
      INTEGER M02SW_BB_HEATER_ON  
      PARAMETER ( M02SW_BB_HEATER_ON = 32768)
C     Missing Previous Granule
      INTEGER M02SW_MISSING_PREV_GRAN 
      PARAMETER (M02SW_MISSING_PREV_GRAN = 65536)
C     Missing Subsequent Granule
      INTEGER M02SW_MISSING_SUBS_GRAN  
      PARAMETER (M02SW_MISSING_SUBS_GRAN = 131072)
C     SRCA measures (two-bits):
      INTEGER M02SRCA_MEAS    
      PARAMETER (M02SRCA_MEAS =  786432)
C          Spectral Calibration Mode
      INTEGER M02SRCA_GRATING_MEAS    
      PARAMETER (M02SRCA_GRATING_MEAS = 2)
C          Radiometric Calibration Mode
      INTEGER M02SRCA_LAMP_MEAS 
      PARAMETER (M02SRCA_LAMP_MEAS  =  0)
C          Spatial Calibration Mode
      INTEGER M02SRCA_SLIT_MEAS
      PARAMETER (M02SRCA_SLIT_MEAS = 1)
C          SRCA OFF
      INTEGER M02SRCA_OFF
      PARAMETER (M02SRCA_OFF = 3)
C           Moon in keep out box for any RSB band
      INTEGER M02MOON_IN_RSB_KOBOX
      PARAMETER (M02MOON_IN_RSB_KOBOX = 1048576) 
C           Moon in keep out box for any TEB band
      INTEGER M02MOON_IN_TEB_KOBOX
      PARAMETER (M02MOON_IN_TEB_KOBOX = 2097152)
C           All SV data are bad for any RSB
      INTEGER M02SV_ALL_BAD
      PARAMETER (M02SV_ALL_BAD = 4194304)
C           All SV data are bad for any RSB 
      INTEGER M02BB_ALL_BAD
      PARAMETER (M02BB_ALL_BAD = 8388608)



C           L1B SDS dimension names                         

      CHARACTER*(*)M02_40NSCANS
      PARAMETER (M02_40NSCANS = '40*nscans')
      CHARACTER*(*)M02_20NSCANS
      PARAMETER (M02_20NSCANS = '20*nscans')
      CHARACTER*(*)M02_10NSCANS
      PARAMETER (M02_10NSCANS = '10*nscans')
      CHARACTER*(*)M02NSCANS
      PARAMETER (M02NSCANS = '2*nscans')
      CHARACTER*(*)M02_NSANCS
      PARAMETER (M02_NSANCS = 'nscans')
      CHARACTER*(*)M02_40NREFSBSCANS
      PARAMETER (M02_40NREFSBSCANS = M02_40NSCANS)
      CHARACTER*(*)M02_20NREFSBSCANS
      PARAMETER (M02_20NREFSBSCANS = M02_20NSCANS)
      CHARACTER*(*)M02_10NREFSBSCANS
      PARAMETER (M02_10NREFSBSCANS = M02_10NSCANS)
      CHARACTER*(*)M02BAND_250M
      PARAMETER (M02BAND_250M = 'Band_250M')
      CHARACTER*(*)M02BAND_500M
      PARAMETER (M02BAND_500M = 'Band_500M')
      CHARACTER*(*)M02BAND_1KM_REFSB
      PARAMETER (M02BAND_1KM_REFSB = 'Band_1KM_RefSB')
      CHARACTER*(*)M02BAND_1KM_EMIS
      PARAMETER (M02BAND_1KM_EMIS = 'Band_1KM_Emissive')
      CHARACTER*(*) M02_Band_1KM_EMISSIVE
      PARAMETER (M02_Band_1KM_EMISSIVE = M02BAND_1KM_EMIS) 
      CHARACTER*(*)M02MAX_EV_FRAMES 
      PARAMETER (M02MAX_EV_FRAMES = 'Max_EV_frames')
      CHARACTER*(*)M02MAX_EV_FRAMES_5
      PARAMETER (M02MAX_EV_FRAMES_5 = 'Max_EV_frames/5+1')
      CHARACTER*(*)M02_4EV_FRAMES
      PARAMETER (M02_4EV_FRAMES = '4*Max_EV_frames')
      CHARACTER*(*)M02_2EV_FRAMES
      PARAMETER (M02_2EV_FRAMES = '2*Max_EV_frames')
      CHARACTER*(*)M02_EV_FRAMES
      PARAMETER (M02_EV_FRAMES = M02MAX_EV_FRAMES)

C  OBC L1B SDS dimension names                         
      CHARACTER*(*)M02_4BB_FRAMES
      PARAMETER (M02_4BB_FRAMES = '4*BB_frames')
      CHARACTER*(*)M02_2BB_FRAMES
      PARAMETER (M02_2BB_FRAMES = '2*BB_frames')
      CHARACTER*(*)M02_BB_FRAMES
      PARAMETER (M02_BB_FRAMES = 'BB_frames')
      CHARACTER*(*)M02_4SD_FRAMES
      PARAMETER (M02_4SD_FRAMES = '4*SD_frames')
      CHARACTER*(*)M02_2SD_FRAMES
      PARAMETER (M02_2SD_FRAMES = '2*SD_frames')
      CHARACTER*(*)M02_SD_FRAMES
      PARAMETER (M02_SD_FRAMES = 'SD_frames')
      CHARACTER*(*)M02_4SRCA_FRAMES
      PARAMETER (M02_4SRCA_FRAMES = '4*SRCA_frames')
      CHARACTER*(*)M02_2SRCA_FRAMES
      PARAMETER (M02_2SRCA_FRAMES = '2*SRCA_frames')
      CHARACTER*(*)M02_SRCA_FRAMES
      PARAMETER (M02_SRCA_FRAMES = 'SRCA_frames')
      CHARACTER*(*)M02_4SV_FRAMES
      PARAMETER (M02_4SV_FRAMES = '4*SV_frames')
      CHARACTER*(*)M02_2SV_FRAMES
      PARAMETER (M02_2SV_FRAMES = '2*SV_frames')
      CHARACTER*(*)M02_SV_FRAMES
      PARAMETER (M02_SV_FRAMES    = 'SV_frames')
      CHARACTER*(*)M02ENGDIM1_00    


C      L1B SDS local attributes for Instrument Data
      CHARACTER*(*)M02BAND_NAMES
      PARAMETER (M02BAND_NAMES = 'band_names')
      CHARACTER*(*)M02RAD_SCALES
      PARAMETER (M02RAD_SCALES = 'radiance_scales')
      CHARACTER*(*)M02RAD_OFFSETS
      PARAMETER (M02RAD_OFFSETS = 'radiance_offsets')
      CHARACTER*(*)M02RAD_UNITS
      PARAMETER (M02RAD_UNITS = 'radiance_units')
      CHARACTER*(*)M02REFL_SCALES
      PARAMETER (M02REFL_SCALES = 'reflectance_scales')
      CHARACTER*(*)M02REFL_OFFSETS
      PARAMETER (M02REFL_OFFSETS = 'reflectance_offsets')
      CHARACTER*(*)M02REFL_UNITS
      PARAMETER (M02REFL_UNITS = 'reflectance_units')
      CHARACTER*(*)M02CNTS_SCALES
      PARAMETER (M02CNTS_SCALES = 'corrected_counts_scales')
      CHARACTER*(*)M02CNTS_OFFSETS
      PARAMETER (M02CNTS_OFFSETS = 'corrected_counts_offsets')
      CHARACTER*(*)M02CNTS_UNITS
      PARAMETER (M02CNTS_UNITS = 'corrected_counts_units')


C Instrument and Uncertainty Data Stored as Scientific Data Sets         

C     Black Body 250M Reflected Solar Bands Raw DN SDS 
      CHARACTER*(*)M02BB_250
      PARAMETER (M02BB_250          = 'BB_250m')

C     Earth View 250M Reflected Solar Bands Scaled Integer SDS 
      CHARACTER*(*)M02EARTH_RAD_250
      PARAMETER (M02EARTH_RAD_250  = 'EV_250_RefSB_Rad')
      CHARACTER*(*)M02EARTH_REFL_250
      PARAMETER (M02EARTH_REFL_250 = M02EARTH_RAD_250)

C     Earth View 250M Reflected Solar Bands Scaled Integer Uncertainty SDS 
      CHARACTER*(*)M02EARTH_RAD_250_UNCERT
      PARAMETER (M02EARTH_RAD_250_UNCERT  = 
     & 'EV_250_RefSB_Uncert_Indexes')
      CHARACTER*(*)M02EARTH_REFL_250_UNCERT
      PARAMETER (M02EARTH_REFL_250_UNCERT  = M02EARTH_RAD_250_UNCERT)

C     Solar Diffuser 250M Reflected Solar Bands Raw DN SDS 
      CHARACTER*(*)M02DIFFUSER_250
      PARAMETER (M02DIFFUSER_250  = 'SD_250m')

C           SRCA 250M Reflected Solar Bands Raw DN SDS 
      CHARACTER*(*)M02SRCA_250
      PARAMETER (M02SRCA_250       ='SRCA_250m')

C           Space View 250M Reflected Solar Bands Raw DN SDS 
      CHARACTER*(*)M02SPACE_250
      PARAMETER (M02SPACE_250 ='SV_250m')

C           Black Body 500M Reflected Solar Bands Raw DN SDS 
      CHARACTER*(*)M02BB_500
      PARAMETER (M02BB_500         ='BB_500m')

C           Earth View 500M Reflected Solar Bands Scaled Integer SDS 
      CHARACTER*(*)M02EARTH_RAD_500
      PARAMETER (M02EARTH_RAD_500 = 'EV_500_RefSB')
      CHARACTER*(*)M02EARTH_REFL_500
      PARAMETER (M02EARTH_REFL_500 = M02EARTH_RAD_500)

C          Earth View 500M Reflected Solar Bands Scaled Integer Uncertainty SDS 
      CHARACTER*(*)M02EARTH_RAD_500_UNCERT
      PARAMETER (M02EARTH_RAD_500_UNCERT = 
     & 'EV_500_RefSB_Uncert_Indexes')
      CHARACTER*(*)M02EARTH_REFL_500_UNCERT
      PARAMETER (M02EARTH_REFL_500_UNCERT  = M02EARTH_RAD_500_UNCERT)

C           Solar Diffuser 500M Reflected Solar Bands Raw DN SDS 
      CHARACTER*(*)M02DIFFUSER_500
      PARAMETER (M02DIFFUSER_500 = 'SD_500m')

C           SRCA 500M Reflected Solar Bands Raw DN SDS 
      CHARACTER*(*)M02SRCA_500
      PARAMETER (M02SRCA_500       ='SRCA_500m')

C           Space View 500M Reflected Solar Bands Raw DN SDS 
      CHARACTER*(*)M02SPACE_500
      PARAMETER (M02SPACE_500 ='SV_500m')

C           Black Body 1000M Day Bands Raw DN SDS 
      CHARACTER*(*)M02BB_1000
      PARAMETER (M02BB_1000         ='BB_1km_day')

C           Black Body 1000M Night Bands Raw DN SDS 
      CHARACTER*(*)M02BB_EMIS_1000
      PARAMETER (M02BB_EMIS_1000         ='BB_1km_night')

C           Earth View 1000M Reflective Bands Scaled Integer Radiance SDS 
      CHARACTER*(*)M02EARTH_RAD_1000
      PARAMETER (M02EARTH_RAD_1000 = 'EV_1KM_RefSB')
      CHARACTER*(*)M02EARTH_REFL_1000
      PARAMETER (M02EARTH_REFL_1000 = M02EARTH_RAD_1000)
      CHARACTER*(*) M02EV_1KM_REFSB
      PARAMETER (M02EV_1KM_REFSB = M02EARTH_RAD_1000)

C           Earth View 1000M Reflected Solar Bands Scaled Integer 
C           Uncertainty SDS 
      CHARACTER*(*)M02EARTH_RAD_1000_UNCERT
      PARAMETER (M02EARTH_RAD_1000_UNCERT  =
     & 'EV_1KM_RefSB_Uncert_Indexes')
      CHARACTER*(*)M02EARTH_REFL_1000_UNCERT
      PARAMETER (M02EARTH_REFL_1000_UNCERT  = M02EARTH_RAD_1000_UNCERT)
      CHARACTER*(*) M02EV_1KM_REFSB_UNCERT_IDX
      PARAMETER (M02EV_1KM_REFSB_UNCERT_IDX = M02EARTH_RAD_1000_UNCERT)

C           Earth View 1000M Emissive Bands Scaled Integer SDS 
      CHARACTER*(*)M02EARTH_EMIS_RAD_1000
      PARAMETER (M02EARTH_EMIS_RAD_1000 ='EV_1KM_Emissive')
      CHARACTER*(*) M02EV_1KM_EMISSIVE
      PARAMETER (M02EV_1KM_EMISSIVE = M02EARTH_EMIS_RAD_1000)

C           Earth View 1000M Emissive Bands Scaled Integer 
C           Uncertainty SDS 
      CHARACTER*(*)M02EARTH_EMIS_RAD_1000_UNCERT
      PARAMETER (M02EARTH_EMIS_RAD_1000_UNCERT  =
     & 'EV_1KM_Emissive_Uncert_Indexes')
      CHARACTER*(*) M02EV_1KM_EMISSIVE_UNCERT_IDX
      PARAMETER (M02EV_1KM_EMISSIVE_UNCERT_IDX = 
     & M02EARTH_EMIS_RAD_1000_UNCERT)

C           Solar Diffuser 1000M Day Bands Raw DN SDS
      CHARACTER*(*)M02DIFFUSER_1000
      PARAMETER (M02DIFFUSER_1000 ='SD_1km_day')

C           Solar Diffuser 1000M Night Bands Raw DN SDS
      CHARACTER*(*)M02DIFFUSER_EMIS_1000
      PARAMETER (M02DIFFUSER_EMIS_1000 ='SD_1km_night')

C           SRCA 1000M Day Bands Raw DN SDS 
      CHARACTER*(*)M02SRCA_1000
      PARAMETER (M02SRCA_1000       = 'SRCA_1km_day')

C           SRCA 1000M Night Bands Raw DN SDS 
      CHARACTER*(*)M02SRCA_EMIS_1000
      PARAMETER (M02SRCA_EMIS_1000       = 'SRCA_1km_night')

C           Space View 1000M Day Bands Raw DN SDS 
      CHARACTER*(*)M02SPACE_1000
      PARAMETER (M02SPACE_1000 = 'SV_1km_day')

C           Space View 1000M Night Bands Raw DN SDS 
      CHARACTER*(*)M02SPACE_EMIS_1000
      PARAMETER (M02SPACE_EMIS_1000 = 'SV_1km_night')

C           250m Earth View data, aggregated to 1km resolution
      CHARACTER*(*) M02EV_250_AGGR1KM_REFS
      PARAMETER (M02EV_250_AGGR1KM_REFS = 'EV_250_Aggr1km_RefSB')

C      250m Earth View data uncertainty index values, */
C      aggregated to 1km resolution */
      CHARACTER*(*) M02EV_250_AGGR1KM_REFSB_UNCERT_IDX
      PARAMETER (M02EV_250_AGGR1KM_REFSB_UNCERT_IDX =
     & 'EV_250_Aggr1km_RefSB_Uncert_Indexes')

C     Number of samples used in the 250m Earth View data to aggregate to 1km
      CHARACTER*(*) M02EV_250_AGGR1KM_REFSB_SAMPLES_USED
      PARAMETER (M02EV_250_AGGR1KM_REFSB_SAMPLES_USED = 
     & 'EV_250_Aggr1km_RefSB_Samples_Used')

C           500m Earth View data, aggregated to 1km resolution
      CHARACTER*(*) M02EV_500M_AGGR1KM_REFSB
      PARAMETER (M02EV_500M_AGGR1KM_REFSB = 'EV_500_Aggr1km_RefSB')

C      500m Earth View data uncertainty index values, */
C      aggregated to 1km resolution */
      CHARACTER*(*) M02EV_500M_AGGR1KM_REFSB_UNCERT_IDX
      PARAMETER (M02EV_500M_AGGR1KM_REFSB_UNCERT_IDX = 
     & 'EV_500M_Aggr1km_RefSB_Uncert_Indexes')

C     Number of samples used in the 500m Earth View data to aggregate to 1km
      CHARACTER*(*) M02EV_500_AGGR1KM_REFSB_SAMPLES_USED
      PARAMETER (M02EV_500_AGGR1KM_REFSB_SAMPLES_USED = 
     & 'EV_500_Aggr1km_RefSB_Samples_Used')

C           250m Earth View data, aggregated to 500m resolution
      CHARACTER*(*) M02EV_250_AGGR500M_REFS
      PARAMETER (M02EV_250_AGGR500M_REFS = 'EV_250_Aggr500_RefSB')

C      250m Earth View data uncertainty index values, */
C      aggregated to 500m resolution */
      CHARACTER*(*) M02EV_250_AGGR500M_REFSB_UNCERT_IDX
      PARAMETER (M02EV_250_AGGR500M_REFSB_UNCERT_IDX =
     & 'EV_250_Aggr500_RefSB_Uncert_Indexes')

C     Number of samples used in the 250m Earth View data to aggregate to 500m
      CHARACTER*(*) M02EV_250_AGGR500M_REFSB_SAMPLES_USED
      PARAMETER (M02EV_250_AGGR500M_REFSB_SAMPLES_USED = 
     & 'EV_250_Aggr500_RefSB_Samples_Used')


C     MODIS Level 1B Band-Subsetting SDSs                    */
      CHARACTER*(*)M02_250M_BAND_NUMS
      PARAMETER (M02_250M_BAND_NUMS  = 'Band_250M')
      CHARACTER*(*)M02_500M_BAND_NUMS
      PARAMETER (M02_500M_BAND_NUMS  = 'Band_500M')
      CHARACTER*(*)M02_1000M_REF_BAND_NUMS
      PARAMETER (M02_1000M_REF_BAND_NUMS = 'Band_1KM_RefSB')
      CHARACTER*(*)M02_1000M_EM_BAND_NUMS
      PARAMETER (M02_1000M_EM_BAND_NUMS = 'Band_1KM_Emissive')


C    L1B Engineering Data Stored in the OBC file as a Science Data Sets     

      CHARACTER*(*) M02SCIENCE_STATE
      PARAMETER (M02SCIENCE_STATE =  'science_state')

      CHARACTER*(*) M02FPA_AEM_CONFIG  
      PARAMETER (M02FPA_AEM_CONFIG = 'fpa_aem_config')

C           FPA DCR offset Data                             
      CHARACTER*(*)M02FPA_DCR_OFFSET
      PARAMETER (M02FPA_DCR_OFFSET = 'fpa_dcr_offset')

C           Raw Mirror Encder Data                          
      CHARACTER*(*)M02MIRROR_ENCODER, M02RAW_MIR_ENC
      PARAMETER (M02RAW_MIR_ENC    = 'raw_mir_enc')

C           Current/Prior HK Telemtry                       
      CHARACTER*(*)M02HK_TELEM, M02RAW_HK_TELEM
      PARAMETER (M02RAW_HK_TELEM = 'raw_hk_telem')
      PARAMETER (M02HK_TELEM = M02RAW_HK_TELEM)

C           Science Engineering Data                        
      CHARACTER*(*)M02SCI_ENG, M02RAW_SCI_ENG
      PARAMETER (M02RAW_SCI_ENG =  'raw_sci_eng')
      PARAMETER (M02SCI_ENG = M02RAW_SCI_ENG)

C           Parameter Table                                 
      CHARACTER*(*)M02PARM_TABLE, M02RAW_PARAM 
      PARAMETER ( M02RAW_PARAM = 'raw_param')
      PARAMETER (M02PARM_TABLE = M02RAW_PARAM)

      CHARACTER*(*) M02PV_MEAS 
      PARAMETER (M02PV_MEAS  = 'raw_pv_gains')

C           View Sector Start                         
      CHARACTER*(*)M02VIEW_START, M02RAW_VS_START
      PARAMETER ( M02RAW_VS_START = 'raw_vs_def')
      PARAMETER (M02VIEW_START = M02RAW_VS_START)

C           Raw S/C Ancillary Data                          
      CHARACTER*(*)M02SC_ANCIL, M02RAW_SC_ANCIL
      PARAMETER (M02RAW_SC_ANCIL =  'raw_sc_ancil')
      PARAMETER (M02SC_ANCIL = M02RAW_SC_ANCIL)

      CHARACTER*(*)M02SW_TOT_FRAMES
      PARAMETER (M02SW_TOT_FRAMES  ='Frame count array')  

      CHARACTER*(*)M02SW_SD_START_TIME
      PARAMETER (M02SW_SD_START_TIME  ='SD start time') 
      CHARACTER*(*)M02SD_SEC_ST_TIME 
      PARAMETER (M02SD_SEC_ST_TIME =  M02SW_SD_START_TIME)

      CHARACTER*(*)M02SRCA_SEC_ST_TIME 
      PARAMETER (M02SRCA_SEC_ST_TIME = 'SRCA start time')

      CHARACTER*(*)M02BB_SEC_ST_TIME  
      PARAMETER (M02BB_SEC_ST_TIME  = 'BB start time')

      CHARACTER*(*)M02SW_SV_START_TIME
      PARAMETER (M02SW_SV_START_TIME  ='SV start time') 
      CHARACTER*(*)M02SV_SEC_ST_TIME 
      PARAMETER (M02SV_SEC_ST_TIME =  M02SW_SV_START_TIME)

      CHARACTER*(*)M02SW_SCAN_START
      PARAMETER (M02SW_SCAN_START  = 'EV start time') 

      CHARACTER*(*) M02SW_SRCA_ON   
      PARAMETER (M02SW_SRCA_ON  = 'SRCA calibration mode')

      CHARACTER*(*)M02SW_L1_SCAN_QUALITY
      PARAMETER (M02SW_L1_SCAN_QUALITY   = 'Scan quality array') 


C      L1B SDS local attributes for Geolocation Data
      CHARACTER*(*)M02GEO_LINES
      PARAMETER (M02GEO_LINES = 'line_numbers')
      CHARACTER*(*)M02GEO_FRAMES
      PARAMETER (M02GEO_FRAMES = 'frame_numbers')

C     Level 1B Geolocation SDSs
      CHARACTER*(*)M02LONGITUDE
      PARAMETER (M02LONGITUDE = 'Longitude')
      CHARACTER*(*)M02LATITUDE
      PARAMETER (M02LATITUDE =  'Latitude')
      CHARACTER*(*)M02_HEIGHT
      PARAMETER (M02_HEIGHT =   'Height')
      CHARACTER*(*)M02_SENSORZENITH 
      PARAMETER (M02_SENSORZENITH = 'SensorZenith')
      CHARACTER*(*)M02_SENSORAZIMUTH 
      PARAMETER (M02_SENSORAZIMUTH = 'SensorAzimuth')
      CHARACTER*(*)M02_RANGE
      PARAMETER (M02_RANGE =   'Range')
      CHARACTER*(*)M02_SOLARZENITH 
      PARAMETER (M02_SOLARZENITH  = 'SolarZenith')
      CHARACTER*(*)M02SW_SD_SUN_ZENITH
      PARAMETER (M02SW_SD_SUN_ZENITH  = M02_SOLARZENITH)
      CHARACTER*(*)M02_SOLARAZIMUTH  
      PARAMETER (M02_SOLARAZIMUTH = 'SolarAzimuth')
      CHARACTER*(*)M02SW_SD_SUN_AZIMUTH
      PARAMETER (M02SW_SD_SUN_AZIMUTH = M02_SOLARAZIMUTH)
      CHARACTER*(*)M02_GFLAGS 
      PARAMETER (M02_GFLAGS =    'gflags')

!C***********************************************************************
C                 MODIS GEOLOCATION V2 PRODUCT FORMAT  
C           MODIS SDST, 11/17/98, Geolocation data, Version 2.4      
!C***********************************************************************

C     Product type identifier       
      CHARACTER*(*)M03_PROD_ID
      PARAMETER (M03_PROD_ID    ='M0D03')

C   GEO SwathName                           
      CHARACTER*(*) M03_SWATH_NAME  
      PARAMETER (M03_SWATH_NAME =  'MODIS_Swath_Type_GEO')

C     MODIS Geolocation Product-Specific Attributes stored as   
C     Additional Attributes in Inventory Metadata
      CHARACTER*(*)M03SCI_STATE
      PARAMETER (M03SCI_STATE = 'SCI_STATE')
      CHARACTER*(*)M03SCI_ABNORM
      PARAMETER (M03SCI_ABNORM = 'SCI_ABNORM')

C   MODIS Geolocation Specific Granule Metadata
C   These fields will be stored as individual HDF attributes.         

      CHARACTER*(*) M03NUMBER_OF_SCANS
      PARAMETER (M03NUMBER_OF_SCANS =  'Number of Scans')

      CHARACTER*(*) M03MAX_EARTH_FRAMES  
      PARAMETER (M03MAX_EARTH_FRAMES =  'Max Earth Frames')

      CHARACTER*(*)M03MAX_SD_FRAMES     
      PARAMETER (M03MAX_SD_FRAMES =     'Max SD Frames')

      CHARACTER*(*)M03MAX_SV_FRAMES   
      PARAMETER (M03MAX_SV_FRAMES =   'Max SV Frames')

      CHARACTER*(*)M03INCOMP_SCANS   
      PARAMETER (M03INCOMP_SCANS  =   'Incomplete Scans')

      CHARACTER*(*) M03MISSING_PACKETS
      PARAMETER (M03MISSING_PACKETS =  'Missing Packets')

      CHARACTER*(*) M03BAD_PKTS     
      PARAMETER (M03BAD_PKTS =  'Packets with bad CRC')

      CHARACTER*(*) M03DISC_PKTS    
      PARAMETER (M03DISC_PKTS =  'Discarded Packets')

      CHARACTER*(*) M03BAD_PACKETS 
      PARAMETER (M03BAD_PACKETS =   M03BAD_PKTS)

      CHARACTER*(*) M03DISCARD_PACKETS 
      PARAMETER (M03DISCARD_PACKETS =  M03DISC_PKTS)

      CHARACTER*(*) M03EPHEMERIS_ATT_SRCE 
      PARAMETER (M03EPHEMERIS_ATT_SRCE = 'Ephemeris/Attitude Source')

      CHARACTER*(*) M03PARAM_VERS   
      PARAMETER (M03PARAM_VERS =  'Parameter Version')

      CHARACTER*(*) M03GEO_EST_ERROR      
      PARAMETER (M03GEO_EST_ERROR =  'GEO_EST_RMS_ERROR')

      CHARACTER*(*) M03CUM_GFLAGS
      PARAMETER (M03CUM_GFLAGS =  'Cumulated gflags')

C     Dimension Names:                         

C     Number of elements in a cartesian vector 
      CHARACTER*(*) M03VECDIM 
      PARAMETER (M03VECDIM =  'vecdim')

C     Number of MODIS bands 
      CHARACTER*(*) M03NUMBANDS 
      PARAMETER (M03NUMBANDS =  'numbands')

C     Number of MODIS scans  
      CHARACTER*(*) M03NSCANS  
      PARAMETER (M03NSCANS  =   'nscans')

C     Number of data quality values per scan 
      CHARACTER*(*) M03NUMQUAL   
      PARAMETER (M03NUMQUAL =   'numqual')

C     Maximum number of Earth view frames per scan 
      CHARACTER*(*) M03MFRAMES  
      PARAMETER (M03MFRAMES =  'mframes')

C     Number of MODIS scans times 10 
      CHARACTER*(*) M03NSCANS_10 
      PARAMETER (M03NSCANS_10  =  'nscans*10')

C     Mirror encoder data values per scan 
      CHARACTER*(*) M03NUMENC   
      PARAMETER (M03NUMENC =   'numenc')

C     SDS attribute names    
      CHARACTER*(*) M03ROLL_ELEM  
      PARAMETER (M03ROLL_ELEM = 'roll_element')
      CHARACTER*(*) M03PITCH_ELEM  
      PARAMETER (M03PITCH_ELEM =  'pitch_element')
      CHARACTER*(*) M03YAW_ELEM    
      PARAMETER (M03YAW_ELEM  =  'yaw_element')

C     Processing and Geometric Parameter SDS
C     Focal length for detectors SDS 
      CHARACTER*(*)M03FOCAL_LENGTH
      PARAMETER (M03FOCAL_LENGTH  ='Focal_length')    

C       Scan IFOV Offsets of band trailing edges with respect to the
C       Optical Center 
      CHARACTER*(*)M03BAND_POSITION
      PARAMETER (M03BAND_POSITION  ='band_position')    

C       Nominal detector spacing in the cross-scan direction       
      CHARACTER*(*)M03DETECTOR_SPACE
      PARAMETER (M03DETECTOR_SPACE  ='detector_space')      

C       Offsets of detector positions from nominal locations on
C       the focal plane  
      CHARACTER*(*)M03DETECTOR_OFFSETS
      PARAMETER (M03DETECTOR_OFFSETS  = 'detector_offsets')  

C     Band readout times relative to ideal band SDS 
      CHARACTER*(*)M03T_OFFSET
      PARAMETER (M03T_OFFSET  ='T_offset')      

C     Band readout times relative to ideal band SDS 
      CHARACTER*(*)M03NUM_SAMPLES
      PARAMETER (M03NUM_SAMPLES = 'num_samples')      


C
C     Scan-level Metadata
C
C     Scan number in granule
      CHARACTER*(*)M03SCAN_NO
      PARAMETER (M03SCAN_NO  = 'Scan number')  

C     Number of Earth view frames in scan
      CHARACTER*(*)M03EV_FRAMES
      PARAMETER (M03EV_FRAMES = 'EV frames')  

C     Number of solar diffuser frames in scan
      CHARACTER*(*)M03SD_FRAMES
      PARAMETER (M03SD_FRAMES  = 'SD frames')

C     Number of space view frames in scan
      CHARACTER*(*)M03SV_FRAMES
      PARAMETER (M03SV_FRAMES  = 'SV frames')

C     Earth view start time (TAI)
      CHARACTER*(*)M03EV_START_TIME
      PARAMETER (M03EV_START_TIME = 'EV start time')  

C     Solar diffuser view start time (TAI)
      CHARACTER*(*)M03SD_START_TIME
      PARAMETER (M03SD_START_TIME = 'SD start time')  

C     Space view start time (TAI)
      CHARACTER*(*)M03SV_START_TIME
      PARAMETER (M03SV_START_TIME = 'SV start time')  

C     Sun vector zenith angle in SD frame (with respect to SD Z axis) 
      CHARACTER*(*)M03SD_SUN_ZENITH
      PARAMETER (M03SD_SUN_ZENITH = 'SD Sun zenith')  

C     Sun vector azimuth angle in SD frame 
C     (CW rotation about Z axis from Y axis)
      CHARACTER*(*)M03SD_SUN_AZIMUTH
      PARAMETER (M03SD_SUN_AZIMUTH = 'SD Sun azimuth')  

C     Mirror side
      CHARACTER*(*)M03MIR_SIDE
      PARAMETER (M03MIR_SIDE = 'Mirror side')

C     Moon unit vector in instrument frame
      CHARACTER*(*)M03MOON_VECTOR
      PARAMETER (M03MOON_VECTOR = 'Moon Vector')  

C     Unit Sun vector in ECR frame at scan center time SDS 
      CHARACTER*(*)M03SUN_REF
      PARAMETER (M03SUN_REF  ='sun_ref')  

C     Number of mirror encoder samples for this scan SDS 
      CHARACTER*(*)M03NUM_IMPULSE
      PARAMETER (M03NUM_IMPULSE  ='num_impulse')      

C     Mirror angles from encoder data SDS
      CHARACTER*(*)M03IMPULSE_ENC
      PARAMETER (M03IMPULSE_ENC  ='impulse_enc')      

C     Mirror encoder sample times from start of scan SDS;   
      CHARACTER*(*)M03IMPULSE_TIME
      PARAMETER (M03IMPULSE_TIME  ='impulse_time')    

C     L1A scan quality flags   (defined by Level 1A)
      CHARACTER*(*)M03L1_SCAN_QUALITY
      PARAMETER (M03L1_SCAN_QUALITY ='L1 scan quality')    

C     Geolocation scan quality flags 
      CHARACTER*(*)M03GEO_SCAN_QUALITY
      PARAMETER (M03GEO_SCAN_QUALITY ='Geo scan quality')    

C     Scan-Level Navigation Metadata
C
C     Earth view center frame time (TAI)
      CHARACTER*(*)M02SW_EV_CENTER_TIME
      PARAMETER (M02SW_EV_CENTER_TIME  ='EV center time') 
      CHARACTER*(*)M03EV_CENTER_TIME
      PARAMETER (M03EV_CENTER_TIME  = M02SW_EV_CENTER_TIME) 

C     ECR orbit position at scan center time SDS 
      CHARACTER*(*)M03ORB_POS
      PARAMETER (M03ORB_POS  ='orb_pos')  

C     ECR orbit velocity at scan center time SDS 
      CHARACTER*(*)M03ORB_VEL
      PARAMETER (M03ORB_VEL  ='orb_vel')  

C     ECR-to-instrument frame transformation matrix at scan center time SDS 
      CHARACTER*(*)M03T_INST2ECR
      PARAMETER (M03T_INST2ECR  ='T_inst2ECR')  

C     Spacecraft attitude at scan center time expressed in the Orbital
C     Reference frame.
      CHARACTER*(*)M03ATTITUDE_ANGLES
      PARAMETER (M03ATTITUDE_ANGLES  ='attitude_angles')  

C     Spatial Element Geolocation Data
C
C     Geodetic longitude SDS  
      CHARACTER*(*)M03LONGITUDE
      PARAMETER (M03LONGITUDE  ='Longitude')    

C     Geodetic latitude SDS 
      CHARACTER*(*)M03LATITUDE
      PARAMETER (M03LATITUDE   ='Latitude')     

C     Height above ellipsoid SDS 
      CHARACTER*(*)M03HEIGHT
      PARAMETER (M03HEIGHT  ='Height')    

C     Sensor zenith SDS  
      CHARACTER*(*)M03SENSOR_ZEN
      PARAMETER (M03SENSOR_ZEN  ='SensorZenith')            

C     Sensor azimuth SDS  
      CHARACTER*(*)M03SENSOR_AZ
      PARAMETER (M03SENSOR_AZ  ='SensorAzimuth')            

C     Range (pixel to sensor) SDS 
      CHARACTER*(*)M03RANGE
      PARAMETER (M03RANGE  = 'Range')                  

C     Solar zenith SDS 
      CHARACTER*(*)M03SOLAR_ZENITH
      PARAMETER (M03SOLAR_ZENITH  = 'SolarZenith')           

C     Solar azimuth SDS  
      CHARACTER*(*)M03SOLAR_AZIMUTH
      PARAMETER (M03SOLAR_AZIMUTH  = 'SolarAzimuth')         

C     EOS Land/Sea Mask SDS 
      CHARACTER*(*)M03LAND_SEAMASK
      PARAMETER (M03LAND_SEAMASK = 'Land/SeaMask')

C     Geolocation flags SDS 
      CHARACTER*(*)M03GFLAGS
      PARAMETER (M03GFLAGS   ='gflags')         


!C***********************************************************************
C                 MODIS GEOLOCATION CONTROL POINT PRODUCT FORMAT  
C           MODIS SDST, 3/30/98, Geolocation data, Version 1.1      
!C***********************************************************************

C     Product type identifier       
      CHARACTER*(*)M03CP_PROD_ID
      PARAMETER (M03CP_PROD_ID    ='M0D03CP')

C     Control Point Specific Metadata
      CHARACTER*(*)M03CSCID
      PARAMETER (M03CSCID ='Spacecraft ID')         
      CHARACTER*(*)M03CPROD_DATE
      PARAMETER (M03CPROD_DATE = 'Production Date')         
      CHARACTER*(*)M03C_GRANULENUMBER
      PARAMETER (M03C_GRANULENUMBER = 'GRANULENUMBER')         
      CHARACTER*(*)M03C_RANGE_BEG_DATE
      PARAMETER (M03C_RANGE_BEG_DATE = 'RANGEBEGINNINGDATE')         
      CHARACTER*(*)M03C_RANGE_BEG_TIME
      PARAMETER (M03C_RANGE_BEG_TIME = 'RANGEBEGINNINGTIME')         
      CHARACTER*(*)M03C_RANGE_END_DATE
      PARAMETER (M03C_RANGE_END_DATE = 'RANGEENDINGDATE')         
      CHARACTER*(*)M03C_RANGE_END_TIME
      PARAMETER (M03C_RANGE_END_TIME = 'RANGEENDINGTIME')         
      CHARACTER*(*)M03CSC_START_POS
      PARAMETER (M03CSC_START_POS = 's/c start position')      
      CHARACTER*(*)M03CSC_END_POS
      PARAMETER (M03CSC_END_POS = 's/c end position')      
      CHARACTER*(*)M03C_ORBIT_NUM
      PARAMETER (M03C_ORBIT_NUM = 'ORBITNUMBER')      
      CHARACTER*(*) M03CPARAM_VERS   
      PARAMETER (M03CPARAM_VERS =  'Parameter Version')
      CHARACTER*(*) M03CGEO_PARAM_VERS   
      PARAMETER (M03CGEO_PARAM_VERS =  'Geo Parameter Version')
      CHARACTER*(*) M03CBAND_NUM
      PARAMETER (M03CBAND_NUM = 'Band_number')
      CHARACTER*(*) M03CCLOUD_SEL
      PARAMETER (M03CCLOUD_SEL =  'Cloud_select')
      CHARACTER*(*) M03CSNOW_SEL
      PARAMETER (M03CSNOW_SEL =  'Snow_select')
      CHARACTER*(*) M03CICE_SEL
      PARAMETER (M03CICE_SEL =  'Ice_select')
      CHARACTER*(*) M03CLAND_SEL
      PARAMETER (M03CLAND_SEL =  'land_select')
      CHARACTER*(*) M03C1ISLE_SEL
      PARAMETER (M03C1ISLE_SEL =  'single_island_select')
      CHARACTER*(*) M03C3ISLE_SEL
      PARAMETER (M03C3ISLE_SEL =  'triplet_select')
      CHARACTER*(*) M03CNUM_RECORDS
      PARAMETER (M03CNUM_RECORDS =  'Number of Records')

C     Control Point Match Vdata
      CHARACTER*(*) M03CP_MATCHES
      PARAMETER (M03CP_MATCHES =  'Control Point Matches')
C
C     Vdata Field Names
      CHARACTER*(*) M03CP_LOCX
      PARAMETER (M03CP_LOCX =  'Control Point Location x')
      CHARACTER*(*) M03CP_LOCY
      PARAMETER (M03CP_LOCY =  'Control Point Location y')
      CHARACTER*(*) M03CP_LOCZ
      PARAMETER (M03CP_LOCZ =  'Control Point Location z')
      CHARACTER*(*) M03CP_OBSX
      PARAMETER (M03CP_OBSX =  'Observed Control Point x')
      CHARACTER*(*) M03CP_OBSY
      PARAMETER (M03CP_OBSY =  'Observed Control Point y')
      CHARACTER*(*) M03CP_OBSZ
      PARAMETER (M03CP_OBSZ =  'Observed Control Point z')
      CHARACTER*(*) M03CP_SCPOSX
      PARAMETER (M03CP_SCPOSX =  'S/C position x')
      CHARACTER*(*) M03CP_SCPOSY
      PARAMETER (M03CP_SCPOSY =  'S/C position y')
      CHARACTER*(*) M03CP_SCPOSZ
      PARAMETER (M03CP_SCPOSZ =  'S/C position z')
      CHARACTER*(*) M03CP_SCVELX
      PARAMETER (M03CP_SCVELX =  'S/C velocity x')
      CHARACTER*(*) M03CP_SCVELY
      PARAMETER (M03CP_SCVELY =  'S/C velocity y')
      CHARACTER*(*) M03CP_SCVELZ
      PARAMETER (M03CP_SCVELZ =  'S/C velocity z')
      CHARACTER*(*) M03CP_SCROLL
      PARAMETER (M03CP_SCROLL =  'S/C attitude roll')
      CHARACTER*(*) M03CP_SCPITCH
      PARAMETER (M03CP_SCPITCH = 'S/C attitude pitch')
      CHARACTER*(*) M03CP_SCYAW
      PARAMETER (M03CP_SCYAW =   'S/C attitude yaw')
      CHARACTER*(*) M03CP_OBST
      PARAMETER (M03CP_OBST =  'Time of observation')
      CHARACTER*(*) M03CP_VIEWX
      PARAMETER (M03CP_VIEWX =  'Control Point view vector x')
      CHARACTER*(*) M03CP_VIEWY
      PARAMETER (M03CP_VIEWY =  'Control Point view vector y')
      CHARACTER*(*) M03CP_VIEWZ
      PARAMETER (M03CP_VIEWZ =  'Control Point view vector z')
      CHARACTER*(*) M03CP_OBS_LINE
      PARAMETER (M03CP_OBS_LINE =  'Observation line number')
      CHARACTER*(*) M03CP_OBS_FRAME
      PARAMETER (M03CP_OBS_FRAME =  'Observation frame number')
      CHARACTER*(*) M03CP_DEM_ERROR
      PARAMETER (M03CP_DEM_ERROR =  'DEM uncertainty')
      CHARACTER*(*) M03CP_MEAS_ERROR
      PARAMETER (M03CP_MEAS_ERROR =  'Measurement uncertainty')
      CHARACTER*(*) M03CP_ID
      PARAMETER (M03CP_ID =  'Control Point ID')
      CHARACTER*(*) M03CP_SCAN_NUM
      PARAMETER (M03CP_SCAN_NUM =  'Scan Number')
      CHARACTER*(*) M03CP_TYPE
      PARAMETER (M03CP_TYPE =  'Control Point Type')
      CHARACTER*(*) M03CP_BAND
      PARAMETER (M03CP_BAND =  'MODIS band used')
      CHARACTER*(*)M03CP_MIR_SIDE
      PARAMETER (M03CP_MIR_SIDE  ='Mirror Side')      
      CHARACTER*(*) M03CP_ERROR_FLAG
      PARAMETER (M03CP_ERROR_FLAG =  'Error Flag')
      CHARACTER*(*) M03CP_SPARE1
      PARAMETER (M03CP_SPARE1 =  'Spare1')
      CHARACTER*(*) M03CP_SPARE2
      PARAMETER (M03CP_SPARE2 =  'Spare2')
