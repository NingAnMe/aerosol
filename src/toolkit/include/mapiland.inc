CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C                 mapiland.inc utilities header file                 C
C                                                                    C
C                            MODLAND                                 C
C                                                                    C
C                 HDF Object Identifier Constants                    C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

C----------------------------------------------------------------------
C !F77-INC
C
C !Purpose:      Utilities header file containing constants for MOD LAND
C               discipline HDF object names.
C
C !Description: 
C
C    The header file mapiland.inc is part of a larger software system
C    called the MODIS Applications Programming Interface (API) Utility,
C    abbreviated M-API.  The M-API Utility consists of subroutines which
C    allow MODIS Science Team supplied software to read and write data
C    and metadata fromCto HDF files.  The functionality of the M-API is
C    defined in the MODIS API Specification.
C
C    The mapiland.inc file contains macros for the the specific names
C    of data objects contained in the following MODIS product HDF files:
C
C                MOD.AM1.geoang.L2G              Vers V1 Rev 1 29-Apr-1996
C                MOD.AM1.pntr_xxxx.L2            Vers V1 Rev 5 11-Jun-1996
C                MOD PR09_L2G_xxxx               Vers V1 Rev 2 08-May-1996 
C                MOD.AM1,brdfsubs.L3             Vers V1 Rev 2 08-Apr-1996
C                MOD.AM1.texture_16dy.L3         Vers V1 Rev 3 24-Apr-1996
C                MOD.AM1.bars_16dy.L3            Vers V1 Rev 2 08-Apr-1996
C                MOD.AMI.brdf_16dy.L3            Vers V1 Rev 3  8-Apr-1996
C                MOD.AM1.sref1.L2                Vers V1 Rev 3 24-Apr-1996
C                MOD.AM1.sref1_250m.L2G          Vers V1 Rev 2 08-May-1996
C                MOD.AM1.sref1_500m.L2G          Vers V1 Rev 2 08-May-1996
C                MOD10_L2                        Vers V2 Rev 2 03-Oct-1997
C                MOD10L2G                        Vers V2 Rev 3 05-Dec-1997 
C                MOD10A1                         Vers V2 Rev 2 23-Dec-1997
C                MOD.AM1.V1.lst.L2               Vers V1 Rev 5 16-Apr-1996
C                MOD11_L2G                       Beta
C                MOD.AM1.V1.lst_1dy_cmg.L3       Vers V1 Rev 3 17-Jun-1996
C                MOD.AM1.V1.lst_8dy_cmg.L3       Vers V1 Rev 3 17-Jun-1996
C                MOD.AM1.V1.lst_1m_cmg.L3        Vers V1 Rev 3 17-Jun-1996
C                MOD.AM1.V1.lc_1km.L3.3m         Vers V1 Rev 4 16-Apr-1996
C                MOD.AM1.V1.landcov_32dy.L3      Vers V1 Rev 8 29-May-1996
C                MOD.AM1.V1.lc.L3.1m             Vers V1 Rev 4 16-Apr-1996
C                MOD.AM1.V1.lc_halfdegree.L3.3m  Vers V1 Rev 4 16-Apr-1996
C                MOD14_L2                        Vers V1 Rev 3 02-May-1996
C                MOD.AM1.V1.fire.L2G             Vers V1 Rev 2 08-May 1996
C                MOD14_L3                        Beta          28-Nov-1995
C                MOD.AM1.V1.fpar_lai_8dy.L3      Vers V1 Rev 4 09-Sep-1996
C                MOD.AM1.V1.fpar_lai_8dy_cmg.L3  Vers V1 Rev 4 09-Sep-1996
C                MOD.AM1.V1.npp_1yr.L4           Vers V1 Rev 4 09-Sep-1996
C                MOD.AM1.V1.npp_1yr_cmg.L4       Vers V1 Rev 4 09-Sep-1996
C                MOD.AM1.V1.psn_8dy.L4           Vers V1 Rev 4 09-Sep-1996
C                MOD.AM1.V1.psn_8dy_cmg.L4       Vers V1 Rev 4 09-Sep-1996
C                MOD.AM1.V1.seaice_max.L2        Vers V1 Rev 7 24-Apr-1996
C                MOD29_L2G                       Beta
C                MOD.AM1.seaice_max_dy.L3        Vers V1 Rev 5 24-Apr-1996
C                MOD.AM1.V1.snow_10dy.L3         Vers V1 Rev 6 24-Apr-1996
C                MOD.AM1.vi_1m.L3                Vers V1 Rev 6 06-May-1996
C                MOD.AM1.V1.vi_8dy.L3            Vers V1 Rev 6 06-May-1996
C                MOD.AM1.vi_16dy.L3              Vers V1 Rev 6 06-May-1996
C                MOD.AM1.vi_8dy_cmg.L3           Vers V1 Rev 6 06-May-1996
C                MOD.AM1.vi_16dy_cmg.L3          Vers V1 Rev 5 24-Apr-1996
C                MOD.AM1.vi_1m_cmg.L3            Vers V1 Rev 6 06-May-1996
C                MOD.AM1.V1.seaice_10dy.L3       Vers V1 Rev 6 24-Apr-1996
C
C
C !Input Parameters: none.
C
C !Output Parameters: none.
C
C !Revision History: 
C 
C !Team-unique Header: 
C
C   This software is developed by the MODIS Science Data Support
C   Team for the National Aeronautics and Space Administration,
C   Goddard Space Flight Center, under contract NAS5-32373.
C
C !References and Credits: 
C
C   Written by    Vicky Lin        01/29/96
C   Research and Data Systems Corporation
C   SAIC/GSC MODIS SCIENCE DATA SUPPORT OFFICE
C   7501 FORBES BLVD, SEABROOK MD 20706
C   vlin@ltpmail.gsfc.nasa.gov
C
C       modified by
C               Frederick J. Shaw   10-Apr-96
C               General Science Corp.
C               MODIS SCIENCE DATA SUPPORT OFFICE
C               7501 Forbes Blvd. Seabrook MD 20706
C               fshaw@@ltpmail.gsfc.nasa.gov
C               updated include file using version 1 file specs.
C
C	modified by
C		Arvind Solanki	05-Mar-97
C               General Science Corp.
C               MODIS SCIENCE DATA SUPPORT OFFICE
C               7501 Forbes Blvd. Seabrook MD 20706
C               solanki@@ltpmail.gsfc.nasa.gov
C               Updated include file snow cover parts 
C               using version 2 file specs.
C
C !END
C----------------------------------------------------------------------+
                                                                        

C-------------------------  MOD LAND  ----------------------------+
C                    M-Common-A Metadata                          |
C--                  Common Global Metadata                       | 
C-----------------------------------------------------------------+

      character*(*) MLPIXELS_PER_SCAN, MLNUMBER_OF_LINES,
     &              MLTOTALNUMRETRIEVEDLSTGRIDSINFILE  
      parameter (MLPIXELS_PER_SCAN = 'Pixels_per_scan_line')
      parameter (MLNUMBER_OF_LINES = 'Number_of_scan_lines')
      parameter (MLTOTALNUMRETRIEVEDLSTGRIDSINFILE      =
     &     'TOTALNUMBEROFRETRIEVEDLSTGRIDSINFILE')

C------------------------- L2Gs & L3s ----------------------------*

      character*(*) MLPIXELS_PER_LINE, MLLINES_PER_TILE, MLPARM1, 
     &              MLTOTAL_OBSERVATIONS, MLNUMBER_OF_PARAMS, MLPARM2,
     &              MLMAX_OBSERVATIONS, MLNUMBER_OF_GRANULES, MLPARM3,
     &              MLGRANULE_IDS, MLFILE_FORMAT, MLPARM4, MLPARM5, 
     &              MLPARM6, MLPARM7, MLYEAR, MLDOY, MLNUMBER_OF_ROWS, 
     &              MLNEST_LEVEL, MLREF_LONGITUDE, MLANGULAR_SIZE, 
     &              MLIROW_START, MLNCOL_MAX, MLITILE_HORIZ, 
     &              MLITILE_VERT, MLNTILE_HORIZ, MLNTILE_VERT,
     &              MLMAX_LINE_INDEX, MLMAX_SAMPLE_INDEX,
     &              MLMIN_LINE_INDEX, MLMIN_SAMPLE_INDEX 

      parameter (MLPIXELS_PER_LINE    = 'Pixels_per_line')
      parameter (MLLINES_PER_TILE     = 'Lines_per_tile')
C     parameter (MLTOTAL_OBSERVATIONS = 'Total_observations')
      parameter (MLTOTAL_OBSERVATIONS = 'Total Observations')
      parameter (MLNUMBER_OF_PARAMS   = 'Num_parameters')
C     parameter (MLMAX_OBSERVATIONS   = 'Maximum_observations')
      parameter (MLMAX_OBSERVATIONS   = 'Maximum Observations')
C     parameter (MLNUMBER_OF_GRANULES = 'Number_of_granules')
      parameter (MLNUMBER_OF_GRANULES = 'Number of Granules')
      parameter (MLGRANULE_IDS        = 'Granule_IDs')
      parameter (MLMAX_LINE_INDEX     = 'Maximum Line Index')      
      parameter (MLMAX_SAMPLE_INDEX   = 'Maximum Sample Index')
      parameter (MLMIN_LINE_INDEX     = 'Minimum Line Index')
      parameter (MLMIN_SAMPLE_INDEX   = 'Minimum Sample Index')

C  OBJECT = L2G Storage Format     
C  DEFINITION = A flag relflecting if the file is in
C               the 'Full' or 'Compact'  MODLAND L2G file format. 
C     parameter (MLFILE_FORMAT = 'File_Format')
      parameter (MLFILE_FORMAT = 'L2G Storage Format')
      parameter (MLPARM1       = 'Parameter1')
      parameter (MLPARM2       = 'Parameter2')
      parameter (MLPARM3       = 'Parameter3')
      parameter (MLPARM4       = 'Parameter4')
      parameter (MLPARM5       = 'Parameter5')
      parameter (MLPARM6       = 'Parameter6')
      parameter (MLPARM7       = 'Parameter7')
      parameter (MLYEAR        = 'Year')
      parameter (MLDOY         = 'Day_of_year')
      parameter (MLNUMBER_OF_ROWS = 'nrow')

C  DEFINITION = Grid nesting level, an integer fraction of the
C               number of rows in the MODLAND integerized sinusoidal
C               grid.                                   
C     parameter (MLNEST_LEVEL     = 'nest_lev')
      parameter (MLNEST_LEVEL     = 'Grid Nesting Level')

      parameter (MLREF_LONGITUDE  = 'ref_lon_in_deg')

C  OBJECT = Characteristic Bin Angular Dimension
C  DEFINITION = The characteristic bin angular dimension
C               in arc-seconds of this MODLAND tile.
C     parameter (MLANGULAR_SIZE   = 'ang_size_in_arcsec')
      parameter (MLANGULAR_SIZE 
     &           = 'Characteristic Bin Angular Dimension')

      parameter (MLIROW_START     = 'irow_start')
      parameter (MLNCOL_MAX       = 'ncol_max')
      parameter (MLITILE_HORIZ    = 'itile_horiz')
      parameter (MLITILE_VERT     = 'itile_vert')
      parameter (MLNTILE_HORIZ    = 'ntile_horiz')
      parameter (MLNTILE_VERT     = 'ntile_vert')

C----------------------------------------------------------------------+
C                       M-Common A Metadata                            |
C                       Object Level Attributes                        |
C----------------------------------------------------------------------+ 
      character *(*) MLDATA_LINES, MLMAX_NUMBER_DATA_SAMPLES_LINE,
     &               MLNADIR_DATA_RES
C  OBJECT = Data Lines                                            
C  DEFINITION = The number of data lines contained in this SDS.   
      parameter (MLDATA_LINES   =       'Data Lines')

C  OBJECT = Maximum Number of Data Samples Per Line                       
C  DEFINITION = The maximum number of data samples per line in this SDS.   
      parameter (MLMAX_NUMBER_DATA_SAMPLES_LINE = 
     &           'Maximum Number of Data Samples Per Line')

C  OBJECT = Nadir Data Resolution
C  DEFINITION = Describes the spatial resolution at nadir of the data
C               stored in  an SDS.
      parameter (MLNADIR_DATA_RES  =        'Nadir Data Resolution')

C----------------------------------------------------------------------+
C       M-Common-B Metadata                                                
C       Global Attributes                                                 
                                                                        
C----------------------------------------------------------------------+
      character*(*) MLCENTRAL_MERIDIAN, MLCHAR_BIN_SIZE,
     &              MLCOL_STORAGE_FORMAT, MLDATA_COLUMNS, MLDATA_ROWS,
     &              MLDATA_UPPER_LEFT_COORD, MLGLOBAL_GRID_COLUMNS,
     &              MLGLOBAL_GRID_ROWS, MLGLOBAL_GRID_COR_BIN_CTR,
     &              MLPOLE, MLSPHERE_RADIUS,
     &              MLNUMBER_OF_LST_DIFFERENCE_BINS


C  OBJECT = Central Meridian                                            
C  DEFINITION = 
      parameter ( MLCENTRAL_MERIDIAN        = 'Central Meridian')

C  OBJECT = Characteristic Bin Size                                
C  DEFINITION = The characteristic bin size of this MODLAND tile.   
      parameter ( MLCHAR_BIN_SIZE           = 'Characteristic Bin Size')

C  OBJECT = Column Storage Format                                           
C  DEFINITION = Specifies whether the extra data column in the MODLAND      
C  integerized sinusoidal grid tile is to the left or right of the center. 
      parameter ( MLCOL_STORAGE_FORMAT      = 'Column Storage Format')

C  OBJECT = Data Columns                                                      
C  DEFINITION = The maximum number of data columns (samples) per row in this  
C  SDS.                                                                       
      parameter ( MLDATA_COLUMNS            = 'Data Columns')

C  OBJECT = Data Rows                                                   
C  DEFINITION = The number of data rows contained in this MODIS tile.    
      parameter ( MLDATA_ROWS               = 'Data Rows')

C  OBJECT = Data Upper Left Coordinates                                    
C  DEFINITION = The horizontal and vertical coordinates of the upper left  
C  corner of the MODLAND tile in reference to the MODLAND integerized      
C  sinusoidal grid.                                                         
      parameter ( MLDATA_UPPER_LEFT_COORD =
     &            'Data Upper Left Coordinates')

C  OBJECT = Global Grid Columns                                               
C  DEFINITION = The number of global grid columns in the MODLAND integerized  
C  sinusoidal grid.                                                           
      parameter ( MLGLOBAL_GRID_COLUMNS     = 'Global Grid Columns')

C  OBJECT = Global Grid Rows                                                  
C  DEFINITION = The number of global grid rows in the MODLAND
C               integerized sinusoidal grid.                                  
      parameter ( MLGLOBAL_GRID_ROWS        = 'Global Grid Rows')

C  OBJECT = Global Grid Upper Left Corner Bin Center                          
C  DEFINITION = The location of the center of the MODLAND global grid
C               upper left corner bin in global grid coordinates
C               (in meters).            
      parameter ( MLGLOBAL_GRID_COR_BIN_CTR =
     &            'Global Grid Upper Left Corner Bin Center')

C  OBJECT = Pole                                                            
C  DEFINITION = The pole specified in this lambert azimuthal equal area     
C  projection.                                                               
      parameter ( MLPOLE                    = 'Pole')

      parameter ( MLNUMBER_OF_LST_DIFFERENCE_BINS =  
     &            'Number of LST Difference Bins')

C  OBJECT = Sphere Radius                                                   
C  DEFINITION = The sphere radius in meters of this MODLAND
C               global grid.     
      parameter ( MLSPHERE_RADIUS           = 'Sphere Radius')

C-------------------------  MOD LAND  ----------------------------*
C--                       Common SDS's                          --*
C-----------------------------------------------------------------+
      character*(*) MLNUMBER_OF_OBS, MLNUMBER_OF_COLS, MLSTART_COLUMN, 
     &              MLCOLS_PER_ROW, MLSTART_PIX, MLOBS_PER_LINE

C L2G number of observations per pixel contained within L2G file SDS
      parameter (MLNUMBER_OF_OBS = 'num_observations')
 
C The number of columns in the full ISCCP grid for each row (line) 
C contained within the L2G file SDS.       
      parameter (MLNUMBER_OF_COLS = 'ncol')
 
C The start column in the full ISCCP grid for each row (line) 
C contained within the L2G file (starting at zero) SDS. 
      parameter (MLSTART_COLUMN = 'icol_start')
 
C The number of columns in each row contained within the L2G file SDS.  
      parameter (MLCOLS_PER_ROW = 'ncol_tile')
 
C The start pixel of the first valid column in each row (line)
C contained within the L2G file (starting at zero) SDS. 
      parameter (MLSTART_PIX    = 'ipix_start')
 
C Number of observations per line SDS
      parameter (MLOBS_PER_LINE = 'nobs_line')

C Columns per Global Grid Row                                   
      character*(*) MLCOLUMNS_GLOBAL_GRID_ROW  
      parameter (MLCOLUMNS_GLOBAL_GRID_ROW 
     &           = 'Columns per Global Grid Row')

      character*(*) ML1KMDATA_SAM_PER_LINE, MLNUM_INSTR_SCANS 

C  SDS_OBJECT = 1km Data Samples Per Line 
C  LONG_NAME = 1km Data Samples Per 1km Data Line in this MODIS Granule
C  DEFINITION = 1km Data Samples Per 1km Data Line in this MODIS Granule
      parameter ( ML1KMDATA_SAM_PER_LINE  = '1km Data Samples Per Line')

C   OBJECT = Number of Instrument Scans                                      
C   DEFINITION = Number of instrument scans contained
C                in this MODIS granule.  
      parameter ( MLNUM_INSTR_SCANS  =  'Number of Instrument Scans')


C-------------------------  MOD LAND  -------------------------------------+
C                    Object Level Attributes                               |
C--------------------------------------------------------------------------+

      character*(*) MLSPSO_parameterS
      parameter (MLSPSO_parameterS = 'SPSO_parameter')

C-------------------------  MOD LAND  -------------------------------------+
C                    E-Product-Inventory                                   |
C                    Metadata                                              |
C--------------------------------------------------------------------------+

 
      character*(*) MLSUBS_CODE_ID, MlPERCENT_LAND_IN_TILE,
     &              MLPERCENT_FILLED_PIXELS, MLBRDF_CODE_ID,
     &              MLMODEL_DEF_FILE_ID, MLALBE_DO_TABLE_ID, 
     &              MLPERCENT_LAND_INTILE, MLPERCENT_NEW_BRDFS, 
     &              MLPERCENT_ADJUSTED_BRDFS, MLPERCENT_FILL_BRDFS,
     &              MLPERCENT_GOOD_QUALITY, MLPERCENT_NEW_BARS,
     &              MLPERCENT_ADJUSTED_BARS, MLPERCENT_FILL_BARS,
     &              MLTOTAL_RETR_LST_SAM_INFILE 
                                                        
      parameter( MLSUBS_CODE_ID            = 'SUBSCODEID')
      parameter( MlPERCENT_LAND_IN_TILE    = 'PERCENTLANDINTILE')
      parameter( MLPERCENT_FILLED_PIXELS   = 'PERCENTFILLEDPIXELS')
      parameter( MLBRDF_CODE_ID            = 'BRDFCODEID')
      parameter( MLMODEL_DEF_FILE_ID       = 'MODELDEFFILEID')
      parameter( MLALBE_DO_TABLE_ID        = 'ALBEDOTABLEID')
      parameter( MLPERCENT_LAND_INTILE     = 'PERCENTLANDINTILE')
      parameter( MLPERCENT_NEW_BRDFS       = 'PERCENTNEWBRDFS')
      parameter( MLPERCENT_ADJUSTED_BRDFS  = 'PERCENTADJUSTEDBRDFS')
      parameter( MLPERCENT_FILL_BRDFS      = 'PERCENTFILLBRDFS')
      parameter( MLPERCENT_GOOD_QUALITY    = 'PERCENTGOODQUALITY')
      parameter( MLPERCENT_NEW_BARS        = 'PERCENTNEWBARS')
      parameter( MLPERCENT_ADJUSTED_BARS   = 'PERCENTADJUSTEDBARS')
      parameter( MLPERCENT_FILL_BARS       = 'PERCENTFILLBARS')
      parameter( MLTOTAL_RETR_LST_SAM_INFILE 
     &            = 'TOTALRETRIEVEDLSTSAMPLESINFILE')

C---------------------------------------------------------------+
C                    MOD.AM1.geoang.L2G                         |
C                    L2G Viewing Geometry File                  |
C                    Vers V1 Rev 1 29-Apr-1996                  |
C---------------------------------------------------------------+
      character*(*) M09ANG_PROD_ID
C     parameter (M09ANG_PROD_ID                = 'MOD09_ANG_L2G_1KM')
      parameter (M09ANG_PROD_ID                = 'MOD.AM1.geoang.L2G')

      character*(*) M09SENSOR_ZENITH, M09SENSOR_AZIMUTH, 
     &              M09SENSOR_DISTANCE, M09SOLAR_ZENITH,
     &              M09SOLAR_AZIMUTH

C Zenith angle to sensor SDS                              
      parameter (M09SENSOR_ZENITH              = 'SensorZenith')
 
C Azimuth angle to sensor SDS                             
      parameter (M09SENSOR_AZIMUTH             = 'SensorAzimuth')
 
C Distance to sensor SDS                          
      parameter (M09SENSOR_DISTANCE            = 'Range')
 
C Zenith angle to sun SDS                                 
      parameter (M09SOLAR_ZENITH               = 'SolarZenith')
 
C Azimuth angle to sun SDS                                
      parameter (M09SOLAR_AZIMUTH              = 'SolarAzimuth')
 
C---------------------------------------------------------------+
C                       MOD.AM1.pntr_xxxx.L2                    |
C                       L2G Pointers                            |
C                       Vers V1 Rev 2 29-April-1996             |
C---------------------------------------------------------------+
      character*(*)  M09PNT1K_PROD_ID
C     parameter (M09PNT1K_PROD_ID       = 'MOD09_PNT_L2G_1KM')
      parameter (M09PNT1K_PROD_ID       = 'MOD.AM1.pntr_1km.L2G')

      character*(*) M09PNT500_PROD_ID
C     parameter (M09PNT500_PROD_ID      = 'MOD09_PNT_L2G_500M')
      parameter (M09PNT500_PROD_ID      = 'MOD.AM1.pntr_500m.L2G')

      character*(*) M09PNT250_PROD_ID
C     parameter (M09PNT250_PROD_ID      = 'MOD09_PNT_L2G_250M')
      parameter (M09PNT250_PROD_ID      = 'MOD.AM1.pntr_250m.L2G')

      character*(*) M09GRANULE_PNT, M09OBS_IN_GRANULE, M09CELL_CENTER,
     &              M09SAMPLE_CENTER, M09OBS_COVERAGE, M09CELL_COVERAGE,
     &              M09L2G_LINE, M09OFFSET_RES, M09IOBS_RES, 
     &              M09NUMBER_OF_OBSERVATIONS_PER_ROW


C Pointer to granule IDs from which the observation came SDS.  
C Zero relative.  Fill value is 255.              
      parameter (M09GRANULE_PNT = 'granule_pnt')
 
C Sample number of observation (1 km spatial element) in granule SDS 
      parameter (M09OBS_IN_GRANULE = 'sample')
 
C Sub-pixel (delta) line location of cell center in observation 
C footprint SDS.  Relative to center of observation specified by 
C (line, sample).                                 
      parameter (M09CELL_CENTER = 'dline')
 
C Sub-pixel (delta) line location of cell center in observation 
C footprint SDS.  Relative to center of observation specified by 
C (line, sample).                                 
      parameter (M09SAMPLE_CENTER = 'dsample')
 
C Observation coverage SDS: area of intersection between 
C observation footprint and cell divided by area of observation.
      parameter (M09OBS_COVERAGE  = 'obscov')
 
C Cell coverage SDS: area of intersection between observation footprint 
C and cell divided by area of cell.
      parameter (M09CELL_COVERAGE = 'cellcov')
 
C SDS Name: line
C !Description:    Line number of observation in granule.  Zero relative.   
      parameter (M09L2G_LINE    =   'line')

C SDS Name: offset_res
C !Description: Row and column offset of corresponding observation in
C               next coarser resolution grid.                             
      parameter( M09OFFSET_RES  =    'offset_res')

C SDS Name:   iobs_res
C !Description: Number of corresponding observation in next coarser
C              resolution grid.                                           
      parameter( M09IOBS_RES  =    'iobs_res')

C SDS Name:      Number of Observations per Row
C !Description:   The number of observations in each row in the compact
C                storage form of the L2G grid.                           
      parameter( M09NUMBER_OF_OBSERVATIONS_PER_ROW =
     &            'Number of Observations per Row')

C-----------------------------------------------------------------------+
C            MOD PR09_L2G_500M                                          |
C            L2G Surface Reflectance and VI Parameters                  |
C            Version V1 rev 2 08-May-1996                               |  
C---------1---------2---------3---------4---------5---------6---------7-+
      character*(*) M09_L2G_500M_PROD_ID 
C     parameter (M09_L2G_500M_PROD_ID      = 'MOD09_L2G_500M')
      parameter (M09_L2G_500M_PROD_ID   = 'MOD.AM1.V1.srefl_500m.L2G')

      character*(*) M09BAND3_SURF_REFL, M09BAND4_SURF_REFL,
     &              M09BAND5_SURF_REFL, M09BAND6_SURF_REFL, 
     &              M09BAND7_SURF_REFL, M09QUALITY_500

C  Surface Reflectance for MODIS Band 3 SDS               
      parameter (M09BAND3_SURF_REFL = 'sur_refl_b03')
 
C Surface Reflectance for MODIS Band 4 SDS                
      parameter (M09BAND4_SURF_REFL = 'sur_refl_b04')
 
C Surface Reflectance for MODIS Band 5 SDS                
      parameter (M09BAND5_SURF_REFL = 'sur_refl_b05')
 
C Surface Reflectance for MODIS Band 6 SDS                
      parameter (M09BAND6_SURF_REFL = 'sur_refl_b06')
 
C Surface Reflectance for MODIS Band 7 SDS                
      parameter (M09BAND7_SURF_REFL = 'sur_refl_b07')
 
C Indicators of the quality of the 500 m reflectance data SDS 
      parameter (M09QUALITY_500 = 'QC_500m')
 
C---------------------------------------------------------------+
C            MOD PR09_L2G_250M                                  |
C            250M Resolution L2G Surface Reflectance            |
C            Version V1 Rev 2  08-May-96                        | 
C---------------------------------------------------------------+
      character*(*) M09_L2G_250M_PROD_ID 
C     parameter (M09_L2G_250M_PROD_ID      = 'MOD09_L2G_250M')
      parameter (M09_L2G_250M_PROD_ID   = 'MOD.AM1.V1.srefl_250m.L2G')

      character*(*) M09BAND1_SURF_REFL, M09BAND2_SURF_REFL, 
     &              M09QUALITY_250
 
C  Surface Reflectance for MODIS Band 1 SDS        
      parameter (M09BAND1_SURF_REFL = 'sur_refl_b01')

C Surface Reflectance for MODIS Band 2 SDS         
      parameter (M09BAND2_SURF_REFL = 'sur_refl_b02')

C Indicators of the quality of the 250 m reflectance and
C VI data integrity SDS.                   
      parameter (M09QUALITY_250     = 'QC_250m')
C---------------------------------------------------------------+
C                      MOD PR09_REFLDB                          |
C          16 day L2G database for input into MOD09-BRDF        |
C          into the BRDF/Albedo process, appended to daily      |          
C                      Version V1  rev 2  8-Apr-96              |     
C---------------------------------------------------------------+
      character*(*) M09_REFLDB_PROD_ID
C     parameter (M09_REFLDB_PROD_ID     = 'MOD09SUBS_L2G_16DY')
      parameter (M09_REFLDB_PROD_ID     = 'MOD.AM1.brdfsubs.L3')

      character*(*) M09_REFLDB_ANGULAR_SIZE, M09_OBS_INFO, 
     &              M09_OBS_INFO_WORDS, M09_ANGLES, M09_ANGLES_OBS,
     &              M09_ANGLES_NUM, M09_REFLDB_SURF_REFL, 
     &              M09_SURF_REFL_OBS,  
     &              M09_QUALITY_WEIGHTS, M09_QUALITY_OBS, 
     &              M09_QUALITY_WORDS, M09_SURF_REFL_BANDS, 
     &              M09_SURF_REFL_ITEMS 

C Global Metadata                                         
      parameter (M09_REFLDB_ANGULAR_SIZE = 'ang_size (in arcsec)')
 
C General information on observational basis SDS          
      parameter (M09_OBS_INFO            = 'Obs_Info')

C Obs_Info_Items #3-#9 Number of observations in each band in the
C                      16-day period.
C SDS attribute: 
C     parameter (M09_OBS_INFO_WORDS      = 'words')
      parameter (M09_OBS_INFO_WORDS      = 'Obs_Info_Items')
C     parameter (M09_ANGLES_OBS          = 'N_obs_dy')
      parameter (M09_ANGLES_OBS          = 'Num_Obs_Max')
C     parameter (M09_ANGLES_NUM          = 'N_angles')
      parameter (M09_ANGLES_NUM          = 'Num_Angles')
C     parameter (M09_SURF_REFL_OBS       = 'N_obs_dy')
      parameter (M09_SURF_REFL_OBS       = 'Num_Obs_Max')
C     parameter (M09_QUALITY_OBS         = 'N_obs_dy')
      parameter (M09_QUALITY_OBS         = 'Num_Obs_Max')
C     parameter (M09_QUALITY_WORDS       = 'words')
      parameter (M09_QUALITY_WORDS       = 'Num_Weights_QC')
C     parameter (M09_SURF_REFL_BANDS     = 'N_bands')
      parameter (M09_SURF_REFL_BANDS     = 'Num_Land_Bands')
      parameter (M09_SURF_REFL_ITEMS     = 'Obs_Info_Item')

C Viewing and illumination angles SDS                     
      parameter (M09_ANGLES              = 'Angles')

C Surface reflectances in bands 1- 7 SDS                                
      parameter (M09_REFLDB_SURF_REFL    = 'Surface_Refl')

C Quality and weights of the respective observations     
      parameter (M09_QUALITY_WEIGHTS     = 'Weights_QC')

C---------------------------------------------------------------+
C                MOD.AM1.texture_16dy.L3                        |
C       16-day composited band 2 maximum texture database       |
C                Version V1 Rev 3   24-Apr-96                   |
C---------------------------------------------------------------+  
      character *(*) M09_TEXTURE_16DY_PROD_ID, M09_TEXTURE_BAND2, 
     &               M09_TEXTURE_QC 

      parameter (M09_TEXTURE_16DY_PROD_ID  = 'MOD.AM1.texture_16dy.L3')

C SDS Name:       TEXTURE_BAND2
C !Description:    Texture measure, calculated as the standard deviation of
C               all 250m band 2 surface reflectances associated with each
C               1000m cell, divided by the mean of these 250m reflectances,
C               maximum value over 16 days.                                   
      parameter (M09_TEXTURE_BAND2        =   'TEXTURE_BAND2')

C SDS Name:       TEXTURE_QC
C !Description:    Overall quality of the texture measure
                                                                               
      parameter (M09_TEXTURE_QC           =   'TEXTURE_QC')

C-----------------------------------------------------------------------+
C             MOD.AM1.bars_16dy.L3                         
C             BRDF-adjusted surface reflectances             
C             Version V1 Rev. 2  8-Apr-1996                  
C-----------------------------------------------------------------------+

C               Bars Local Attribute                                    
      character*(*) M09BARS_PROD_ID  

C     parameter  M09BARS_PROD_ID    =     'MOD09_BARS')
      parameter (M09BARS_PROD_ID    =     'MOD.AMI.bars_16dy.L3')

C   Nadir-equivalent surface reflectances for MODIS
C   bands 1-7 SDS			
      character*(*)M09BARS
      parameter (M09BARS                 = 'BARS')

C Num_Bands #1 = Nadir-equivalent surface refl. for MODIS band 1
C Num_Bands #2 = Nadir-equivalent surface refl. for MODIS band 2
C Num_Bands #3 = Nadir-equivalent surface refl. for MODIS band 3
C Num_Bands #4 = Nadir-equivalent surface refl. for MODIS band 4
C Num_Bands #5 = Nadir-equivalent surface refl. for MODIS band 5
C Num_Bands #6 = Nadir-equivalent surface refl. for MODIS band 6
C Num_Bands #7 = Nadir-equivalent surface refl. for MODIS band 7
C SDS Attribue:
      character*(*)M09NUM_BANDS
      parameter (M09NUM_BANDS  =  'Num_Bands')

C Overall quality of the BRDF-adjusted surface reflectances SDS 
      character*(*)M09BARS_QC
      parameter (M09BARS_QC	= 'BARS_QC')

C The number of columns in the full ISCCP grid for each row
C  (line) contained within this L2G file.  SDS	
      character*(*)M09NCOL
      parameter (M09NCOL		= 'ncol')

C The start column in the full ISCCP grid for each row (line)
C contained within this L2G file (starting at zero).  SDS   
      character*(*)M09L2G_ICOL_START
      parameter (M09L2G_ICOL_START	= 'icol_start')

C The number of columns in each row (line) contained within
C this L2G file.  SDS				
      character*(*)M09NCOL_TILE
      parameter (M09NCOL_TILE		= 'ncol_tile')

C The start pixel of the first valid column in each row (line)
C  contained within this L2G file (starting at zero).  SDS   
      character*(*)M09IPIX_START
      parameter (M09IPIX_START          = 'ipix_start')

C--------------------------------------------------------------------------+
C                      MOD_AM1.brdf_16dy.L3	                           |
C                      16 day BRDF product	                           | 
C                      Version V1 rev 5 24-Apr-96                          |
C--------------------------------------------------------------------------+

      character*(*) M09_L3_PROD_ID 

C     parameter (M09_L3_PROD_ID    =         'MOD09_L3_16DY_G') 
      parameter (M09_L3_PROD_ID    =         'MOD_AM1.brdf_16dy.L3')

C Identifier for BRDF models chosen SDS	
      character*(*)M09BRDF_MODEL_ID
      parameter (M09BRDF_MODEL_ID	= 'BRDF_Model_ID')

C-------------------------------------------------------------------------+
C BRDF_Model_ID  SDS                                                      |
C Num_Output_Models #1 is the identifier of the BRDF model                |
C                    chosen as best for each pixel                        |
C Num_Output_Models #2 is the identifier for a globally                   |
C                    uniform BRDF model                                   |
C-------------------------------------------------------------------------+
      character*(*) M09BRDF_NUM_OUTPUT_MODELS
C     parameter (M09BRDF_NUM_OUTPUT_MODELS      =  'Num_Output_Models')
      parameter (M09BRDF_NUM_OUTPUT_MODELS  = 'BRDF_Num_Output_Models')

C RMSE for BRDF models chosen SDS	
      character*(*)M09BRDF_MODEL_RMSE
      parameter (M09BRDF_MODEL_RMSE             = 'BRDF_Model_RMSE')  

C BRDF quality control SDS		
      character*(*)M09QUALITY
C     parameter (M09QUALITY                     = 'QC')
      parameter (M09QUALITY                     = 'Quality_Control')

C BRDF parameters for the seven land bands SDS 
      character*(*)M09BRDF_PARAMETERS
      parameter (M09BRDF_PARAMETERS             = 'BRDF_Parameters')

C Albedo parameters for broadband, < 0.7 mu-m,
C	> 0.7 mu-m, and the seven land bands SDS 
      character*(*)M09ALBEDO
      parameter (M09ALBEDO                      = 'ALbedo')
                     
      character*(*) M09NUM_LAND_BANDS_PLUS3, M09ALBEDO_TYPE,
     &              M09NUM_QC_WORDS, M09INVERSION_INFO,
     &              M09N_INFO

C Num_Land_Bands_Plus3  #9 is the more than 0.7 mu-m broadband
C Num_Land_Bands_Plus3 #10 is the total broadband
C Albedo SDS attribute data:
C----------------------------------------------------------------------+
      parameter (M09NUM_LAND_BANDS_PLUS3 = 'Num_Land_Bands_Plus3')

C----------------------------------------------------------------------+
C Albedo_Type #1 holds white-sky albedo
C        Albedo_Type #2 holds black-sky albedo at the mean sun angle
C----------------------------------------------------------------------+
      parameter (M09ALBEDO_TYPE  =   'Albedo_type')

C----------------------------------------------------------------------+
C  Num_QC_Words #1 gives the information for the best BDRF model
C  Num_QC_Words #2 gives the information for the global BDRF model
C  Num_QC_Words #3 and #4 give generally applicable information
C----------------------------------------------------------------------+
      parameter (M09NUM_QC_WORDS  =      'Num_QC_Words')

C----------------------------------------------------------------------+
C SDS Name:       Inversion_Info
C !Description:    Provides information on model fitting results
C234567----------------------------------------------------------------+
      parameter (M09INVERSION_INFO   =   'Inversion_Info')

C----------------------------------------------------------------------+
C        N_Info #1 is the rmse of the best model
C        N_Info #2 is the rmse of the uniform model
C        N_Info #3 is an inferred land surface structural parameter
C        N_Info #4 is a descriptor of model fitting patterns
C        N_Info #5 is a descriptor TBD
C        N_Info #6 is a descriptor TBD
C        Inversion_Info  SDS metadata:
C----------------------------------------------------------------------+
      parameter (M09N_INFO                        =    'N_Info')

C 	A measure of fit from RMSE and sampling of all
C		 models tested SDS			
      character*(*)M09FIT_ASSESS
      parameter (M09FIT_ASSESS               	= 'Fit_Assessments')

C 	The number of columns in the full ISCCP grid for each
C	row (line) contained within this L3 file. SDS
      character*(*)M09_NCOL
      parameter (M09_NCOL                     = 'ncol')

C 	The start column in the full ISCCP frid for each row (line)
C	contained within this L3 file (starting at zero). SDS 
      character*(*)M09_ICOL_START
      parameter (M09_ICOL_START		= 'icol_start')

C 	The number of columns in each row (line) contained within
C	this L3 file. SDS				
      character*(*)M09_NCOL_TILE
      parameter (M09_NCOL_TILE		= 'ncol_tile')

C 	The start pixel of the first valid column in each row (line)
C	contained within this L3 file (starting at zero). SDS 
      character*(*)M09_IPIX_START
      parameter (M09_IPIX_START		= 'ipix_start')

C 	SDS Metadata					
      character*(*)M09N_SELECT_MODELS
      parameter (M09N_SELECT_MODELS		= 'N_select_models')
      character*(*)M09WORDS
      parameter (M09WORDS			= 'words')
      character*(*)M09LAND_BANDS
C     parameter (M09LAND_BANDS	        	= 'land_bands')
      parameter (M09LAND_BANDS	        	= 'Num_Land_Bands')
C     character*(*)M09NUMBER_PARAMETERS
C     parameter (M09NUMBER_PARAMETERS   	= 'number_parameters')
      parameter (M09NUMBER_PARAMETERS   	= 'Num_BRDF_Parameters')
      character*(*)M09LANDBANDS_BROADBAND_OTHER
      parameter (M09LANDBANDS_BROADBAND_OTHER =
     &            'land_bands_and_broadband_and_<>_0.7mu-m')
      character*(*)M09N_MODELS
      parameter (M09N_MODELS                  = 'N_models')

C-----------------------------------------------------------------------+
C 	MOD.AM1.srefl.L2		
C 	L2 Surface Reflectance and VI Parameters
C 	Version V1 Revision 6 4/24/1996				
C-----------------------------------------------------------------------+
      character*(*) M0913_L2_PROD_ID 
      parameter ( M0913_L2_PROD_ID           = 'MOD.AM1.srefl.L2')


C 	SurfaceReflectance for MODIS Band 1 SDS		
      character  M0913BAND1_SURF_REFL*(*)
      parameter (M0913BAND1_SURF_REFL        = 'sur_refl_b01')

C 	SurfaceReflectance for MODIS Band 2 SDS		
      character  M0913BAND2_SURF_REFL*(*)
      parameter (M0913BAND2_SURF_REFL        = 'sur_refl_b02')

C 	SurfaceReflectance for MODIS Band 3 SDS		
      character  M0913BAND3_SURF_REFL*(*)
      parameter (M0913BAND3_SURF_REFL        = 'sur_refl_b03')

C 	SurfaceReflectance for MODIS Band 4 SDS		
      character  M0913BAND4_SURF_REFL*(*)
      parameter (M0913BAND4_SURF_REFL        = 'sur_refl_b04')

C 	SurfaceReflectance for MODIS Band 5 SDS		
      character  M0913BAND5_SURF_REFL*(*)
      parameter (M0913BAND5_SURF_REFL        = 'sur_refl_b05')

C 	SurfaceReflectance for MODIS Band 6 SDS		
      character  M0913BAND6_SURF_REFL*(*)
      parameter (M0913BAND6_SURF_REFL        = 'sur_refl_b06')

C 	SurfaceReflectance for MODIS Band 7 SDS		
      character  M0913BAND7_SURF_REFL*(*)
      parameter (M0913BAND7_SURF_REFL        = 'sur_refl_b07')

C 	NDVI index at 250m SDS				
      character  M0913_NDVI_INDEX*(*)
      parameter (M0913_NDVI_INDEX            = 'NDVI_index')

C 	MVI index at 250m SDS				
      character  M0913_MVI_INDEX*(*)
      parameter (M0913_MVI_INDEX             = 'MVI_index')

C 	Indicators of the quality of the 250m reflectance and
C	VI data integrity.	
      character  M0913QUALITY_250*(*)
      parameter (M0913QUALITY_250            = 'QC_250m')

C 	Indicators of the quality of the 500m reflectance and
C	VI data integrity.      
      character  M0913QUALITY_500*(*)
      parameter (M0913QUALITY_500            = 'QC_500m')

C 	SDS Metadata					
      character  M0913NUM_DETECTORS*(*)
      parameter (M0913NUM_DETECTORS          = 'num_detectors') 
      character  M0913SAMPLING*(*)
      parameter (M0913SAMPLING               = 'sampling')

C      Global Metadata
      character*(*) M09ROW_NADIR_DATA_RESOLUTION, 
     &              M09COLUMN_NADIR_DATA_RESOLUTION  

      parameter ( M09ROW_NADIR_DATA_RESOLUTION =  
     &            'Row Nadir Data Resolution')
      parameter ( M09COLUMN_NADIR_DATA_RESOLUTION =  
     &            'Column Nadir Data Resolution')

C---------------------------------------------------------------+
C                    MOD10_L2                                   |  
C                    L2 Daily Snow Cover                     	|
C                    Vers V2 Rev 2 03-Oct-1997                  | 
C---------------------------------------------------------------+
      character*(*) M10L2_PROD_ID

      parameter (M10L2_PROD_ID          = 'MOD10_L2')
C     parameter (M10L2_PROD_ID          = 'MOD.AM1.V1.snow.L2')

      character*(*) M10NO_DECISION, M10PROCESSED_PIXELS, M10SNOW_PIXELS,
     &              M10PERCENT_SNOW, M10PERCENT_NOT_SNOW, M10NDSI_ABOVE,
     &              M10NDSI_BELOW, M10ZERO_DIVIDE, M10DAILY_SNOW,
     &              M10OUT_OF_RANGE_INPUT,
     &              M10TOTAL_NOT_SNOW_PIXELS, M10AREA_SNOW,
     &              M10AREA_NOT_SNOW, M10SOLAR_ZENITH85, 
     &              M10CLOUD_OBSCURED, M10QA_OVERALL, M10Snow_Cover,
     &              M10SNOW_COVER

C SDS Metadata
C SDS Name:    Snow Cover
C !Description: Snow cover extent as identified by the algorithm for every
C       pixel in the granule.                                               

      parameter (M10Snow_Cover   =          'Snow Cover')
      parameter (M10SNOW_COVER   =          'Snow Cover')

C--------1---------2---------3---------4---------5---------6---------7--

C Global Metadata                                         
      parameter (M10PROCESSED_PIXELS      
     &            = 'Number_of_pixels_processed')
      parameter (M10SNOW_PIXELS             = 'Total_snow_pixels')
      parameter (M10TOTAL_NOT_SNOW_PIXELS   = 'Total_not_snow_pixels')
      parameter (M10AREA_SNOW               = 'Area_snow')
      parameter (M10AREA_NOT_SNOW           = 'Area_not_snow')
      parameter (M10PERCENT_SNOW            = 'Percentage_snow')
      parameter (M10PERCENT_NOT_SNOW        = 'Percentage_not_snow')
      parameter (M10NDSI_ABOVE              = 'Above_range_NDSI')
      parameter (M10NDSI_BELOW              = 'Below_range_NDSI')
      parameter (M10ZERO_DIVIDE             = 'Division_by_zero')
      parameter (M10OUT_OF_RANGE_INPUT      = 'Out_of_range_input')
      parameter (M10NO_DECISION             = 'No_decision')
      parameter (M10SOLAR_ZENITH85          = 'Solar_zenith>=85')
      parameter (M10CLOUD_OBSCURED          = 'Cloud_obscured')
      parameter (M10QA_OVERALL              = 'QA_overall')

 
C L2/L2G Identification of daily snow cover on the land surface SDS 
      parameter (M10DAILY_SNOW = 'daily_snow_cover')
 
C---------------------------------------------------------------+
C                     MOD10_L2G                                 | 
C                     L2G Snow Cover Product              	| 
C                     Vers V2 rev 3 05-Dec-1997                 | 
C---------------------------------------------------------------+
      character*(*) M10L2G_PROD_ID
C     parameter (M10L2G_PROD_ID          = 'MOD10_L2G')
C     parameter (M10L2G_PROD_ID          = 'MOD.AM1.V1.snow.L2G')
      parameter (M10L2G_PROD_ID          = 'MOD10L2G')
 
      character*(*)M10L2G_Snow_Cover, M10L2G_NUM_OBSERV, 
     &       M10L2G_COL_GLB_GRID_ROW, M10L2G_NUM_OBSERVS_ROW,   
     &       M10L2G_SNOW_COVER_1,M10L2G_SNOW_COVER_F,
     &       M10L2G_SNOW_COVER_C

C  SDS Metadata
C
C  SDS Name : num_observations
C !Description : The number of observations per pixel contained in this
C                L2G file.
C  SDS Name : Snow_Cover_1
C !Description : Snow_covered_land - first layer.
C
C  SDS Name : Snow_Cover_f
C !Description : Snow_covered_land - additional layers, full
C
C  SDS Name : Snow_Cover_c
C !Description : Snow_covered_land - additional layers, compact
C
C  SDS Names : Snow_Cover_PixelQA_1
C              Snow_Cover_PixelQA_f
C              Snow_Cover_PixelQA_c
C !Description : SDS of spatial QA data. QA data is stored as bit flags.
C                They contain information on the run of the algorithm
C                and may be used investigate the quality of the data product.
C
C  SDS Name : nadd_obs_row
C !Description : The number of additional observations in each row in the
C                compact storage form of the L2G grid. 
C


      parameter (M10L2G_NUM_OBS       =       'num_observations')
      parameter (M10L2G_Snow_Cover    =       'Snow Cover')
      parameter (M10L2G_SNOW_COVER    =       'Snow Cover')
      parameter (M10L2G_SNOW_COVER_1  =       'Snow_Cover_1')
      parameter (M10L2G_SNOW_COVER_F  =       'Snow_Cover_f')
      parameter (M10L2G_SNOW_COVER_C  =       'Snow_Cover_c')
      parameter (M10L2G_SNOW_COVER_PIXELQA_1  =       'Snow_Cover_PixelQA_1')
      parameter (M10L2G_SNOW_COVER_PIXELQA_F  =       'Snow_Cover_PixelQA_f')
      parameter (M10L2G_SNOW_COVER_PIXELQA_C  =       'Snow_Cover_PixelQA_c')
      parameter (M10L2G_NADD_OBS_ROW          =       'nadd_obs_row')


      parameter (M10L2G_NUM_OBSERV    =       MLNUMBER_OF_OBS)
      parameter (M10L2G_COL_GLB_GRID_ROW   =  MLCOLUMNS_GLOBAL_GRID_ROW)
      parameter (M10L2G_NUM_OBSERVS_ROW    =
     &            'Number of Observations per Row') 

C---------------------------------------------------------------+
C                     MOD10A1                                   | 
C                     L3 Daily Snow Cover                    	| 
C                     Vers V2 Rev 2 23-Dec-1997                 | 
C---------------------------------------------------------------+
      character*(*) M10L3_PROD_ID
C     parameter (M10L3_PROD_ID          = 'MOD10_L3_DY_G')
C     parameter (M10L3_PROD_ID          = 'MOD.AM1.V1.snow_dy.L3')
      parameter (M10L3_PROD_ID          = 'MOD_PR10A1')
 
C SDS Metadata
C SDS Name :  Day_Tile_Snow_Cover
C !Description : This SDS is a result of finding the 'best' or the highest
C                value of observation from the L2G snow cover data.
C
C SDS Name :  Snow_Spatial_QA
C !Description : QA information for each cell derived from the QA information
C                of pixels analyzed to determine the modal observation of the
C                cell from the L2G snow cover input data product.
C

      character*(*) M10GRIDDED_SNOW
      character*(*) M10TILE_AREA, M10SNOW_COVER_AREA, M10SNOW_COVER_PCT,
     &              M10NO_OBS_AREA, M10NO_OBS_AREA_PCT, M10OBS_TOTAL,
     &              M10OBS_EXCLUDED, M10OBS_USED, 
     &              M10L3_DAY_TILE_SNOW_COVER, M10L3_SNOW_SPATIAL_QA

C L3 Identification of daily snow cover on the land surface SDS 

      parameter ( M10L3_DAY_TILE_SNOW_COVER = 'Day_Tile_Snow_Cover')
      parameter ( M10L3_SNOW_SPATIAL_QA = 'Snow_Spatial_QA')

      parameter ( M10GRIDDED_SNOW    = 'Daily_gridded_snow_cover')
      parameter ( M10TILE_AREA       = 'tile_area')
      parameter ( M10SNOW_COVER_AREA = 'snow_cover_area')
      parameter ( M10SNOW_COVER_PCT  = 'snow_cover_pct')
      parameter ( M10NO_OBS_AREA     = 'no_obs_area')
      parameter ( M10NO_OBS_AREA_PCT = 'no_obs_area_pct')
      parameter ( M10OBS_TOTAL       = 'obs_total')
      parameter ( M10OBS_EXCLUDED    = 'obs_excluded')
      parameter ( M10OBS_USED        = 'obs_used')
 
C---------------------------------------------------------------+
C                 MOD.AM1.LST.L2                                |   
C                 L2 Land Surface Temperature                   | 
C                 Version V1 Rev 5 16-Apr-1996                  |    
C---------------------------------------------------------------+
      character*(*) M11L2_PROD_ID
C     parameter (M11L2_PROD_ID          = 'MOD11_L2')
      parameter (M11L2_PROD_ID          = 'MOD.AM1.V1.lst.L2')

      character*(*) M11BAND29OR20_EMIS, M11SURF_TEMP, M11QUALITY,
     &              M11ERRORS, M11BAND31_EMIS, M11BAND32_EMIS

C L2/L2G Identification of Land Surface Temperature SDS 
      parameter (M11SURF_TEMP  = 'LST')
 
C L2/L2G LST Quality Indicator  SDS.                              
      parameter (M11QUALITY           = 'QC')
 
C L2/L2G Error in land surface temperature measurements SDS      
      parameter (M11ERRORS            = 'Error_LST')
 
C L2/L2G/L3 Band 31 emissivity SDS                                  
      parameter (M11BAND31_EMIS       = 'Emis_31')
 
C L2/L2G/L3 Band 32 emissivity SDS                                  
      parameter (M11BAND32_EMIS       = 'Emis_32')
 
C L2/L2G Band 29 or band 20 emissivity SDS                       
C     parameter (M11BAND29OR20_EMIS   = 'Emis_29or20')
      parameter (M11BAND29OR20_EMIS   = 'Emis_29')
 
      character*(*) M11VIEW_TIME,  M11VIEW_ANGLE 

C SDS Name:       View_angle
C !Description:    zenith angle of MODIS viewing at the pixel               
      parameter (M11VIEW_ANGLE        =    'View_angle')

C SDS Name:       View_time
C !Description:    local-sun time of MODIS viewing at the pixel             
      parameter (M11VIEW_TIME          =      'View_time')

C---------------------------------------------------------------------+
C                       MOD 11-L2G                                    |
C                       Land Surface Temperature                      |
C                       Beta Version                                  |
C---------------------------------------------------------------------+

C       Product type identifier                                        
      parameter (M11L2G_PROD_ID        =      'MOD11_L2G')
 
C---------------------------------------------------------------+
C                    MOD.AM1.V1.lst_1dy_cmg.L3                  |   
C                    Daily Land Surface Temperature             |
C                    Vers V1 Rev 3 17-Jun-1996                  | 
C---------------------------------------------------------------+

      character*(*) M11L3_1DY_PROD_ID
C     parameter (M11L3_PROD_ID          = 'MOD11_L3_8DY')
      parameter (M11L3_1DY_PROD_ID      = 'MOD.AM1.V1.lst_1dy_cmg.L3')

 
      character*(*) M11L3SURF_TEMP, M11NARROW_LST, M11L3QUALITY,
     &              M11STD_DEV, M11L3BAND29OR20_EMIS,
     &              M11MEAN_LST_DAY, M11MEAN_LST_NIGHT,
     &              M11STDV_LST_DAY, M11STDV_LST_NIGHT,
     &              M11HIST_LST_DAY, M11HIST_LST_NIGHT,
     &              M11BAND29_ANG_COEFS, M11BAND31_ANG_COEFS,
     &              M11BAND32_ANG_COEFS, M11MEAN_DAY_TIME_OF_LST,
     &              M11MEAN_NIGHT_TIME_OF_LST, 
     &              M11LST_DIFFERENCE_BINS_IN_HIST_LST 

C L3 Identification of Land Surface Temperature SDS
      parameter (M11L3SURF_TEMP           = 'LST')

C Land surface temperature in view within 45deg SDS
      parameter (M11NARROW_LST            = 'LST_view<45d')

C Daily daytime mean Land-surface temperature at half-degree equal
C  angle grids                                             
      parameter (M11MEAN_LST_DAY          = 'Mean_LST_Day')

C Daily nighttime mean Land-surface temperature at half-degree equal
C   angle grids                                             
      parameter (M11MEAN_LST_NIGHT        =  'Mean_LST_NIGHT')

C Standard deviation in daily daytime CMG land surface temperature    
      parameter (M11STDV_LST_DAY          =   'Stdv_LST_Day')

C Standard deviation in daily nighttime CMG land surface temperature   
      parameter (M11STDV_LST_NIGHT        =  'Stdv_LST_Night')

C Histogram (in percentage) of daily daytime CMG LST Relative to Mean_LST_Day 
      parameter (M11HIST_LST_DAY           = 'Hist_LST_Day')

C Histogram (in percentage) of daily nighttime CMG LST Relative to Mean_LST_Night
      parameter (M11HIST_LST_NIGHT         =  'Hist_LST_Night')

C L3 LST Quality Indicator  SDS.
      parameter (M11L3QUALITY             = 'QC')

C Land-Surface Temperature Standard Deviation SDS
      parameter (M11STD_DEV               = 'Stdv_LST')

C L3 Band 29 or band 20 emissivity SDS
      parameter (M11L3BAND29OR20_EMIS     = 'Emis_29')

C L3 Band 31 emissivity SDS                                          
      parameter  (M11L3BAND_EMIS_31        =  'Emis_31')

C L3 Band 32 emissivity SDS                                          
      parameter (M11L3BAND_EMIS_32        =  'Emis_32')

C  Angular coefficients for Band 31 emissivity SDS
      parameter (M11BAND31_ANG_COEFS      = 'Ang_Coef_Emis_31')

C  Angular coefficients for Band 32 emissivity SDS
      parameter (M11BAND32_ANG_COEFS      = 'Ang_Coef_Emis_32')

C Angular coefficients for Band 29 emissivity SDS 
      parameter (M11BAND29_ANG_COEFS       = 'Ang_Coef_Emis_29')

C Percentage of area where LST was retrieved                        
      parameter (M11PERCENTAGE_AREA_LST_RETRIEVED =
     &           "Percentage_Area_LST_Retrieved")

C Mean observation time of the daytime LST                            
      parameter (M11MEAN_DAY_TIME_OF_LST  =  'Mean_Day_Time_of_LST')

C Mean observation time of the night LST                               
      parameter (M11MEAN_NIGHT_TIME_OF_LST =  'Mean_Night_Time_of_LST')

C Center of LST difference bins used in LST Histogram                  
      parameter (M11LST_DIFFERENCE_BINS_IN_HIST_LST  =  
     &         'LST_Difference_Bins_In_Hist_LST')

C---------------------------------------------------------------+
C                    MOD.AM1.V1.lst.8dy_cmg.L3                  |   
C                    Land Surface Temperature                   |
C                    Vers V1 Rev 3 23-Apr-1996                  | 
C---------------------------------------------------------------+
      character*(*) M11L3_8DY_PROD_ID
      parameter (M11L3_8DY_PROD_ID      = 'MOD.AM1.V1.lst_8dy_cmg.L3')

C---------------------------------------------------------------+
C                    MOD.AM1.V1.lst_1m_cmg.L3                   |
C                    Land Surface Temperature                   |
C                    Vers V1 Rev 3 23-Apr-1996                  | 
C---------------------------------------------------------------+
      character*(*) M11L3_1M_PROD_ID
      parameter (M11L3_1M_PROD_ID       = 'MOD.AM1.V1.lst_1m_cmg.L3')


C---------------------------------------------------------------+
C                MOD.AM1.V1.lc_1km.L3.3m                        | 
C                Land Cover Type/Land Cover Change              |
C                Vers V1 rev 4 16-Apr-1996                      |
C---------------------------------------------------------------+
      character*(*) M12L3_PROD_ID
C     parameter (M12L3_PROD_ID      = 'MOD12_L3_3MN_D/MOD12_L3_3MN_F')
      parameter (M12L3_PROD_ID      = 'MOD.AM1.V1.lc_1km.L3.3m')

      character*(*) M12ANGULAR_SIZE, M12LAND_COVER 
 
C Global Metadata                                       
      parameter (M12ANGULAR_SIZE = 'ang_size (in arcsec)')
 
C Identification of land cover type SDS                  
C     parameter (M12LAND_COVER   = 'Land_cover_type')
      parameter (M12LAND_COVER   = 'Land Cover Type')

      character*(*)  M12PERCENTPIXELSPROCCOMPLETELY,
     &               M12PERCENTCHANGEDPREVPRODGENER,
     &               M12PROPORTIONEACHCOVERTYPEPRES,
     &               M12QUALITY, M12PRODS_GENERATED, M12SNOW_MONTHS,
     &               M12BRDFS_USED, M12BRDF_STOCK, M12LST_VALS_USED,
     &               M12VI_STOCK, M12QUALITY1, M12QUALITY2, 
     &               M12LAND_COVER_CHANGE, M12CHANGE_QUALITY
 
      parameter (M12PERCENTPIXELSPROCCOMPLETELY =
     &          'Percentage of pixels processed completely')
      parameter (M12PERCENTCHANGEDPREVPRODGENER = 
     &           'Percent changed from previous product generation')
      parameter (M12PROPORTIONEACHCOVERTYPEPRES = 
     &           'Proportion of each cover type present')

C Identification of Overall quality of the land cover SDS
C     parameter (M12QUALITY      = 'LC_overall_quality_QC')
      parameter (M12QUALITY      = 'Type Overall QC')
 
C Identification of Number of products generated since   
C last classification update SDS                         
C     parameter (M12PRODS_GENERATED = 'LC_num_product_gen')
      parameter (M12PRODS_GENERATED = 'Num Product Gen')
 
C Identification of Number of snow months over pervious 12 months SDS
C     parameter (M12SNOW_MONTHS    = 'LC_snow_months')
      parameter (M12SNOW_MONTHS    = 'Snow Months')
 
C Identification of Number of BRDFs used for classification 
C that have been derived within the pass 12 months SDS      
C     parameter (M12BRDFS_USED     = 'LC_num_BRDF')
      parameter (M12BRDFS_USED     = 'Num BRDF')
 
C Identification of Confidence in BRDF/reflectance correction SDS
C     parameter (M12BRDF_STOCK     = 'LC_BRDF_confidence')
      parameter (M12BRDF_STOCK     = 'BRDF Internal QC')
 
C Identification of Number of LST values used for classification SDS 
C     parameter (M12LST_VALS_USED  = 'LC_num_LST')
      parameter (M12LST_VALS_USED  = 'Num LST')
 
C Identification of Confidence in VI over 12 months             
C     parameter (M12VI_STOCK       = 'LC_VI_confidence')
      parameter (M12VI_STOCK       = 'VI Internal QC')
 
C Identification of TBD quality control for land cover type SDS 
      parameter (M12QUALITY1       = 'Land_cover_TBD_1')
 
C Identification of TBD quality control for land cover type SDS 
      parameter (M12QUALITY2       = 'Land_cover_TBD_2')
 
C Identification of Land cover change SDS                    
C     parameter (M12LAND_COVER_CHANGE = 'Land_cover_change')
      parameter (M12LAND_COVER_CHANGE = 'Land Cover Change')
 
C Identification of Quality control for land cover change SDS
C     parameter (M12CHANGE_QUALITY    = 'Land_cover_change_QC')
      parameter (M12CHANGE_QUALITY    = 'Land Cover Change QC')
                                                                        
C----------------------------------------------------------------------+
C                    MOD.AM1.V1.landcov_32dy.L3                        |
C                    Land Cover Monthly Database                       |
C                    Vers V1 rev 8 29-May-1996                         |
C----------------------------------------------------------------------+
         
      character*(*) M12L31M_PROD_ID   
      parameter (M12L31M_PROD_ID     =   'MOD.AM1.V1.landcov_32dy.L3')

      character*(*) M12ECOREGION_LABEL,
     &              M12LAND_CVR_CLS_PRESENT_TILE,
     &              M12LW, M12TEXT, M12TEXT_QC, M12BAR, M12BA_BANDS,
     &              M12BAR_QC, M12BRDF, M12MODELS_KEPT, M12BRDF_QC,       
     &              M12VI, M12VI_QC, M12SNOW, M12SNOW_QC, M12LST,      
     &              M12LST_QC, M12LAND_COVER_TYPE_HDEG,
     &              M12TYPE_OVERALL_QC_HDEG, M12NUM_PRODUCT_GEN_HDEG,
     &              M12SNOW_MONTHS_HDEG, M12NUM_BRDF_HDEG,
     &              M12BRDF_INTERNAL_QC_HDEG,
     &              M12LAND_COVER_CHANGE_QC_HDEG 
  
      parameter ( M12ECOREGION_LABEL          = 'Ecoregion label(s)')
      parameter ( M12LAND_CVR_CLS_PRESENT_TILE = 
     &            'Land cover classes present in tile')

C !Description:    Land/water flag                                      
C       SDS:                                                                  
      parameter ( M12LW                       = 'LW')

C !Description: Standard deviation divided by mean of 250m Band 2 pixels  
C       in a L3 1-km grid cell, composited monthly                             
C       SDS:                                                                    
      parameter ( M12TEXT                     = 'Text')

C !Description: Internal quality control flag for texture                 
C       SDS:                                                                    
      parameter ( M12TEXT_QC                  = 'Text QC')

C !Description: BRDF-adjusted reflectances, composited monthly            
C       SDS:                                                                    
      parameter ( M12BAR                      = 'BAR')

C  metadata product:
      parameter ( M12BA_BANDS                 = 'bar_bands')

C !Description: Internal quality control flag for BAR                     
C       SDS:                                                                    
      parameter ( M12BAR_QC                   = 'BAR QC')

C !Description: Best and worst monthly BRDF models (lowest and highest    
C                    RMSE's)                                                   
C       SDS:                                                                    
      parameter ( M12BRDF                     = 'BRDF')
      parameter ( M12MODELS_KEPT              = 'models_kept')

C !Description: Internal quality control flag for BRDF                    
C       SDS:                                                                    
      parameter ( M12BRDF_QC                   = 'BRDF QC')

C  SDS Name:       VI                                                       
C !Description:    Monthly composite of VI                                  
C     SDS:                                                                      
      parameter ( M12VI                        = 'VI')

C  SDS Name:       VI QC
C !Description: Internal quality control flag for VI
C     SDS:
      parameter ( M12VI_QC                       = 'VI QC')

C  SDS Name:       Snow                                                     
C !Description:    Monthly composite of snow cover                          
C     SDS:                                                                      
      parameter ( M12SNOW                        = 'Snow')

C  SDS Name:       Snow QC                                                  
C !Description:    Internal quality control flag for snow                   
C     SDS:                                                                      
      parameter ( M12SNOW_QC                     = 'Snow QC')

C  SDS Name:       LST                                                      
C !Description:    Monthly composite of land surface temperature            
C     SDS:                                                                      
      parameter ( M12LST                         = 'LST')


C  SDS Name:       LST QC                                                   
C !Description:    Internal quality control flag for LST                    
C     SDS:                                                                      
      parameter ( M12LST_QC                      = 'LST QC')

C----------------------------------------------------------------------------+
C                   MOD.AM1.lc_halfdegree.L3.3m                              |
C                   Land Cover Type/Land Cover Change at half-degree         |
C                   spatial resolution                                       |
C                   Vers V1 rev 6 01-May-1996                                |
C----------------------------------------------------------------------------+
      character*(*) M12L3HD_PROD_ID
      parameter (M12L3HD_PROD_ID        = 'MOD.AM1.lc_halfdegree.L3.3m')

C  SDS Name:       Land Cover Type H-deg                                      
C !Description:    Land cover type, half-degree resoluton, keyed to IGBP-DIS  
C   categories                                                                
C  SDS:                                                                    
      parameter ( M12LAND_COVER_TYPE_HDEG   = 'Land Cover Type H-deg')

      character*(*) M12COVERTYPESPRESENTDGRADCELL, 
     &              M12PROPCOVERTYEPRESENTDGRADCELL

      parameter (M12COVERTYPESPRESENTDGRADCELL   =  
     &           'Cover types present in each degraded cell')

      parameter (M12PROPCOVERTYEPRESENTDGRADCELL = 
     &    'Proportion of each cover type present in each degraded cell')

C SDS Name:       Type Overall QC H-deg                                      
C !Description:    Overall quality of the land cover classification,          
C                 half-degree resolution                                     
C SDS:                                                                    
      parameter ( M12TYPE_OVERALL_QC_HDEG   = 'Type Overall QC H-deg')

C SDS Name:       Num Product Gen H-deg                                      
C !Description:    Number of products generated since last classification     
C                 update,                                                    
C SDS:                                                                    
      parameter ( M12NUM_PRODUCT_GEN_HDEG    = 'Num Product Gen H-deg')

C  SDS Name:       Snow Months H-deg                                      
C !Description:    Number of snow months over previous 12 months,         
C                  half-degree                                            
C  SDS:                                                                    
      parameter ( M12SNOW_MONTHS_HDEG         = 'Snow Months H-deg')


C  SDS Name:       Num BRDF H-deg                                         
C !Description:    Number of BRDFs used for classification that           
C    have been derived within the past 12 months, half-degree resolution    
C  SDS:                                                                    
      parameter ( M12NUM_BRDF_HDEG            = 'Num BRDF H-deg')

C SDS Name:       BRDF Internal QC H-deg                                 
C !Description:    Internal quality flag for BRDF/reflectance correction, 
C                 half-degree resolution                                 
C SDS:                                                                    
      parameter ( M12BRDF_INTERNAL_QC_HDEG  = 'BRDF Internal QC H-deg')

C SDS Name:       Num LST H-deg                                          
C !Description:    Number of LST values used for classification           
C                 over last 12 months, half-degree resolution            
C SDS:                                                                    
      parameter ( M12NUM_LST_HDEG          = 'Num LST H-deg')

      character*(*) M12VI_INTERNAL_QC_HDEG, M12LAND_COVER_CHANGE_HDEG 

C  SDS Name:       VI Internal QC H-deg                                
C !Description:    Internal quality flag for VI over past 12 months,   
C               half-degree resolution                                  
      parameter (M12VI_INTERNAL_QC_HDEG    =  'VI Internal QC H-deg')

C   SDS Name:       Land Cover Change H-deg                           *
C !Description:    Land cover change, half-degree resolution         */
      parameter ( M12LAND_COVER_CHANGE_HDEG   = 
     &            'Land Cover Change H-deg')

C SDS Name:       Land Cover Change QC H-deg                             
C !Description:    Quality control for land cover change,                 
C                      half-degree resolution                                 
C      SDS:                                                                    
      parameter ( M12LAND_COVER_CHANGE_QC_HDEG
     &             = 'Land Cover Change QC H-deg')

C---------------------------------------------------------------+
C              MOD 14_L2                                        |
C              L2 Thermal Anomaly/Fire Products                 |
C              Vers. V1          02-May-1996                    |
C---------------------------------------------------------------+
      character*(*) M14L2_PROD_ID
      parameter (M14L2_PROD_ID          = 'MOD14_L2')
 
      character*(*) M14LAND_FIRE, M14ENERGY, M14FIRE_CLASS, M14QUALITY

C L2/L2G Identification of fire on the land surface SDS 
      parameter (M14LAND_FIRE = 'fire_mask')
 
C L2/L2G/L3 Total emmitted energy detected SDS.       
C     parameter (M14ENERGY    = 'Energy')
      parameter (M14ENERGY    = 'power')
 
C L2/L2G/L3 Class of fire detected SDS                
      parameter (M14FIRE_CLASS = 'smold')
 
C Fire quality control SDS                            
      parameter (M14QUALITY   = 'Fire_qc')
 
C---------------------------------------------------------------+
C                         MOD 14-L2G                            |
C               L2G Thermal Anomaly/Fire Products               |
C                    Beta            27-9-95                    |
C---------------------------------------------------------------+
      character*(*) M14L2G_PROD_ID 
C     parameter (M14L2G_PROD_ID         = 'MOD14_L2G')
      parameter (M14L2G_PROD_ID         = 'MOD.AM1.V1.fire.L2G')

C L2G/L3 Fire quality control SDS                     
      character*(*) M14L2GQUALITY
      parameter (M14L2GQUALITY          = 'fire_qc')

C---------------------------------------------------------------+
C               MOD 14-L3                                       |
C               L3 Thermal Anomaly/Fire Products                |
C               Beta            11/28/95                        |
C---------------------------------------------------------------+

C  Product type identifier                                 
      character*(*)M14L3_PROD_ID 
      parameter (M14L3_PROD_ID    =        'MOD14_L3')

C---------------------------------------------------------------------+
C               MOD15 L3                                              |
C               Tile 1-KM Gridded PRODUCTS: FPAR, LAI                 | 
C               V1              0909/1996                             |
C---------------------------------------------------------------------+

C      Product type identifier                                        
      character*(*) M15L3_PROD_ID  
      parameter (M15L3_PROD_ID =  'MOD.AM1.V1.fpar_lai_8dy.L3')

C      Global Metadata for Product                                    
      character*(*)M15HIGHEST_PIX_COMPLETED, M15TOTAL_PIXELS_PER_TILE,
     &  M15NTH_RESTART, M15PCT_TILE_COMPLETED, M15PCT_CALC_BY_LUT, 
     &  M15PCT_CALC_BY_EMPIRICAL 

      parameter (M15HIGHEST_PIX_COMPLETED = 'highest_pix_completed')
      parameter (M15TOTAL_PIXELS_PER_TILE = 'total_pixels_per_tile')
      parameter (M15NTH_RESTART            =  'nth_restart')
      parameter (M15PCT_TILE_COMPLETED     =  'pct_tile_completed')
      parameter (M15PCT_CALC_BY_LUT        =  'pct_calc_by_lut')
      parameter (M15PCT_CALC_BY_EMPIRICAL = 'pct_calc_by_empirical')

C Tile 1-KM Gridded PRODUCTS: FPAR, LAI. SDS                                  

C SDS Name:       FPAR_1K
C !Description:    Fraction of absorbed photosynthetically active radiation
C                 at 1 KM and 8 day resolution, Parameter 5367.                    
      character*(*)M15FPAR_1K 
      parameter (M15FPAR_1K           =         'FPAR_1K')

C SDS Name:       FPAR_1K_QC
C !Description:    Root mean squared error (RMSE) of fraction of absorbed
C                 photosynthetically active radiation.                             
      character*(*)M15FPAR_1K_QC 
      parameter (M15FPAR_1K_QC       =              'FPAR_1K_QC')


C SDS Name:       LAI_1K
C !Description:    Leaf area index at 1 km resolution, 8 day composite              
      character*(*)M15LAI_1K   
      parameter (M15LAI_1K           =          'LAI_1K')

C SDS Name:       LAI_1K_QC
C !Description:    Quality control for leaf area index.                            
      character*(*)M15LAI_1K_QC  
      parameter (M15LAI_1K_QC       =            'LAI_1K_QC')

C SDS Name:       Columns per Global Grid Row
C !Description:    The number of valid data columns in the MODLAND
C                 integerized sinusoidal grid for each global grid row
C                 contained this tile                                                 
      character*(*)M15COL_GLOB_GRD_RW 
      parameter (M15COL_GLOB_GRD_RW =   'Columns per Global Grid Row')

C Dimension Names:                                                                 
      character*(*)M15DATA_ROWS, M15DATA_COLUMNS, M15GLOB_GRD_RWS 

      parameter (M15DATA_ROWS           =          'Data Rows')
      parameter (M15DATA_COLUMNS        =          'Data Columns')
      parameter (M15GLOB_GRD_RWS        =          'Global Grid Rows')

C-----------------------------------------------------------------------------+
C               MOD15 L3                                                      |
C               Tile 25-KM Climate Modeling Grid PRODUCTS: FPAR, LAI          |
C               V1              0909/1996                                     |
C-----------------------------------------------------------------------------+

C      Product type identifier                                                     
      character*(*)M15L3_25KM_PROD_ID 
      parameter (M15L3_25KM_PROD_ID = 'MOD.AM1.V1.fpar_lai_8dy_cmg.L3')

C      Global Metadata for Product                                                 
C     character*(*)M15HIGHEST_PIX_COMPLETED, M15TOTAL_PIXELS_PER_TILE, 
C    &             M15PCT_CALC_BY_EMPIRICAL

C     parameter (M15HIGHEST_PIX_COMPLETED =    'highest_pix_completed')
C     parameter (M15TOTAL_PIXELS_PER_TILE =    'total_pixels_per_tile')
C     parameter (M15PCT_CALC_BY_EMPIRICAL =    'pct_calc_by_empirical')

C Tile 1-KM Gridded PRODUCTS: FPAR, LAI. SDS                                       

C SDS Name:     FPAR_25K
C !Description:  Fraction of absorbed photosynthetically active radiation
C               at 1 KM and 8 day resolution, Parameter 5367.                    
      character*(*)M15FPAR_25K 
      parameter (M15FPAR_25K                =       'FPAR_25K')

C SDS Name:     FPAR_25K_QC
C !Description:  Root mean squared error (RMSE) of fraction of absorbed
C               photosynthetically active radiation.                             
      character*(*)M15FPAR_25K_QC  
      parameter (M15FPAR_25K_QC   =     'FPAR_25K_QC')

C SDS Name:       LAI_25K
C !Description: Leaf area index at 1 km resolution, 8 day composite              
      character*(*)M15LAI_25K
      parameter (M15LAI_25K                =        'LAI_25K')

C SDS Name:       LAI_25K_QC
C !Description:    Quality control for leaf area index.                            
      character*(*)M15LAI_25K_QC 
      parameter (M15LAI_25K_QC             =        'LAI_25K_QC')

C SDS Name:       Columns per Global Grid Row
C !Description:    The number of valid data columns in the MODLAND
C                 integerized sinusoidal grid for each global grid row
C     character*(*)M15COL_GLOB_GRD_RW  
C     parameter (M15COL_GLOB_GRD_RW  =   'Columns per Global Grid Row')

C Dimension Names:                                                             
C     character*(*) M15DATA_ROWS, M15DATA_COLUMNS, M15GLOB_GRD_RWS 

C     parameter (M15DATA_ROWS        =             'Data Rows')
C     parameter (M15DATA_COLUMNS     =             'Data Columns')
C     parameter (M15GLOB_GRD_RWS     =             'Global Grid Rows')
C23456789112345678921234567893123456789412345678951234567896123456789712

C----------------------------------------------------------------------+
C               MOD 17 L4                                              |
C               Annual 1-KM Gridded PRODUCTS: NPP                      |
C               V1              0909/1996                              |
C----------------------------------------------------------------------+

C      Product type identifier                                                 
      character*(*)M17L4_PROD_ID
      parameter (M17L4_PROD_ID      =    'MOD.AM1.V1.npp_1yr.L4')

C      Global Metadata for Product                                             
      character M17HIGHEST_PIX_COMPLETED, M17TOTAL_PIXELS_PER_TILE,
     & M17NTH_RESTART

      parameter (M17HIGHEST_PIX_COMPLETED = 'highest_pix_completed')
      parameter (M17TOTAL_PIXELS_PER_TILE = 'total_pixels_per_tile')
      parameter (M17NTH_RESTART           = 'nth_restart')

C Tile 1-KM Gridded PRODUCTS: NPP                                              

C SDS Name:       NPP_1K
C !Description:    Net primary productivity at 1 KM, annual composite.              
      character*(*)M17NPP_1K 
      parameter (M17NPP_1K               =         'NPP_1K')

C SDS Name:       NPP_1K_QC
C !Description:    Quality control for annual composited NPP 1k                     
      character*(*)M17NPP_1K_QC 
      parameter (M17NPP_1K_QC      =        'NPP_1K_QC')


C SDS Name:       Columns per Global Grid Row
C !Description:  The number of valid data columns in the MODLAND
C               integerized sinusoidal grid for each global grid row
C               contained this tile                                              
      character*(*)M17COL_GLOB_GRD_RW  
      parameter (M17COL_GLOB_GRD_RW  = 'Columns per Global Grid Row')

C Dimension Names:                                                             
      character*(*)M17DATA_ROWS, M17DATA_COLUMNS, M17GlOB_GRD_RWS 

      parameter (M17DATA_ROWS            =         'Data Rows')
      parameter (M17DATA_COLUMNS         =         'Data Columns')
      parameter (M17GlOB_GRD_RWS         =         'Global Grid Rows')

C----------------------------------------------------------------------+
C               MOD 17 L4                                              |
C               Annual 25 KM Climate Modeling Grid PRODUCT: NPP        |
C               V1              0909/1996                              |
C----------------------------------------------------------------------+
C      Product type identifier                                                 
      character*(*)M17_25K_PROD_ID 
      parameter (M17_25K_PROD_ID   =   'MOD.AM1.V1.npp_1yr_cmg.L4')

C      Global Metadata for Product                                             
      character*(*)M17_25K_HIGHEST_PIX_CMPLTD,
     &             M17_25K_TOTAL_PIXELS_TILE, M17_25K_NTH_RESTART

      parameter (M17_25K_HIGHEST_PIX_CMPLTD = 'highest_pix_completed')
      parameter (M17_25K_TOTAL_PIXELS_TILE = 'total_pixels_per_tile')
      parameter (M17_25K_NTH_RESTART       =  'nth_restart')

C Tile 25-KM Gridded PRODUCTS: NPP                                              

C SDS Name:       NPP_25K
C !Description: Quality control for annual composited NPP 25 KM for the
C                climate modeling grid    
      character*(*)M17NPP_25K_QC  
      parameter (M17NPP_25K_QC  =       'NPP_25K_QC')

C SDS Name:       Columns per Global Grid Row
C !Description:    The number of valid data columns in the MODLAND
C               integerized sinusoidal grid for each global grid row
C               contained this tile                                              
      character*(*)M17_25K_COL_GLB_GRD 
      parameter (M17_25K_COL_GLB_GRD = 'Columns per Global Grid Row')

C Dimension Names:                                                             
      character*(*)M17_25K_DATA_ROWS, M17_25K_DATA_COLUMNS,
     & M17_25K_GlOB_GRD_RWS  

      parameter (M17_25K_DATA_ROWS    =     'Data Rows')
      parameter (M17_25K_DATA_COLUMNS =     'Data Columns')
      parameter (M17_25K_GlOB_GRD_RWS =     'Global Grid Rows')

C----------------------------------------------------------------------+
C               MOD 17 L4                                              |
C               Tile 1-KM Gridded PRODUCTS: PSN                        |
C               V1              0909/1996                              |
C----------------------------------------------------------------------+

C      Product type identifier                                                 
      character*(*) M17_PSN_1KM_PROD_ID  
      parameter (M17_PSN_1KM_PROD_ID = 'MOD.AM1.V1.psn_8dy.L4')

C      Global Metadata for Product                                             
      character*(*) M17_1K_PSN_HIGHEST_PIX_CPLT,
     &              M17_1K_PSN_TOTAL_PIX_TILE, M17_1K_PSN_NTH_RESTART
  
      parameter (M17_1K_PSN_HIGHEST_PIX_CPLT = 'highest_pix_completed')
      parameter (M17_1K_PSN_TOTAL_PIX_TILE  =  'total_pixels_per_tile')
      parameter (M17_1K_PSN_NTH_RESTART     =            'nth_restart')

C  8 day composite PSN at 1 KM, Parameter 3716                                 

C SDS Name:  PSN_1K     
C !Description: Photosynthesis (gross primary production),
C               at 1KM, 8 day composite, Parameter 3716.                         
      character*(*) M17PSN_1K  
      parameter (M17PSN_1K     =        'PSN_1K')

C SDS Name:  PSN_1K_QC
C !Description:   Quality control for 8-day composited PSN                          
      character*(*) M17PSN_1K_QC 
      parameter (M17PSN_1K_QC         = 'PSN_1K_QC')

C SDS Name:       Columns per Global Grid Row
C !Description:    The number of valid data columns in the MODLAND
C               integerized sinusoidal grid for each global grid row
C               contained this tile                                               
      character*(*) M17_PSN_1K_COL_GRD   
      parameter (M17_PSN_1K_COL_GRD = 'Columns per Global Grid Row')

C Dimension Names:                                                             
      character*(*) M17_PSN_1K_DATA_ROWS, M17_PSN_1K_DATA_COLUMNS,
     &              M17_PSN_1K_GlOB_GRD_RWS 

      parameter (M17_PSN_1K_DATA_ROWS   =         'Data Rows')
      parameter (M17_PSN_1K_DATA_COLUMNS =        'Data Columns')
      parameter (M17_PSN_1K_GlOB_GRD_RWS =       'Global Grid Rows')

C----------------------------------------------------------------------+
C               MOD 17 L4                                              |
C               Tile 25-KM Climate Modeling Grid PRODUCT: PSN          |
C               V1              0909/1996                              |
C----------------------------------------------------------------------+

C      Product type identifier                                                 
      character*(*) M17_PSN_25KM_PROD_ID 
      parameter (M17_PSN_25KM_PROD_ID = 'MOD.AM1.V1.psn_8dy_cmg.L4')

C      Global Metadata for Product                                             
      character*(*) M17_PSN_25K_HGH_PIX_CPLTD, 
     &              M17_PSN_25K_PXLS_TILE,
     &              M17_PSN_25K_NTH_RESTART  

      parameter (M17_PSN_25K_HGH_PIX_CPLTD = 'highest_pix_completed')
      parameter (M17_PSN_25K_PXLS_TILE     = 'total_pixels_per_tile')
      parameter (M17_PSN_25K_NTH_RESTART   =      'nth_restart')

C  8 day composite PSN at 25 KM, Parameter 3716
C   for the climate modeling grid.                                               

C SDS Name:  PSN_25K
C !Description: Photosynthesis (gross primary production), at 25KM,
C              8 day composite, Parameter 3716.                         
      character*(*) M17PSN_25K      
      parameter (M17PSN_25K          =              'PSN_25K')

C SDS Name:  PSN_25K_QC
C !Description:   Quality control for 8-day composited PSN                           
      character*(*) M17PSN_25K_QC  
      parameter (M17PSN_25K_QC       =            'PSN_25K_QC')

C SDS Name:       Columns per Global Grid Row
C !Description:    The number of valid data columns in the MODLAND
C               integerized sinusoidal grid for each global grid row
C               contained this tile                                              
      character*(*) M17_PSN_25K_COL_GLB  
      parameter (M17_PSN_25K_COL_GLB = 'Columns per Global Grid Row')

C Dimension Names:                                                             
      character*(*) M17_PSN_25K_DATA_ROWS, M17_PSN_25K_DATA_COLUMNS,
     & M17_PSN_25K_GlOB_GRD_RWS  

      parameter (M17_PSN_25K_DATA_ROWS     =      'Data Rows')
      parameter (M17_PSN_25K_DATA_COLUMNS  =      'Data Columns')
      parameter (M17_PSN_25K_GLOB_GRD_RWS   =    'Global Grid Rows')

C----------------------------------------------------------------------+
C                   MOD.AM1.V1.seaice_max.L2                           |
C                   L2 Daily Sea-Ice Cover                             | 
C                   Version V1 rev. 7 24-April-1996                    |
C----------------------------------------------------------------------+

      character*(*) M29L2_PROD_ID
C     parameter (M29L2_PROD_ID          = 'MOD29_L2')
      parameter (M29L2_PROD_ID          = 'MOD.AM1.V1.seaice_max.L2')

      character*(*) M29SEA_ICE_PIXELS, M29NOT_SEA_ICE_PERCENT,
     &              M29SEA_ICE_PERCENT, M29NDSI_ABOVE, M29NDSI_BELOW,
     &              M29ZERO_DIVIDE, M29OUT_OF_RANGE, M29NO_DECISION,
     &              M29DAILY_SEA_ICE

C Global Metadata                                         
      parameter (M29SEA_ICE_PIXELS  = 'Total_sea_ice_pixels')
      parameter (M29SEA_ICE_PERCENT = 'Percentage_sea_ice')
      parameter (M29NOT_SEA_ICE_PERCENT = 'Percentage_not_sea_ice')
      parameter (M29NDSI_ABOVE      = 'Above_range_NDSI')
      parameter (M29NDSI_BELOW      = 'Below_range_NDSI')
      parameter (M29ZERO_DIVIDE     = 'Division_by_zero')
      parameter (M29OUT_OF_RANGE    = 'Out_of_range_input')
      parameter (M29NO_DECISION     = 'No_decision')
 
C Identification of daily sea ice cover SDS 
      parameter (M29DAILY_SEA_ICE   = 'daily_sea_ice_cover')
 
C--------------------------------------------------------------------+
C                     MOD 29-L2G                                     |
C                     L2 Sea Ice Extent                              |
C                     Beta Version                                   |
C--------------------------------------------------------------------+

      character*(*) M29L2G_PROD_ID
      parameter (M29L2G_PROD_ID         = 'MOD29_L2G')
 
      character*(*) M29L2GDAILY_SEA_ICE

C Daily Ice Cover SDS                                     
      parameter (M29L2GDAILY_SEA_ICE = 'daily_ice_cover')
 
C---------------------------------------------------------------+
C                    MOD.AM1.seaice_max_dy.L3                   |
C                    L3 Daily Sea-Ice Cover                     |
C                    Version V1 Rev, 5 24-April-1996            |
C---------------------------------------------------------------+

      character*(*) M29L3_PROD_ID
C     parameter (M29L3_PROD_ID          = 'MOD29_L3_DY_G')
      parameter (M29L3_PROD_ID          = 'MOD.AM1.seaice_max_dy.L3')
 
      character*(*) M29L3DAILY_SEA_ICE

C Identification of daily sea ice cover SDS 
      parameter (M29L3DAILY_SEA_ICE = 'daily_gridded_sea_ice_cover')
 
C---------------------------------------------------------------+
C                    MOD.AM1.V1.snow_10dy.L3           		|
C                    Ten day composited snow cover product      |
C                    Version V1 Rev. 6          24-April-1996   |
C---------------------------------------------------------------+

      character*(*) M33L3_PROD_ID
C     parameter (M33L3_PROD_ID          = 'MOD33_L3_WK_G')
      parameter (M33L3_PROD_ID          = 'MOD.AM1.V1.snow_10dy.L3')


C  SDS Name:       Composite_Snow_Cover
C !Description:    Product is a result of compositing snow cover
C                  for ten days of MOD10L3 products for every cell
C                  in the tile.   Data represents the number of days
C                  that snow was identified in a cell.
C       SDS:
      character*(*) M33WEEKLY_SNOW

C     parameter (M33WEEKLY_SNOW = 'weekly_snow_cover')
      parameter (M33WEEKLY_SNOW = 'Composite_Snow_Cover')
 
      character*(*) M33ZERO_DAY_COVER, M33ONE_DAY_COVER,
     & M33TWO_DAY_COVER,
     & M33THREE_DAY_COVER, M33FOUR_DAY_COVER, M33FIVE_DAY_COVER,
     & M33SIX_DAY_COVER, M33SEVEN_DAY_COVER,  M33EIGHT_DAY_COVER, 
     & M33NINE_DAY_COVER, M33TEN_DAY_COVER, M33ELEVEN_DAY_COVER,
     & M33MAX_SNOW_COVER_AREA, M33TILE_AREA

      parameter ( M33TILE_AREA           = 'tile_area')
      parameter ( M33ZERO_DAY_COVER      = 'zero_day_cover') 
      parameter ( M33ONE_DAY_COVER       = 'one_day_cover')   
      parameter ( M33TWO_DAY_COVER       = 'two_day_cover') 
      parameter ( M33THREE_DAY_COVER     = 'three_day_cover')  
      parameter ( M33FOUR_DAY_COVER      = 'four_day_cover') 
      parameter ( M33FIVE_DAY_COVER      = 'five_day_cover') 
      parameter ( M33SIX_DAY_COVER       = 'six_day_cover')  
      parameter ( M33SEVEN_DAY_COVER     = 'seven_day_cover') 
      parameter ( M33EIGHT_DAY_COVER     = 'eight_day_cover')  
      parameter ( M33NINE_DAY_COVER      = 'nine_day_cover')  
      parameter ( M33TEN_DAY_COVER       = 'ten_day_cover')  
      parameter ( M33ELEVEN_DAY_COVER    = 'eleven_day_cover')     
      parameter ( M33MAX_SNOW_COVER_AREA = 'max_snow_cover_area') 
      parameter ( M33MAX_SNOW_COVER_PCT  = 'max_snow_cover_pct')
      parameter ( M33TEN_DAY_MEAN        = 'ten_day_mean')

C       SDS:                                                            */
 
      character*(*) M33MEAN_SNOW_COVER, M33MEAN_TEN_DAY_MEAN,
     &              M33QA_OVERALL

      parameter ( M33MEAN_SNOW_COVER    = 'Mean_Snow_Cover')
      parameter ( M33MEAN_TEN_DAY_MEAN  = 'mean_ten_day_mean')
      parameter ( M33QA_OVERALL         = 'QA_overall')

C---------------------------------------------------------------+
C                  MOD.AM1.vi_1m.L3                             |
C                  Monthly vegetation indices                   | 
C                  Version V1  rev. 6  06-May-1996              |
C---------------------------------------------------------------+

      character*(*) M34L3_PROD_ID 
C     parameter (M34L3_PROD_ID          = 'MOD34_L3_MN')
      parameter (M34L3_PROD_ID          = 'MOD.AM1.vi_1m.L3')

 
      character*(*) M34MVI, M34NDVI_ZENITH_ANGLES, M34MVI_QUALITY,
     &              M34MVI_ZENITH_ANGLES, M34NDVI_QUALITY, M34NDVI 

C NDVI SDS 
C     parameter (M34NDVI = 'ndvi')
      parameter (M34NDVI = 'NDVI_250_M')
 
C MVI SDS 
C     parameter (M34MVI  = 'mvi')
      parameter (M34MVI  = 'MVI_250_M')
 
C View zenith angles for NDVI SDS 
      parameter (M34NDVI_ZENITH_ANGLES = 'VZ_NDVI')
 
C View zenith angles for MVI SDS 
      parameter (M34MVI_ZENITH_ANGLES  = 'VZ_MVI')
 
C Quality control for NDVI SDS 
C     parameter (M34NDVI_QUALITY = 'QC_NDVI')
      parameter (M34NDVI_QUALITY = 'NDVI_250_M_QC')
 
C Quality control for MVI SDS 
C     parameter (M34MVI_QUALITY  = 'QC_MVI')
      parameter (M34MVI_QUALITY  = 'MVI_250_M_QC')

C------------------------------------------------------------------------+
C                       MOD.AM1.V1.vi_8dy.L3                             |   
C                       8 day composite NDVI and MVI @ 250 m            |  
C                       Version V1 Rev 6 06-May-1996                     |  
C------------------------------------------------------------------------+

      character*(*) M34L3_10DY_250M_PROD_ID
C     parameter (M34L3_10DY_250M_PROD_ID = 'MOD34_L3_10DY_250M')
      parameter (M34L3_10DY_250M_PROD_ID = 'MOD.AM1.V1.vi_8dy.L3')


C SDS Name:      NDVI_250_10DY
C !Description:   NDVI_250_10DY                                          
      character*(*) M34L3_NDVI_250_10DY             

C SDS Name:      MVI_250_10DY
C !Description:   MVI_250_10DY                                            
      character*(*) M34MVI_250_10DY                 

C SDS Name:      NDVI_250_10day_QC
C !Description:   Quality control for NDVI                                 
      character*(*) M34NDVI_250_10DAY_QC              

C SDS Name:      MVI_250_10DY_QC
C !Description:   Quality control for MVI                                   
      character*(*) M34MVI_250_10DY_QC               

C     parameter ( M34L3_NDVI_250_10DY          = 'NDVI_250_10DY')
C     parameter ( M34MVI_250_10DY              = 'MVI_250_10DY')
C     parameter ( M34NDVI_250_10DAY_QC         = 'NDVI_250_10day_QC')
C     parameter ( M34MVI_250_10DY_QC           = 'MVI_250_10DY_QC')

      parameter ( M34L3_NDVI_250_10DY          = 'NDVI_250_8DY')
      parameter ( M34MVI_250_10DY              = 'MVI_250_8DY')
      parameter ( M34NDVI_250_10DAY_QC         = 'NDVI_250_8DY_QC')
      parameter ( M34MVI_250_10DY_QC           = 'MVI_250_8DY_QC')

C----------------------------------------------------------------------+
C                       MOD.AM1.vi_16dy.L3                             |
C                       16 day composite NDVI and MVI @ 250 m          |
C                       Version V1 Rev 6 06-May-1996                   |   
C----------------------------------------------------------------------+

      character*(*) M34L3_16DY_250M_PROD_ID
C     parameter (M34L3_16DY_250M_PROD_ID = 'MOD34_L3_16DY_250M')
      parameter (M34L3_16DY_250M_PROD_ID = 'MOD.AM1.vi_16dy.L3')


C SDS Name:      NDVI_250_16DY
C !Description:   NDVI_250_16DY                                           
      character*(*)  M34L3_NDVI_250_16DY         

C SDS Name:      MVI_250_16DY
C !Description:   MVI_250_16DY                                             
      character*(*)  M34MVI_250_16DY            

C SDS Name:      NDVI_250_16day_QC
C !Description:   Quality control for NDVI                                  
      character*(*)  M34NDVI_250_16DAY_QC         

C SDS Name:      MVI_250_16DY_QC
C !Description:   Quality control for MVI                               
      character*(*)  M34MVI_250_16DY_QC          

      parameter (  M34L3_NDVI_250_16DY   = 'NDVI_250_16DY')
      parameter (  M34MVI_250_16DY       = 'MVI_250_16DY')
C     parameter (  M34NDVI_250_16DAY_QC  = 'NDVI_250_16day_QC')
      parameter (  M34NDVI_250_16DAY_QC  = 'NDVI_250_16DY_QC')
      parameter (  M34MVI_250_16DY_QC    = 'MVI_250_16DY_QC')

C--------------------------------------------------------------------------+
C                       MOD.AM1.vi_8dy_cmg.L3                              |
C                       8 day composite NDVI and MVI @ 25 km               | 
C                       Version V1 Rev 6 06-May-1996                       |
C--------------------------------------------------------------------------+

      character*(*) M34L3_10DY_25KM_PROD_ID
C     parameter (M34L3_10DY_25KM_PROD_ID = 'MOD34_L3_10DY_25km')
      parameter (M34L3_10DY_25KM_PROD_ID = 'MOD.AM1.vi_8dy_cmg.L3')

C      Product type identifier                                             
      character*(*)  M34L3_NDVI_25_10DY, M34MVI_25_10DY,  
     &               M34NDVI_25_10DAY_QC, M34MVI_25_10DY_QC  

C     parameter (  M34L3_NDVI_25_10DY   = 'NDVI_25_10DY')
C     parameter (  M34MVI_25_10DY       = 'MVI_25_10DY')
C     parameter (  M34NDVI_25_10DAY_QC  = 'NDVI_25_10day_QC')
C     parameter (  M34MVI_25_10DY_QC    = 'MVI_25_10DY_QC')

      parameter (  M34L3_NDVI_25_10DY   = 'NDVI_25_8DY')
      parameter (  M34MVI_25_10DY       = 'MVI_25_8DY')
      parameter (  M34NDVI_25_10DAY_QC  = 'NDVI_25_8DY_QC')
      parameter (  M34MVI_25_10DY_QC    = 'MVI_25_8DY_QC')


C--------------------------------------------------------------------------+
C                       MOD.AM1.vi_16dy_cmg.L3                            |
C                       16 day composite NDVI and MVI @ 25 km              |
C                       Version V1 Rev 5 24-April-1996                     |
C--------------------------------------------------------------------------+

      character*(*) M34L3_16DY_25KM_PROD_ID 
C     parameter (M34L3_16DY_25KM_PROD_ID = 'MOD34_L3_16DY_25km')
      parameter (M34L3_16DY_25KM_PROD_ID = 'MOD.AM1.vi_16dy_cmg.L3')


C SDS Name:      NDVI_25_16DY
C !Description:   NDVI_25_16DY                                             
      character*(*)  M34L3_NDVI_25_16DY           

C SDS Name:      MVI_25_16DY
C !Description:   MVI_25_16DY                                              
      character*(*)  M34MVI_25_16DY             

C SDS Name:      NDVI_25_16day_QC
C !Description:   Quality control for NDVI                                  
      character*(*)  M34NDVI_25_16DAY_QC       

C SDS Name:      MVI_25_16DY_QC
C !Description:   Quality control for MVI                                   
      character*(*)  M34MVI_25_16DY_QC        

      parameter (M34L3_NDVI_25_16DY     = 'NDVI_25_16DY')
      parameter (M34MVI_25_16DY         = 'MVI_25_16DY')
C     parameter (M34NDVI_25_16DAY_QC    = 'NDVI_25_16day_QC')
      parameter (M34NDVI_25_16DAY_QC    = 'NDVI_25_16DY_QC')
      parameter (M34MVI_25_16DY_QC      = 'MVI_25_16DY_QC')

C--------------------------------------------------------------------------+
C                       MOD.AM1.vi_1m_cmg.L3                               | 
C                       monthly composite NDVI and MVI @ 25 km             | 
C                       Version V1 Rev 6 06-May-1996                       | 
C--------------------------------------------------------------------------+
      character*(*) M34L3_MN_25KM_PROD_ID
C     parameter (M34L3_MN_25KM_PROD_ID   = 'MOD34_L3_MN_25km')
      parameter (M34L3_MN_25KM_PROD_ID   = 'MOD.AM1.vi_1m_cmg.L3')


C SDS Name:      NDVI_25_M
C !Description:   NDVI_25_M                                             
      character*(*)  M34L3_NDVI_25_M        

C SDS Name:      MVI_25_M
C !Description:   MVI_25_M                                              
      character*(*)  M34MVI_25_M           

C SDS Name:      NDVI_25_M_QC
C !Description:   Quality control for NDVI                                  
      character*(*)  M34NDVI_25_M_QC      

C SDS Name:      MVI_25_M_QC
C !Description:   Quality control for MVI                             
      character*(*)  M34MVI_25_M_QC      

      parameter (M34L3_NDVI_25_M    = 'NDVI_25_M')
      parameter (M34MVI_25_M        = 'MVI_25_M')
      parameter (M34NDVI_25_M_QC    = 'NDVI_25_M_QC')
      parameter (M34MVI_25_M_QC     = 'MVI_25_M_QC')

C--------------------------------------------------------------------------+
C                       MOD.AM1.seaice_10dy.L3                             | 
C                       Ten day composited ice cover product.              | 
C                       Version V1 Rev 6 24-April-1996                     | 
C--------------------------------------------------------------------------+
C--------1---------2----------3---------4----------5---------6----------7-+
      character*(*) M42L3_PROD_ID

C     parameter (M42L3_PROD_ID           = 'MOD42_L3_WK_G')
      parameter (M42L3_PROD_ID           = 'MOD.AM1.seaice_10dy.L3')

C SDS Name:    Composite_Ice_Cover                                        |
C !Description: Product is a result of compositing ice cover for           |
C              ten days of MOD10L3 products for every cell in             |
C              the tile.   Data represents the number of days             | 
C              that ice was identified in a cell.                         |

      character*(*) M42WEEKLY_SEA_ICE
      parameter (M42WEEKLY_SEA_ICE = 'Composite_Ice_Cover')

C  metadata:
      character*(*) M42MAX_ICE_COVER_AREA, M42MAX_ICE_COVER_PCT,
     &              M42TILE_AREA, M42ZERO_DAY_COVER, M42ONE_DAY_COVER,
     &              M42TWO_DAY_COVER, M42THREE_DAY_COVER,
     &              M42FOUR_DAY_COVER, M42FIVE_DAY_COVER,
     &              M42SIX_DAY_COVER, M42SEVEN_DAY_COVER, 
     &              M42EIGHT_DAY_COVER, M42NINE_DAY_COVER,
     &              M42TEN_DAY_COVER,
     &              M42ELEVEN_DAY_COVER, M42MAX_SNOW_COVER_AREA, 
     &              M42MAX_SNOW_COVER_PCT, M42TEN_DAY_MEAN,
     &              M42QA_OVERALL

      parameter ( M42TILE_AREA           = 'tile_area')
      parameter ( M42ZERO_DAY_COVER      = 'zero_day_cover')
      parameter ( M42ONE_DAY_COVER       = 'one_day_cover')
      parameter ( M42TWO_DAY_COVER       = 'two_day_cover')
      parameter ( M42THREE_DAY_COVER     = 'three_day_cover')
      parameter ( M42FOUR_DAY_COVER      = 'four_day_cover')
      parameter ( M42FIVE_DAY_COVER      = 'five_day_cover')
      parameter ( M42SIX_DAY_COVER       = 'six_day_cover')
      parameter ( M42SEVEN_DAY_COVER     = 'seven_day_cover')
      parameter ( M42EIGHT_DAY_COVER     = 'eight_day_cover')
      parameter ( M42NINE_DAY_COVER      = 'nine_day_cover')
      parameter ( M42TEN_DAY_COVER       = 'ten_day_cover')
      parameter ( M42ELEVEN_DAY_COVER    = 'eleven_day_cover')
      parameter ( M42MAX_SNOW_COVER_AREA = 'max_snow_cover_area')
      parameter ( M42MAX_SNOW_COVER_PCT  = 'max_snow_cover_pct')
      parameter ( M42TEN_DAY_MEAN        = 'ten_day_mean')
      parameter ( M42QA_OVERALL          = 'QA_overall')
      parameter ( M42MAX_ICE_COVER_AREA  = 'max_ice_cover_area')
      parameter ( M42MAX_ICE_COVER_PCT   = 'max_ice_cover_pct')

C-------------------------------------------------------------------------+
C
C  SDS Name:    Mean_Ice_Cover
C !Description: Product is a result of compositing ice cover for ten days of
C               MOD10L3 products for every cell in the tile.  Data represents
C               the mean number of days that ice was identified in a cell.
C-------------------------------------------------------------------------+
       character*(*) M42MEAN_ICE_COVER

       parameter ( M42MEAN_ICE_COVER   =  'Mean_Ice_Cover')

C  metadata:
C  
      character*(*) M42ICE_COVER_AREA, M42ICE_COVER_PCT

      parameter ( M42ICE_COVER_AREA  = 'ice_cover_area')
      parameter ( M42ICE_COVER_PCT   = 'ice_cover_pct')

