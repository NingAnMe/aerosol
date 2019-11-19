/************************************************************************
*         		mapiland.h utilities header file                *
*                     					                * 
*				 MODLAND                                * 
*                                                                       *
*			HDF Object Identifier Constants	                *
*************************************************************************
*************************************************************************
* !C-INC
*
* !Purpose:	Utilities header file containing constants for MOD LAND
*		discipline HDF object names.
*
* !Description: The Header file mapi.h is part of a larger software
*              system called the MODIS Application Program Interface (API)
*              Utility, abbreviated M-API.  The M-API Utility consists of
*              subroutines which allow MODIS Science Team-supplied software
*              to read in Level 1B radiance bands and write out output
*              products and metadata to HDF files.  The functionality of the
*              M-API is defined in the MODIS API User's Guide, Version 1,
*              dated 4/3/95.
*
*	       The mapiland.h file contains macros for the the specific
*	       names of data objects contained in the following MODIS  
*	       and product HDF files:
*
*                MOD.AM1.geoang.L2G              Vers V1 Rev 1 29-Apr-1996
*                MOD.AM1.pntr_xxxx.L2G           Vers V1 Rev 5 11-Jun-1996
*                MOD.PR09_L2G_xxxx               Beta
*                MOD.AM1.sref1.L2                Vers V1 Rev 3 11-Apr-1996 
*                MOD.AM1.V1.srefl_500m.L2G       Vers V1 Rev 2 08-May-1996
*                MOD.AM1.V1.srefl_250m.L2G       Vers V1 Rev 2 08-May-1996
*                MOD.AM1,brdfsubs.L3             Vers V1 Rev 2  8-Apr-1996
*                MOD.AM1.texture_16dy.L3         Vers V1 Rev 3 24-Apr-1996 
*                MOD.AM1.bars_16dy.L3            Vers V1 Rev 2 08-Apr-1996
*                MOD.AM1.brdf_16dy.L3            Vers V1 Rev 3 08-Apr-1996
*                MOD10_L2                        Vers V2 Rev 2 03-Oct-1997
*                MOD10L2G                        Vers V2 Rev 3 05-Dec-1997
*                MOD10A1                         Vers V2 Rev 2 23-Dec-1997
*                MOD.AM1.V1.lst.L2               Vers V1 Rev 5 16-Apr-1996
*                MOD11_L2G                       Beta
*                MOD.AM1.V1.lst_1dy_cmg.L3       Vers V1 Rev 3 17-Jun-1996
*                MOD.AM1.V1.lst_8dy_cmg.L3       Vers V1 Rev 6 06-May-1996
*                MOD.AM1.V1.lst_1m_cmg.L3        Vers V1 Rev 6 06-May-1996
*                MOD.AM1.V1.lc_1km.L3.3m         Vers V1 Rev 6 01-May-1996
*                MOD.AM1.V1.landcov_32dy.L3      Vers V1 Rev 8 29-May-1996
*                MOD.AM1.V1.lc_halfdegree.L3.3m  Vers V1 Rev 6 01-May-1996
*                MOD.AM1.V1.fire.L2G             Vers V1 Rev 2 08-May-1996 
*                MOD14_L3                        Beta          
*                MOD.AM1.V1.fpar_lai_8dy.L3      Vers V1 Rev 4 09-Sep-1996
*                MOD.AM1.V1.fpar_lai_8dy_cmg.L3  Vers V1 Rev 4 09-Sep-1996
*                MOD.AM1.V1.npp_1yr.L4           Vers V1 Rev 4 09-Sep-1996
*                MOD.AM1.V1.npp_1yr_cmg.L4       Vers V1 Rev 4 09-Sep-1996
*                MOD.AM1.V1.psn_8dy.L4           Vers V1 Rev 4 09-Sep-1996
*                MOD.AM1.V1.psn_8dy_cmg.L4       Vers V1 Rev 4 09-Sep-1996
*                MOD.AM1.V1.seaice_max.L2        Vers V1 Rev 7 24-Apr-1996
*                MOD29_L2G                       Beta
*                MOD.AM1.seaice_max_dy.L3        Vers V1 Rev 5 24-Apr-1996 
*                MOD.AM1.V1.snow_10dy.L3         Vers V1 Rev 6 24-Apr-1996
*                MOD.AM1.vi_1m.L3                Vers V1 Rev 6 06-May-1996
*                MOD.AM1.V1.vi_8dy.L3            Vers V1 Rev 6 06-May-1996
*                MOD.AM1.vi_16dy.L3              Vers V1 Rev 6 06-May-1996
*                MOD.AM1.vi_8dy_cmg.L3           Vers V1 Rev 6 06-May-1996
*                MOD.AM1.vi_16dy_cmg.L3          Vers V1 Rev 5 24-Apr-1996
*                MOD.AM1.vi_1m_cmg.L3            Vers V1 Rev 6 06-May-1996
*                MOD.AM1.V1.seaice_10dy.L3       Vers V1 Rev 6 24-Apr-1996
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
* 	$Log: mapiland.h,v $
 * Revision 1.23  1998/04/08  19:14:50  fshaw
 * updated using new file specs
 *
 * Revision 1.23  1998/04/08  19:14:50  fshaw
 * updated using new file specs
 *
 * Revision 1.22 1998/03/04   18:40:12   solanki
 * Updated to Version 2
 * in MOD10* sections (Snow Cover) to match filespecs
 *
 * Revision 1.21  1998/03/02  20:39:12  fshaw
 * added bangs to prologs
 *
 * Revision 1.10  1996/05/10  14:44:28  fshaw
 * inserted   in prolog
 * used sed script file
 *
 * Revision 1.9  1996/05/01  19:18:33  fshaw
 * updated file using current V1 file specs
 *
*
* !Team-unique Header: 
*
* !References and Credits: 
*      This software is developed by the MODIS Science Data Support
*      Team for the National Aeronautics and Space Administration,
*      Goddard Space Flight Center, under contract NAS5-32373.
*
*	Written by
*               Jeffrey J. Blanchette  1996/01/17
*		General Science Corp.
*		MODIS SCIENCE DATA SUPPORT OFFICE
*		7501 FORBES BLVD, SEABROOK MD 20706
*	 	jeffrey.j.blanchette@@gsfc.nasa.gov
*
*
*       modified by
*               Frederick J. Shaw   10-Apr-96
*               General Science Corp.
*               MODIS SCIENCE DATA SUPPORT OFFICE
*               7501 Forbes Blvd. Seabrook MD 20706
*               fshaw@@ltpmail.gsfc.nasa.gov
*               updated include file using version 1 file specs.
*
* !Design Notes: 
*
*!END*********************************************************************
*/

#ifndef MAPILAND
#define	MAPILAND

/**********************************************************************/
/* 			MOD LAND                                      */
/*			Common SDS's                                  */
/**********************************************************************/

/*	L2G number of observations per pixel contained within L2G file SDS      */
#define MLNUMBER_OF_OBS	     		"num_observations"

/*	The number of columns in the full ISCCP grid for each row
                (line) contained within the L2G file SDS.            */
#define MLNUMBER_OF_COLS		"ncol"

/*	The start column in the full ISCCP grid for each row (line) 
                contained within the L2G file (starting at zero) SDS. */
#define MLSTART_COLUMN			"icol_start"

/*	The number of columns in each row (line) contained within 
                the L2G file SDS.                                     */
#define	MLCOLS_PER_ROW			"ncol_tile"

/*	The start pixel of the first valid column in each row (line)
                contained within the L2G file (starting at zero) SDS. */
#define MLSTART_PIX			"ipix_start"

/*	Number of observations per line SDS                           */
#define MLOBS_PER_LINE			"nobs_line"

/* Columns per Global Grid Row                                        */
#define MLCOLUMNS_GLOBAL_GRID_ROW       "Columns per Global Grid Row"

/* SDS_OBJECT = 1km Data Samples Per Line                             *
 * LONG_NAME = 1km Data Samples Per 1km Data Line in this MODIS Granule*
 * DEFINITION = 1km Data Samples Per 1km Data Line in this MODIS Granule      */
#define  ML1KMDATA_SAM_PER_LINE  "1km Data Samples Per Line"

/*********************************************************************/
/* 			MOD LAND                      	              */
/*                      M-Common-A Metadata                           */
/*			Common Global Metadata		              */
/**********************************************************************/

#define MLPIXELS_PER_SCAN		"Pixels_per_scan_line"
#define MLNUMBER_OF_LINES		"Number_of_scan_lines"

/*  OBJECT = Number of Instrument Scans                               *
 *  DEFINITION = Number of instrument scans contained in this MODIS granule.*/
#define MLNUM_INSTR_SCANS            "Number of Instrument Scans"

#define MLTOTALNUMRETRIEVEDLSTGRIDSINFILE "TOTALNUMBEROFRETRIEVEDLSTGRIDSINFILE"
/***********************************************************************
 *                      M-Common A Metadata                           *
 *                      Object Level Attributes                       *
 **********************************************************************/

/* OBJECT = Data Lines                                            *
 * DEFINITION = The number of data lines contained in this SDS.   */
#define MLDATA_LINES               "Data Lines"

/* OBJECT = Maximum Number of Data Samples Per Line                      *
 * DEFINITION = The maximum number of data samples per line in this SDS. */
#define MLMAX_NUMBER_DATA_SAMPLES_LINE    "Maximum Number of Data Samples Per Line"

/* OBJECT = Nadir Data Resolution                                      *
 * DEFINITION = Describes the spatial resolution at nadir of the data stored in *
 * an SDS.                                                            */
#define MLNADIR_DATA_RES           "Nadir Data Resolution"

/**********************************************************************
                        M-Common-B Metadata                            *
***********************************************************************/

#define MLPIXELS_PER_LINE		"Pixels_per_line"
#define MLLINES_PER_TILE		"Lines_per_tile"
#define MLNUMBER_OF_PARAMS		"Num_parameters"
#define MLGRANULE_IDS                   "Granule_IDs"                     

/* #define MLMAX_OBSERVATIONS		"Maximum_observations"
   #define MLNUMBER_OF_GRANULES		"Number_of_granules"
   #define MLTOTAL_OBSERVATIONS		"Total_observations"           */

#define MLMAX_OBSERVATIONS		"Maximum Observations"
#define MLNUMBER_OF_GRANULES		"Number of Granules"
#define MLTOTAL_OBSERVATIONS		"Total Observations"           
#define MLMAX_LINE_INDEX                "Maximum Line Index"           
#define MLMAX_SAMPLE_INDEX              "Maximum Sample Index"           
#define MLMIN_LINE_INDEX                "Minimum Line Index"           
#define MLMIN_SAMPLE_INDEX              "Minimum Sample Index"           

/* OBJECT = L2G Storage Format                                         *
 * DEFINITION = A flag reflecting if the file is in the 'Full'         *
 * or 'Compact'. MODLAND L2G file format.                              */
#define MLFILE_FORMAT			"L2G Storage Format"

#define MLPARM1				"Parameter1"
#define MLPARM2				"Parameter2"
#define MLPARM3				"Parameter3"
#define MLPARM4				"Parameter4"
#define MLPARM5				"Parameter5"
#define MLPARM6				"Parameter6"
#define MLPARM7				"Parameter7"
#define MLYEAR				"Year"
#define MLDOY				"Day_of_year"
#define MLNUMBER_OF_ROWS		"nrow"
#define MLREF_LONGITUDE			"ref_lon_in_deg"
#define MLIROW_START			"irow_start"
#define MLNCOL_MAX              	"ncol_max"
#define MLITILE_HORIZ			"itile_horiz"
#define MLITILE_VERT			"itile_vert"
#define MLNTILE_HORIZ			"ntile_horiz"
#define MLNTILE_VERT			"ntile_vert"

/**********************************************************************
 *      M-Common-B Metadata                                           *
 *      Global Attributes                                             * 
 *********************************************************************/

/* OBJECT = Central Meridian                                          *
 * DEFINITION = Central longitude meridian of the MODLAND global grid. */
#define MLCENTRAL_MERIDIAN            "Central Meridian"

/*  OBJECT = Characteristic Bin Angular Dimension                     *
 *  DEFINITION = The characteristic bin angular dimension in          *
 *   arc-seconds of this                                              */
#define MLANGULAR_SIZE	"Characteristic Bin Angular Dimension"

/* OBJECT = Characteristic Bin Size                                   *
 * DEFINITION = The characteristic bin size of this MODLAND tile. */
#define MLCHAR_BIN_SIZE               "Characteristic Bin Size"

/* OBJECT = Column Storage Format                                     *
 * DEFINITION = Specifies whether the extra data column in the MODLAND     *
 * integerized sinusoidal grid tile is to the left or right of the center. */
#define MLCOL_STORAGE_FORMAT          "Column Storage Format"

/* OBJECT = Data Columns                                              *
 * DEFINITION = The maximum number of data columns (samples) per row in this *
 * SDS.                                                               */
#define MLDATA_COLUMNS                "Data Columns"

/* OBJECT = Data Rows                                                  *
 * DEFINITION = The number of data rows contained in this MODIS tile.  */
#define MLDATA_ROWS                   "Data Rows"

/* OBJECT = Data Upper Left Coordinates                               *
 * DEFINITION = The horizontal and vertical coordinates of the upper left *
 * corner of the MODLAND tile in reference to the MODLAND integerized  *
 * sinusoidal grid.                                                   */
#define MLDATA_UPPER_LEFT_COORD       "Data Upper Left Coordinates"

/* OBJECT = Global Grid Columns                                       *
 * DEFINITION = The number of global grid columns in the MODLAND integerized *
 * sinusoidal grid.                                                   */
#define MLGLOBAL_GRID_COLUMNS         "Global Grid Columns"

/* OBJECT = Global Grid Rows                                          *
 * DEFINITION = The number of global grid rows in the MODLAND integerized    *
 * sinusoidal grid.                                                   */
#define MLGLOBAL_GRID_ROWS            "Global Grid Rows"

/* OBJECT = Global Grid Upper Left Corner Bin Center                  *
 * DEFINITION = The location of the center of the MODLAND global grid upper  *
 * left corner bin in global grid coordinates (in meters).            */
#define MLGLOBAL_GRID_COR_BIN_CTR  "Global Grid Upper Left Corner Bin Center"

/* OBJECT = Pole                                                      * 
 * DEFINITION = The pole specified in this lambert azimuthal equal area*
 * projection.                                                        */
#define MLPOLE                        "Pole"

#define MLNUMBER_OF_LST_DIFFERENCE_BINS    "Number of LST Difference Bins"

/* OBJECT = Sphere Radius                                             *
 * DEFINITION = The sphere radius in meters of this MODLAND global grid.   */
#define MLSPHERE_RADIUS               "Sphere Radius"

/* OBJECT = Grid Nesting Level                                        *
 * DEFINITION = Grid nesting level, an integer fraction of the number of rows *
 * in the MODLAND integerized sinusoidal grid                         */
#define MLNEST_LEVEL			"Grid Nesting Level"

/*********************************************************************/
/* 			MOD LAND                      	             */
/*			Object Level Attributes                      */
/*********************************************************************/
#define MLSPSO_PARAMETERS		"SPSO_parameter" 

/*********************************************************************/
/* 			MOD LAND                      	             */
/*			E-Product-Inventory                          */
/*                      Metadata                                     */
/*********************************************************************/

#define MLSUBS_CODE_ID                  "SUBSCODEID"
#define MLPERCENT_LAND_IN_TILE          "PERCENTLANDINTILE"
#define MLPERCENT_FILLED_PIXELS         "PERCENTFILLEDPIXELS"
#define MLBRDF_CODE_ID                  "BRDFCODEID"         
#define MLMODEL_DEF_FILE_ID             "MODELDEFFILEID"
#define MLALBE_DO_TABLE_ID              "ALBEDOTABLEID"
#define MLPERCENT_LAND_INTILE           "PERCENTLANDINTILE"
#define MLPERCENT_NEW_BRDFS             "PERCENTNEWBRDFS"
#define MLPERCENT_ADJUSTED_BRDFS        "PERCENTADJUSTEDBRDFS"
#define MLPERCENT_FILL_BRDFS            "PERCENTFILLBRDFS"
#define MLPERCENT_GOOD_QUALITY          "PERCENTGOODQUALITY" 
#define MLPERCENT_NEW_BARS              "PERCENTNEWBARS"
#define MLPERCENT_ADJUSTED_BARS         "PERCENTADJUSTEDBARS"
#define MLPERCENT_FILL_BARS             "PERCENTFILLBARS"
#define MLTOTAL_RETR_LST_SAMP_INFILE    "TOTALRETRIEVEDLSTSAMPLESINFILE"

/********************************************************************* 
 * 			MOD.AM1.geoang.L2G                           * 
 *			L2G Viewing Geometry File	             * 
 *			Vers V1 Rev 1 29-Apr-1996	             * 
 ********************************************************************/
/*#define M09ANG_PROD_ID                "MOD09_ANG_L2G_1KM"     */
#define M09ANG_PROD_ID	                "MOD.AM1.geoang.L2G"     

/*	Zenith angle to sensor SDS				    */
#define M09SENSOR_ZENITH		"SensorZenith"

/*	Azimuth angle to sensor SDS			            */
#define M09SENSOR_AZIMUTH		"SensorAzimuth"

/*	Distance to sensor SDS    				    */
#define M09SENSOR_DISTANCE		"Range"

/*	Zenith angle to sun SDS				            */
#define M09SOLAR_ZENITH			"SolarZenith"

/*	Azimuth angle to sun SDS			          */
#define M09SOLAR_AZIMUTH		"SolarAzimuth"

/**********************************************************************
 *                      MOD.AM1.pntr_xxxx.L2G                         * 
 *			L2G Pointers               	              * 
 *			Vers V1 Rev 2 29-April-1996                   * 
***********************************************************************/
/*  #define M09PNT1K_PROD_ID		"MOD09_PNT_L2G_1KM"
    #define M09PNT500_PROD_ID		"MOD09_PNT_L2G_500M"
    #define M09PNT250_PROD_ID		"MOD09_PNT_L2G_250M"          */

#define M09PNT1K_PROD_ID		"MOD.AM1.pntr_1km.L2G"         
#define M09PNT500_PROD_ID		"MOD.AM1.pntr_500m.L2G"
#define M09PNT250_PROD_ID		"MOD.AM1.pntr_250m.L2G"                        
/*  Pointer to granule IDs from which the observation came SDS.  
                Zero relative.  Fill value is 255.                 */
#define M09GRANULE_PNT			"granule_pnt"

/*  Sample number of observation (1 km spatial element) in granule SDS */
#define M09OBS_IN_GRANULE		"sample"

/*  Sub-pixel (delta) line location of cell center in observation 
    footprint SDS.  Relative to center of observation specified by 
    (line, sample).	                                            */
#define M09CELL_CENTER			"dline"

/* Sub-pixel (delta) line location of cell center in observation 
                footprint SDS.  Relative to center of observation specified by 
                (line, sample).					*/
#define M09SAMPLE_CENTER		"dsample"

/*	 Observation coverage SDS: area of intersection between 
         observation footprint and cell divided by area of observation. */
#define M09OBS_COVERAGE			"obscov"

/*	 Cell coverage SDS: area of intersection between 
                observation footprint and cell divided by area of cell.  */
#define M09CELL_COVERAGE		"cellcov"

/* SDS Name: line
*!Description:    Line number of observation in granule.  Zero relative. */
#define M09L2G_LINE	                 "line"

/* SDS Name: offset_res
!Description: Row and column offset of corresponding observation in
                next coarser resolution grid.                           */
#define M09OFFSET_RES          	         "offset_res"

/* SDS Name:   iobs_res
*!Description: Number of corresponding observation in next coarser
               resolution grid.                                         */
#define M09IOBS_RES          	         "iobs_res"

/* SDS Name:      Number of Observations per Row
*!Description:   The number of observations in each row in the compact
                 storage form of the L2G grid.                         */
#define M09NUMBER_OF_OBSERVATIONS_PER_ROW "Number of Observations per Row"

/********************************************************************
 *                      MOD.AM1.srefl.L2                            *
 *                      L2 Surface Reflectance Band                 *
 *                      Version V1 rev 6 24-Apr-1996                *
 ********************************************************************/
/* #define M09_L2_PROD_ID              "MOD.AM1.V1.srefl.L2" */
   #define M09_L2_PROD_ID              M0913_L2_PROD_ID              
#define M0913_L2_PROD_ID                "MOD.AM1.srefl.L2"

/*      SurfaceReflectance for MODIS Band 1 SDS         */
#define M0913BAND1_SURF_REFL            "sur_refl_b01"

/*      SurfaceReflectance for MODIS Band 2 SDS         */
#define M0913BAND2_SURF_REFL            "sur_refl_b02"

/*      SurfaceReflectance for MODIS Band 3 SDS         */
#define M0913BAND3_SURF_REFL            "sur_refl_b03"

/*      SurfaceReflectance for MODIS Band 4 SDS         */
#define M0913BAND4_SURF_REFL            "sur_refl_b04"

/*      SurfaceReflectance for MODIS Band 5 SDS         */
#define M0913BAND5_SURF_REFL            "sur_refl_b05"

/*      SurfaceReflectance for MODIS Band 6 SDS         */
#define M0913BAND6_SURF_REFL            "sur_refl_b06"

/*      SurfaceReflectance for MODIS Band 7 SDS         */
#define M0913BAND7_SURF_REFL            "sur_refl_b07"

/*      NDVI index at 250m SDS                          */
#define M0913_NDVI_INDEX                "NDVI_index"

/*      MVI index at 250m SDS                           */
#define M0913_MVI_INDEX                 "MVI_index"

/*      Indicators of the quality of the 250m reflectance and  *
 *      VI data integrity.                                     */
#define M0913QUALITY_250                "QC_250m"

/*      Indicators of the quality of the 500m reflectance and  *
        VI data integrity.                                     */
#define M0913QUALITY_500                "QC_500m"

/*      SDS Metadata                                    */
#define M0913NUM_DETECTORS              "num_detectors"
#define M0913SAMPLING                   "sampling"

/*      Global Metadata                                                       */
#define M0913ROW_NADIR_DATA_RESOLUTION       "Row Nadir Data Resolution"
#define M0913COLUMN_NADIR_DATA_RESOLUTION    "Column Nadir Data Resolution"

/********************************************************************
 *                      MOD PR09_L2G_500M                           *
 *                      L2G Surface Reflectance and VI Parameters   *
 *                      Version V1 rev 2 08-May-1996                *
 ********************************************************************/
/*#define M09_L2G_500M_PROD_ID		"MOD09_L2G_500M"            */  
#define M09_L2G_500M_PROD_ID		"MOD.AM1.V1.srefl_500m.L2G"           

/*	 Surface Reflectance for MODIS Band 3 SDS                   */
#define M09BAND3_SURF_REFL	    	"sur_refl_b03"

/*	Surface Reflectance for MODIS Band 4 SDS                    */
#define	M09BAND4_SURF_REFL	    	"sur_refl_b04"

/*	Surface Reflectance for MODIS Band 5 SDS                    */
#define	M09BAND5_SURF_REFL	    	"sur_refl_b05"

/*	Surface Reflectance for MODIS Band 6 SDS                    */
#define	M09BAND6_SURF_REFL	    	"sur_refl_b06"

/*	Surface Reflectance for MODIS Band 7 SDS                    */
#define	M09BAND7_SURF_REFL	    	"sur_refl_b07"

/*	Indicators of the quality of the 500 m reflectance data SDS */
#define M09QUALITY_500			"QC_500m"

/*******************************************************************
*                       MOD PR09_L2G_250M                          *
*                       250M Resolution L2G Surface Reflectance    *
*                       Version V1 Rev 2  08-May-96                *
********************************************************************/
/*#define M09_L2G_250M_PROD_ID		"MOD09_L2G_250M"            */
#define M09_L2G_250M_PROD_ID		"MOD.AM1.V1.srefl_250m.L2G"

/*	 Surface Reflectance for MODIS Band 1 SDS                  */
#define M09BAND1_SURF_REFL		"sur_refl_b01"

/*	Surface Reflectance for MODIS Band 2 SDS                      */
#define	M09BAND2_SURF_REFL		"sur_refl_b02"

/*	Indicators of the quality of the 250 m reflectance and
                VI data integrity     
        SDS:                                                          */
#define M09QUALITY_250			"QC_250m"

/**********************************************************************/
/* 		        MOD.AM1,brdfsubs.L3	                      */
/*             16-day subsetted reflectance database for input        */
/*            into the BRDF/Albedo process, appended to daily         */
/*			Version V1  rev 2  8-Apr-96                   */
/**********************************************************************/
#define M09_REFLDB_PROD_ID		"MOD.AM1.brdfsubs.L3"

/*	Global Metadata						*/
#define M09_REFLDB_ANGULAR_SIZE		"ang_size (in arcsec)"

/*	General information on observational basis SDS		*/
#define M09_OBS_INFO			"Obs_Info"

/* Obs_Info_Items #1 land/sea/water indicator
   Obs_Info_Items #2 maximum number of observations in any band
   Obs_Info_Items #3-#9 Number of observations in each band in the
                        16-day period.
  	SDS attribute:                                           */
/* #define M09_OBS_INFO_WORDS		"words"                 */
#define M09_OBS_INFO_WORDS		"Obs_Info_Items"

/*	Viewing and illumination angles	SDS			*/
#define M09_ANGLES			"Angles"

/* Num_Angles #1 view zenith angle
   Num_Angles #2 view azimuth angle
   Num_Angles #3 illumination zenith angle
   Num_Angles #4 illumination azimuth angle
   SDS attributes:                                             */
/* #define M09_ANGLES_OBS		"N_obs_dy"              */
/* #define M09_ANGLES_NUM		"N_angles"              */
#define M09_ANGLES_OBS	       		"Num_Obs_Max"      
#define M09_ANGLES_NUM	         	"Num_Angles"   

/*	Surface reflectances in bands 1- 7 SDS			*/
#define M09_REFLDB_SURF_REFL		"Surface_Refl"

/*	SDS attributes:                                         */
/* #define M09_SURF_REFL_OBS		"N_obs_dy"              */
/* #define M09_SURF_REFL_BANDS		"N_bands"               */
#define M09_SURF_REFL_OBS		"Num_Obs_Max"
#define M09_SURF_REFL_BANDS		"Num_Land_Bands"
#define M09_SURF_REFL_ITEMS		"Obs_Info_Items"


/*	 Quality and weights of the respective observations	*/
#define M09_QUALITY_WEIGHTS		"Weights_QC"            

/*      SDS attributes:                                         */
/* #define M09_QUALITY_OBS		"N_obs_dy"              */
#define M09_QUALITY_OBS	       		"Num_Obs_Max"
#define M09_QUALITY_WORDS      		"Num_Weights_QC"            

/****************************************************************
 *               MOD.AM1.texture_16dy.L3                        *
 *      16-day composited band 2 maximum texture database       *
 *               Version V1 Rev 3   24-Apr-96                   *
*****************************************************************/
#define M09_TEXTURE_16DY_PROD_ID	"MOD.AM1.texture_16dy.L3"

/* SDS Name:       TEXTURE_BAND2

*!Description:    Texture measure, calculated as the standard deviation of
                all 250m band 2 surface reflectances associated with each
                1000m cell, divided by the mean of these 250m reflectances,
                maximum value over 16 days.                                 */
#define M09_TEXTURE_BAND2  "TEXTURE_BAND2"

/* SDS Name:       TEXTURE_QC

*!Description:    Overall quality of the texture measure
                                                                             */
#define M09_TEXTURE_QC "TEXTURE_QC"

/**********************************************************************/
/*		MOD.AM1.bars_16dy.L3                                  */
/*		BRDF-adjusted surface reflectances                    */
/*		Version V1 Rev. 2  8-Apr-1996                         */
/**********************************************************************/

/*              Bars Local Attribute                                  */
/* #define M09BARS_PROD_ID		"MOD09_BARS"                  */
#define M09BARS_PROD_ID	        	"MOD.AMI.bars_16dy.L3"

/*	Nadir-equivalent surface reflectances for MODIS
	bands 1-7 SDS	                                              */
#define M09BARS		        	"BARS"

/*                                                                      
Num_Bands #1 = Nadir-equivalent surface refl. for MODIS band 1
Num_Bands #2 = Nadir-equivalent surface refl. for MODIS band 2
Num_Bands #3 = Nadir-equivalent surface refl. for MODIS band 3
Num_Bands #4 = Nadir-equivalent surface refl. for MODIS band 4
Num_Bands #5 = Nadir-equivalent surface refl. for MODIS band 5
Num_Bands #6 = Nadir-equivalent surface refl. for MODIS band 6
Num_Bands #7 = Nadir-equivalent surface refl. for MODIS band 7
SDS Attribute:                                                       */
#define M09BARS_NUM_BANDS                    "Num_Bands"

/*	Overall quality of the BRDF-adjusted surface reflectances SDS */
#define M09BARS_QC                           "BARS_QC"

/**********************************************************************/
/*		MOD_AM1.brdf_16dy.L3                                  */
/*		16 day BRDF product/Albedo product                    */
/*		Version V1 rev 5 24-Apr-96                            */
/**********************************************************************/
/*#define M09_L3_PROD_ID	       	"MOD09_L3_16DY_G"             */
#define M09_L3_PROD_ID	        	"MOD_AM1.brdf_16dy.L3"

/*	BRDF_Model_ID  SDS                                            */
/*	Identifier for BRDF models chosen SDS                         */
#define M09BRDF_MODEL_ID        	"BRDF_Model_ID"

/**********************************************************************
  BRDF_Model_ID  SDS                                              
  Num_Output_Models #1 is the identifier of the BRDF model
                     chosen as best for each pixel
 Num_Output_Models #2 is the identifier for a globally
                     uniform BRDF model
**********************************************************************/
#define M09BRDFNUM_OUTPUT_MODELS            "BRDF_Num_Output_Models"

/**********************************************************************
  	RMSE for BRDF models chosen SDS	  
**********************************************************************/
#define M09BRDF_MODEL_RMSE      	"BRDF_Model_RMSE"  

/*	BRDF parameters for the seven land bands SDS                 */
/* BRDF parameters SDS attribute data:                              */
#define M09BRDF_PARAMETERS      	"BRDF_Parameters"
#define M09LAND_BANDS	        	"Num_Land_Bands"   
#define M09NUMBER_PARAMETERS    	"Num_BRDF_Parameters"    

/*	Albedo parameters for broadband, < 0.7 mu-m,
		> 0.7 mu-m, and the seven land bands SDS             */
#define M09ALBEDO	        	"Albedo"

/**********************************************************************
 Num_Land_Bands_Plus3  #8 is the less than 0.7 mu-m broadband
 Num_Land_Bands_Plus3  #9 is the more than 0.7 mu-m broadband
 Num_Land_Bands_Plus3 #10 is the total broadband
  Albedo SDS attribute data:
***********************************************************************/
#define M09NUM_LAND_BANDS_PLUS3         "Num_Land_Bands_Plus3"

/**********************************************************************
 Albedo_Type #1 holds white-sky albedo
         Albedo_Type #2 holds black-sky albedo at the mean sun angle
**********************************************************************/
#define M09ALBEDO_TYPE                  "Albedo_type"

/*	BRDF quality control SDS                                     */
/*#define M09QUALITY	        	"QC"                         */
#define M09QUALITY	        	"Quality_Control"

/*********************************************************************
Num_QC_Words #1 gives the information for the best BDRF model
   Num_QC_Words #2 gives the information for the global BDRF model
   Num_QC_Words #3 and #4 give generally applicable information
*********************************************************************/
#define M09NUM_QC_WORDS                 "Num_QC_Words"

/********************************************************************
SDS Name:       Inversion_Info
*!Description:    Provides information on model fitting results
********************************************************************/
#define M09INVERSION_INFO               "Inversion_Info"

/*       N_Info #1 is the rmse of the best model
         N_Info #2 is the rmse of the uniform model
         N_Info #3 is an inferred land surface structural parameter
         N_Info #4 is a descriptor of model fitting patterns
         N_Info #5 is a descriptor TBD
         N_Info #6 is a descriptor TBD
         Inversion_Info  SDS metadata:                           */
#define M09N_INFO                       "N_Info"

/*	A measure of fit from RMSE and sampling of all
		 models tested SDS			*/
#define M09FIT_ASSESS	        	"Fit_Assessments"

/*      The number of columns in the full ISCCP grid for each
        row (line) contained within this L3 file. (obsolete) SDS    */
#define M09NCOL                         "ncol"

/*      The start column in the full ISCCP frid for each row (line)
        contained within this L3 file (starting at zero). (obsolete) SDS */
#define M09ICOL_START                    "icol_start"

/*      The number of columns in each row (line) contained within
        this L3 file. (obsolete) SDS                               */
#define M09NCOL_TILE                     "ncol_tile"

/*      The start pixel of the first valid column in each row (line)
        contained within this L3 file (starting at zero). (obsolete) SDS */
#define M09IPIX_START                    "ipix_start"

/*	SDS Metadata                                         */
/* #define M09LAND_BANDS	      	"land_bands"         */
/* #define M09NUMBER_PARAMETERS    	"number_parameters"  */
#define M09LANDBANDS_BROADBAND_OTHER    "land_bands_and_broadband_and_<>_0.7mu-m"
#define M09N_MODELS	        	"N_models"
#define M09WORDS			"words"
#define M09N_SELECT_MODELS		"N_select_models"

/******************************************************************
 * 			MOD10_L2                                  *
 *			L2 Daily Snow Cover 		          *
 *			Vers V2 Rev 2 03-Oct-1997                 *
******************************************************************/

/* #define M10L2_PROD_ID             "MOD10_l2          "       */
/*#define M10L2_PROD_ID		     "MOD.AM1.V1.snow.L2"       */
 #define M10L2_PROD_ID               "MOD10_L2          "

/* SDS Metadata
  SDS Names:
		Snow Cover
		Snow Cover PixelQA
!Description:   Snow cover extent as identified by the algorithm for every
                pixel in the granule.                                 */

#define M10Snow_Cover                "Snow Cover"
#define M10SNOW_COVER                "Snow Cover"
#define M10SNOW_COVER_PIXELQA	     "Snow Cover PixelQA"

/*               	Global Metadata                        */
#define M10PROCESSED_PIXELS          "Number_of_pixels_processed"
#define M10SNOW_PIXELS         	     "Total_snow_pixels"
#define M10TOTAL_NOT_SNOW_PIXELS     "Total_not_snow_pixels"
#define M10AREA_SNOW                 "Area_snow"
#define M10AREA_NOT_SNOW             "Area_not_snow"
#define M10PERCENT_SNOW    	     "Percentage_snow"
#define M10PERCENT_NOT_SNOW          "Percentage_not_snow"
#define M10NDSI_ABOVE		     "Above_range_NDSI"
#define M10NDSI_BELOW	  	     "Below_range_NDSI"
#define M10ZERO_DIVIDE		     "Division_by_zero"
#define M10OUT_OF_RANGE_INPUT        "Out_of_range_input"
#define M10NO_DECISION               "No_decision"
#define M10SOLAR_ZENITH85            "Solar_zenith>=85"
#define M10CLOUD_OBSCURED            "Cloud_obscured"
#define M10QA_OVERALL                "QA_overall"

/*  L2/L2G Identification of daily snow cover on the land surface SDS */
#define M10DAILY_SNOW		     "daily_snow_cover"

/**********************************************************************
 *                      MOD10L2G                                      * 
 *                      L2G Snow Cover Product                        * 
 *			Vers V2 Rev 3 05-DEC-1997                     * 
 **********************************************************************/
/* #define M10L2G_PROD_ID            "MOD10_L2G"             */
/* #define M10L2G_PROD_ID	     "MOD.AM1.V1.snow.L2G"   */
#define M10L2G_PROD_ID		     "MOD10L2G"

/* SDS Metadata
 * SDS Names :		num_observations
 *			Snow_Cover_1
 *			Snow_Cover_PixelQA_1
 *			Snow_Cover_f
 *			Snow_Cover_PixelQA_f
 *			Snow_Cover_c
 *			Snow_Cover_PixelQA_c
 *			nadd_obs_row
*/
/* number of observations per pixel */
#define M10L2G_NUM_OBSERVATIONS      "num_observations"

/* Snow Covered Land - First Layer */
#define M10L2G_SNOW_COVER_1          "Snow_Cover_1"

/* Snow Covered Land Additional Layers - Full */
#define M10L2G_SNOW_COVER_F          "Snow_Cover_f"

/* Snow Covered Land Additional Layers - Compact */
#define M10L2G_SNOW_COVER_C          "Snow_Cover_c"

/* SDS of spatial QA data */
#define M10L2G_SNOW_COVER_PIXELQA_1  "Snow_Cover_PixelQA_1"
#define M10L2G_SNOW_COVER_PIXELQA_F  "Snow_Cover_PixelQA_f"
#define M10L2G_SNOW_COVER_PIXELQA_C  "Snow_Cover_PixelQA_c"

/* Number of additional observations in each row in the compact form of L2G grid */
#define M10L2G_NADD_OBS_ROW          "nadd_obs_row"


/*                 Global Metadata                  */
#define M10L2G_NUM_OBSERV            MLNUMBER_OF_OBS        
#define M10L2G_COL_GLB_GRID_ROW      MLCOLUMNS_GLOBAL_GRID_ROW
#define M10L2G_NUM_OBSERVS_ROW       "Number of Observations per Row"


/*********************************************************************
 * 			MOD10A1                   	             * 
 *			L3 Daily Snow Cover 		             * 
 *			Vers V2 rev 2 23-Dec-1997                    * 
 *********************************************************************/
/* #define M10L3_PROD_ID	     "MOD.AM1.V1.snow_dy.L3" */
#define M10L3_PROD_ID		     "MOD_PR10A1"

/*  L3 Identification of daily snow cover on the land surface         *
 *  SDS Metadata
 *  SDS Names:                                                   
 *      		Day_Tile_Snow_Cover                          
 *			Snow_Spatial_QA             
*/

#define M10DAY_TILE_SNOW_COVER       "Day_Tile_Snow_Cover"
#define M10SNOW_SPATIAL_QA           "Snow_Spatial_Cover"

/*              Global Metadata                     */
#define M10GRIDDED_SNOW		     "Daily_Gridded_Snow_Cover"
#define M10TILE_AREA                 "tile_area"
#define M10SNOW_COVER_AREA           "snow_cover_area"
#define M10SNOW_COVER_PCT            "snow_cover_pct"
#define M10NO_OBS_AREA               "no_obs_area"
#define M10NO_OBS_AREA_PCT           "no_obs_area_pct"
#define M10OBS_TOTAL                 "obs_total"
#define M10OBS_EXCLUDED              "obs_excluded"
#define M10OBS_USED                  "obs_used"

/**********************************************************************
*			MOD.AM1.LST.L2    		              * 
*			L2 Land Surface Temperature    	              *
*			Version V1 Rev 5 16-Apr-1996	              *
**********************************************************************/
#define M11L2_PROD_ID			"MOD.AM1.V1.lst.L2"

/*	L2/L2G Identification of Land Surface Temperature SDS        */
#define M11SURF_TEMP			"LST"

/*	L2/L2G LST Quality Indicator  SDS. 	                      */
#define M11QUALITY		        "QC"

/*	L2/L2G Error in land surface temperature measurements SDS     */
#define M11ERRORS		   	"Error_LST"

/*	L2/L2G/L3 Band 31 emissivity SDS                              */
#define M11BAND31_EMIS			"Emis_31"

/*	L2/L2G/L3 Band 32 emissivity SDS                              */
#define M11BAND32_EMIS			"Emis_32"

/*      L2/L2G Band 29 or band 20 emissivity SDS                      */
/*#define M11BAND29OR20_EMIS            "Emis_29or20"                 */
#define M11BAND29OR20_EMIS              "Emis_29"

/*  SDS Name:       View_angle
*!Description:    zenith angle of MODIS viewing at the pixel            */
#define M11VIEW_ANGLE                    "View_angle"

/* SDS Name:       View_time
*!Description:    local-sun time of MODIS viewing at the pixel          */
#define M11VIEW_TIME                     "View_time"

/**********************************************************************
 *      		MOD 11-L2G				      *
*			Land Surface Temperature    	              *
*			Beta Version 	              	              *
***********************************************************************/

/*	Product type identifier				             */
#define M11L2G_PROD_ID			"MOD11_L2G"

/**********************************************************************
*                       MOD.AM1.V1.lst_1dy_cmg.L3                     *
*			Daily Land Surface Temperature                *
*			Vers V1 Rev 3 17-Jun-1996                     *
***********************************************************************/
                                                                        
/*	Product type identifier	             	                      */
/* #define M11L3_PROD_ID                         "MOD.AM1.LST.L3.8d"  */
#define M11L3_1DY_PROD_ID		"MOD.AM1.V1.lst_1dy_cmg.L3"

/*	L3 Identification of Land Surface Temperature SDS             */
#define M11L3SURF_TEMP			"LST"

/*	Land surface temperature in view within 45deg SDS             */
#define M11NARROW_LST			"LST_view<45d"

/* Daily daytime mean Land-surface temperature at half
   degree equal angle grids                                           */
#define M11MEAN_LST_DAY                 "Mean_LST_Day"

/* Daily nighttime mean Land-surface temperature at half
   degree equal angle grids                                           */
#define M11MEAN_LST_NIGHT               "Mean_LST_NIGHT"

/*  Standard deviation in daily daytime CMG land surface temperature  */
#define M11STDV_LST_DAY                 "Stdv_LST_Day"

/*  Standard deviation in daily nighttime CMG land surface temperature */
#define M11STDV_LST_NIGHT               "Stdv_LST_Night"

/* Histogram (in percentage) of daily daytime CMG LST Relative to
 Mean_LST_Day                                                         */
#define M11HIST_LST_DAY                 "Hist_LST_Day"

/* Histogram (in percentage) of daily nighttime CMG LST Relative to
 Mean_LST_Night                                                       */
#define M11HIST_LST_NIGHT               "Hist_LST_Night"

/*	L3 LST Quality Indicator  SDS. 	                              */
#define M11L3QUALITY		        "QC"

/*	Land-Surface Temperature Standard Deviation SDS	              */
#define M11STD_DEV		        "Stdv_LST"

/*      L3 Band 29 or band 20 emissivity SDS                          */
#define M11L3BAND29OR20_EMIS            "Emis_29"                    

/*	L3 Band 31 emissivity SDS                                        */
#define	M11L3BAND_EMIS_31              "Emis_31"                

/*	L3 Band 32 emissivity SDS                                        */
#define	M11L3BAND_EMIS_32              "Emis_32"         

/*	 Angular coefficients for Band 29 emissivity SDS              */
#define	M11BAND29_ANG_COEFS		  "Ang_Coef_Emis_29"

/*	 Angular coefficients for Band 31 emissivity SDS              */
#define	M11BAND31_ANG_COEFS		  "Ang_Coef_Emis_31"

/*	 Angular coefficients for Band 32 emissivity SDS              */
#define	M11BAND32_ANG_COEFS		  "Ang_Coef_Emis_32"

/* Percentage of area where LST was retrieved                         */
#define M11PERCENTAGE_AREA_LST_RETRIEVED  "Percentage_Area_LST_Retrieved"

/*  Mean observation time of the daytime LST                          */
#define M11MEAN_DAY_TIME_OF_LST           "Mean_Day_Time_of_LST"

/* Mean observation time of the night LST                             */
#define M11MEAN_NIGHT_TIME_OF_LST         "Mean_Night_Time_of_LST"

/* Center of LST difference bins used in LST Histogram                */
#define M11LST_DIFFERENCE_BINS_IN_HIST_LST "LST_Difference_Bins_In_Hist_LST"

/**********************************************************************
*                       MOD.AM1.V1.lst.8dy_cmg.L3                     *
*			Land Surface Temperature    	              *
*			Vers V1 Rev 3 23-Apr-1996                     *
***********************************************************************/
                                                                        
/*	Product type identifier	             	                      */
#define M11L3_8DY_PROD_ID		"MOD.AM1.V1.lst_8dy_cmg.L3"


/**********************************************************************
*                       MOD.AM1.V1.lst_1m_cmg.L3                      *
*			Land Surface Temperature    	              *
*			Vers V1 Rev 3 23-Apr-1996                     *
***********************************************************************/
                                                                        
/*	Product type identifier	             	                      */
#define M11L3_1M_PROD_ID_1M                 "MOD.AM1.V1.lst_1m_cmg.L3"


/****************************************************************************
 *                   MOD.AM1.V1.lc_1km.L3.3m                                *
 *                   Land Cover Type/Land Cover Change                      *
 *                   Vers V1 rev 6 01-May-1996                              *
 ****************************************************************************/

/*      Product type identifier                                     */
/*#define M12L3_PROD_ID			"MOD12_L3_3MN_D/MOD12_L3_3MN_F"*/
#define M12L3_PROD_ID			"MOD.AM1.V1.lc_1km.L3.3m"

/*      Global Metadata   (obsolete)                                  */
#define M12ANGULAR_SIZE			"ang_size (in arcsec)"

/*      Identification of land cover type, keyed to IGBP-DIS categories  *
 *      SDS:                                                             */
#define M12LAND_COVER		         "Land Cover Type"   

#define M12PERCENTPIXELSPROCCOMPLETELY "Percentage of pixels processed completely"
#define M12PERCENTCHANGEDPREVPRODGENER "Percent changed from previous product generation"
#define M12PROPORTIONEACHCOVERTYPEPRES "Proportion of each cover type present"

/*      Identification of Overall quality of the land cover           *
 *      SDS:                                                          */
/* #define M12QUALITY			"LC_overall_quality_QC"       */
#define M12QUALITY			"Type Overall QC"               

/*      Identification of Number of products generated since        * 
 *      last classification update                                    *
 *      SDS:                                                        */
/*#define M12PRODS_GENERATED	        "LC_num_product_gen"        */
#define M12PRODS_GENERATED	        "Num Product Gen"

/*      Identification of Number of snow months over previous 12 months*
 *      SDS:                                                                */
/*#define M12SNOW_MONTHS	        "LC_snow_months"           */
#define M12SNOW_MONTHS		        "Snow Months"

/*      Identification of Number of BRDFs used for classification     * 
 *      that have been derived within the pass 12 months              *
 *      SDS:                                                          */
/*#define M12BRDFS_USED		         "LC_num_BRDF"                      */
#define M12BRDFS_USED		         "Num BRDF"

/*      Identification of Confidence in BRDF/reflectance correction   *
 *      SDS:                                                          */
/*#define M12BRDF_STOCK		         "LC_BRDF_confidence"         */
#define M12BRDF_STOCK		         "BRDF Internal QC"

/*      Identification of Number of LST values used for classification *
 *      SDS:                                                           */
/*#define M12LST_VALS_USED	         "LC_num_LST"                */
#define M12LST_VALS_USED	         "Num LST"                          

/*      Identification of Confidence in VI over 12 months            *
 *      SDS:                                                         */
/*#define M12VI_STOCK		         "LC_VI_confidence"          */
#define M12VI_STOCK		         "VI Internal QC"                     

/*      Identification of TBD quality control for land cover type     *
 *      SDS                                                           */
#define M12QUALITY1		        "Land_cover_TBD_1"           

/*      Identification of TBD quality control for land cover type     *
 *      SDS:                                                          */
#define M12QUALITY2		       "Land_cover_TBD_2"           

/*      Identification of Land cover change                           *
 *      SDS:                                                          */
/*#define M12LAND_COVER_CHANGE	        "Land_cover_change"          */
#define M12LAND_COVER_CHANGE	        "Land Cover Change"

/*      Identification of Quality control for land cover change
        SDS:                                                          */
/*#define M12CHANGE_QUALITY	       "Land_cover_change_QC"        */
#define M12CHANGE_QUALITY	       "Land Cover Change QC"

/**********************************************************************
 *                   MOD.AM1.V1.landcov_32dy.L3                       *
 *                   Land Cover Monthly Database                      *
 *                   Vers V1 rev 8 29-May-1996                        *
 **********************************************************************/
/* #define M12L31M_PROD_ID              "MOD.AM1.V1.lc.L3.1m"  */
#define M12L31M_PROD_ID	                 "MOD.AM1.V1.landcov_32dy.L3"

/*                   E-Product-S Metadata                             *
 *                                                                    */
#define M12ECOREGION_LABEL               "Ecoregion label(s)"
#define M12LAND_CVR_CLS_PRESENT_TILE     "Land cover classes present in tile"

/*      Description:    Land/water flag                              *
 *      SDS:                                                         */
#define M12LW                                "LW"                     

/*      Description: Standard deviation divided by mean of 250m Band 2 pixels *
 *      in a L3 1-km grid cell, composited monthly                    *
 *      SDS:                                                                  */
#define M12TEXT                              "Text"                     

/*      Description: Internal quality control flag for texture        *
 *      SDS:                                                          */
#define M12TEXT_QC                           "Text QC"                     

/*      Description: BRDF-adjusted reflectances, composited monthly   *
 *      SDS:                                                          */
#define M12BAR                               "BAR"                     

/*      metadata product:                                             */
#define M12BAR_BANDS                          "bar_bands"

/*      Description: Internal quality control flag for BAR           *
 *      SDS:                                                         */
#define M12BAR_QC                            "BAR QC"                     

/* Description: Best and worst monthly BRDF models (lowest and highest   *
 *                   RMSE's)                                  *
 *      SDS:                                                          */
#define M12BRDF                              "BRDF"                     

/*      metadata product:                                             */
#define M12BRDF_MODELS_KEPT                  "models_kept"

/*      Description: Internal quality control flag for BRDF           *
 *                                                                    *
 *      SDS:                                                          */
#define M12BRDF_QC                            "BRDF QC"                     

/*    SDS Name:       VI                                             *
*!Description:    Monthly composite of VI                         *
 *    SDS:                                                            */
#define M12VI                                 "VI"                     

/*    SDS Name:       VI QC                                           *
*!Description: Internal quality control flag for VI               *
 *    SDS:                                                            */
#define M12VI_QC                                "VI QC"                     

/*    SDS Name:       Snow                                            *
*!Description:    Monthly composite of snow cover                 *
 *    SDS:                                                            */
#define M12SNOW                              "Snow"                     


/*    SDS Name:       Snow QC                                         *
*!Description:    Internal quality control flag for snow          *
 *    SDS:                                                            */
#define M12SNOW_QC                              "Snow QC"                     

/*    SDS Name:       LST                                             *
*!Description:    Monthly composite of land surface temperature   *
 *    SDS:                                                            */
#define M12LST                                  "LST"                     

/*    SDS Name:       LST QC                                          *
*!Description:    Internal quality control flag for LST           *
 *    SDS:                                                            */
#define M12LST_QC                               "LST QC"                

/*********************************************************************
 *                   MOD.AM1.lc_halfdegree.L3.3m                       *
 *                   Land Cover Type/Land Cover Change at half-degree  *
 *                   spatial resolution                                *
 *                   Vers V1 rev 6 01-May-1996                         *
 ***********************************************************************/
#define M12CMG_PROD_ID	                 "MOD.AM1.lc_halfdegree.L3.3m"

/*  SDS Name:       Land Cover Type H-deg                              *
*!Description:    Land cover type, half-degree resoluton, keyed to IGBP-DIS *
 *  categories                                                                *
 *      SDS:                                                                  */
#define M12LAND_COVER_TYPE_HDEG            "Land Cover Type H-deg"                     

#define M12COVERTYPESPRESENTDGRADCELL "Cover types present in each degraded cell"
#define M12PROPCOVERTYEPRESENTDGRADCELL "Proportion of each cover type present in each degraded cell"

/*  SDS Name:       Type Overall QC H-deg                              *
*!Description:    Overall quality of the land cover classification,  *
 *                  half-degree resolution                             *
 *      SDS:                                                           */
#define M12TYPE_OVERALL_QC_HDEG            "Type Overall QC H-deg"       

/*  SDS Name:       Num Product Gen H-deg                              *
*!Description:    Number of products generated since last classification    *
 *                  update,                                           *
 *      SDS:                                                          */
#define M12NUM_PRODUCT_GEN_HDEG             "Num Product Gen H-deg"

/*      SDS Name:       Snow Months H-deg                             *
*!Description:    Number of snow months over previous 12 months,*
 *                      half-degree                                   *
 *      SDS:                                                          */
#define M12SNOW_MONTHS_HDEG                  "Snow Months H-deg"                  
/*      SDS Name:       Num BRDF H-deg                                *
*!Description:    Number of BRDFs used for classification that  *
 *      have been derived within the past 12 months, half-degree resolution   *
 *      SDS:                                                          */
#define M12NUM_BRDF_HDEG                     "Num BRDF H-deg"                    

/*      SDS Name:       BRDF Internal QC H-deg                       *
*!Description:    Internal quality flag for BRDF/reflectance correction,*
 *                      half-degree resolution                       *
 *      SDS:                                                         */
#define M12BRDF_INTERNAL_QC_HDEG              "BRDF Internal QC H-deg"                     

/*      SDS Name:       Num LST H-deg                                  *
*!Description:    Number of LST values used for classification   *
 *                      over last 12 months, half-degree resolution    *
 *      SDS:                                                          */
#define M12NUM_LST_HDEG                       "Num LST H-deg"                     
/* SDS Name:       VI Internal QC H-deg                               *
*!Description:    Internal quality flag for VI over past 12 months,  *
 *              half-degree resolution                                */
#define M12VI_INTERNAL_QC_HDEG                  "VI Internal QC H-deg"

/*  SDS Name:       Land Cover Change H-deg                           *
*!Description:    Land cover change, half-degree resolution         */
#define M12LAND_COVER_CHANGE_HDEG            "Land Cover Change H-deg"

/*      SDS Name:       Land Cover Change QC H-deg                   *
*!Description:    Quality control for land cover change,        *
 *                      half-degree resolution                        */
#define M12LAND_COVER_CHANGE_QC_HDEG      "Land Cover Change QC H-deg"         

/**********************************************************************
 *                   MOD 14_L2                                        *
 *                   L2 Thermal Anomaly/Fire Products                 *
 *                   Vers V1		02-May-1996                   *
 **********************************************************************/

/*	Product type identifier                                       */
#define M14L2_PROD_ID			"MOD14_L2"

/*	L2/L2G Identification of fire on the land surface SDS         */
#define M14LAND_FIRE			"fire_mask"

/*	L2/L2G/L3 Total emmitted energy detected SDS. 	              */
/*#define M14ENERGY			"Energy"                      */
#define M14ENERGY			"power"

/*	L2/L2G/L3 Class of fire detected SDS		              */
#define M14FIRE_CLASS			"smold"

/*	Fire quality control SDS			              */
/*#define M14QUALITY			"Fire_QC"             */
#define M14QUALITY		    	"fire_qc"

/*********************************************************************
*		MOD.AM1.V1.fire.L2G                                  *
*		L2G Thermal Anomaly/Fire Products                    *
*               Vers V1 Rev  2   8-May-1996                          *
**********************************************************************/

/*	Product type identifier		                             */
/*#define M14L2G_PROD_ID			"MOD14_L2G"          */
#define M14L2G_PROD_ID  			"MOD.AM1.V1.fire.L2G"

/*	Fire quality control SDS			               */
#define M14L2GQUALITY		    	"fire_qc"

/***********************************************************************
*               MOD 14-L3                                              *    	
*		L3 Thermal Anomaly/Fire Products	               *
*		Beta		11/28/95		               *
 ***********************************************************************/

/*	Product type identifier                                        */
#define M14L3_PROD_ID			"MOD14_L3"

/***********************************************************************
*               MOD15 L3                                               *
*               Tile 1-KM Gridded PRODUCTS: FPAR, LAI                  *
*               V1              09/09/1996                             *
 ***********************************************************************/

/*      Product type identifier                                        */
#define M15L3_PROD_ID                   "MOD.AM1.V1.fpar_lai_8dy.L3"

/*      Global Metadata for Product                                    */
#define M15HIGHEST_PIX_COMPLETED        "highest_pix_completed"
#define M15TOTAL_PIXELS_PER_TILE        "total_pixels_per_tile"
#define M15NTH_RESTART                  "nth_restart"
#define M15PCT_TILE_COMPLETED           "pct_tile_completed"
#define M15PCT_CALC_BY_LUT              "pct_calc_by_lut"
#define M15PCT_CALC_BY_EMPIRICAL        "pct_calc_by_empirical"

/* Tile 1-KM Gridded PRODUCTS: FPAR, LAI. SDS                                  */

/* SDS Name:       FPAR_1K
!Description:    Fraction of absorbed photosynthetically active radiation
                at 1 KM and 8 day resolution, Parameter 5367.                   */
#define M15FPAR_1K                         "FPAR_1K"

/* SDS Name:       FPAR_1K_QC
!Description:    Root mean squared error (RMSE) of fraction of absorbed
                photosynthetically active radiation.                            */
#define M15FPAR_1K_QC                      "FPAR_1K_QC"

/* SDS Name:       LAI_1K
!Description:    Leaf area index at 1 km resolution, 8 day composite             */
#define M15LAI_1K                         "LAI_1K"

/* SDS Name:       LAI_1K_QC
!Description:    Quality control for leaf area index.                            */
#define M15LAI_1K_QC                      "LAI_1K_QC"

/* SDS Name:       Columns per Global Grid Row
!Description:    The number of valid data columns in the MODLAND
                integerized sinusoidal grid for each global grid row
                contained this tile                                                 */
#define M15COL_GLOB_GRD_RW                "Columns per Global Grid Row"

/* Dimension Names:                                                                 */
#define M15DATA_ROWS                      "Data Rows"
#define M15DATA_COLUMNS                   "Data Columns"
#define M15GLOB_GRD_RWS                   "Global Grid Rows"

/************************************************************************************
*               MOD15 L3                                                            *
*               Tile 25-KM Climate Modeling Grid PRODUCTS: FPAR, LAI                *
*               V1              09/09/1996                                          *
*************************************************************************************/

/*      Product type identifier                                                     */
#define M15L3_25KM_PROD_ID                   "MOD.AM1.V1.fpar_lai_8dy_cmg.L3"

/*      Global Metadata for Product                                                 */
#define M15HIGHEST_PIX_COMPLETED        "highest_pix_completed"
#define M15TOTAL_PIXELS_PER_TILE        "total_pixels_per_tile"
#define M15NTH_RESTART                  "nth_restart"
#define M15PCT_CALC_BY_LUT              "pct_calc_by_lut"
#define M15PCT_CALC_BY_EMPIRICAL        "pct_calc_by_empirical"

/* Tile 1-KM Gridded PRODUCTS: FPAR, LAI. SDS                                       */

/* SDS Name:       FPAR_25K
!Description:    Fraction of absorbed photosynthetically active radiation
                at 1 KM and 8 day resolution, Parameter 5367.                   */
#define M15FPAR_25K                         "FPAR_25K"

/* SDS Name:       FPAR_25K_QC
!Description:    Root mean squared error (RMSE) of fraction of absorbed
                photosynthetically active radiation.                            */
#define M15FPAR_25K_QC                         "FPAR_25K_QC"

/* SDS Name:       LAI_25K
!Description:    Leaf area index at 1 km resolution, 8 day composite             */
#define M15LAI_25K                         "LAI_25K"

/* SDS Name:       LAI_25K_QC
!Description:    Quality control for leaf area index.                            */
#define M15LAI_25K_QC                      "LAI_25K_QC"

/* SDS Name:       Columns per Global Grid Row
!Description:    The number of valid data columns in the MODLAND
                integerized sinusoidal grid for each global grid row
                contained this tile                                             */
#define M15COL_GLOB_GRD_RW                "Columns per Global Grid Row"

/* Dimension Names:                                                             */
#define M15DATA_ROWS                      "Data Rows"
#define M15DATA_COLUMNS                   "Data Columns"
#define M15GLOB_GRD_RWS                   "Global Grid Rows"

/********************************************************************************
*               MOD 17 L4                                                       *
*               Annual 1-KM Gridded PRODUCTS: NPP                               *
*               V1              09/09/1996                                      *
*********************************************************************************/

/*      Product type identifier                                                 */
#define M17L4_PROD_ID                   "MOD.AM1.V1.npp_1yr.L4"

/*      Global Metadata for Product                                             */
#define M17HIGHEST_PIX_COMPLETED        "highest_pix_completed"
#define M17TOTAL_PIXELS_PER_TILE        "total_pixels_per_tile"
#define M17NTH_RESTART                  "nth_restart"

/* Tile 1-KM Gridded PRODUCTS: NPP                                              */

/* SDS Name:       NPP_1K
!Description:    Net primary productivity at 1 KM, annual composite.             */
#define M17NPP_1K                         "NPP_1K"

/* SDS Name:       NPP_1K_QC
!Description:    Quality control for annual composited NPP 1k                    */
#define M17NPP_1K_QC                         "NPP_1K_QC"

/* SDS Name:       Columns per Global Grid Row
!Description:    The number of valid data columns in the MODLAND
                integerized sinusoidal grid for each global grid row
                contained this tile                                              */
#define M17COL_GLOB_GRD_RW                "Columns per Global Grid Row"

/* Dimension Names:                                                             */
#define M17DATA_ROWS                      "Data Rows"
#define M17DATA_COLUMNS                   "Data Columns"
#define M17GlOB_GRD_RWS                   "Global Grid Rows"

/********************************************************************************
*               MOD 17 L4                                                       *
*               Annual 25 KM Climate Modeling Grid PRODUCT: NPP                 *
*               V1              09/09/1996                                      *
*********************************************************************************/

/*      Product type identifier                                                 */
#define M17_25K_PROD_ID                   "MOD.AM1.V1.npp_1yr_cmg.L4"

/*      Global Metadata for Product                                             */
#define M17_25K_HIGHEST_PIX_COMPLETED        "highest_pix_completed"
#define M17_25K_TOTAL_PIXELS_PER_TILE        "total_pixels_per_tile"
#define M17_25K_NTH_RESTART                  "nth_restart"

/* Tile 25-KM Gridded PRODUCTS: NPP                                              */

/* SDS Name:       NPP_25K
!Description: Net primary productivity at 25 KM, annual composite, for
                the climate modeling grid, Parameter 2703.                       */
#define M17NPP_25K                            "NPP_25K"

/* SDS Name:       NPP_25K_QC
!Description:   Quality control for annual composited NPP 25 KM for the climate modeling grid   */
#define M17NPP_25K_QC                         "NPP_25K_QC"

/* SDS Name:       Columns per Global Grid Row
!Description:    The number of valid data columns in the MODLAND
                integerized sinusoidal grid for each global grid row
                contained this tile                                              */
#define M17_25K_COL_GLB_GRD                "Columns per Global Grid Row"

/* Dimension Names:                                                             */
#define M17_25K_DATA_ROWS                      "Data Rows"
#define M17_25K_DATA_COLUMNS                   "Data Columns"
#define M17_25K_GlOB_GRD_RWS                   "Global Grid Rows"

/********************************************************************************
*               MOD 17 L4                                                       *
*               Tile 1-KM Gridded PRODUCTS: PSN                                 *
*               V1              09/09/1996                                      *
*********************************************************************************/

/*      Product type identifier                                                 */
#define M17_PSN_1KM_PROD_ID                   "MOD.AM1.V1.psn_8dy.L4"

/*      Global Metadata for Product                                             */
#define M17_1K_PSN_HIGHEST_PIX_CPLT        "highest_pix_completed"
#define M17_1K_PSN_TOTAL_PIX_TILE          "total_pixels_per_tile"
#define M17_1K_PSN_NTH_RESTART             "nth_restart"

/*  8 day composite PSN at 1 KM, Parameter 3716                                 */

/* SDS Name:  PSN_1K    K 
!Description: Photosynthesis (gross primary production),
                at 1KM, 8 day composite, Parameter 3716.                        */
#define M17PSN_1K                            "PSN_1K"

/* SDS Name:  PSN_1K_QC    
!Description:   Quality control for 8-day composited PSN                         */
#define M17PSN_1K_QC                         "PSN_1K_QC"

/* SDS Name:       Columns per Global Grid Row
!Description:    The number of valid data columns in the MODLAND
                integerized sinusoidal grid for each global grid row
                contained this tile                                              */
#define M17_PSN_1K_COL_GRD                "Columns per Global Grid Row"

/* Dimension Names:                                                             */
#define M17_PSN_1K_DATA_ROWS                      "Data Rows"
#define M17_PSN_1K_DATA_COLUMNS                   "Data Columns"
#define M17_PSN_1K_GlOB_GRD_RWS                   "Global Grid Rows"

/********************************************************************************
*               MOD 17 L4                                                       *
*               Tile 25-KM Climate Modeling Grid PRODUCT: PSN                   *
*               V1              09/09/1996                                      *
*********************************************************************************/

/*      Product type identifier                                                 */
#define M17_PSN_25KM_PROD_ID                   "MOD.AM1.V1.psn_8dy_cmg.L4"

/*      Global Metadata for Product                                             */
#define M17_PSN_25K_HGH_PIX_CPLTD        "highest_pix_completed"
#define M17_PSN_25K_PXLS_TILE        "total_pixels_per_tile"
#define M17_PSN_25K_NTH_RESTART                  "nth_restart"

/*  8 day composite PSN at 25 KM, Parameter 3716
    for the climate modeling grid.                                              */

/* SDS Name:  PSN_25K     
!Description: Photosynthesis (gross primary production), at 25KM, 8 day composite,
             Parameter 3716.                        */
#define M17PSN_25K                            "PSN_25K"

/* SDS Name:  PSN_25K_QC    
!Description:   Quality control for 8-day composited PSN                          */
#define M17PSN_25K_QC                         "PSN_25K_QC"

/* SDS Name:       Columns per Global Grid Row
!Description:    The number of valid data columns in the MODLAND
                integerized sinusoidal grid for each global grid row
                contained this tile                                              */
#define M17_PSN_25K_COL_GLB                "Columns per Global Grid Row"

/* Dimension Names:                                                             */
#define M17_PSN_25K_DATA_ROWS                      "Data Rows"
#define M17_PSN_25K_DATA_COLUMNS                   "Data Columns"
#define M17_PSN_25K_GLOB_GRD_RWS                   "Global Grid Rows"

/**********************************************************************************
*                        MOD.AM1.V1.seaice_max.L2                                 * 
*		         L2 Daily Sea-Ice Cover                                   *
*                        Version V1 rev. 7 24-April-1996                          *
***********************************************************************************/

/*	Product type identifier                                        */
/*#define M29L2_PROD_ID			"MOD29_L2"                     */
#define M29L2_PROD_ID			"MOD.AM1.V1.seaice_max.L2"

/*	Global Metadata                                                */
#define M29SEA_ICE_PIXELS               "Total_sea_ice_pixels"
#define M29SEA_ICE_PERCENT              "Percentage_sea_ice"
#define M29NOT_SEA_ICE_PERCENT          "Percentage_not_sea_ice"
#define M29NDSI_ABOVE	                "Above_range_NDSI"
#define M29NDSI_BELOW	                "Below_range_NDSI"
#define M29ZERO_DIVIDE	                "Division_by_zero"
#define M29OUT_OF_RANGE	                "Out_of_range_input"
#define M29NO_DECISION	                "No_decision"

/*	Identification of daily sea ice cover SDS                     */
#define M29DAILY_SEA_ICE	        "daily_sea_ice_cover"

/***********************************************************************
*                          MOD 29-L2G                                  * 
*		           L2 Sea Ice Extent      		       *
*                          Beta Version                                *
************************************************************************/

/*	Product type identifier					*/
#define M29L2G_PROD_ID			"MOD29_L2G"

/*	Daily Ice Cover SDS					*/
#define M29L2GDAILY_SEA_ICE		"daily_ice_cover"

/***********************************************************************
*                        MOD.AM1.seaice_max_dy.L3                      *
*		         L3 Daily Sea-Ice Cover       		       *
*                        Version V1 Rev, 5 24-April-1996               *
***********************************************************************/

/*	Product type identifier                                        */
/* #define M29L3_PROD_ID                "MOD29_L3_DY_G"                */
#define M29L3_PROD_ID			"MOD.AM1.seaice_max_dy.L3"

/*	Identification of daily sea ice cover SDS */
#define M29L3DAILY_SEA_ICE	       "daily_gridded_sea_ice_cover" 


/*********************************************************************
*                  MOD.AM1.V1.snow_10dy.L3                           * 
*                  Ten day composited snow cover product             *
*                  Version V1 Rev. 6          24-April-1996          *
**********************************************************************/
/*  Product type identifier                                          */
#define M33L3_PROD_ID			"MOD.AM1.V1.snow_10dy.L3"

/*                                                                   *
 * SDS Name: Composite_Snow_Cover                                    *
*!Description: Product is a result of compositing snow cover         *
 * for ten days of MOD10L3 products for every cell in the tile.      *
 * Data represents the number of days that snow was identified       *
 * in a cell.                                                        *
 *	SDS:                                                         */
#define M33WEEKLY_SNOW		        "Composite_Snow_Cover"

/* SDS Metadata                                                      */

#define M33TILE_AREA                    "tile_area"
#define M33ZERO_DAY_COVER               "zero_day_cover"
#define M33ONE_DAY_COVER                "one_day_cover"               
#define M33TWO_DAY_COVER                "two_day_cover"                 
#define M33THREE_DAY_COVER              "three_day_cover"             
#define M33FOUR_DAY_COVER               "four_day_cover"                
#define M33FIVE_DAY_COVER               "five_day_cover"                
#define M33SIX_DAY_COVER                "six_day_cover"                 
#define M33SEVEN_DAY_COVER              "seven_day_cover"               
#define M33EIGHT_DAY_COVER              "eight_day_cover"               
#define M33NINE_DAY_COVER               "nine_day_cover"               
#define M33TEN_DAY_COVER                "ten_day_cover"                 
#define M33ELEVEN_DAY_COVER             "eleven_day_cover"              
#define M33MAX_SNOW_COVER_AREA          "max_snow_cover_area"           
#define M33MAX_SNOW_COVER_PCT           "max_snow_cover_pct"
#define M33TEN_DAY_MEAN                 "ten_day_mean"

/*	SDS:                                                         */
#define M33MEAN_SNOW_COVER             "Mean_Snow_Cover"
 
#define M33MEAN_TEN_DAY_MEAN            "mean_ten_day_mean"  
#define M33QA_OVERALL                   "QA_overall"

/**********************************************************************
*                        MOD.AM1.vi_1m.L3                       *
*                        Monthly vegetation indices                   *
*                        Version V1 rev. 6 06-May-1996              *
*********************************************************************/

/*      Product type identifier                                      */
/*  #define M34L3_PROD_ID                      "MOD.AM1.vi_250m.L3.1m" */
#define M34L3_PROD_ID                      "MOD.AM1.vi_1m.L3"

/*      NDVI SDS */
/* #define M34NDVI                         "ndvi"                     */
#define M34NDVI                         "NDVI_250_M"

/*      MVI SDS */
/* #define M34MVI                          "mvi"                      */
#define M34MVI                             "MVI_250_M"

/*      View zenith angles for NDVI SDS */
#define M34NDVI_ZENITH_ANGLES              "VZ_NDVI"

/*      View zenith angles for MVI SDS */
#define M34MVI_ZENITH_ANGLES               "VZ_MVI"

/*      Quality control for NDVI SDS */
/* #define M34NDVI_QUALITY                    "QC_NDVI"              */
#define M34NDVI_QUALITY                    "NDVI_250_M_QC"

/*      Quality control for MVI SDS */
/*  #define M34MVI_QUALITY                     "QC_MVI"               */
 
#define M34MVI_QUALITY                     "MVI_250_M_QC"

/********************************************************************* 
*                       MOD.AM1.V1.vi_8dy.L3                         *
*                       8 day composite NDVI and MVI @ 250m          *
*                       Version V1 Rev 6 06-May-1996                 * 
**********************************************************************/
/*	Product type identifier	                                            */
/* #define M34L3_10DY_250M_PROD_ID	"MOD.AM1.V1.vi_250m.L3.10d"  */
#define M34L3_10DY_250M_PROD_ID	    	"MOD.AM1.V1.vi_8dy.L3"

/* SDS Name:      NDVI_250_10DY
B
*!Description:   NDVI_250_10DY                                        */
/* #define M34NDVI_250_10DY             "NDVI_250_10DY"               */
#define M34NDVI_250_10DY                "NDVI_250_8DY"

/* SDS Name:      MVI_250_10DY
*!Description:   MVI_250_10DY                                          */
/* #define M34MVI_250_10DY                 "MVI_250_10DY              */
#define M34MVI_250_10DY                 "MVI_250_8DY"

/* SDS Name:      NDVI_250_10day_QC
*!Description:   Quality control for NDVI                                  */
/*#define M34NDVI_250_10DAY_QC              "NDVI_250_10day_QC"       */
#define M34NDVI_250_10DAY_QC              "NDVI_250_8DY_QC"

/* SDS Name:      MVI_250_10DY_QC
*!Description:   Quality control for MVI                                   */
/* #define M34MVI_250_10DY_QC               "MVI_250_10DY_QC"         */
#define M34MVI_250_10DY_QC               "MVI_250_8DY_QC"

/********************************************************************* 
*                       MOD.AM1.vi_16dy.L3                           *
*                       16 day composite NDVI and MVI @ 250 m        *
*                       Version V1 Rev 6 06-May-1996                 * 
**********************************************************************/
/*	Product type identifier	                                         */
/*#define M34L3_16DY_250M_PROD_ID	"MOD.AM1.vi_250m.L3.16d"     */
#define M34L3_16DY_250M_PROD_ID		"MOD.AM1.vi_16dy.L3"

/* SDS Name:      NDVI_250_16DY
*!Description:   NDVI_250_16DY                                       */
#define M34NDVI_250_16DY                 "NDVI_250_16DY"

/* SDS Name:      MVI_250_16DY
*!Description:   MVI_250_16DY                                        */
#define M34MVI_250_16DY                  "MVI_250_16DY"

/* SDS Name:      NDVI_250_16day_QC
*!Description:   Quality control for NDVI                            */
/* #define M34NDVI_250_16DAY_QC              "NDVI_250_16day_QC"     */
#define M34NDVI_250_16DAY_QC              "NDVI_250_16DY_QC"

/* SDS Name:      MVI_250_16DY_QC
*!Description:   Quality control for MVI                             */
#define M34MVI_250_16DY_QC               "MVI_250_16DY_QC"

/********************************************************************** 
*                       MOD.AM1.vi_8dy_cmg.L3	                      *
*                       8 day composite NDVI and MVI @ 25 km         *
*                       Version V1 Rev 6 06-May-1996                  * 
***********************************************************************/
/*	Product type identifier	                                          */
/* #define M34L3_10DY_25KM_PROD_ID      "MOD.AM1.vi_25km.L3.10d"  */
#define M34L3_10DY_25KM_PROD_ID	      "MOD.AM1.vi_8dy_cmg.L3"

/* SDS Name:      NDVI_25_10DY
*!Description:   NDVI_25_10DY                                         */
/* #define M34NDVI_25_10DY            "NDVI_25_10DY"                */
#define M34NDVI_25_10DY            "NDVI_25_8DY"

/* SDS Name:      MVI_25_10DY
*!Description:   MVI_25_10DY                                          */
/* #define M34MVI_25_10DY                 "MVI_25_10DY"             */
#define M34MVI_25_10DY                 "MVI_25_8DY"

/* SDS Name:      NDVI_25_10day_QC
*!Description:   Quality control for NDVI                              */
/*#define M34NDVI_25_10DAY_QC           "NDVI_25_10Day_QC"            */
#define M34NDVI_25_10DAY_QC           "NDVI_25_8DY_QC"

/* SDS Name:      MVI_25_10DY_QC
*!Description:   Quality control for MVI                                   */
/*#define M34MVI_25_10DY_QC             "MVI_25_10DY_QC"            */
#define M34MVI_25_10DY_QC             "MVI_25_8DY_QC"

/********************************************************************** 
*                       MOD.AM1.vi_16dy_cmg.L3                        *
*                       16 day composite NDVI and MVI @ 25 km         *
*                       Version V1 Rev 5 24-April-1996                * 
***********************************************************************/
/*	Product type identifier	                                          */
/* #define M34L3_16DY_25KM_PROD_ID       "MOD.AM1.vi_25km.L3.16d"    */
#define M34L3_16DY_25KM_PROD_ID          "MOD.AM1.vi_16dy_cmg.L3"

/* SDS Name:      NDVI_25_16DY
*!Description:   NDVI_25_16DY                                             */
#define M34NDVI_25_16DY                  "NDVI_25_16DY"

/* SDS Name:      MVI_25_16DY
*!Description:   MVI_25_16DY                                              */
#define M34MVI_25_16DY                   "MVI_25_16DY"

/* SDS Name:      NDVI_25_16day_QC
*!Description:   Quality control for NDVI                                  */
/* #define M34NDVI_25_16DAY_QC            "NDVI_25_16day_QC"            */
#define M34NDVI_25_16DAY_QC               "NDVI_25_16DY_QC"

/* SDS Name:      MVI_25_16DY_QC
*!Description:   Quality control for MVI                                   */
#define M34MVI_25_16DY_QC                 "MVI_25_16DY_QC"

/*********************************************************************** 
*                       MOD.AM1.vi_1m_cmg.L3                           *
*                       monthly composite NDVI and MVI @ 25 km         *
*                       Version V1 Rev 6 06-May-1996                   * 
************************************************************************/

/*	Product type identifier	                                           */
#define M34L3_MN_25KM_PROD_ID             "MOD.AM1.vi_1m_cmg.L3"

/* SDS Name:      NDVI_25_M
*!Description:   NDVI_25_M                                             */
#define M34NDVI_25_M                      "NDVI_25_M"

/* SDS Name:      MVI_25_M
*!Description:   MVI_25_M                                              */
#define M34MVI_25_M                       "MVI_25_M"

/* SDS Name:      NDVI_25_M_QC
*!Description:   Quality control for NDVI                               */
#define M34NDVI_25_M_QC                   "NDVI_25_M_QC"

/* SDS Name:      MVI_25_M_QC
*!Description:   Quality control for MVI                               */
#define M34MVI_25_M_QC                    "MVI_25_M_QC"

/*****************************************************************************
*                 MOD.AM1.V1.seaice_10dy.L3                             * 
*                 Ten day composited  ice cover product.                     *
*                 Version V1 Rev. 6 24-April-1996                            *
******************************************************************************/

/*	Product type identifier				                     */
#define M42L3_PROD_ID			"MOD.AM1.V1.seaice_10dy.L3"

/* SDS Name:    Composite_Ice_Cover                                          *
*!Description: Product is a result of compositing ice cover for ten days of *
 *        MOD10L3 products for every cell in the tile.  Data represents      *
 *        the number of days that ice was identified in a cell.              */
/*#define M42WEEKLY_SEA_ICE		"weekly_ice_cover"                   */
#define M42WEEKLY_SEA_ICE		"Composite_Ice_Cover"

/* metadata:                                                                 */
#define M42TILE_AREA                    "tile_area"
#define M42ZERO_DAY_COVER               "zero_day_cover"
#define M42ONE_DAY_COVER                "one_day_cover"               
#define M42TWO_DAY_COVER                "two_day_cover"                 
#define M42THREE_DAY_COVER              "three_day_cover"               
#define M42FOUR_DAY_COVER               "four_day_cover"              
#define M42FIVE_DAY_COVER               "five_day_cover"                
#define M42SIX_DAY_COVER                "six_day_cover"                 
#define M42SEVEN_DAY_COVER              "seven_day_cover"               
#define M42EIGHT_DAY_COVER              "eight_day_cover"              
#define M42NINE_DAY_COVER               "nine_day_cover"               
#define M42TEN_DAY_COVER                "ten_day_cover"                 
#define M42ELEVEN_DAY_COVER             "eleven_day_cover"              
#define M42MAX_SNOW_COVER_AREA          "max_snow_cover_area"           
#define M42MAX_SNOW_COVER_PCT           "max_snow_cover_pct"
#define M42TEN_DAY_MEAN                 "ten_day_mean"
#define M42QA_OVERALL                   "QA_overall"
#define M42MAX_ICE_COVER_AREA           "max_ice_cover_area"
#define M42MAX_ICE_COVER_PCT            "max_ice_cover_pct"

/**********************************************************************
SDS Name:    Mean_Ice_Cover
*!Description: Product is a result of compositing ice cover for ten days of
          MOD10L3 products for every cell in the tile. Data represents
          the mean number of days that ice was identified in a cell.
**********************************************************************/
#define M42MEAN_ICE_COVER               "Mean_Ice_Cover"

/* metadata:                                                          */
#define M42ICE_COVER_AREA               "ice_cover_area"
#define M42ICE_COVER_PCT                "ice_cover_pct"

#endif
