/************************************************************************
*               mapiocean.h utilities header file                       *
*									*
*                           MODOCEAN				        * 
*                                                                       *
*               HDF Object Identifier Constants                         *
*************************************************************************/

/************************************************************************
* !C-INC
*
* !Purpose: Utilities header file containing constants for MODIS 
*           OCEANS discipline HDF object names.
*
* !Description:  
*
*    The Header file mapiocean.h is part of a larger software system 
*    called the MODIS Application Program Interface (API) Utility, 
*    abbreviated M-API.  The M-API Utility consists of subroutines which
*    allow MODIS Science Team supplied software to read and write data 
*    and metadata from/to HDF files.  The functionality of the M-API is 
*    defined in the MODIS API Specification.
*
*    The mapiatmos.h file contains macros for the the specific names of
*    data objects contained in the following MODIS ocean file specifications:
*
*           MOD_PR27 Level 4
*           miami_oc_qc_L2.filespec
*           miami_sst_L2.filespec
*           miami_sst_qc_L2.filespec
*           miami_dr1_L2.filespec
*           miami_dr2_L2.filespec
*           miami_lw_L2.filespec
*
* !Input Parameters:     None
*
* !Output Parameters:    None
*
* !Revision History:
*   $Log: mapiocean.h,v $
*   Revision 1.7  2001/11/02 20:24:00  pliu
*   changed !C prolog to !C-INC.
*
*   Revision 1.6  2000/06/21 18:17:26  solanki
*   Added 'Desing Notes' section in prolog.
*
 * Revision 1.5  1998/03/03  21:17:53  fshaw
 * added bangs to prolog
 *
 * Revision 1.4  1997/07/29  12:50:02  fshaw
 * *** empty log message ***
 *
 * Revision 1.3  1997/03/17  21:32:55  fshaw
 * *** empty log message ***
 *
*
* !Team-unique header:
*
*    This software is developed by the MODIS Science Data Support
*    Team for the National Aeronautics and Space Administration,
*    Goddard Space Flight Center, under contract NAS5-32373.
*
* References and Credits:
*
*    Written by   Frederick J. Shaw  2-4-97 
*    General Sciences Corporation
*    SAIC/GSC MODIS SCIENCE DATA SUPPORT OFFICE
*    7501 FORBES BLVD, SEABROOK MD 20706
*
*    fshaw@ltpmail.gsfc.nasa.gov
*
* Design Notes:
*
* !END
**********************************************************************/

#ifndef MAPIOCEAN
#define MAPIOCEAN

/**------------------  MOD OCEAN  ------------------**/
/**             Product type identifier             **/

#define M27_PROD_ID           "MOD27 HDF output file"

/**------------------  MOD OCEAN  ------------------**/
/**              Common Global Metadata             **/

#define MODSTRUCT             "StructMetadata.0"
#define MODCALIBRATION        "Calibration"
#define MODSWATH              "Swath"

/**------------------  MOD OCEAN  ------------------**/
/**                   Common SDSs                   **/

#define MODL2_FLAGS_1         "l2_flags_1"
#define MODL2_FLAGS_2         "l2_flags_2"
#define MOD_LATITUDE          "Latitude"
#define MOD_LONGITUDE         "Longitude"

/**------------------  MOD OCEAN  ------------------**/
/**             Common Local Metadata               **/


/**------------------  MOD OCEAN  ------------------**/
/**             Common SDS Dimension Names          **/

 #define MODMIAMI_FORMAT          "Miami Format"
 #define MODNUM_REC               "Number of records"
 #define MODNUM_SAMP_REC          "Number of samples per record"
 #define MODNUM_BANDS             "Number of Bands"

/**------------------  MOD OCEAN  ------------------**/
/**             Common Vdata                        **/
#define M18QCCALIBRATION      "Calibration"
#define M18QCBAND             "band"
#define M18QCTYPE             "type"
#define M18QCDEG              "deg"
#define M18QCCOEFF            "coeff"
#define M18QCNAME             "name"
#define M18QCUNITS            "units"
#define M18QCBANDNAME         "bandname"


/**------------------  MOD OCEAN  ------------------**/
/**             Product Specific SDS Names          **/

/*         MOD18 - Water Leaving Radiance             */
 #define M18NLW_412              "nLw_412"                
 #define M18NLW_443              "nLw_443"    
 #define M18NLW_488              "nLw_488"            
 #define M18NLW_531              "nLw_531"           
 #define M18NLW_551              "nLw_551"
 #define M18NLW_667              "nLw_667"               
 #define M18NLW_678              "nLw_678"            

/*         MOD18_QC - Ocean Color Quality Control             */

#define M18QCU_WIND           "U_Wind"
#define M18QCV_WIND           "V_Wind"
#define M18QCPRESSURE         "Pressure"
#define M18QCHUMIDITY         "Humidity"
#define M18QCOZONE            "Ozone"
#define M18QCSOLARZENITH      "SolarZenith"
#define M18QCSOLARAZIMUTH     "SolarAzimuth"
#define M18QCSATELLITEZENITH  "SatelliteZenith"
#define M18QCSATELLITEAZIMUTH "SatelliteAzimuth"
#define M18QCNLW670           "nLw670"
#define M18QCLA765            "La765"
#define M18QCRAY443           "Ray443"
#define M18QCLG865            "Lg865"
#define M18QCLF865            "Lf865"
#define M18QCAER_MODEL1       "Aer_Model1"
#define M18QCAER_MODEL2       "Aer_Model2"

/*        MOD 19  Pigment Concentration             */
#define M19CZCS_PIGMENT               "CZCS_pigment"           
#define M19CHLOR_MODIS                "chlor_MODIS"            
#define M19PIGMENT_C1_TOTAL           "pigment_c1_total"       

/* MOD20 - Chlorophyll fluorescence                 */
#define M20CHLOR_FLUOR_HT             "chlor_fluor_ht"         
#define M20CHLOR_FLUOR_BASE           "chlor_fluor_base"      
#define M20CHLOR_FLUOR_EFFIC          "chlor_fluor_effic"      

/* MOD21 - Chlorophyll a                            */
#define M21CHLOR_A_2                  "chlor_a_2"  
#define M21CHLOR_A_3                  "chlor_a_3" 

/* MOD22 - PAR                                     */
#define M22IPAR                       "ipar"    
#define M22ARP                        "arp" 

/* MOD23 - Suspended solids                        */
#define M23SUSP_SOLIDS_CONC           "susp_solids_conc"       

/*  MOD24 - Organic matter                         */
#define M24ABSORP_COEF_GELB           "absorp_coef_gelb"

/* MOD25 - Coccolith Concentration                 */
#define M25COCCO_PIGMNT_CONC          "cocco_pigmnt_conc"      
#define M25COCCO_CONC_DETACH          "cocco_conc_detach"     
#define M25CALCITE_CONC               "calcite_conc"    

/* MOD26 - Ocean Water Attenuation                 */
#define M26K_490                         "K_490"                   

/****************************************************************
*                       MOD 27_L4                               * 
*          MODIS Level Level 4 Annual Primary productivity      *
*               Version 1      					* 
*****************************************************************/

 #define INPUT_RACES_FILENAME     "Input RACES Filename"
 #define CPHYLL_A_UNITS           "Chlorophyll a Units"
 #define SEA_SURF_TEMP_UNITS      "Sea Surface Temperature Units"
 #define MIXED_LAYER_DEPTH_UNITS  "Mixed-Layer Depth Units"
 #define AVG_DAILY_PSYNTH_ACT_RAD "Average Daily Photosynthetically Active Radiation" 

 #define AREA_UNITS            "Area Units"       
 #define PPC_UNITS             "Ppc Units"
 #define PNN_UNITS             "Pnn Units"
 #define PXC_UNITS             "Pxc Units"
 #define TOT_PXC_UNITS         "Total Pxc Units"
 #define TOT_PPC_UNITS         "Total Ppc Units"
 #define TOT_PNN_UNITS         "Total Pnn Units"

 #define TOT_N_CHL_GCELLS      "Total Number of Chl Gridcells"           
 #define N_B_CHL_GCELLS        "Number of Bad Chl Gridcells"                
 #define N_U_CHL_GCELLS        "Number of Ugly Chl Gridcells"  
 #define N_G_CHL_GCELLS        "Number of Good Chl Gridcells"              

 #define GLB_CPHYLL_AREA       "Global Chlorophyll a Area"                  
 #define GLB_B_CPHYLL_AREA     "Global Bad Chlorophyll a Area"             
 #define GLB_U_CPHYLL_AREA     "Global Ugly Chlorophyll a Area"              
 #define GLB_G_CPHYLL_AREA     "Global Good Chlorophyll a Area"      

 #define GLB_HVAR_CPHYLL_AREA  "Global High-Variance Chlorophyll a Area"   
 #define GLB_LRATE_CPHYLL_AREA "Global Low-Sample Rate Chlorophyll a Area" 

 #define PPC_GLB_N_B_GCELLS  "Ppc Global Number of Bad Gridcells"  
 #define PPC_GLB_N_U_GCELLS  "Ppc Global Number of Ugly Gridcells"  
 #define PPC_GLB_N_G_GCELLS  "Ppc Global Number of Good Gridcells"  

 #define PPC_TOT_GLB_B_AREA  "Ppc Total Global Bad Area"
 #define PPC_TOT_GLB_G_AREA  "Ppc Total Global Good Area"
 #define PPC_TOT_GLB_U_AREA  "Ppc Total Global Ugly Area"               

 #define TOT_GLB_PPC         "Total Global Ppc" 
 #define UNC_TOT_GLB_PPC     "Uncertainty in Total Global Ppc"  

 #define PPC_F1_DESCRIP      "Ppc Function 1 Description"

 #define PPC_F1_TOT_N_GCELLS "Ppc Function 1 Total Number of Gridcells" 
 #define PPC_F1_N_B_GCELLS   "Ppc Function 1 Number of Bad Gridcells" 
 #define PPC_F1_N_U_GCELLS   "Ppc Function 1 Number of Ugly Gridcells"  
 #define PPC_F1_N_G_GCELLS   "Ppc Function 1 Number of Good Gridcells"   

 #define PPC_F1_TOT_AREA     "Ppc Function 1 Total Area"                
 #define PPC_F1_G_AREA       "Ppc Function 1 Good Area"                 
 #define PPC_F1_U_AREA       "Ppc Function 1 Ugly Area"                 
 #define PPC_F1_B_AREA       "Ppc Function 1 Bad Area"         

 #define F1_TOT_PPC          "Function 1 Total Ppc"                     
 #define UNC_F1_TOT_PPC      "Uncertainty in Function 1 Total Ppc"      

 /*  The same pattern continues for Functions 2 thru Functions 15     */

 #define PPC_F16_DESCRIP      "Ppc Function 16 Description"            

 #define PPC_F16_TOT_N_GCELLS "Ppc Function 16 Total Number of Gridcells" 
 #define PPC_F16_N_B_GCELLS  "Ppc Function 16 Number of Bad Gridcells"
 #define PPC_F16_N_U_GCELLS  "Ppc Function 16 Number of Ugly Gridcells"
 #define PPC_F16_N_G_GCELLS  "Ppc Function 16 Number of Good Gridcells"

 #define PPC_F16_TOT_AREA    "Ppc Function 16 Total Area"              
 #define PPC_F16_B_AREA      "Ppc Function 16 Bad Area"                
 #define PPC_F16_U_AREA      "Ppc Function 16 Ugly Area"         
 #define PPC_F16_G_AREA      "Ppc Function 16 Good Area"             
 
 #define F16_TOT_PPC         "Function 16 Total Ppc"       
 #define UNC_F16_TOT_PPC     "Uncertainty in Function 16 Total Ppc"     
 
 #define PNN_GLB_N_B_GCELLS  "Pnn Global Number of Bad Gridcells"    
 #define PNN_GLB_N_U_GCELLS  "Pnn Global Number of Ugly Gridcells"  
 #define PNN_GLB_N_G_GCELLS  "Pnn Global Number of Good Gridcells"     
 
 #define PNN_TOT_GLB_B_AREA  "Pnn Total Global Bad Area"        
 #define PNN_TOT_GLB_U_AREA  "Pnn Total Global Ugly Area"          
 #define PNN_TOT_GLB_G_AREA  "Pnn Total Global Good Area"         

 #define TOT_GLB_PNN         "Total Global Pnn"                     
 #define UNC_TOT_GLB_PNN     "Uncertainty in Total Global Pnn"       

 #define PNN_F1_DESCRP       "Pnn Function 1 Description"       

 #define PNN_F1_TOT_N_GCELLS "Pnn Function 1 Total Number of Gridcells"  
 #define PNN_F1_N_B_GCELLS   "Pnn Function 1 Number of Bad Gridcells" 
 #define PNN_F1_N_G_GCELLS   "Pnn Function 1 Number of Good Gridcells"   
 #define PNN_F1_N_U_GCELLS   "Pnn Function 1 Number of Ugly Gridcells"  

 #define PNN_F1_TOT_AREA     "Pnn Function 1 Total Area"               
 #define PNN_F1_B_AREA       "Pnn Function 1 Bad Area"   
 #define PNN_F1_U_AREA       "Pnn Function 1 Ugly Area"                
 #define PNN_F1_G_AREA       "Pnn Function 1 Good Area"         

 #define F1_TOT_PNN          "Function 1 Total Pnn"               
 #define UNC_F1_TOT_PNN      "Uncertainty in Function 1 Total Pnn"     

 /*  The same pattern continues for Function 2 thru Functions 15     */

 #define PNN_F16_DESCRP      "Pnn Function 16 Description"    

 #define PNN_F16_TOT_N_GCELLS "Pnn Function 16 Total Number of Gridcells" 
 #define PNN_F16_N_B_GCELLS  "Pnn Function 16 Number of Bad Gridcells"  
 #define PNN_F16_N_U_GCELLS  "Pnn Function 16 Number of Ugly Gridcells" 
 #define PNN_F16_N_G_GCELLS  "Pnn Function 16 Number of Good Gridcells" 

 #define PNN_F16_TOT_AREA    "Pnn Function 16 Total Area"            
 #define PNN_F16_B_AREA      "Pnn Function 16 Bad Area"           
 #define PNN_F16_U_AREA      "Pnn Function 16 Ugly Area"        
 #define PNN_F16_G_AREA      "Pnn Function 16 Good Area"     
 
 #define F16_TOT_PNN         "Function 16 Total Pnn"            
 #define UNC_F16_TOT_PNN     "Uncertainty in Function 16 Total Pnn"   

 #define PXC_GLB_N_B_GCELLS  "Pxc Global Number of Bad Gridcells"    
 #define PXC_GLB_N_U_GCELLS  "Pxc Global Number of Ugly Gridcells"      
 #define PXC_GLB_N_G_GCELLS  "Pxc Global Number of Good Gridcells"     

 #define PXC_TOT_GLB_B_AREA  "Pxc Total Global Bad Area"            
 #define PXC_TOT_GLB_U_AREA  "Pxc Total Global Ugly Area"          
 #define PXC_TOT_GLB_G_AREA  "Pxc Total Global Good Area"            

 #define TOT_GLB_PXC         "Total Global Pxc"      
 #define UNC_TOT_GLB_PXC     "Uncertainty in Total Global Pxc"   

 #define PXC_F1_DESCRP       "Pxc Function 1 Description"           

 #define PXC_F1_TOT_N_GCELLS "Pxc Function 1 Total Number of Gridcells"
 #define PXC_F1_NUM_B_GCELLS "Pxc Function 1 Number of Bad Gridcells" 
 #define PXC_F1_NUM_U_GCELLS "Pxc Function 1 Number of Ugly Gridcells"  
 #define PXC_F1_NUM_G_GCELLS "Pxc Function 1 Number of Good Gridcells"  
 
 #define PXC_F1_TOT_AREA     "Pxc Function 1 Total Area"
 #define PXC_F1_B_AREA       "Pxc Function 1 Bad Area"                  
 #define PXC_F1_U_AREA       "Pxc Function 1 Ugly Area"
 #define PXC_F1_G_AREA       "Pxc Function 1 Good Area"            
 
 #define F1_TOT_PXC          "Function 1 Total Pxc"            
 #define UNC_F1_T_PXC        "Uncertainty in Function 1 Total Pxc"      

 /*  The same pattern continues for Function 2 thru Functions 15     */

 #define PXC_F16_DESCRP      "Pxc Function 16 Description"            

 #define PXC_F16_T_N_GCELLS  "Pxc Function 16 Total Number of Gridcells"
 #define PXC_F16_N_B_GCELLS  "Pxc Function 16 Number of Bad Gridcells" 
 #define PXC_F16_N_U_GCELLS  "Pxc Function 16 Number of Ugly Gridcells"
 #define PXC_F16_N_G_GCELLS  "Pxc Function 16 Number of Good Gridcells"

 #define PXC_F16_TOT_AREA    "Pxc Function 16 Total Area"          
 #define PXC_F16_B_AREA      "Pxc Function 16 Bad Area"           
 #define PXC_F16_U_AREA      "Pxc Function 16 Ugly Area"      
 #define PXC_F16_G_AREA      "Pxc Function 16 Good Area"         

 #define F16_TOT_PXC         "Function 16 Total Pxc"          
 #define UNC_F16_TOT_PXC     "Uncertainty in Function 16 Total Pxc"
 
 /* Main VDATA                                                  */

 #define CELL_NUM            "Cell Number"                                
 #define NUM_OF_OBS          "Number of Observations"                    
 #define NUM_OF_GRAN         "Number of Granules"                         
 #define MOCEAN_LAT          "Latitude"
 #define MOCEAN_LONG         "Longtitude"
 #define CELL_AREA           "Cell Area"
 #define CHL_FLG             "Chl a Flags"
 #define ANNUAL_AVG_PPC      "Annual Avg Ppc" 
 #define PPC_UNCERT          "Ppc Uncertainty"
 #define PPC_FLG             "Ppc Flags"
 #define ANNUAL_AVG_PNN      "Annual Avg Pnn"
 #define PNN_UNCERT          "Pnn Uncertainty"
 #define PNN_FLG             "Pnn Flags"
 #define ANNUAL_AVG_PXC      "Annual Avg Pxc"
 #define PXC_UNCERT          "Pxc Uncertainty"
 #define PXC_FLG             "Pxc Flags"

/* MOD28 - Sea Surface Temperature                               */
 #define M28SST                  "sst"
 #define M28SST4                 "sst4"         
 #define M28bright20             "bright20"
 #define M28bright22             "bright22"
 #define M28bright23             "bright23"
 #define M28bright31             "bright31"
 #define M28bright32             "bright32"
 #define M28raw20                "raw20"
 #define M28raw22                "raw22"
 #define M28raw23                "raw23"
 #define M28raw31                "raw31"
 #define M28raw32                "raw32"


/* Vgroup names                                               */
 #define M28GEOLOC_FLDS          "Geolocation Fields"
 #define M28DATA_FIELDS          "Data Fields"

/* MOD31 - Phycoerytherin concentration   */
 
 #define M31PHYCOERYTH_CONC  "phycoeryth_conc"
 #define M31PHYCOU_CONC      "phycou_conc"

/* MOD36 - Clear Water Epsilon            */
 #define M36TOT_ABSORB_1     "tot_absorb_1"
 #define M36TOT_ABSORB_2     "tot_absorb_2"
 #define M36TOT_ABSORB_3     "tot_absorb_3"
 #define M36TOT_ABSORB_4     "tot_absorb_4"
 #define M36TOT_ABSORB_5     "tot_absorb_5"

/* MOD37 - Ocean Aerosol Properties       */
 #define TAU_865             "Tau_865"           
 #define EPS_78              "Eps_78"          
 #define AER_MODEL1          "aer_model1"       
 #define AER_MODEL2          "aer_model2"

/* MOD39 - Clear Water Epsilon            */
 #define EPS_CLR_WATER  "eps_clr_water"         

#endif
