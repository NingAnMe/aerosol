/************************************************************************
*			mapiL1A.h utilities header file			*
*			MODIS L1A DATA PRODUCT FORMAT			*
*				Version 2.2.0				*
*			HDF Object Identifier Constants			*
*************************************************************************

**********************************************************************
* !C-INC
*
* !Purpose:	Utilities header file containing constants for MOD 01
*		product HDF object names.
*
* !Description: The Header file mapiL1A.h is part of a larger software
*              system called the MODIS Applications Programming Interface 
*              Utility, abbreviated M-API.  The M-API Utility consists of
*              subroutines which allow MODIS Science Team-supplied software
*              to read in Level 1B radiance bands and write out output
*              products and metadata to HDF files.  The functionality of the
*              M-API is defined in the MODIS API User's Guide, Version 1,
*              dated 4/3/95.
*
*	       The mapiL1A.h file contains macros for the specific
*	       names of data objects contained in the MOD01 HDF file.
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
*       Revision 01.01 1996/01/31
*       Qi Huang
*       Defined IAW MOD01_L1A File Specification. 1995/10/18
* $Log: mapiL1A.h,v $
* Revision 1.10  2003/02/11 18:21:02  vlin
* Macro M01MAJCYC4OF63 updated
* vlin@saicmodis.com
*
* Revision 1.9  1999/12/14 22:54:05  solanki
* Minor changes in prolog - version 1.5 message.
*
 * Revision 1.8  1999/04/23  21:03:06  jayshree
 * *** empty log message ***
 *
 * Revision 1.7  1999/04/23  20:53:30  jayshree
 * reorganizing mapi RCS
 *
 * Revision 1.5  1998/12/11  15:21:52  solanki
 * modified to match latest L1A file specs. (CCR443).
 *
 * Revision 1.4  1998/12/02  21:10:33  solanki
 * *** empty log message ***
 *
 * Revision 1.3  1998/12/02  21:03:10  solanki
 * Updated to match latest (Oct 05,1998) file specs.
 *
 * Revision 1.3  1998/12/02  21:03:10  solanki
 * Updated to match latest (Oct 05,1998) file specs.
 *
 * Revision 1.6  1998/03/02  20:40:26  fshaw
 * added bangs to prolog
 *
 * Revision 1.3  1997/02/06  12:53:27  fshaw
 * *** empty log message ***
 *
 * Revision 1.2  1996/10/03  15:21:15  fshaw
 * *** empty log message ***
 *
 * Revision 1.1  1996/08/23  12:55:06  fshaw
 * Initial Version 
 *
*
* !Team-unique Header: 
*
* !References and Credits: 
*      This software is developed by the MODIS Science Data Support
*      Team for the National Aeronautics and Space Administration,
*      Goddard Space Flight Center, under contract NAS5-32373.
*
*      HDF portions developed at the National Center for Supercomputing
*      Applications at the University of Illinois at Urbana-Champaign.
*
* !Design Notes: 
*
* !END
********************************************************************
*/

#ifndef M01_PROD_ID

#define M01_PROD_ID            		        "MOD01"
/* #define M01ORBITAL_NODE                "Orbital Node"  Obsolete */
#define M01NUMBER_OF_SCANS             		"Number of Scans"
#define M01NUMBER_DAY_SCANS         		"Number of Day mode scans"
#define M01NUMBER_NIGHT_SCANS       		"Number of Night mode scans"
#define M01MAX_TOTAL_FRAMES            		"Max Total Frames"
#define M01MAX_EARTH_FRAMES            		"Max Earth Frames"
#define M01MAX_SD_FRAMES               		"Max SD Frames"
#define M01MAX_SRCA_FRAMES             		"Max SRCA Frames"
#define M01MAX_BB_FRAMES               		"Max BB Frames"
#define M01MAX_SV_FRAMES               		"Max SV Frames"

#define M01SCAN_TYPES                  		"Scan Types in product"
#define M01SCAN_TYPES_IN_PROD           	M01SCAN_TYPES
#define M01INCOMPL_SCANS               		"Incomplete Scans"
#define M01MISSING_PACKETS             		"Missing Packets"
#define M01DISCARD_PACKETS             		"Discarded packets" 
#define M01PACKTS_BAD_CRC              		"Packets with bad CRC"


/* SCAN-LEVEL METADATA SDSs */

#define	M01SCAN_NUMBER				"Scan number"
#define	M01FRAME_COUNT_ARRAY			"Frame count array"
#define	M01SCAN_TYPE				"Scan Type"
#define	M01SD_START_TIME			"SD start time"
#define	M01SRCA_START_TIME			"SRCA start time"
#define	M01BB_START_TIME			"BB start time"
#define	M01SV_START_TIME			"SV start time"
#define	M01EV_START_TIME			"EV start time"
#define	M01SRCA_CALIBRATION_MODE		"SRCA calibration mode"
#define	M01PACKET_SCAN_COUNT			"Packet scan count"
#define	M01CCSDS_APID                  		"CCSDS Application Identifiers"
#define	M01PACKET_QL             		"Packet expedited data flag"
#define	M01MIRROR_SIDE				"Mirror side"
#define	M01SCAN_QUALITY_ARRAY			"Scan quality array"


/* Pixel Quality Data SDSs */

#define	M01EV_PIX_QUAL	                 	"Earth sector Pixel quality"
#define	M01SD_PIX_QUAL	                 	"SD sector Pixel quality"
#define	M01SRCA_PIX_QUAL	                "SRCA sector Pixel quality"
#define	M01BB_PIX_QUAL	                  	"BB sector Pixel quality" 
#define	M01SV_PIX_QUAL		                "SV sector Pixel quality"


/* Packet Data SDSs */

#define	M01EV_250M		"EV_250m"	/* Bands 1 and 2 */
#define	M01EV_500M		"EV_500m"	/* Bands 3 through 7 */
#define	M01EV_1KM_DAY		"EV_1km_day"	/* Bands 8 through 19 */
#define	M01EV_1KM_NITE		"EV_1km_night"	/* Bands 20 through 36 */
#define	M01SD_250M		"SD_250m"	/* Bands 1 and 2 */
#define	M01SD_500M		"SD_500m"	/* Bands 3 through 7 */
#define	M01SD_1KM_DAY		"SD_1km_day"	/* Bands 8 through 19 */
#define	M01SD_1KM_NITE		"SD_1km_night"	/* Bands 20 through 36 */
#define	M01SRCA_250M		"SRCA_250m"	/* Bands 1 and 2 */
#define	M01SRCA_500M		"SRCA_500m"	/* Bands 3 through 7 */
#define	M01SRCA_1KM_DAY		"SRCA_1km_day"	/* Bands 8 through 19 */
#define	M01SRCA_1KM_NITE	"SRCA_1km_night" /* Bands 20 through 36 */
#define	M01BB_250M		"BB_250m"	/* Bands 1 and 2 */
#define	M01BB_500M		"BB_500m"	/* Bands 3 through 7 */
#define	M01BB_1KM_DAY		"BB_1km_day"	/* Bands 8 through 19 */
#define	M01BB_1KM_NITE		"BB_1km_night"	/* Bands 20 through 36 */
#define	M01SV_250M		"SV_250m"	/* Bands 1 and 2 */
#define	M01SV_500M		"SV_500m"	/* Bands 3 through 7 */
#define	M01SV_1KM_DAY		"SV_1km_day"	/* Bands 8 through 19 */
#define	M01SV_1KM_NITE		"SV_1km_night"	/* Bands 20 through 36 */
#define	M01FPA_AEM_CONFIG	"fpa_aem_config"    /* FPA/AEM Config   */
#define M01SCIENCE_STATE        "science_state"	    /* Science State         */
#define M01SCIENCE_ABNORM       "science_abnormal"  /* Science Abnormal      */
#define	M01FPA_DCR_OFFST	"fpa_dcr_offset"     /* FPA DCR offset data */
#define	M01FPA_DCR_OFFSET	 M01FPA_DCR_OFFST
#define	M01RAW_MIR_ENC		"raw_mir_enc"	   /* Raw mirror encoder data */
#define	M01RAW_VS_START		"raw_vs_def"       /* View Sector Definitions */
#define	M01RAW_VS_DEF		 M01RAW_VS_START
#define	M01RAW_VS_ACT		"raw_vs_act"        /* View Sector Actuals */
#define	M01RAW_SCI_ENG		"raw_sci_eng"	    /* Sci Eng Data  */
#define	M01RAW_HK_TELEM		"raw_hk_telem"	   /* Current/Prior HK Telem  */
#define	M01RAW_SC_ANCIL		"raw_sc_ancil"	    /* Raw s/c ancill data */
#define	M01RAW_PARAM		"raw_param"	    /* Parameter Table   */
#define	M01RAW_PV_GAINS		"raw_pv_gains"	    /* PV Gains */

/* #define	M01FAM_SAMP_DELAY	"fam_samp_delay"     OBSOLETE */
/* #define	M01RAW_CP_EVENT		"raw_cp_event"	     OBSOLETE */
/* #define	M01RAW_FR_EVENT		"raw_fr_event"	     OBSOLETE */
/* #define	M01RAW_DUMP_REQ		"raw_dump_req"	      OBSOLETE */
/* #define	M01RAW_DUMP_DATA	"raw_dump_data"	     OBSOLETE */
/* #define M01FPA_USE		"fpa_use"	       FPA Use               */
/* #define M01FAM_AF0X_MUX_DCR	"fam_af0x_mux_dcr"   OBSOLETE */
/* #define M01SDSM_DETECTOR_SAMPLES "sdsm_det_samples"   OBSOLETE */
	


/*  Discarded Packets List  */
    /*	Vdata name  */

#define M01DISCARDED_PACKETS	"Discarded Packets"


/*  Field name	*/

#define M01DISCARDED_PKTS_FIELD	"discarded_packets"


/*  Engineering Decommutation List  */

#define M01MAJCYCALL1		"Telemetry Major Cycle All Part 1"
#define M01LAST_VALID_SCAN	"LAST_VALID_SCAN"
#define M01SS_CP1553_MAJCYC	"SS_CP1553_MAJCYC"
#define M01SS_CP_CMD_ECHO	"SS_CP_CMD_ECHO"
#define M01SS_CP_LOG_EVENT	"SS_CP_LOG_EVENT"
#define M01SS_CP_MODE		"SS_CP_MODE"
#define M01SS_CP_MODECHG_GO	"SS_CP_MODECHG_GO"
#define M01SS_CP_SPARE		"SS_CP_SPARE"
#define M01SS_CP_VALTLM_COL	"SS_CP_VALTLM_COL"
#define M01SS_CP_VALTLM_FMT	"SS_CP_VALTLM_FMT"
#define M01SS_DR_NAD_STEP	"SS_DR_NAD_STEP"
#define M01SS_DR_SPARE		"SS_DR_SPARE"
#define M01SS_DR_SVD_STEP	"SS_DR_SVD_STEP"
#define M01CS_FR_BBRADTAB	"CS_FR_BBRADTAB"
#define M01SS_FR_LOG_EVENT	"SS_FR_LOG_EVENT"
#define M01IR_PS1_INPUT_CUR	"IR_PS1_INPUT_CUR"
#define M01IR_PS2_INPUT_CUR	"IR_PS2_INPUT_CUR"
#define M01TA_RC_LWIR_CFPA	"TA_RC_LWIR_CFPA"
#define M01TA_RC_SMIR_CFPA	"TA_RC_SMIR_CFPA"
#define M01SD_SA_APX_PERIOD	"SD_SA_APX_PERIOD"


#define M01MAJCYCALL2           "Telemetry Major Cycle All Part 2"
#define M01CR_SM_MIR_HOME_A	"CR_SM_MIR_HOME_A"
#define M01CR_SM_MIR_HOME_B	"CR_SM_MIR_HOME_B"
#define M01CS_SM_MIR_STEP	"CS_SM_MIR_STEP"
#define M01CR_SR_GRAT_CH_A	"CR_SR_GRAT_CH_A"
#define M01CR_SR_GRAT_CH_B	"CR_SR_GRAT_CH_B"
#define M01CR_SR_GRAT_FH_A	"CR_SR_GRAT_FH_A"
#define M01CR_SR_GRAT_FH_B	"CR_SR_GRAT_FH_B"
#define M01CR_SR_IR_SRC_OFF	"CR_SR_IR_SRC_OFF"
#define M01CR_SR_SISHTR_OFF	"CR_SR_SISHTR_OFF"
#define M01CS_FR_GAINTAB	"CS_FR_GAINTAB"
#define M01CR_SR_SLIT_HOMEA	"CR_SR_SLIT_HOMEA"
#define M01CR_SR_SLIT_HOMEB	"CR_SR_SLIT_HOMEB"
#define M01CR_SR_WHL_HOMEA	"CR_SR_WHL_HOMEA"
#define M01CR_SR_WHL_HOMEB	"CR_SR_WHL_HOMEB"
#define M01CS_SR_GRAT_STEP	"CS_SR_GRAT_STEP"
#define M01CS_SR_SLIT_STEP	"CS_SR_SLIT_STEP"
#define M01CS_SR_SRCWH_STEP	"CS_SR_SRCWH_STEP"
#define M01CS_SR_LAMPS		"CS_SR_LAMPS"


#define M01MAJCYCALL3           "Telemetry Major Cycle All Part 3"
#define M01SS_CP_LAST_EVENT	"SS_CP_LAST_EVENT"
#define M01SS_FR_LAST_EVENT	"SS_FR_LAST_EVENT"
#define M01SS_CP_TC1_DAYS	"SS_CP_TC1_DAYS"
#define M01SS_CP_TC2_MILLIS	"SS_CP_TC2_MILLIS"
#define M01SS_CP_TC3_MILLIS	"SS_CP_TC3_MILLIS"
#define M01SS_CP_TC4_MICROS	"SS_CP_TC4_MICROS"
#define M01CS_FR_OFFSETTAB	"CS_FR_OFFSETTAB"
#define M01SS_CP_MACRO_ID	"SS_CP_MACRO_ID"
#define M01SS_CP_MACRO_ON	"SS_CP_MACRO_ON"
#define M01SS_DR_SDD_STEP	"SS_DR_SDD_STEP"


#define M01MAJCYC0OF7		"Telemetry Major Cycle 0 of 7"
#define M01CR_BB_A_PWR_ON	"CR_BB_A_PWR_ON"
#define M01CR_BB_B_PWR_ON	"CR_BB_B_PWR_ON"
#define M01CR_BB_SPARE		"CR_BB_SPARE"
#define M01CS_BB_TEMP_SET	"CS_BB_TEMP_SET"
#define M01IR_BB_HTRA_CURH	"IR_BB_HTRA_CURH"
#define M01IR_BB_HTRB_CURH	"IR_BB_HTRB_CURH"
#define M01TP_BB_TEMP01H	"TP_BB_TEMP01H"
#define M01TP_BB_TEMP02H	"TP_BB_TEMP02H"
#define M01TP_BB_TEMP03H	"TP_BB_TEMP03H"
#define M01TP_BB_TEMP04H	"TP_BB_TEMP04H"
#define M01TP_BB_TEMP05H	"TP_BB_TEMP05H"
#define M01TP_BB_TEMP06H	"TP_BB_TEMP06H"
#define M01TP_BB_TEMP07H	"TP_BB_TEMP07H"
#define M01TP_BB_TEMP08H	"TP_BB_TEMP08H"


#define M01MAJCYC1OF7           "Telemetry Major Cycle 1 of 7"
#define M01TP_BB_TEMP09H	"TP_BB_TEMP09H"
#define M01TP_BB_TEMP10H	"TP_BB_TEMP10H"
#define M01TP_BB_TEMP11H	"TP_BB_TEMP11H"
#define M01TP_BB_TEMP12H	"TP_BB_TEMP12H"
#define M01CR_CE_A_ON		"CR_CE_A_ON"
#define M01CR_CE_B_ON		"CR_CE_B_ON"
#define M01CR_CPA_EEP_WRE_M	"CR_CPA_EEP_WRE_M"
#define M01CR_CPB_EEP_WRE_M	"CR_CPB_EEP_WRE_M"
#define M01CR_CP_A_ON_M		"CR_CP_A_ON_M"
#define M01CR_CP_B_ON_M		"CR_CP_B_ON_M"
#define M01CR_CP_SET_TMF_A	"CR_CP_SET_TMF_A"
#define M01CS_CP_SPARE		"CS_CP_SPARE"
#define M01SS_CP_IMOK_ON	"SS_CP_IMOK_ON"
#define M01SS_CP_SPARE_1	"SS_CP_SPARE_1"
#define M01SS_CP_SPARE_2	"SS_CP_SPARE_2"
#define M01SS_CP_RESET_SRC	"SS_CP_RESET_SRC"
#define M01SS_CP_SCAN_EST	"SS_CP_SCAN_EST"
#define M01SS_CP_LOG_STATE	"SS_CP_LOG_STATE"
#define M01SS_FR_LOG_STATE	"SS_FR_LOG_STATE"
#define M01SS_FR_SCIABNORM	"SS_FR_SCIABNORM"
#define M01CS_FR_SPARE_1	"CS_FR_SPARE_1"
#define M01CS_FR_SPARE_2	"CS_FR_SPARE_2"
#define M01SS_CP_STATUS_04	"SS_CP_STATUS_04"


#define M01MAJCYC2OF7           "Telemetry Major Cycle 2 of 7"
#define M01SS_CP_STATUS_05	"SS_CP_STATUS_05"
#define M01SS_CP_STATUS_06	"SS_CP_STATUS_06"
#define M01SS_CP_STATUS_07	"SS_CP_STATUS_07"
#define M01SS_CP_STATUS_08	"SS_CP_STATUS_08"
#define M01SS_CP_STATUS_09	"SS_CP_STATUS_09"
#define M01SS_CP_UART_RESET	"SS_CP_UART_RESET"
#define M01SS_CP_UART_HUNT	"SS_CP_UART_HUNT"
#define M01SS_CP_UART_SYNC	"SS_CP_UART_SYNC"


#define M01MAJCYC3AOF7          "Telemetry Major Cycle 3A of 7"
#define M01SS_CP_UART_NORM	"SS_CP_UART_NORM"
#define M01SS_CP_SPARE		"SS_CP_SPARE"
#define M01SS_CP_TMF_GONE	"SS_CP_TMF_GONE"
#define M01CR_DR_DRV_ON		"CR_DR_DRV_ON"
#define M01CR_DR_FS_ENABL_M	"CR_DR_FS_ENABL_M"
#define M01CR_DR_FS_SW_CLSD	"CR_DR_FS_SW_CLSD"
#define M01CR_DR_NAD_CLSD	"CR_DR_NAD_CLSD"
#define M01CR_DR_NAD_FS_ON	"CR_DR_NAD_FS_ON"
#define M01CR_DR_NAD_OPEN	"CR_DR_NAD_OPEN"
#define M01CR_DR_PRI_FS_SEL	"CR_DR_PRI_FS_SEL"
#define M01CR_DR_SDD_CLSD	"CR_DR_SDD_CLSD"
#define M01CR_DR_SDD_DRV_A	"CR_DR_SDD_DRV_A"
#define M01CR_DR_SDD_OPEN	"CR_DR_SDD_OPEN"
#define M01CR_DR_SDFS_DRVON	"CR_DR_SDFS_DRVON"
#define M01CR_DR_SPARE		"CR_DR_SPARE"
#define M01CR_DR_SDS_OPEN	"CR_DR_SDS_OPEN"
#define M01CR_DR_SVD_CLSD	"CR_DR_SVD_CLSD"
#define M01CR_DR_SVD_FS_ON	"CR_DR_SVD_FS_ON"
#define M01CR_DR_SVD_OPEN	"CR_DR_SVD_OPEN"
#define M01CR_DR_UNLACH_AON	"CR_DR_UNLACH_AON"
#define M01CR_DR_UNLACH_BON	"CR_DR_UNLACH_BON"
#define M01CS_DR_SVD_AT_OG	"CS_DR_SVD_AT_OG"


#define M01MAJCYC3BOF7          "Telemetry Major Cycle 3B of 7"
#define M01TP_DR_NAD_FS		"TP_DR_NAD_FS"
#define M01TP_DR_SPARE		"TP_DR_SPARE"
#define M01TP_DR_SVD_FS		"TP_DR_SVD_FS"
#define M01CR_FI_A_ON		"CR_FI_A_ON"
#define M01CR_FI_PORT_A_ON	"CR_FI_PORT_A_ON"
#define M01CR_FI_A_RESET	"CR_FI_A_RESET"
#define M01CR_FI_B_ON		"CR_FI_B_ON"
#define M01CR_FI_PORT_B_ON	"CR_FI_PORT_B_ON"
#define M01CR_FI_B_RESET	"CR_FI_B_RESET"
#define M01CR_FO_BLK1_ON	"CR_FO_BLK1_ON"
#define M01CR_FO_SPARE_1	"CR_FO_SPARE_1"
#define M01CR_FO_BLK2_ON	"CR_FO_BLK2_ON"
#define M01CR_FO_SPARE_2	"CR_FO_SPARE_2"
#define M01CR_FO_BLK3_ON	"CR_FO_BLK3_ON"
#define M01CR_FO_SPARE_3	"CR_FO_SPARE_3"
#define M01CR_FO_BLK4_ON	"CR_FO_BLK4_ON"
#define M01CS_FR_SCI_NORMAL	"CS_FR_SCI_NORMAL"
#define M01SR_FO_BLK1_MODE	"SR_FO_BLK1_MODE"
#define M01SR_FO_BLK2_MODE	"SR_FO_BLK2_MODE"
#define M01SR_FO_BLK3_MODE	"SR_FO_BLK3_MODE"
#define M01SR_FO_BLK4_MODE	"SR_FO_BLK4_MODE"


#define M01MAJCYC3COF7          "Telemetry Major Cycle 3C of 7"
#define M01CR_FRA_EEP_WRE	"CR_FRA_EEP_WRE"
#define M01CR_FRB_EEP_WRE	"CR_FRB_EEP_WRE"
#define M01CR_FR_A_ON		"CR_FR_A_ON"
#define M01CR_FR_A_RESET	"CR_FR_A_RESET"
#define M01CR_FR_B_ON		"CR_FR_B_ON"
#define M01CR_FR_B_RESET	"CR_FR_B_RESET"
#define M01CS_FR_DAY_RATE	"CS_FR_DAY_RATE"
#define M01CS_FR_DELAY_BB	"CS_FR_DELAY_BB"
#define M01CS_FR_DELAY_EA	"CS_FR_DELAY_EA"
#define M01CS_FR_DELAY_SD	"CS_FR_DELAY_SD"
#define M01CS_FR_DELAY_SP	"CS_FR_DELAY_SP"
#define M01CS_FR_DELAY_SR	"CS_FR_DELAY_SR"
#define M01CR_SR_LAMPS_LOW	"CR_SR_LAMPS_LOW"
#define M01SR_FR_A_MODELONG	"SR_FR_A_MODELONG"
#define M01SR_FR_B_MODELONG	"SR_FR_B_MODELONG"
#define M01SS_FR_SPARE		"SS_FR_SPARE"


#define M01MAJCYC4AOF7          "Telemetry Major Cycle 4A of 7"
#define M01SS_FR_SPARE_1	"SS_FR_SPARE_1"
#define M01SS_FR_PKT_TYPE	"SS_FR_PKT_TYPE"
#define M01SS_FR_RESET_SRC	"SS_FR_RESET_SRC"
#define M01SS_FR_SPARE_2	"SS_FR_SPARE_2"
#define M01CS_PC_3132_SRISE	"CS_PC_3132_SRISE"
#define M01CS_PC_3132_SFALL	"CS_PC_3132_SFALL"
#define M01CS_PC_3334_SRISE	"CS_PC_3334_SRISE"
#define M01CS_PC_3334_SFALL	"CS_PC_3334_SFALL"
#define M01CS_PC_3536_SRISE	"CS_PC_3536_SRISE"
#define M01CS_PC_3536_SFALL	"CS_PC_3536_SFALL"
#define M01CR_PCLWA_ECAL_ON	"CR_PCLWA_ECAL_ON"
#define M01CR_PCLWB_ECAL_ON	"CR_PCLWB_ECAL_ON"
#define M01CR_PCLW_A_ON		"CR_PCLW_A_ON"
#define M01CR_PCLW_B_ON		"CR_PCLW_B_ON"
#define M01CR_PS1SHDN_ENA_M	"CR_PS1SHDN_ENA_M"
#define M01CR_PS2SHDN_ENA_M	"CR_PS2SHDN_ENA_M"
#define M01CR_PVLWA_CSUB_ON	"CR_PVLWA_CSUB_ON"
#define M01CR_PVLWA_ECAL_ON	"CR_PVLWA_ECAL_ON"
#define M01CR_PVLWB_CSUB_ON	"CR_PVLWB_CSUB_ON"
#define M01CR_PVLWB_ECAL_ON	"CR_PVLWB_ECAL_ON"
#define M01CR_PVLW_A_ON		"CR_PVLW_A_ON"
#define M01CR_PVLW_B_ON		"CR_PVLW_B_ON"


#define M01MAJCYC4BOF7          "Telemetry Major Cycle 4B of 7"
#define M01CR_PVLW_S_DELAYH	"CR_PVLW_S_DELAYH"
#define M01CR_PVNIRA_ECALON	"CR_PVNIRA_ECALON"
#define M01CR_PVNIRB_ECALON	"CR_PVNIRB_ECALON"
#define M01CR_PVNIR_A_ON	"CR_PVNIR_A_ON"
#define M01CR_PVNIR_B_ON	"CR_PVNIR_B_ON"
#define M01CR_PVNIR_S_DELYH	"CR_PVNIR_S_DELYH"
#define M01CR_PVSMA_CSUB_ON	"CR_PVSMA_CSUB_ON"
#define M01CR_PVSMA_ECAL_ON	"CR_PVSMA_ECAL_ON"
#define M01CR_PVSMB_CSUB_ON	"CR_PVSMB_CSUB_ON"
#define M01CR_PVSMB_ECAL_ON	"CR_PVSMB_ECAL_ON"
#define M01CR_PVSM_A_ON		"CR_PVSM_A_ON"
#define M01CR_PVSM_B_ON		"CR_PVSM_B_ON"
#define M01CR_PVSM_S_DELAYH	"CR_PVSM_S_DELAYH"
#define M01CR_PVVISA_ECALON	"CR_PVVISA_ECALON"
#define M01CR_PVVISB_ECALON	"CR_PVVISB_ECALON"
#define M01CR_PVVIS_A_ON	"CR_PVVIS_A_ON"
#define M01CR_PVVIS_B_ON	"CR_PVVIS_B_ON"
#define M01CR_PVVIS_S_DELYH	"CR_PVVIS_S_DELYH"
#define M01CR_PV_A_MEM_RAM	"CR_PV_A_MEM_RAM"
#define M01CR_PV_B_MEM_RAM	"CR_PV_B_MEM_RAM"
#define M01CR_PV_ECAL_ENA_A	"CR_PV_ECAL_ENA_A"
#define M01CR_PV_ECAL_ENA_B	"CR_PV_ECAL_ENA_B"
#define M01VR_PVLW_VCALH	"VR_PVLW_VCALH"
#define M01CS_FR_PC_DCR_ON	"CS_FR_PC_DCR_ON"
#define M01CS_FR_PV_DCR_ON	"CS_FR_PV_DCR_ON"


#define M01MAJCYC5AOF7          "Telemetry Major Cycle 5A of 7"
#define M01VR_PVNIR_VCALH	"VR_PVNIR_VCALH"
#define M01VR_PVSM_VCALH	"VR_PVSM_VCALH"
#define M01VR_PVVIS_VCALH	"VR_PVVIS_VCALH"
#define M01CR_RC_CFPA_T1SET	"CR_RC_CFPA_T1SET"
#define M01CR_RC_CFPA_T3SET	"CR_RC_CFPA_T3SET"
#define M01CR_RC_CSHTR_ON	"CR_RC_CSHTR_ON"
#define M01CR_RC_CSTLM_ON	"CR_RC_CSTLM_ON"
#define M01CR_RC_ISHTR_ON	"CR_RC_ISHTR_ON"
#define M01CR_RC_ISTLM_ON	"CR_RC_ISTLM_ON"
#define M01CR_RC_LWHTR_ON	"CR_RC_LWHTR_ON"
#define M01CR_RC_LWTLM_ON	"CR_RC_LWTLM_ON"
#define M01CR_RC_OSHTR_ON	"CR_RC_OSHTR_ON"
#define M01CR_RC_OSTLM_ON	"CR_RC_OSTLM_ON"
#define M01CR_RC_SMHTR_ON	"CR_RC_SMHTR_ON"
#define M01CR_RC_SMTLM_ON	"CR_RC_SMTLM_ON"


#define M01MAJCYC5BOF7          "Telemetry Major Cycle 5B of 7"
#define M01CR_SA_A_HI_GAIN	"CR_SA_A_HI_GAIN"
#define M01CR_SA_A_SCAN_ON	"CR_SA_A_SCAN_ON"
#define M01CR_SA_B_HI_GAIN	"CR_SA_B_HI_GAIN"
#define M01CR_SA_B_SCAN_ON	"CR_SA_B_SCAN_ON"
#define M01SR_SA_A_PH_LOCK	"SR_SA_A_PH_LOCK"
#define M01SR_SA_B_PH_LOCK	"SR_SA_B_PH_LOCK"
#define M01CR_SM_SDSM_A_ON	"CR_SM_SDSM_A_ON"
#define M01CR_SM_SDSM_B_ON	"CR_SM_SDSM_B_ON"
#define M01CR_SR_A_ON		"CR_SR_A_ON"
#define M01CR_SR_B_ON		"CR_SR_B_ON"
#define M01CR_SR_SISFB_RAD	"CR_SR_SISFB_RAD"
#define M01CR_SR_L_SHDN_ENA	"CR_SR_L_SHDN_ENA"
#define M01IR_SR_10WLA_CURH	"IR_SR_10WLA_CURH"
#define M01IR_SR_10WLB_CURH	"IR_SR_10WLB_CURH"
#define M01IR_SR_1WLA_CURH	"IR_SR_1WLA_CURH"
#define M01IR_SR_1WLB_CURH	"IR_SR_1WLB_CURH"
#define M01TA_SR_IR_SRC_A	"TA_SR_IR_SRC_A"
#define M01TA_SR_IR_SRC_B	"TA_SR_IR_SRC_B"


#define M01MAJCYC6OF7           "Telemetry Major Cycle 6 of 7"
#define M01TP_SR_GRAT_ELEX	"TP_SR_GRAT_ELEX"
#define M01TP_SR_GRAT_MOTOR	"TP_SR_GRAT_MOTOR"
#define M01TP_SR_LAMP_RING	"TP_SR_LAMP_RING"
#define M01TP_SR_MIR2_DET	"TP_SR_MIR2_DET"
#define M01VR_SR_LAMPS_H	"VR_SR_LAMPS_H"
#define M01VR_SR_SRC_A_RADH	"VR_SR_SRC_A_RADH"
#define M01VR_SR_SRC_B_RADH	"VR_SR_SRC_B_RADH"
#define M01CR_TG_A_ON		"CR_TG_A_ON"
#define M01CR_TG_A_RESET	"CR_TG_A_RESET"
#define M01CR_TG_B_ON		"CR_TG_B_ON"
#define M01CR_TG_B_RESET	"CR_TG_B_RESET"
#define M01CS_FR_ENC_DELTA	"CS_FR_ENC_DELTA"
#define M01CS_SR_USE_L10WX1	"CS_SR_USE_L10WX1"
#define M01CS_SR_USE_L10WX2	"CS_SR_USE_L10WX2"
#define M01CS_SR_USE_L10WX3	"CS_SR_USE_L10WX3"
#define M01CS_SR_USE_L1WX1	"CS_SR_USE_L1WX1"


#define M01MAJCYC7OF7           "Telemetry Major Cycle 7 of 7"
#define M01TA_SR_SRC_A_SIPD	"TA_SR_SRC_A_SIPD"
#define M01TA_SR_SRC_B_SIPD	"TA_SR_SRC_B_SIPD"


#define M01MAJCYC0OF63          "Telemetry Major Cycle 0 of 63"
#define M01TP_AO_LWIR_LENS	"TP_AO_LWIR_LENS"
#define M01TP_AO_LWIR_OBJ	"TP_AO_LWIR_OBJ"
#define M01TP_AO_PX_NZ_CORN	"TP_AO_PX_NZ_CORN"
#define M01TP_AO_SMIR_LENS	"TP_AO_SMIR_LENS"
#define M01TP_AO_SMIR_OBJ	"TP_AO_SMIR_OBJ"


#define M01MAJCYC1OF63          "Telemetry Major Cycle 1 of 63"
#define M01TP_AO_VNDICH_HSG	"TP_AO_VNDICH_HSG"
#define M01TP_CE_CAL2		"TP_CE_CAL2"
#define M01TP_CP_A_1553		"TP_CP_A_1553"
#define M01TP_CP_B_1553		"TP_CP_B_1553"
#define M01VR_CP_N11V		"VR_CP_N11V"
#define M01VR_CP_N5V		"VR_CP_N5V"
#define M01VR_CP_P11V		"VR_CP_P11V"


#define M01MAJCYC2OF63          "Telemetry Major Cycle 2 of 63"
#define M01VR_CP_P5V		"VR_CP_P5V"
#define M01TP_DR_NAD		"TP_DR_NAD"
#define M01TP_DR_SDD		"TP_DR_SDD"
#define M01TP_DR_SVD		"TP_DR_SVD"
#define M01TP_FR_A_ENGINE	"TP_FR_A_ENGINE"
#define M01TP_FR_B_ENGINE	"TP_FR_B_ENGINE"
#define M01TP_ME_CHAS_TOP	"TP_ME_CHAS_TOP"


#define M01MAJCYC3OF63          "Telemetry Major Cycle 3 of 63"
#define M01TP_MF_CALBKHD_SR	"TP_MF_CALBKHD_SR"
#define M01TP_MF_CVR_OP_SR	"TP_MF_CVR_OP_SR"
#define M01TP_MF_NAD_APT_NX	"TP_MF_NAD_APT_NX"
#define M01TP_MF_NAD_APT_NY	"TP_MF_NAD_APT_NY"
#define M01TP_MF_NX_AOBKHD	"TP_MF_NX_AOBKHD"
#define M01TP_MF_PX_AOBKHD	"TP_MF_PX_AOBKHD"
#define M01TP_MF_SV_PORT	"TP_MF_SV_PORT"


#define M01MAJCYC4OF63          "Telemetry Major Cycle 4 of 63"
#define M01TP_MF_TOP_BY_KM2	"TP_MF_TOP_BY_KM2"
#define M01TP_MF_YZ_CALBKHD	"TP_MF_YZ_CALBKHD"
#define M01TP_MF_Z_BKHD_BB	"TP_MF_Z_BKHD_BB"
#define M01TA_PC_B31_MUX	"TA_PC_B31_MUX"
#define M01TA_PC_B32_MUX	"TA_PC_B32_MUX"
#define M01TA_PC_B33_MUX	"TA_PC_B33_MUX"
#define M01TA_PC_B34_MUX	"TA_PC_B34_MUX"


#define M01MAJCYC5OF63          "Telemetry Major Cycle 5 of 63"
#define M01TA_PC_B35_MUX	"TA_PC_B35_MUX"
#define M01TA_PC_B36_MUX	"TA_PC_B36_MUX"
#define M01TP_PC_CLAM_MNT	"TP_PC_CLAM_MNT"
#define M01VR_PC_B31_GND	"VR_PC_B31_GND"
#define M01VR_PC_B31_RN12V	"VR_PC_B31_RN12V"
#define M01VR_PC_B31_RN5V	"VR_PC_B31_RN5V"
#define M01VR_PC_B31_RP12V	"VR_PC_B31_RP12V"


#define M01MAJCYC6OF63          "Telemetry Major Cycle 6 of 63"
#define M01VR_PC_B31_RP5V	"VR_PC_B31_RP5V"
#define M01VR_PC_B32_GND	"VR_PC_B32_GND"
#define M01VR_PC_B32_RN12V	"VR_PC_B32_RN12V"
#define M01VR_PC_B32_RN5V	"VR_PC_B32_RN5V"
#define M01VR_PC_B32_RP12V	"VR_PC_B32_RP12V"
#define M01VR_PC_B32_RP5V	"VR_PC_B32_RP5V"
#define M01VR_PC_B33_GND	"VR_PC_B33_GND"
#define M01VR_PC_B33_RN12V	"VR_PC_B33_RN12V"


#define M01MAJCYC7OF63          "Telemetry Major Cycle 7 of 63"
#define M01VR_PC_B33_RN5V	"VR_PC_B33_RN5V"
#define M01VR_PC_B33_RP12V	"VR_PC_B33_RP12V"
#define M01VR_PC_B33_RP5V	"VR_PC_B33_RP5V"
#define M01VR_PC_B34_GND	"VR_PC_B34_GND"
#define M01VR_PC_B34_RN12V	"VR_PC_B34_RN12V"
#define M01VR_PC_B34_RN5V	"VR_PC_B34_RN5V"
#define M01VR_PC_B34_RP12V	"VR_PC_B34_RP12V"


#define M01MAJCYC8OF63          "Telemetry Major Cycle 8 of 63"
#define M01VR_PC_B34_RP5V	"VR_PC_B34_RP5V"
#define M01VR_PC_B35_GND	"VR_PC_B35_GND"
#define M01VR_PC_B35_RN12V	"VR_PC_B35_RN12V"
#define M01VR_PC_B35_RN5V	"VR_PC_B35_RN5V"
#define M01VR_PC_B35_RP12V	"VR_PC_B35_RP12V"
#define M01VR_PC_B35_RP5V	"VR_PC_B35_RP5V"
#define M01VR_PC_B36_GND	"VR_PC_B36_GND"


#define M01MAJCYC9OF63          "Telemetry Major Cycle 9 of 63"
#define M01VR_PC_B36_RN12V	"VR_PC_B36_RN12V"
#define M01VR_PC_B36_RN5V	"VR_PC_B36_RN5V"
#define M01VR_PC_B36_RP12V	"VR_PC_B36_RP12V"
#define M01VR_PC_B36_RP5V	"VR_PC_B36_RP5V"
#define M01TP_PS1_CVTR_SW	"TP_PS1_CVTR_SW"
#define M01TP_PS1_DIODE_OUT	"TP_PS1_DIODE_OUT"
#define M01TP_PS1_DWNREG_SW	"TP_PS1_DWNREG_SW"


#define M01MAJCYC10OF63         "Telemetry Major Cycle 10 of 63"
#define M01TP_PS1_PRELOAD	"TP_PS1_PRELOAD"
#define M01TP_PS2_CVTR_SW	"TP_PS2_CVTR_SW"
#define M01TP_PS2_DIODE_OUT	"TP_PS2_DIODE_OUT"
#define M01TP_PS2_DWNREG_SW	"TP_PS2_DWNREG_SW"
#define M01TP_PS2_PRELOAD	"TP_PS2_PRELOAD"
#define M01VR_PS1_N15V_A1ME	"VR_PS1_N15V_A1ME"
#define M01VR_PS1_N15V_A2AF	"VR_PS1_N15V_A2AF"
#define M01CS_CP_MODIS_MOD	"CS_CP_MODIS_MOD"


#define M01MAJCYC11OF63         "Telemetry Major Cycle 11 of 63"
#define M01VR_PS1_N15V_A3AS	"VR_PS1_N15V_A3AS"
#define M01VR_PS1_N30V_A1ME	"VR_PS1_N30V_A1ME"
#define M01VR_PS1_N8V_A2	"VR_PS1_N8V_A2"
#define M01VR_PS1_P15V_A1ME	"VR_PS1_P15V_A1ME"
#define M01VR_PS1_P15V_A2AF	"VR_PS1_P15V_A2AF"
#define M01VR_PS1_P15V_A3AS	"VR_PS1_P15V_A3AS"
#define M01VR_PS1_P30V_A1	"VR_PS1_P30V_A1"
#define M01VR_PS1_P5_6V_D1	"VR_PS1_P5_6V_D1"


#define M01MAJCYC12OF63         "Telemetry Major Cycle 12 of 63"
#define M01VR_PS1_P88V_A1ME	"VR_PS1_P88V_A1ME"
#define M01VR_PS1_P8V_A2	"VR_PS1_P8V_A2"
#define M01VR_PS1_P8V_N1ME	"VR_PS1_P8V_N1ME"
#define M01VR_PS2_N15V_A1ME	"VR_PS2_N15V_A1ME"
#define M01VR_PS2_N15V_A2AF	"VR_PS2_N15V_A2AF"
#define M01VR_PS2_N15V_A3AS	"VR_PS2_N15V_A3AS"
#define M01VR_PS2_N30V_A1ME	"VR_PS2_N30V_A1ME"
#define M01VR_PS2_N8V_A2	"VR_PS2_N8V_A2"


#define M01MAJCYC13OF63         "Telemetry Major Cycle 13 of 63"
#define M01VR_PS2_P15V_A1ME	"VR_PS2_P15V_A1ME"
#define M01VR_PS2_P15V_A2AF	"VR_PS2_P15V_A2AF"
#define M01VR_PS2_P15V_A3AS	"VR_PS2_P15V_A3AS"
#define M01VR_PS2_P30V_A1	"VR_PS2_P30V_A1"
#define M01VR_PS2_P5_6V_D1	"VR_PS2_P5_6V_D1"
#define M01VR_PS2_P88V_A1ME	"VR_PS2_P88V_A1ME"
#define M01VR_PS2_P8V_A2	"VR_PS2_P8V_A2"
#define M01VR_PS2_P8V_N1ME	"VR_PS2_P8V_N1ME"


#define M01MAJCYC14OF63         "Telemetry Major Cycle 14 of 63"
#define M01TA_PVLW_PWB4_10	"TA_PVLW_PWB4_10"
#define M01TA_PVNIR_PWB2_8	"TA_PVNIR_PWB2_8"
#define M01TA_PVNIR_PWB3_9	"TA_PVNIR_PWB3_9"
#define M01TA_PVSM_PWB5_11	"TA_PVSM_PWB5_11"
#define M01TA_PVSM_PWB6_12	"TA_PVSM_PWB6_12"
#define M01TA_PVVIS_PWB1_7	"TA_PVVIS_PWB1_7"
#define M01VR_PVLW_P30V		"VR_PVLW_P30V"


#define M01MAJCYC15OF63         "Telemetry Major Cycle 15 of 63"
#define M01VR_PVLW_RN11V	"VR_PVLW_RN11V"
#define M01VR_PVLW_RN5V		"VR_PVLW_RN5V"
#define M01VR_PVLW_RP11V	"VR_PVLW_RP11V"
#define M01VR_PVLW_RP5V		"VR_PVLW_RP5V"
#define M01VR_PVNIR_P30V	"VR_PVNIR_P30V"
#define M01VR_PVNIR_P5VD3_9	"VR_PVNIR_P5VD3_9"
#define M01VR_PVNIR_RN11V28	"VR_PVNIR_RN11V28"
#define M01VR_PVNIR_RN11V39	"VR_PVNIR_RN11V39"


#define M01MAJCYC16OF63         "Telemetry Major Cycle 16 of 63"
#define M01VR_PVNIR_RN5V2_8	"VR_PVNIR_RN5V2_8"
#define M01VR_PVNIR_RN5V3_9	"VR_PVNIR_RN5V3_9"
#define M01VR_PVNIR_RP11V28	"VR_PVNIR_RP11V28"
#define M01VR_PVNIR_RP11V39	"VR_PVNIR_RP11V39"
#define M01VR_PVNIR_RP5V2_8	"VR_PVNIR_RP5V2_8"
#define M01VR_PVNIR_RP5V3_9	"VR_PVNIR_RP5V3_9"
#define M01VR_PVSM_P30V		"VR_PVSM_P30V"
#define M01VR_PVSM_P5VD6_12	"VR_PVSM_P5VD6_12"


#define M01MAJCYC17OF63         "Telemetry Major Cycle 17 of 63"
#define M01VR_PVSM_RN11V511	"VR_PVSM_RN11V511"
#define M01VR_PVSM_RN11V612	"VR_PVSM_RN11V612"
#define M01VR_PVSM_RN5V5_11	"VR_PVSM_RN5V5_11"
#define M01VR_PVSM_RN5V6_12	"VR_PVSM_RN5V6_12"
#define M01VR_PVSM_RP11V511	"VR_PVSM_RP11V511"
#define M01VR_PVSM_RP11V612	"VR_PVSM_RP11V612"
#define M01VR_PVSM_RP5V5_11	"VR_PVSM_RP5V5_11"
#define M01VR_PVSM_RP5V6_12	"VR_PVSM_RP5V6_12"


#define M01MAJCYC18OF63         "Telemetry Major Cycle 18 of 63"
#define M01VR_PVVIS_P30V	"VR_PVVIS_P30V"
#define M01VR_PVVIS_RN11V	"VR_PVVIS_RN11V"
#define M01VR_PVVIS_RN5V	"VR_PVVIS_RN5V"
#define M01VR_PVVIS_RP11V	"VR_PVVIS_RP11V"
#define M01VR_PVVIS_RP5V	"VR_PVVIS_RP5V"
#define M01TA_RC_CS		"TA_RC_CS"
#define M01TA_RC_CS_OG		"TA_RC_CS_OG"


#define M01MAJCYC19OF63         "Telemetry Major Cycle 19 of 63"
#define M01TA_RC_IS		"TA_RC_IS"
#define M01TA_RC_IS_OG		"TA_RC_IS_OG"
#define M01TA_RC_OS_OG		"TA_RC_OS_OG"
#define M01TP_RC_MNT_RING	"TP_RC_MNT_RING"
#define M01TP_RC_SPARE		"TP_RC_SPARE"


#define M01MAJCYC20OF63         "Telemetry Major Cycle 20 of 63"
#define M01TP_RC_SPARE		"TP_RC_SPARE"
#define M01SA_SPARE_1		"SA_SPARE_1"
#define M01SA_SPARE_2		"SA_SPARE_2"
#define M01TP_SA_A_MTR		"TP_SA_A_MTR"
#define M01SS_SA_SPARE		"SS_SA_SPARE"
#define M01TP_SA_SPARE		"TP_SA_SPARE"


#define M01MAJCYC21OF63         "Telemetry Major Cycle 21 of 63"
#define M01TP_SA_RCT1_MIR	"TP_SA_RCT1_MIR"
#define M01TP_SA_SPARE		"TP_SA_SPARE"
#define M01TP_SA_RCT2_MIR	"TP_SA_RCT2_MIR"
#define M01SA_SPARE		"SA_SPARE"
#define M01VR_SA_A_MTR_TORQ	"VR_SA_A_MTR_TORQ"
#define M01VR_SA_A_RN11V	"VR_SA_A_RN11V"


#define M01MAJCYC22OF63         "Telemetry Major Cycle 22 of 63"
#define M01VR_SA_A_RP11V	"VR_SA_A_RP11V"
#define M01SA_SPARE		"SA_SPARE"
#define M01VR_SA_B_MTR_TORQ	"VR_SA_B_MTR_TORQ"
#define M01VR_SA_B_RN11V	"VR_SA_B_RN11V"
#define M01VR_SA_B_RP11V	"VR_SA_B_RP11V"
#define M01TP_SD_SPARE		"TP_SD_SPARE"
#define M01TP_SM_DET_AMP3	"TP_SM_DET_AMP3"


#define M01MAJCYC23OF63         "Telemetry Major Cycle 23 of 63"
#define M01TP_SR_MONO_CHAS1	"TP_SR_MONO_CHAS1"
#define M01TP_SR_MONO_CHAS2	"TP_SR_MONO_CHAS2"
#define M01TP_SR_SNOUT		"TP_SR_SNOUT"
#define M01TP_SS_SPARE		"TP_SS_SPARE"
#define M01VR_TC_CSCKT_PV	"VR_TC_CSCKT_PV"
#define M01VR_TC_ISCKT_PV	"VR_TC_ISCKT_PV"
#define M01VR_TC_LWCKT_NV	"VR_TC_LWCKT_NV"


#define M01MAJCYC24OF63         "Telemetry Major Cycle 24 of 63"
#define M01VR_TC_LWCKT_PV	"VR_TC_LWCKT_PV"
#define M01SS_TC_SPARE_1	"SS_TC_SPARE_1"
#define M01VR_TC_OSCKT_PV	"VR_TC_OSCKT_PV"
#define M01VR_TC_SMCKT_NV	"VR_TC_SMCKT_NV"
#define M01VR_TC_SMCKT_PV	"VR_TC_SMCKT_PV"
#define M01SS_TC_SPARE_2	"SS_TC_SPARE_2"
#define M01VR_TC_VISCKT_NV	"VR_TC_VISCKT_NV"
#define M01VR_TC_VISCKT_PV	"VR_TC_VISCKT_PV"


#define M01MAJCYC25OF63         "Telemetry Major Cycle 25 of 63"
#define M01TP_TE_FOLD_MIR	"TP_TE_FOLD_MIR"
#define M01TP_TE_PRI_MIR	"TP_TE_PRI_MIR"
#define M01TP_TE_SEC_MIR	"TP_TE_SEC_MIR"
#define M01TP_TM_ANLG_CKT	"TP_TM_ANLG_CKT"
#define M01VR_TM_REF_ACT1_1	"VR_TM_REF_ACT1_1"
#define M01VR_TM_REF_ACT1_2	"VR_TM_REF_ACT1_2"


#define M01MAJCYC26OF63         "Telemetry Major Cycle 26 of 63"
#define M01VR_TM_REF_ACT1_3	"VR_TM_REF_ACT1_3"
#define M01VR_TM_REF_ACT2_1	"VR_TM_REF_ACT2_1"
#define M01VR_TM_REF_ACT2_2	"VR_TM_REF_ACT2_2"
#define M01VR_TM_REF_ACT3_1	"VR_TM_REF_ACT3_1"
#define M01VR_TM_REF_ACT4_1	"VR_TM_REF_ACT4_1"


#define M01MAJCYC27OF63         "Telemetry Major Cycle 27 of 63"
#define M01VR_TM_REF_ACT5_1	"VR_TM_REF_ACT5_1"
#define M01VR_TM_REF_ACT5_2	"VR_TM_REF_ACT5_2"
#define M01VR_TM_REF_ACT5_3	"VR_TM_REF_ACT5_3"
#define M01VR_TM_REF_ACT6_1	"VR_TM_REF_ACT6_1"
#define M01VR_TM_REF_ACT6_3	"VR_TM_REF_ACT6_3"


#define M01MAJCYC28OF63         "Telemetry Major Cycle 28 of 63"
#define M01VR_TM_REF_ACT7_1	"VR_TM_REF_ACT7_1"
#define M01VR_TM_REF_BB_1	"VR_TM_REF_BB_1"
#define M01VR_TM_REF_BB_2	"VR_TM_REF_BB_2"
#define M01VR_TM_REF_BB_3	"VR_TM_REF_BB_3"
#define M01VR_TM_REF_PRT1	"VR_TM_REF_PRT1"


#define M01MAJCYC29OF63         "Telemetry Major Cycle 29 of 63"
#define M01VR_TM_REF_PSV1	"VR_TM_REF_PSV1"
#define M01VR_TM_REF_PSV2	"VR_TM_REF_PSV2"
#define M01VR_TM_REF_PSV3	"VR_TM_REF_PSV3"
#define M01VR_TM_REF_PSV4	"VR_TM_REF_PSV4"
#define M01VR_TM_REF_PSV5	"VR_TM_REF_PSV5"


#define M01MAJCYC30OF63         "Telemetry Major Cycle 30 of 63"
#define M01VR_TM_REF_ACTGND	"VR_TM_REF_ACTGND"
#define M01TA_AO_VIS_FPA	"TA_AO_VIS_FPA"
#define M01TA_AO_NIR_FPA	"TA_AO_NIR_FPA"
#define M01VR_RC_LW_FPA_HTR	"VR_RC_LW_FPA_HTR"
#define M01VR_RC_SM_FPA_HTR	"VR_RC_SM_FPA_HTR"


#define M01MAJCYC31OF63         "Telemetry Major Cycle 31 of 63"
#define M01VR_TM_REF_PRT2	"VR_TM_REF_PRT2"
#define M01VR_TM_REF_PSV6	"VR_TM_REF_PSV6"
#define M01VR_TM_REF_PSV7	"VR_TM_REF_PSV7"
#define M01VR_TM_REF_PSV8	"VR_TM_REF_PSV8"


#define M01MAJCYC32OF63         "Telemetry Major Cycle 32 of 63"
#define M01IR_SA_A_ECDR_LED	"IR_SA_A_ECDR_LED"
#define M01IR_SA_B_ECDR_LED	"IR_SA_B_ECDR_LED"
#define M01VR_SA_A_ECDR_MON	"VR_SA_A_ECDR_MON"
#define M01VR_SA_B_ECDR_MON	"VR_SA_B_ECDR_MON"


#define M01CURR_SC_ANCIL_DATA		"Current S/C Ancillary Data"
#define M01PACKET_HEADER		"PACKET_HEADER"
#define M01TIME_STAMP			"TIME_STAMP"
#define M01FLAG_BYTE			"FLAG_BYTE"
#define M01TIME_CONVERSION		"TIME_CONVERSION"

#define M01SC_POSITION_X		"S/C_POSITION_X"
#define M01SC_POSITION_Y		"S/C_POSITION_Y"
#define M01SC_POSITION_Z		"S/C_POSITION_Z"
#define M01SC_VELOCITY_X		"S/C_VELOCITY_X"
#define M01SC_VELOCITY_Y		"S/C_VELOCITY_Y"
#define M01SC_VELOCITY_Z		"S/C_VELOCITY_Z"

#define M01RESERVED_ANGLE_ROLL		"RESERVED_ANGLE_ROLL"
#define M01ATTITUDE_ANGLE_ROLL		"ATTITUDE_ANGLE_ROLL"
#define M01RESERVED_ANGLE_PITCH		"RESERVED_ANGLE_PITCH"
#define M01ATTITUDE_ANGLE_PITCH		"ATTITUDE_ANGLE_PITCH"
#define M01RESERVED_ANGLE_YAW		"RESERVED_ANGLE_YAW"
#define M01ATTITUDE_ANGLE_YAW		"ATTITUDE_ANGLE_YAW"
#define M01RESERVED_RATE_ROLL		"RESERVED_RATE_ROLL"
#define M01ATTITUDE_RATE_ROLL		"ATTITUDE_RATE_ROLL"
#define M01RESERVED_RATE_PITCH		"RESERVED_RATE_PITCH"
#define M01ATTITUDE_RATE_PITCH		"ATTITUDE_RATE_PITCH"
#define M01RESERVED_RATE_YAW		"RESERVED_RATE_YAW"
#define M01ATTITUDE_RATE_YAW		"ATTITUDE_RATE_YAW"

#define M01MAGNETIC_COIL_CURRENT_X	"MAGNETIC_COIL_CURRENT_X"
#define M01MAGNETIC_COIL_CURRENT_Y	"MAGNETIC_COIL_CURRENT_Y"
#define M01MAGNETIC_COIL_CURRENT_Z	"MAGNETIC_COIL_CURRENT_Z"
#define M01SOLAR_ARRAY_CURRENT		"SOLAR_ARRAY_CURRENT"
#define M01SOLAR_POSITION_X		"SOLAR_POSITION_X"
#define M01SOLAR_POSITION_Y		"SOLAR_POSITION_Y"
#define M01SOLAR_POSITION_Z		"SOLAR_POSITION_Z"
#define M01MOON_POSITION_X		"MOON_POSITION_X"
#define M01MOON_POSITION_Y		"MOON_POSITION_Y"
#define M01MOON_POSITION_Z		"MOON_POSITION_Z"


#define M01PRIOR_SC_ANCIL_DATA		"Prior S/C Ancillary Data"


#define M01COMMAND_PARAM		"Command Parameters"
#define M01SET_BB_HTR_TEMP		"SET_BB_HTR_TEMP"
#define M01SET_CP_OPER_MODE		"SET_CP_OPER_MODE"
#define M01SET_FR_RATE			"SET_FR_RATE"
#define M01SET_FR_SCI_APID		"SET_FR_SCI_APID"
#define M01SET_SR_L10WX1		"SET_SR_L10WX1"
#define M01SET_FR_PKT_TYPE		"SET_FR_PKT_TYPE"

#define M01SET_PVVIS_VCAL		"SET_PVVIS_VCAL"
#define M01SET_PVNIR_VCAL		"SET_PVNIR_VCAL"
#define M01SET_PVSM_ITWK_V		"SET_PVSM_ITWK_V"
#define M01SET_PVLW_ITWK_V		"SET_PVLW_ITWK_V"
#define M01SET_PVSM_VDET_V		"SET_PVSM_VDET_V"
#define M01SET_PVLW_VDET_V		"SET_PVSM_VDET_V"

#define M01SET_FR_ENG_APID		"SET_FR_ENG_APID"
#define M01SET_FR_L10WX2		"SET_FR_L10WX2"
#define M01ENABLE_CP_IMOK		"ENABLE_CP_IMOK"
#define M01SET_FR_BBRADTAB		"SET_FR_BBRADTAB"
#define M01SET_FR_OFFSETTAB		"SET_FR_OFFSETTAB"
#define M01SET_FR_GAIN_TAB		"SET_FR_GAIN_TAB"
#define M01TEST_FR_BBRAD		"TEST_FR_BBRAD"
#define M01SET_FR_SCI_QLK		"SET_FR_SCI_QLK"
#define M01SET_FR_SR_DELAY		"SET_FR_SR_DELAY"
#define M01SET_FR_BB_DELAY		"SET_FR_BB_DELAY"
#define M01SET_CP_TMF_BUS		"SET_CP_TMF_BUS"
#define M01OPEN_DR_UL_LOCK		"OPEN_DR_UL_LOCK"
#define M01SET_FR_SD_DELAY		"SET_FR_SD_DELAY"
#define M01SET_FR_SP_DELAY		"SET_FR_SP_DELAY"
#define M01SET_PV_MEM			"SET_PV_MEM"
#define M01SET_FR_ENG_QLK		"SET_FR_ENG_QLK"
#define M01CP_SPARE			"CP_SPARE"
#define M01SET_SR_3L10W			"SET_SR_3L10W"
#define M01SET_FR_ENC_DELTA		"SET_FR_ENC_DELTA"
#define M01SET_PVSMIR_ECAL		"SET_PVSMIR_ECAL"
#define M01SET_PVLW_ECAL		"SET_PVLW_ECAL"

#define M01TEST_FR_PCOFFSET		"TEST_FR_PCOFFSET"
#define M01TEST_FR_PVOFFSET		"TEST_FR_PVOFFSET"
#define M01TEST_FR_PVGAIN		"TEST_FR_PVGAIN"

#define M01SET_PVVIS_NSTEP		"SET_PVVIS_NSTEP"
#define M01SET_PVNIR_NSTEP		"SET_PVNIR_NSTEP"
#define M01SET_PVSM_NSTEP		"SET_PVSM_NSTEP"
#define M01SET_PVLW_NSTEP		"SET_PVLW_NSTEP"

#define M01SET_FR_EA_DELAY		"SET_FR_EA_DELAY"
#define M01SET_PVSMIR_CSUB		"SET_PVSMIR_CSUB"
#define M01SET_PVLW_CSUB		"SET_PVLW_CSUB"
#define M01SET_DR_SVD_UL		"SET_DR_SVD_UL"
#define M01SET_DR_NAD_UL		"SET_DR_NAD_UL"
#define M01SET_DR_SDD_UL		"SET_DR_SDD_UL"
#define M01SET_DR_SDD_FS		"SET_DR_SDD_FS"
#define M01SET_FR_PV_DCRCMP		"SET_FR_PV_DCRCMP"
#define M01SET_FR_PC_DCRCMP		"SET_FR_PC_DCRCMP"
#define M01FR_SPARE_1			"FR_SPARE_1"
#define M01FR_SPARE_2			"FR_SPARE_2"
#define M01SET_SR_SIPD_HTR		"SET_SR_SIPD_HTR"
#define M01SET_SR_SIS_FB		"SET_SR_SIS_FB"
#define M01SET_SR_LOV_SHDN		"SET_SR_LOV_SHDN"
#define M01SET_SR_LAMPLEVEL		"SET_SR_LAMPLEVEL"
#define M01SET_SR_LAMPS			"SET_SR_LAMPS"
#define M01SET_CP_LOGSTATE		"SET_CP_LOGSTATE"
#define M01SET_FR_LOGSTATE		"SET_FR_LOGSTATE"
#define M01SET_SR_IR_SRC		"SET_SR_IR_SRC"
#define M01FR_SPARE_3			"FR_SPARE_3"
#define M01SET_SR_LIWX1			"SET_SR_L1WX1"
#define M01SET_PVVIS_ECAL		"SET_PVVIS_ECAL"
#define M01SET_PVNIR_ECAL		"SET_PVNIR_ECAL"
#define M01SET_FR_SCIABNORM		"SET_FR_SCIABNORM"
#define M01FILL_BITS			"FILL_BITS"



#define M01BB_MEASURES		"Engineering BB data"
#define M01IR_BB_HTRA_CURR	"IR_BB_HTRA_CURR"
#define M01IR_BB_HTRB_CURR	"IR_BB_HTRB_CURR"
#define M01TP_BB_TEMP01		"TP_BB_TEMP01"
#define M01TP_BB_TEMP02		"TP_BB_TEMP02"
#define M01TP_BB_TEMP03		"TP_BB_TEMP03"
#define M01TP_BB_TEMP04		"TP_BB_TEMP04"
#define M01TP_BB_TEMP05		"TP_BB_TEMP05"
#define M01TP_BB_TEMP06		"TP_BB_TEMP06"
#define M01TP_BB_TEMP07		"TP_BB_TEMP07"
#define M01TP_BB_TEMP08		"TP_BB_TEMP08"
#define M01TP_BB_TEMP10		"TP_BB_TEMP10"
#define M01TP_BB_TEMP12		"TP_BB_TEMP12"
#define M01TP_BB_TEMP_AVG	"TP_BB_TEMP_AVG"


#define M01CP_MEASURES          "Engineering CP valid format"
#define M01SS_CP_VALENG_FMT	"SS_CP_VALENG_FMT"


#define M01FAM_AF01_MUX_DCR	"Engineering FAM AF01 mux"
#define M01VR_PC_B31C10_DCR	"VR_PC_B31C10_DCR"
#define M01VR_PC_B31C09_DCR	"VR_PC_B31C09_DCR"
#define M01VR_PC_B31C08_DCR	"VR_PC_B31C08_DCR"
#define M01VR_PC_B31C07_DCR	"VR_PC_B31C07_DCR"
#define M01VR_PC_B31C06_DCR	"VR_PC_B31C06_DCR"
#define M01VR_PC_B31C05_DCR	"VR_PC_B31C05_DCR"
#define M01VR_PC_B31C04_DCR	"VR_PC_B31C04_DCR"
#define M01VR_PC_B31C03_DCR	"VR_PC_B31C03_DCR"
#define M01VR_PC_B31C02_DCR	"VR_PC_B31C02_DCR"
#define M01VR_PC_B31C01_DCR	"VR_PC_B31C01_DCR"


#define M01FAM_AF02_MUX_DCR     "Engineering FAM AF02 mux"
#define M01VR_PC_B32C10_DCR	"VR_PC_B32C10_DCR"
#define M01VR_PC_B32C09_DCR	"VR_PC_B32C09_DCR"
#define M01VR_PC_B32C08_DCR	"VR_PC_B32C08_DCR"
#define M01VR_PC_B32C07_DCR	"VR_PC_B32C07_DCR"
#define M01VR_PC_B32C06_DCR	"VR_PC_B32C06_DCR"
#define M01VR_PC_B32C05_DCR	"VR_PC_B32C05_DCR"
#define M01VR_PC_B32C04_DCR	"VR_PC_B32C04_DCR"
#define M01VR_PC_B32C03_DCR	"VR_PC_B32C03_DCR"
#define M01VR_PC_B32C02_DCR	"VR_PC_B32C02_DCR"
#define M01VR_PC_B32C01_DCR	"VR_PC_B32C01_DCR"


#define M01FAM_AF03_MUX_DCR     "Engineering FAM AF03 mux"
#define M01VR_PC_B33C10_DCR	"VR_PC_B33C10_DCR"
#define M01VR_PC_B33C09_DCR	"VR_PC_B33C09_DCR"
#define M01VR_PC_B33C08_DCR	"VR_PC_B33C08_DCR"
#define M01VR_PC_B33C06_DCR	"VR_PC_B33C06_DCR"
#define M01VR_PC_B33C04_DCR	"VR_PC_B33C04_DCR"
#define M01VR_PC_B33C03_DCR	"VR_PC_B33C03_DCR"
#define M01VR_PC_B33C02_DCR	"VR_PC_B33C02_DCR"
#define M01VR_PC_B33C01_DCR	"VR_PC_B33C01_DCR"


#define M01FAM_AF04_MUX_DCR     "Engineering FAM AF04 mux"
#define M01VR_PC_B34C10_DCR	"VR_PC_B34C10_DCR"
#define M01VR_PC_B34C09_DCR	"VR_PC_B34C09_DCR"
#define M01VR_PC_B34C08_DCR	"VR_PC_B34C08_DCR"
#define M01VR_PC_B34C07_DCR	"VR_PC_B34C07_DCR"
#define M01VR_PC_B34C06_DCR	"VR_PC_B34C06_DCR"
#define M01VR_PC_B34C05_DCR	"VR_PC_B34C05_DCR"
#define M01VR_PC_B34C04_DCR	"VR_PC_B34C04_DCR"
#define M01VR_PC_B34C03_DCR	"VR_PC_B34C03_DCR"
#define M01VR_PC_B34C02_DCR	"VR_PC_B34C02_DCR"
#define M01VR_PC_B34C01_DCR	"VR_PC_B34C01_DCR"


#define M01FAM_AF05_MUX_DCR     "Engineering FAM AF05 mux"
#define M01VR_PC_B35C10_DCR	"VR_PC_B35C10_DCR"
#define M01VR_PC_B35C09_DCR	"VR_PC_B35C09_DCR"
#define M01VR_PC_B35C08_DCR	"VR_PC_B35C08_DCR"
#define M01VR_PC_B35C07_DCR	"VR_PC_B35C07_DCR"
#define M01VR_PC_B35C06_DCR	"VR_PC_B35C06_DCR"
#define M01VR_PC_B35C05_DCR	"VR_PC_B35C05_DCR"
#define M01VR_PC_B35C04_DCR	"VR_PC_B35C04_DCR"
#define M01VR_PC_B35C03_DCR	"VR_PC_B35C03_DCR"
#define M01VR_PC_B35C02_DCR	"VR_PC_B35C02_DCR"
#define M01VR_PC_B35C01_DCR	"VR_PC_B35C01_DCR"


#define M01FAM_AF06_MUX_DCR     "Engineering FAM AF06 mux"
#define M01VR_PC_B36C10_DCR	"VR_PC_B36C10_DCR"
#define M01VR_PC_B36C09_DCR	"VR_PC_B36C09_DCR"
#define M01VR_PC_B36C08_DCR	"VR_PC_B36C08_DCR"
#define M01VR_PC_B36C07_DCR	"VR_PC_B36C07_DCR"
#define M01VR_PC_B36C06_DCR	"VR_PC_B36C06_DCR"
#define M01VR_PC_B36C05_DCR	"VR_PC_B36C05_DCR"
#define M01VR_PC_B36C04_DCR	"VR_PC_B36C04_DCR"
#define M01VR_PC_B36C03_DCR	"VR_PC_B36C03_DCR"
#define M01VR_PC_B36C02_DCR	"VR_PC_B36C02_DCR"
#define M01VR_PC_B36C01_DCR	"VR_PC_B36C01_DCR"


#define M01REG_S_DELAY		"Engineering Reg Sample Delay"
#define M01CR_PVLW_S_DELAY	"CR_PVLW_S_DELAY"
#define M01CR_PVNIR_S_DELAY	"CR_PVNIR_S_DELAY"
#define M01CR_PVSM_S_DELAY	"CR_PVSM_S_DELAY"
#define M01CR_PVVIS_S_DELAY	"CR_PVVIS_S_DELAY"


#define M01LWIR_MEASURES	"Engineering LWIR data"
#define M01VR_PVLW_ITWKA	"VR_PVLW_ITWKA"
#define M01VR_PVLW_VCAL		"VR_PVLW_VCAL"
#define M01VR_PVLW_VDDA		"VR_PVLW_VDDA"
#define M01VR_PVLW_VDDD		"VR_PVLW_VDDD"
#define M01VR_PVLW_VDDOUT	"VR_PVLW_VDDOUT"
#define M01VR_PVLW_VDET		"VR_PVLW_VDET"
#define M01VR_PVLW_VPWELL	"VR_PVLW_VPWELL"


#define M01NIR_MEASURES         "Engineering NIR data"
#define M01VR_PVNIR_ITWKA	"VR_PVNIR_ITWKA"
#define M01VR_PVNIR_VCAL	"VR_PVNIR_VCAL"
#define M01VR_PVNIR_VD1		"VR_PVNIR_VD1"
#define M01VR_PVNIR_VDDA	"VR_PVNIR_VDDA"
#define M01VR_PVNIR_VDDD	"VR_PVNIR_VDDD"
#define M01VR_PVNIR_VDDOUT	"VR_PVNIR_VDDOUT"
#define M01VR_PVNIR_VDET	"VR_PVNIR_VDET"
#define M01VR_PVNIR_VGUARD	"VR_PVNIR_VGUARD"
#define M01VR_PVNIR_VPWELL	"VR_PVNIR_VPWELL"


#define M01SMIR_MEASURES        "Engineering SMIR data"
#define M01VR_PVSM_ITWKA	"VR_PVSM_ITWKA"
#define M01VR_PVSM_VCAL		"VR_PVSM_VCAL"
#define M01VR_PVSM_VDDA		"VR_PVSM_VDDA"
#define M01VR_PVSM_VDDD		"VR_PVSM_VDDD"
#define M01VR_PVSM_VDDOUT	"VR_PVSM_VDDOUT"
#define M01VR_PVSM_VDET		"VR_PVSM_VDET"
#define M01VR_PVSM_VPWELL	"VR_PVSM_VPWELL"


#define M01VIS_MEASURES         "Engineering VIS data"
#define M01VR_PVVIS_ITWKA	"VR_PVVIS_ITWKA"
#define M01VR_PVVIS_VCAL	"VR_PVVIS_VCAL"
#define M01VR_PVVIS_VD1		"VR_PVVIS_VD1"
#define M01VR_PVVIS_VDDA	"VR_PVVIS_VDDA"
#define M01VR_PVVIS_VDDD	"VR_PVVIS_VDDD"
#define M01VR_PVVIS_VDDOUT	"VR_PVVIS_VDDOUT"
#define M01VR_PVVIS_VDET	"VR_PVVIS_VDET"
#define M01VR_PVVIS_VGUARD	"VR_PVVIS_VGUARD"
#define M01VR_PVVIS_VPWELL	"VR_PVVIS_VPWELL"


#define M01VIEW_SMPL		"Engineering View Sample"
#define M01SS_SM_VIEW_SMPL1	"SS_SM_VIEW_SMPL1"
#define M01SS_SM_VIEW_SMPL2	"SS_SM_VIEW_SMPL2"
#define M01SS_SM_VIEW_SMPL3	"SS_SM_VIEW_SMPL3"


#define M01SDSM_MEASURES        "Engineering SDSM data"
#define M01VR_SM01_SMPL1	"VR_SM01_SMPL1"
#define M01VR_SM01_SMPL2	"VR_SM01_SMPL2"
#define M01VR_SM01_SMPL3	"VR_SM01_SMPL3"
#define M01VR_SM02_SMPL1	"VR_SM02_SMPL1"
#define M01VR_SM02_SMPL2	"VR_SM02_SMPL2"
#define M01VR_SM02_SMPL3	"VR_SM02_SMPL3"
#define M01VR_SM03_SMPL1	"VR_SM03_SMPL1"
#define M01VR_SM03_SMPL2	"VR_SM03_SMPL2"
#define M01VR_SM03_SMPL3	"VR_SM03_SMPL3"
#define M01VR_SM04_SMPL1	"VR_SM04_SMPL1"
#define M01VR_SM04_SMPL2	"VR_SM04_SMPL2"
#define M01VR_SM04_SMPL3	"VR_SM04_SMPL3"
#define M01VR_SM05_SMPL1	"VR_SM05_SMPL1"
#define M01VR_SM05_SMPL2	"VR_SM05_SMPL2"
#define M01VR_SM05_SMPL3	"VR_SM05_SMPL3"
#define M01VR_SM06_SMPL1	"VR_SM06_SMPL1"
#define M01VR_SM06_SMPL2	"VR_SM06_SMPL2"
#define M01VR_SM06_SMPL3	"VR_SM06_SMPL3"
#define M01VR_SM07_SMPL1	"VR_SM07_SMPL1"
#define M01VR_SM07_SMPL2	"VR_SM07_SMPL2"
#define M01VR_SM07_SMPL3	"VR_SM07_SMPL3"
#define M01VR_SM08_SMPL1	"VR_SM08_SMPL1"
#define M01VR_SM08_SMPL2	"VR_SM08_SMPL2"
#define M01VR_SM08_SMPL3	"VR_SM08_SMPL3"
#define M01VR_SM09_SMPL1	"VR_SM09_SMPL1"
#define M01VR_SM09_SMPL2	"VR_SM09_SMPL2"
#define M01VR_SM09_SMPL3	"VR_SM09_SMPL3"


#define M01SRCA_MEASURES        "Engineering SRCA data"
#define M01CS_SR_LAMP_USE	"CS_SR_LAMP_USE"
#define M01IR_SR_10WLA_CURR	"IR_SR_10WLA_CURR"
#define M01IR_SR_10WLB_CURR	"IR_SR_10WLB_CURR"
#define M01IR_SR_1WLA_CURR	"IR_SR_1WLA_CURR"
#define M01IR_SR_1WLB_CURR	"IR_SR_1WLB_CURR"
#define M01VR_SR_LAMPS		"VR_SR_LAMPS"
#define M01VR_SR_SELF_CAL1	"VR_SR_SELF_CAL1"
#define M01VR_SR_SELF_CAL2	"VR_SR_SELF_CAL2"
#define M01VR_SR_SELF_CAL3	"VR_SR_SELF_CAL3"
#define M01VR_SR_SPCT_NORM1	"VR_SR_SPCT_NORM1"
#define M01VR_SR_SPCT_NORM2	"VR_SR_SPCT_NORM2"
#define M01VR_SR_SPCT_NORM3	"VR_SR_SPCT_NORM3"
#define M01VR_SR_SRC_A_RAD	"VR_SR_SRC_A_RAD"
#define M01VR_SR_SRC_B_RAD	"VR_SR_SRC_B_RAD"
#define M01CR_SR_A_ONE		"CR_SR_A_ONE"
#define M01CR_SR_B_ONE		"CR_SR_B_ONE"
#define M01CR_SR_GRAT_CH_AE	"CR_SR_GRAT_CH_AE"
#define M01CR_SR_GRAT_CH_BE	"CR_SR_GRAT_CH_BE"
#define M01CR_SR_GRAT_FH_AE	"CR_SR_GRAT_FH_AE"
#define M01CR_SR_GRAT_FH_BE	"CR_SR_GRAT_FH_BE"
#define M01CR_SR_IR_SRCOFFE	"CR_SR_IR_SRCOFFE"
#define M01CR_SR_LAMPS_LOWE	"CR_SR_LAMPS_LOWE"
#define M01CR_SR_LSHDN_ENAE	"CR_SR_LSHDN_ENAE"
#define M01CR_SR_SISFB_RADE	"CR_SR_SISFB_RADE"
#define M01CR_SR_SISHTROFFE	"CR_SR_SISHTROFFE"
#define M01CR_SR_SLIT_HOMAE	"CR_SR_SLIT_HOMAE"
#define M01CR_SR_SLIT_HOMBE	"CR_SR_SLIT_HOMBE"
#define M01CR_SR_WHL_HOMEAE	"CR_SR_WHL_HOMEAE"
#define M01CR_SR_WHL_HOMEBE	"CR_SR_WHL_HOMEBE"
#define M01CS_SR_GRAT_STEPE	"CS_SR_GRAT_STEPE"
#define M01CS_SR_LAMPSE		"CS_SR_LAMPSE"
#define M01CS_SR_SLIT_STEPE	"CS_SR_SLIT_STEPE"
#define M01CS_SR_SRCWH_STPE	"CS_SR_SRCWH_STPE"
#define M01CS_SR_USEL10WX1E	"CS_SR_USEL10WX1E"
#define M01CS_SR_USEL10WX2E	"CS_SR_USEL10WX2E"
#define M01CS_SR_USEL10WX3E	"CS_SR_USEL10WX3E"
#define M01CS_SR_USEL1WX1E	"CS_SR_USEL1WX1E"


#define M01TEMP_MEASURES        "Engineering Temperature data"
#define M01TA_SR_IR_SRC_AE	"TA_SR_IR_SRC_AE"
#define M01TA_SR_IR_SRC_BE	"TA_SR_IR_SRC_BE"
#define M01TA_SR_SRC_A_SPDE	"TA_SR_SRC_A_SPDE"
#define M01TA_SR_SRC_B_SPDE	"TA_SR_SRC_B_SPDE"
#define M01TP_SR_GRAT_ELEXE	"TP_SR_GRAT_ELEXE"
#define M01TP_SR_GRAT_MTRE	"TP_SR_GRAT_MTRE"
#define M01TP_SR_LAMP_RINGE	"TP_SR_LAMP_RINGE"
#define M01TP_SR_MIR2_DETE	"TP_SR_MIR2_DETE"
#define M01TP_SR_MONO_CHS1E	"TP_SR_MONO_CHS1E"
#define M01TP_SR_MONO_CHS2E	"TP_SR_MONO_CHS2E"
#define M01TP_SR_SNOUTE		"TP_SR_SNOUTE"
#define M01TA_AO_NIR_FPAE	"TA_AO_NIR_FPAE"
#define M01TA_AO_VIS_FPAE	"TA_AO_VIS_FPAE"
#define M01TA_RC_LWIR_CFPAE	"TA_RC_LWIR_CFPAE"
#define M01TA_RC_SMIR_CFPAE	"TA_RC_SMIR_CFPAE"
#define M01TP_MF_CALBHD_SRE	"TP_MF_CALBHD_SRE"
#define M01TP_SA_A_MTRE		"TP_SA_A_MTRE"
#define M01TP_SA_RCT1_MIRE	"TP_SA_RCT1_MIRE"
#define M01TP_SA_RCT2_MIRE	"TP_SA_RCT2_MIRE"


#endif
