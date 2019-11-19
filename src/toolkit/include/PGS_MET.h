/*-------------------------------------------------------------------------*/
/*                                                                         */
/*  COPYRIGHT[copyright mark] 2000, Raytheon System Company, its vendors,  */
/*  and suppliers.  ALL RIGHTS RESERVED.                                   */
/*                                                                         */
/*-------------------------------------------------------------------------*/
/****************************************************************************
BEGIN_FILE_PROLOG:

FILENAME:

        PGS_MET.h

DESCRIPTION:

        This file contains typedefs, defines, and includes specific to the
        Metadata management portion of the PGS Toolkit.
AUTHOR:
        Alward N. Siyyid/ EOSL
        Carol S. W. Tsai / Space Applications Corporation
        Abe Taaheri / EEmergent Information Technologies, Inc.

HISTORY:
        16-Mar-95 ANS  Initial version
	31-May-95 ANS  Updates resulting from the code inspection
        16-Oct-97 CSWT Added function prototype for PGS_MET_ConvertToMCF()
        26-Nov-97 CSWT Added new ddfinition for strings "PGSd_MET_MAX_STRING_SET_L"
                       and "PGSd_MET_MAX_ARRAY_ELEMENT_SIZE"
        23-Jan-01 AT   Increased PGSd_MET_MAX_ARRAY_ELEMENT_SIZE to 2000
                       and added prototypes for PGS_MET_SetMultiAttr and
                       PGS_MET_SetMultiAttrF.
        30-Mar-2001 AT Modified for HDF5 support
        24-Apr-2002 AT Added new flags for opening HDF5 files using
                       PGS_MET_SDstart()
END_FILE_PROLOG:
*****************************************************************************/

#ifndef PGSMET_h
#define PGSMET_h

/* System Include files */
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <float.h>
#include <limits.h>
/* Toolkit include files */

#include <PGS_TYPES.h>
#include <PGS_SMF.h>
#include <PGS_PC.h>
#include <PGS_MET_13.h>
#include <hdf5.h>

/* odl include files */

#ifdef PGS_MET_COMPILE   /* only met compilation */
#include <CUC/odldef.h>
#include <CUC/odlinter.h>
#endif /* only met compilation */

/* hdf include files /
#ifdef DEC_ALPHA	/ DEC Alpha platform /
#include <rpc/types.h>	/ this avoids typedef conflict with int32, uint32 /
#endif / DEC_ALPHA /
#include <hdf.h>
#include <mfhdf.h>
*/
/* type defs */

#define PGSd_MET_NUM_OF_MCF 20
#define PGSd_MET_NUM_OF_GROUPS 20
#define PGSd_MET_GROUP_NAME_L 105
#define PGSd_MET_SIGNATURE_L 500
#define PGSd_MET_NUM_OF_SIG 20
typedef char *PGSt_MET_handle;
typedef char PGSt_MET_all_handles[PGSd_MET_NUM_OF_GROUPS][PGSd_MET_GROUP_NAME_L];


/* teh following definitions are for special datetime metadata which require special treatment */
#define PGSd_MET_CDT "CALENDARDATETIME"
#define PGSd_MET_CD  "CALENDARDATE"
#define PGSd_MET_CT  "TIMEOFDAY"

#define PGSd_MET_RBDT "RANGEBEGINNINGDATETIME"
#define PGSd_MET_RBD  "RANGEBEGINNINGDATE"
#define PGSd_MET_RBT  "RANGEBEGINNINGTIME"

#define PGSd_MET_REDT "RANGEENDINGDATETIME"
#define PGSd_MET_RED  "RANGEENDINGDATE"
#define PGSd_MET_RET "RANGEENDINGTIME"
 
/* definitions */

#define PGSd_MET_LSAT_GRP_STR "PGS_MET_LSAT_GRP="
#define PGSd_MET_GROUP_STR "PGS_MET_GRP_NAME="

#define PGSd_MCF_TEMP_FILE 10254
#define PGSd_MAX_HDF_ATTR_SIZE 5000

#define PGSd_MET_NAME_CLASS_DIVIDER '.'
#define PGSd_MET_CORE_DATA "COREDATA1"
#define PGSd_MET_PARAMETER_SET "SET"
#define PGSd_MET_PARAMETER_NOTSET "NOT_SET"

#define PGSd_MET_NAME_L 100
#define PGSd_MET_CLASS_L 100

#define PGSd_MET_MAX_STRING_SET_L 255 
#define PGSd_MET_MAX_ARRAY_ELEMENT_SIZE 2000 
#define PGSd_MET_DD_STRING_L 100
#define PGSd_MET_MAX_ERR_INSERTS 4
#define PGSd_MET_FILE_ID_L 10

#define PGSd_MET_DD_FILE_ID 10251
#define PGSd_MET_MCF_FILE 10250
#define PGSd_MET_TEMP_ATTR_FILE 10252

#define PGSd_MET_ATTR_VALUE_STR "VALUE"
#define PGSd_MET_NEW_VALUE_STR "NEW_VALUE"
#define PGSd_MET_INTEGER_STR "INTEGER"
#define PGSd_MET_SHORT_STR "SHORT"
#define PGSd_MET_LONG_STR "LONG"
#define PGSd_MET_UINTEGER_STR "UNSIGNEDINT"
#define PGSd_MET_FLOAT_STR "FLOAT"
#define PGSd_MET_DOUBLE_STR "DOUBLE"
#define PGSd_MET_STRING_STR "STRING"
#define PGSd_MET_ATTR_NUMOFVAL_STR "NUM_VAL"
#define PGSd_MET_ATTR_TYPE_STR "TYPE"
#define PGSd_MET_FREE_RANGE "FREE_RANGE"
#define PGSd_MET_MCF_NAME "MCF"
#define PGSd_MET_SET_BY_MCF "MCF"
#define PGSd_MET_SET_BY_PGE "PGE"
#define PGSd_MET_SET_BY_PCF "PCF"
#define PGSd_MET_SET_BY_TK "TK"
#define PGSd_MET_SET_BY_DP "DP"
#define PGSd_MET_SET_BY_DAAC "DAAC"
#define PGSd_MET_SET_BY_DSS "DSS"
#define PGSd_MET_INVENT_DATA "INVENTORYMETADATA"
#define PGSd_MET_DATA_LOC_STR "DATA_LOCATION"
#define PGSd_MET_MANDATORY_STR "MANDATORY"
#define PGSd_MET_CLASS_STR "CLASS"
#define PGSd_MET_MAND_FALSE_STR "FALSE"
#define PGSd_MET_SET_BY_NONE "NONE"
#define PGSd_MET_MASTER_GROUP "MASTERGROUP"
#define PGSd_MET_GROUP_TYPE_STR "GROUPTYPE"
#define PGSd_MET_TEMPOARY_FLAG "T"
#define PGSd_MET_MULTIPLE_FLAG "M"
#define PGSd_MET_DATETIME_STR	"DATETIME"
#define PGSd_MET_DATE_STR "DATE"
#define PGSd_MET_TIME_STR "TIME"
#define PGSd_MET_ASCII_DUMP 10255
#define PGSd_MET_INT_MAX 2147483647
#define PGSd_MET_DBL_MAX 1.797693E+308
#define PGSd_MET_MULTI_FLAG 1
#define PGSd_MET_MAX_NUM_FILES 256

#ifndef HDF4_ACC_RDONLY
#define  HDF4_ACC_RDONLY 1
#endif

#ifndef HDF5_ACC_RDONLY
#define  HDF5_ACC_RDONLY 11
#endif

#ifndef HDF4_ACC_RDWR
#define  HDF4_ACC_RDWR 3
#endif

#ifndef HDF5_ACC_RDWR
#define  HDF5_ACC_RDWR 13
#endif

#ifndef HDF4_ACC_CREATE
#define  HDF4_ACC_CREATE 4
#endif

#ifndef HDF5_ACC_CREATE
#define  HDF5_ACC_CREATE 14
#endif

/* Added following 3 tags to make TOOLKIT consistent with hdfeos5 */

#ifndef HE5F_ACC_RDONLY
#define  HE5F_ACC_RDONLY 101
#endif

#ifndef HE5F_ACC_RDWR
#define  HE5F_ACC_RDWR 100
#endif

#ifndef HE5F_ACC_TRUNC
#define  HE5F_ACC_TRUNC 102
#endif



/* function prototypes */

#ifdef __cplusplus
extern "C" {
#endif

PGSt_SMF_status PGS_MET_Init(		 /* Initializes metedata configuration
                                          * file (MCF)
                                          */
             PGSt_PC_Logical fileId, 	 /* file id for the file containing
					  * MCF data
					  */
             PGSt_MET_all_handles mdHandle); /* Handle for the MCF in memory */

PGSt_SMF_status
PGS_MET_SetAttr(
             PGSt_MET_handle mdHandle,  /* Handle for the MCF in memory */
	     char *	     attrName,   /* Parameter name to be set */ 
	     void *	     attrValue); /* Parameter value to be set */

PGSt_SMF_status
PGS_MET_SetAttrF(                           /* Sets metadata attribute values
                                             */
             PGSt_MET_handle    mdHandle,   /* metadata group within MCF 
					     * containing the parameter */
             char *             attrNameStr,/* Parameter attr to be set*/
             void *             attrValue,  /* Attribute value buffer with
                                             * the value to be set */
             unsigned int       strLength); /* string length if the void buffer
					     * is character string */
PGSt_SMF_status
PGS_MET_SetMultiAttr(			    /* Sets metadata attribute 
					     * values */
             PGSt_MET_handle  	mdHandle,   /* metadata group within MCF 
					     * containing the parameter */
             char *		attrNameStr,/* Parameter attr to be set */
             PGSt_integer       numofval,   /* number of attribute values */
             void *		attrValue); /* Parameter value to be set */

PGSt_SMF_status
PGS_MET_SetMultiAttrF(			    /* Sets metadata attribute values
					     */
             PGSt_MET_handle  	mdHandle,    /* metadata group within MCF 
					      * containing the parameter */
             char * 		attrNameStr, /* Parameter attr to be set */
             PGSt_integer       numofval,    /* number of attribute values */
             void *		attrValue,   /* Attribute value buffer with
					      * the value to be set */
	     unsigned int       strLength);  /*string length if the void buffer
					      * is character string */

PGSt_SMF_status
PGS_MET_GetSetAttr(
       PGSt_MET_handle md_handle,                /* Handle for the MCF in memory
						  */ 
       char *          parmName, 		 /* Parameter name to be
						  * retrieved
						  */
       void *          parmValue);               /* Parameter value buffer
				          	  * to hold the retrieved value
					          */
PGSt_SMF_status
PGS_MET_GetSetAttrF(                        /* Retrieves metadata attr values
                                             * which are already set by the Init
                                             * or the user. If the metadata value
                                             * is not set, a warning is returned
                                             */
             PGSt_MET_handle    mdHandle,   /* list of groups within MCF */
             char               *attrNameStr,  /* Parameter attr to be retrieved */
             void               *attrValue, /* Attribute value buffer to hold
                                             * the retrieved value
                                             */
             unsigned int       strLength); /* string length if the void buffer is character string
                                            */

PGSt_SMF_status
PGS_MET_GetPCAttr(
       PGSt_PC_Logical fileId,                /* file id for the file containing
                                               * parameter data
                                               */
       PGSt_integer version,	 	      /* product version number */
       char *         hdfAttrName,	      /* name of the HDF attribute to be searched */
       char *         parmName,		      /* Parameter name to be retrieved
 					       */
       void *       parmValue); 	      /* Parameter value buffer to hold
                                               * the retrieved value
                                               */
PGSt_SMF_status
PGS_MET_GetPCAttrF(
       PGSt_PC_Logical  fileId,                /* file id for the file containing
                                               * parameter data
                                               */
       PGSt_integer     version,               /* product version number */
       char *           hdfAttrName,           /* name of hdf attribute to be searched */
       char *           parmName,              /* Parameter name to be retrieved
                                                */
       void *           parmValue,             /* Parameter value buffer to hold
                                                * the retrieved value
                                                */
       unsigned int     stringSize);                     /* size of fortran strings send as a hidden argument */

PGSt_SMF_status
PGS_MET_Write(
             PGSt_MET_handle mdHandle,  /* Handle for the MCF in memory */ 
	     char * 	     hdfAttrName, /* name of hdf attribute to be attached */
             PGSt_integer    hdfFileId); /* HDF product file id or local
					  * data id
					  */
PGSt_SMF_status
PGS_MET_GetConfigData(                      /* Retrieves metadata attr values
                                             * from the configuration section of the PC file
                                             */
             char               *attrNameStr,  /* Parameter attr to be retrieved */
             void               *attrValue); /* Attribute value buffer to hold
                                              * the retrieved value
                                              */

PGSt_SMF_status
PGS_MET_GetConfigDataF(                     /* Retrieves metadata attr values
                                             * from the configuration section of the PC file
                                             */
             char               *attrName,  /* Parameter attr to be retrieved */
             void               *attrValue, /* Attribute value buffer to hold
                                             * the retrieved value
                                             */
             unsigned int       stringSize); /* size of the fotran character array element */

void
PGS_MET_Remove(void);			/* Removes MCF after it is written to HDF */

PGSt_SMF_status
PGS_MET_SDstart(char *filename, PGSt_uinteger flags, PGSt_integer *HDFfid);

PGSt_SMF_status
PGS_MET_SDstartF(char *filename, PGSt_integer flags, PGSt_integer *HDFfid);

PGSt_SMF_status
PGS_MET_SDend(PGSt_integer HDFfid);

PGSt_SMF_status
PGS_MET_HDFFileType(char*        fileName, /* File name */
                    PGSt_integer * FILE_IS_HDF4,
                    PGSt_integer * FILE_IS_HDF5,
                    PGSt_integer * FILE_NOT_HDF);


#ifdef __cplusplus
}
#endif

/* prototypes for support functions */

#ifdef PGS_MET_COMPILE   /* only met compilation */

PGSt_SMF_status
PGS_MET_RetrieveConfigData(                 /* Retrieves the value of Config parameter
                                             * from the PC table, checks it against DD and
                                             * and attaches it to the given aggregate node
                                             */
             AGGREGATE attrNode);            /* aggregate node representing the parameter */

PGSt_SMF_status
PGS_MET_GetConfigByLabel(                   /* This function returns teh ODL compatible
                                             * parm=value configuration string from
                                             * the PC table
                                             */
            char *      attrName,           /* The name of the attribute */
            char *      data);             /* retrieved string data */

PGSt_SMF_status
PGS_MET_ConvertToOdl(                       /* This function is used to convert
                                             * a char buffer into an ODL format */
            char *      aggName,            /* The name to be given to the new aggregate */
            char *      data,               /* data to be converted to ODL */
            AGGREGATE   *aggNode);           /* ODL aggregate to be returned */

PGSt_SMF_status
PGS_MET_CheckAgainstDD(                     /* Checks a given metadata for type range etc */
             AGGREGATE metaDataNode);        /* Parameter odl node to be tested */

PGSt_SMF_status
PGS_MET_CheckRange(                         /* Checks a given single value  of metadata against
                                             * possible values in the data dictionary
                                             */
             char *parmName,                /* meta data parameter name */
             void *parmValue,               /* parameter value to be checked */
             PGSt_integer odlType);           /* data type as defined in odl eg. TV_INTEGER etc */

PGSt_SMF_status
PGS_MET_LoadAggregate( 			/* Parses a given file containing 
					 * attribute data and loads the
					 * data into memory as an ODL tree
					 * structure 
					 */
             PGSt_PC_Logical fileId,    /* PC file id containing attributes*/ 
             char    	     *aggName,   /* Aggregate name */
	     PGSt_integer    fileMode,  /* PC file mode */
	     AGGREGATE 	     *mdHandle);  /* meta data handle pointer */

PGSt_SMF_status
PGS_MET_GetDDAttr(                          /* Retrieves data dictionary info */
             char *parmName, 		    /* Parameter name */
             char *attrName, 		    /* Parameter attr to be retrieved */
             void *attrValue); 		    /* Attribute value buffer to hold
                                             * the retrieved value
                                             */

PGSt_SMF_status
PGS_MET_CheckAttr(                          /* Ensures that all mandatory
                                             * attributes are set
                                             */
             AGGREGATE mdHandle);     /* MCF node handle */

PGSt_SMF_status
PGS_MET_SearchAttr(                          /* This function is used by PGS_MET_GetPCAttr to
                                              *   retrieve the Product Specific Attributes 
                                              */
             PGSt_MET_handle    mdHandle,    /* list of groups within MCF */
             AGGREGATE          mdGroupNode, /* group Node */
             char               attrNameStr[80],  /* Parameter attr to be retrieved */
             void               *attrValue); /* Attribute value buffer to hold
                                              * the retrieved value
                                              */ 
PGSt_SMF_status
PGS_MET_ConvertToMCF(                           /* This function is used to convert HDF-EOS/non
                                                 * HDF-EOS Metadata files to a file that is
                                                 * similiar to the metadata configuration
                                                 * file (MCF)
                                                 */   
                     PGSt_PC_Logical fileId,    /* file id for the file containing
                                                 * parameter data
                                                 */
                     char        *hdfAttrName); /* HDF attribute Name */
void
PGS_MET_ErrorMsg(                           /* prepares andsends error messages 
					     *to the log file 
					     */
                PGSt_SMF_code code,
                char         *functionName,  /* function name where error occurred */
                long         noOfInserts,    /* number of inserts in the dynamic message */
                char         *errInserts[]); /* inserts strings in the error message */
void
PGS_MET_NameAndClass(                    /* parses attribute string into name and class */
                char          *nameStr, /* String containing name and class */
		char          *name,     /* attribute name */  
                char          *attClass);   /* attribute class */
PGSt_SMF_status
PGS_MET_GetSetAttrTD(
       PGSt_MET_handle md_handle,                /* Handle for the MCF in memory
						  */ 
       char *          parmName, 		 /* Parameter name to be
						  * retrieved
						  */
       void *          parmValue);               /* Parameter value buffer
				          	  * to hold the retrieved value
					          */
#endif /* only met compilation */

#include <PGS_MET_Prototypes.h>

#endif
