/*-------------------------------------------------------------------------*/
/*                                                                         */
/*  COPYRIGHT[copyright mark] 2000, Raytheon System Company, its vendors,  */
/*  and suppliers.  ALL RIGHTS RESERVED.                                   */
/*                                                                         */
/*-------------------------------------------------------------------------*/
/*****************************************************************************
BEGIN_FILE_PROLOG:

FILENAME:

	PGS_PC.h

DESCRIPTION:

	This file contains typedefs, #defines, and #includes specific to the
	Process Control portion of the PGS Toolkit.

AUTHOR:
	Ray Milburn / Applied Research Corporation
        Carol Tsai / Applied Research Corporation

HISTORY:
	18-Mar-94 RM Initial version
	12-Apr-94 RM Added some #define(d) variables
	18-Dec-94 RM Added some #define(d) variables and some new structures
			for TK4.
	28-Dec-94 RM Added variable for extra memory - from comments of
			code inspection of PGS_PC_InitCom.c held on
			22-Dec-94.
	05-Apr-95 RM Updated for TK5.  Added new variables which allow
			default file locations to be stored in PCF 
			as opposed to environment variables.
	20-Apr-95 RM Updated for TK5.  Added new flags that are used
			as RUNTIME and DELETE flags.
	13-Dec-95 RM Updated for TK6.  Added functionality to allow
			multiple intances of PRODUCT OUTPUT FILES.
	05-Jan-96 RM Updated for TK6.  Added Universal References to 
			file entries for files.  The variable bufferSize
			in the file structure was changed to universalRef,
			a new function was added, and some #define(s) were
			added to this file.
        05-Mar-96 CT Added new function prototype for PGS_PC_GetFileSize().
	24-Apr-97 RM Added new mnemonics to support multiple versions of 
			PRODUCT INPUT and PRODUCT OUTPUT files.
	30-Jun-97 DH Fixed per ECSed07367. Expanded the size of Universal 
                        Reference field to upper limit imposed by Ephemeris 
                        file header structure (255).

END_FILE_PROLOG:
*****************************************************************************/

#ifndef PGS_PC_h
#define PGS_PC_h

#include <PGS_TYPES.h>  
#include <PGS_SMF.h>
#include <PGS_PC_9.h>
  
#ifdef __cplusplus
extern "C" {
#endif

/*****************************************************************
    Define unique types needed by the Process Control Tools.
    Some of these may be deleted from this file when other
    portions of the PGS Toolkit are completed. 
*****************************************************************/

/*
 * the following typdef was moved to PGS_TYPES.h for TK5
 * because it is needed by PGS_SMF.h:
 * 
 *     typedef PGSt_uinteger PGSt_PC_Logical;
 *
 * 27-Apr-1995 Mike Sucher / A.R.C.
 */


typedef PGSt_integer PGSt_PC_Long_Int;  /* for backwards compatibility */
typedef PGSt_uinteger pgs_logical;	/* for backwards compatibility */
typedef PGSt_integer long_int;		/* for backwards compatibility */

/*****************************************************************
    Functions that the user can use.
*****************************************************************/

PGSt_SMF_status PGS_PC_GetReference(PGSt_PC_Logical,PGSt_integer *,char *);
PGSt_SMF_status PGS_PC_GetConfigData(PGSt_PC_Logical,char *);
PGSt_SMF_status PGS_PC_GetNumberOfFiles(PGSt_PC_Logical,PGSt_integer *);
PGSt_SMF_status PGS_PC_GenUniqueID(PGSt_PC_Logical,char *);
PGSt_SMF_status PGS_PC_GetFileAttr(PGSt_PC_Logical,PGSt_integer,
                       PGSt_integer,PGSt_integer,char *);
PGSt_SMF_status PGS_PC_GetFileByAttr(PGSt_PC_Logical,PGSt_integer (*)(char*),
                       PGSt_integer,PGSt_integer *);
PGSt_SMF_status PGS_PC_GetPCEnv(char *,char *);
PGSt_SMF_status PGS_PC_GetUniversalRef(PGSt_PC_Logical,PGSt_integer *,
                       char *);
PGSt_SMF_status PGS_PC_GetReferenceType(PGSt_PC_Logical, PGSt_integer *);
PGSt_SMF_status PGS_PC_GetFileSize(PGSt_PC_Logical, PGSt_integer,
			PGSt_integer *);

/*****************************************************************
    Define lengths for variables used in the structures.
*****************************************************************/

#define PGSd_PC_FILE_NAME_MAX 256
#define PGSd_PC_PATH_LENGTH_MAX 768
#define PGSd_PC_ID_LENGTH_MAX 200
#define PGSd_PC_VALUE_LENGTH_MAX 200
#define PGSd_PC_PRODDATA_LENGTH_MAX 200
#define PGSd_PC_FILE_PATH_MAX 1025
#define PGSd_PC_LABEL_SIZE_MAX 200
#define PGSd_PC_UREF_LENGTH_MAX 255
#define PGSd_PC_LINE_LENGTH_MAX 2000

/*****************************************************************
    Define mode values to be used in PGS_PC_GetPCSData().
    (we may want to put these in a different header file
    not accessible to the application programmer.)
     ********************************************** 
    This section of #define(s) are the modes that can be 
    passed into PGS_PC_GetPCSData() and PGS_PC_PutPCSData().
*****************************************************************/
#define PGSd_PC_PRODUCTION_RUN_ID 1
#define PGSd_PC_SOFTWARE_ID 2
#define PGSd_PC_CONFIGURATION 5000
#define PGSd_PC_INPUT_FILE_NAME 5100
#define PGSd_PC_INPUT_FILE_ATTRIBUTE 5110
#define PGSd_PC_INPUT_FILE_NUMFILES 5120
#define PGSd_PC_PRODUCT_IN_DEFLOC 5130
#define PGSd_PC_PRODUCT_IN_UREF 5140
#define PGSd_PC_OUTPUT_FILE_NAME 5200
#define PGSd_PC_OUTPUT_FILE_ATTRIBUTE 5210
#define PGSd_PC_PRODUCT_OUT_DEFLOC 5220
#define PGSd_PC_OUTPUT_FILE_NUMFILES 5230
#define PGSd_PC_PRODUCT_OUT_UREF 5240
#define PGSd_PC_TEMPORARY_FILE 5300
#define PGSd_PC_TEMP_INFO_USEASCII 5301
#define PGSd_PC_TEMPDEL_TERM 5302
#define PGSd_PC_DELETE_TEMP 5310
#define PGSd_PC_TEMP_FILE_DEFLOC 5320
#define PGSd_PC_TEMP_FILE_UREF 5330
#define PGSd_PC_INTERMEDIATE_INPUT 5400
#define PGSd_PC_INTER_IN_DEFLOC 5410
#define PGSd_PC_INTER_IN_UREF 5420
#define PGSd_PC_INTERMEDIATE_OUTPUT 5500
#define PGSd_PC_INTER_OUT_USEASCII 5501
#define PGSd_PC_INTER_OUT_DEFLOC 5510
#define PGSd_PC_INTER_OUT_UREF 5520
#define PGSd_PC_SUPPORT_IN_NAME 5600
#define PGSd_PC_SUPPORT_IN_ATTR 5610
#define PGSd_PC_SUPPORT_IN_DEFLOC 5620
#define PGSd_PC_SUPPORT_IN_UREF 5630
#define PGSd_PC_SUPPORT_IN_NUMFILES 5640
#define PGSd_PC_SUPPORT_OUT_NAME 5700
#define PGSd_PC_SUPPORT_OUT_ATTR 5710
#define PGSd_PC_SUPPORT_OUT_DEFLOC 5720
#define PGSd_PC_SUPPORT_OUT_UREF 5730
#define PGSd_PC_SUPPORT_OUT_NUMFILES 5740

/*****************************************************************
    This section of #define(s) are the modes that can be 
    passed into PGS_PC_PutPCSDataAdvanceToLoc() that are 
    above and beyond what is listed above.
*****************************************************************/
#define PGSd_PC_ENDOFFILE 99999

/*****************************************************************
    This section of #define(s) are the modes that can be 
    passed into PGS_PC_GetFileAttr() for the format flag.
*****************************************************************/
#define PGSd_PC_ATTRIBUTE_LOCATION 1
#define PGSd_PC_ATTRIBUTE_STRING 2

/*****************************************************************
    This section of #define(s) are the modes that can be 
    returned from searchFunc in PGS_PC_GetFileByAttr().
*****************************************************************/
#define PGSd_PC_MATCH 1
#define PGSd_PC_NO_MATCH 2

/*****************************************************************
    This section of #define(s) are the file flags that can be
    passed in to PGS_PC_BuildFileShm().
*****************************************************************/
#define PGSd_PC_TYPE_FILE 1
#define PGSd_PC_TYPE_ATTR 2

/*****************************************************************
    This section of #define(s) are the file flags that can be
    passed in to PGS_PC_CalcArrayIndex().
*****************************************************************/
#define PGSd_PC_DIVS_VALUE 1
#define PGSd_PC_MODE_VALUE 2

/*****************************************************************
    This section of #define(s) are the flags types that can be
    returned from to PGS_PC_CheckFlags().
*****************************************************************/
#define PGSd_PC_NO_FLAGS 1
#define PGSd_PC_HAS_DELETE 2
#define PGSd_PC_HAS_RUNTIME 3
#define PGSd_PC_HAS_DELNRUN 4

/*****************************************************************
    This #define(d) variable defines the amount of extra memory 
    needed while running the PGE.
*****************************************************************/
#define PGSd_PC_EXTRA_MEM 15000

/*****************************************************************
    This #define(d) variable defines the amount of memory 
    that can possibly be used by an attribute string in the
    command PGS_PC_GetFileAttrCom.
*****************************************************************/
#define PGSd_PC_ATTRCOM_MEM 1600

/*****************************************************************
    This section of #define(s) are used in PGS_PC_GetPCSData() 
    and PGS_PC_PutPCSData() to determine the sections of the
    input file.
*****************************************************************/
#define PGSd_PC_SYS_CONFIG 1
#define PGSd_PC_INPUT_FILES 2
#define PGSd_PC_OUTPUT_FILES 3
#define PGSd_PC_SUPPORT_INPUT 4
#define PGSd_PC_SUPPORT_OUTPUT 5
#define PGSd_PC_CONFIG_COUNT 6
#define PGSd_PC_INTER_INPUT 7
#define PGSd_PC_INTER_OUTPUT 8
#define PGSd_PC_TEMP_INFO 9
#define PGSd_PC_TOTAL_SEPARATORS 10

/*****************************************************************
    This section of #define(s) defines the number of file 
    types that can be stored in the PCF.
*****************************************************************/
#define PGSd_PC_FILE_TYPES 7

/*****************************************************************
    This section of #define(s) are characters that are commonly
    searched for in the input file.
*****************************************************************/
#define PGSd_PC_NEWLINE '\n'
#define PGSd_PC_DELIMITER '|'
#define PGSd_PC_COMMENT '#'
#define PGSd_PC_DIVIDER '?'
#define PGSd_PC_DEFAULT_LOC '!'
#define PGSd_PC_STRING_SLASH "/"
#define PGSd_PC_SLASH '/'
#define PGSd_PC_HIDIGIT '9'
#define PGSd_PC_LOWDIGIT '0'
#define PGSd_PC_TILDE '~'
#define PGSd_PC_DELETE_FLAG 'D'
#define PGSd_PC_DELETE_STRING "D"
#define PGSd_PC_CHAR_NULL '\0'
#define PGSd_PC_STRING_NULL "\0"
#define PGSd_PC_RUNTIME_FLAG 'R'
#define PGSd_PC_RUNTIME_STR "R"
#define PGSd_PC_DELNRUN_FLAG 'B'
#define PGSd_PC_DELNRUN_STR "B"

/*****************************************************************
    This section of #define(s) are environment variable names
    needed by the Process Control Tools.
*****************************************************************/
#define PGSd_PC_INPUT_FILE_ENVIRONMENT "PGS_PRODUCT_INPUT"
#define PGSd_PC_OUTPUT_FILE_ENVIRONMENT "PGS_PRODUCT_OUTPUT"
#define PGSd_PC_TEMP_ENVIRONMENT "PGS_TEMPORARY_IO"
#define PGSd_PC_INTER_INPUT_ENVIRONMENT "PGS_INTERMEDIATE_INPUT"
#define PGSd_PC_INTER_OUTPUT_ENVIRONMENT "PGS_INTERMEDIATE_OUTPUT"
#define PGSd_PC_INFO_FILE_ENVIRONMENT "PGS_PC_INFO_FILE"
#define PGSd_PC_SUPPT_INPUT_ENVIRONMENT "PGS_SUPPORT_INPUT"
#define PGSd_PC_SUPPT_OUT_ENVIRONMENT "PGS_SUPPORT_OUTPUT"
#define PGSd_PC_PGSHOME_ENVIRONMENT "PGSHOME"
#define PGSd_PC_USESHM_ENV "PGSMEM_USESHM"

/*****************************************************************
    Initialization flags.
*****************************************************************/
#define PGSd_PC_SHMOFF "ShmOff"
#define PGSd_PC_LOGOFF "LogOff"
#define PGSd_PC_USESHM_YES "YES"
#define PGSd_PC_USESHM_NO "NO"

/*****************************************************************
    The following code is the structure that may be used to 
    house the Process Control Tool information.  Currently
    the information is read from a flat file and the proper
    answer is returned to the user.  At any rate, expect these
    to mature over time.  These are the structures that will
    eventually be read from shared memory when all the shared
    memory functions are in place.
*****************************************************************/

/*****************************************************************
     Structure to store file information.
*****************************************************************/

typedef struct {
     PGSt_PC_Logical    index;
     PGSt_uinteger	size;
     PGSt_uinteger	entries;
     char	        universalRef[PGSd_PC_UREF_LENGTH_MAX];
     char		attributeLoc[PGSd_PC_FILE_NAME_MAX];
     char               fileName[PGSd_PC_FILE_NAME_MAX];
     char               path[PGSd_PC_PATH_LENGTH_MAX];
    }PGSt_PC_File_Struct;

/*****************************************************************
     Structure to store User Defined Configuration Parameters.
*****************************************************************/

typedef struct {
     PGSt_PC_Logical    index;
     char               identity[PGSd_PC_ID_LENGTH_MAX];
     char               value[PGSd_PC_VALUE_LENGTH_MAX];
    }PGSt_PC_Configuration_Struct;

/*****************************************************************
     These are the structures to store the different types of
     data in shared memory.
*****************************************************************/

typedef struct {
     char               flag;
     char               data[PGSd_PC_VALUE_LENGTH_MAX];
    }PGSt_PC_SysConfig_Shm;

typedef struct {
     char               flag;
     PGSt_PC_Configuration_Struct configStruct;
    }PGSt_PC_UserConfig_Shm;

typedef struct {
     char               flag;
     PGSt_PC_File_Struct fileStruct;
    }PGSt_PC_File_Shm;

typedef struct {
     char               divider;
     PGSt_uinteger      divPtr[PGSd_PC_TOTAL_SEPARATORS];
     char               defaultLoc[PGSd_PC_FILE_TYPES][PGSd_PC_PATH_LENGTH_MAX];
     PGSt_uinteger      amountUsed;
    }PGSt_PC_HeaderStruct_Shm;

/*****************************************************************
     Include low-level function prototypes.
*****************************************************************/

#ifdef __cplusplus
}
#endif

#include <PGS_PC_Prototypes.h>
#include <PGS_MEM1.h>

#endif
