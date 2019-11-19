/*-------------------------------------------------------------------------*/
/*                                                                         */
/*  COPYRIGHT[copyright mark] 2000, Raytheon System Company, its vendors,  */
/*  and suppliers.  ALL RIGHTS RESERVED.                                   */
/*                                                                         */
/*-------------------------------------------------------------------------*/
/*****************************************************************************
BEGIN_FILE_PROLOG:

FILENAME:

	PGS_PC_Prototypes.h

DESCRIPTION:

	This file contains function prototypes that are specific to the
	Process Control Tools

AUTHOR:
	Ray Milburn / Applied Research Corporation

HISTORY:
	02-Aug-94 RM Initial version
	18-Dec-94 RM Added shared memory functions for TK4
	05-Apr-95 RM Updated for TK5.  Added functions that allow default
			file locations to be stored in PCF instead of
			environment variables.
	20-Apr-95 RM Updated for TK5.  Added functions that allow for the
			marking of RUNTIME files.
	17-Oct-95 RM Added new variable to function 
			PGS_PC_GetPCSDataGetIndex() prototype.  The new 
			variable is the third variable (PGSt_integer) as
			was added as per ECSed01190.
	15-Dec-95 RM Updated for TK6.  Added function PGS_PC_GetPCFTemp().
			This allows the Process Control Tools to work
			across filesystems.

END_FILE_PROLOG:
*****************************************************************************/

#ifndef PGS_PC_Prototypes_h
#define PGS_PC_Prototypes_h

#include <PGS_TYPES.h>  
#include <PGS_SMF.h>
#include <PGS_PC_9.h>

#define NUMFILES 2
#define PCSFILE 0
#define TEMPFILE 1
#define TEMPFILENAME "tempPCS.fil"

#ifdef __cplusplus
extern "C" {
#endif
  
/*****************************************************************
    Function prototypes.
*****************************************************************/


PGSt_SMF_status PGS_PC_GetPCSData(PGSt_integer,PGSt_PC_Logical,char *,
                              PGSt_integer *);
PGSt_SMF_status PGS_PC_PutPCSData(PGSt_integer,void *);
PGSt_SMF_status PGS_PC_GetPCSDataOpenPCSFile(FILE **);
PGSt_SMF_status PGS_PC_GetPCSDataLocateEntry(FILE *,PGSt_integer,
                                    PGSt_PC_Logical,PGSt_integer *,char *);
PGSt_SMF_status PGS_PC_GetPCSDataAdvanceArea(FILE *,int);
PGSt_SMF_status PGS_PC_GetPCSDataGetIndex(FILE *,PGSt_PC_Logical,
                                    PGSt_integer,char *);
PGSt_SMF_status PGS_PC_GetPCSDataRetrieveData(FILE *,PGSt_integer,char *,
                                    char *,PGSt_integer *);
PGSt_SMF_status PGS_PC_GetPCSDataGetRequest(int,char,char *,char *);
PGSt_SMF_status PGS_PC_GetPCSDataGetFileName(int,char *,char *,char *);
PGSt_SMF_status PGS_PC_PutPCSDataOpenFiles(FILE *[]);
PGSt_SMF_status PGS_PC_PutPCSDataAdvanceToLoc(PGSt_integer,FILE *[]);
PGSt_SMF_status PGS_PC_PutPCSDataPutInArea(PGSt_integer,FILE *[],void *);
PGSt_SMF_status PGS_PC_PutPCSDataInsertCheck(FILE *[],PGSt_PC_File_Struct *);
PGSt_SMF_status PGS_PC_PutPCSDataSkipCheck(PGSt_PC_Logical,FILE *[]);
PGSt_SMF_status PGS_PC_PutPCSDataFixBuffer(FILE *[]);
PGSt_SMF_status PGS_PC_BuildAttribute(char *,PGSt_integer,char *);
PGSt_SMF_status PGS_PC_BuildFileShm(char *,PGSt_integer,PGSt_PC_File_Shm *,
                                   char *);
PGSt_SMF_status PGS_PC_GetDataFromShm(char *,PGSt_integer,PGSt_PC_Logical,
                                   PGSt_integer *,char *);
PGSt_SMF_status PGS_PC_GetFileFromShm(char *,PGSt_PC_Logical,
                                   PGSt_PC_File_Shm *,int *);
PGSt_SMF_status PGS_PC_SearchShm(char *,PGSt_integer,PGSt_PC_Logical,
                                  PGSt_integer *,char *);
PGSt_SMF_status PGS_PC_PutDataInShm(char *,PGSt_integer,void *,PGSt_uinteger);
PGSt_SMF_status PGS_PC_DeleteFileShm(char *,PGSt_PC_Logical);
PGSt_SMF_status PGS_PC_WriteNewToShm(PGSt_PC_HeaderStruct_Shm *,char *,
                                  PGSt_PC_File_Struct *,PGSt_uinteger,
                                  PGSt_uinteger,char *);
PGSt_SMF_status PGS_PC_PutPCSDataMarkAtTerm(FILE *[],PGSt_integer,
                                  PGSt_PC_File_Struct *,char *);
PGSt_SMF_status PGS_PC_OneMarkRuntime(char *,PGSt_PC_Logical,PGSt_integer);
PGSt_SMF_status PGS_PC_CalcArrayIndex(PGSt_integer,int,int *);
PGSt_SMF_status PGS_PC_FindDefaultLocLine(FILE *,char *);
PGSt_SMF_status PGS_PC_CheckFlags(char *,PGSt_integer *);
PGSt_SMF_status PGS_PC_MarkRuntimeAscii(char *,PGSt_PC_Logical,PGSt_integer);
PGSt_SMF_status PGS_PC_MarkRuntimeShm(char *,char *,PGSt_PC_Logical,
                                  PGSt_integer);
PGSt_SMF_status PGS_PC_MultiRuntimes(PGSt_PC_Logical [],PGSt_integer [],
                                  PGSt_integer);
PGSt_SMF_status PGS_PC_GetPCFTemp(char []);
PGSt_SMF_status PGS_PC_BuildNumericInput(char *,PGSt_integer *);

#ifdef __cplusplus
}
#endif

#endif
