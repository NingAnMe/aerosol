/*-------------------------------------------------------------------------*/
/*                                                                         */
/*  COPYRIGHT[copyright mark] 2000, Raytheon System Company, its vendors,  */
/*  and suppliers.  ALL RIGHTS RESERVED.                                   */
/*                                                                         */
/*-------------------------------------------------------------------------*/
/******************************************************************************
BEGIN_FILE_PROLOG:

FILENAME:

  	PGS_IO_Gen_Wrap.h

DESCRIPTION:

  	This file contains ANSI prototypes of the PGS Toolkit C I/O 
  	function wrappers.

AUTHOR:
  	Charles Ruedi / Applied Research Corp.

HISTORY:
  	18-Mar-94 CR 	Initial version
	 4-Apr-94 DPH 	Modified prototype to match actual tool
	25-Jul-94 DPH 	Added prototype for _Tmp_Reference tool. 'Replaced' 
		 	PGSt_SMF_Status w/PGSt_SMF_status; typedefs now reference
			PGS types.
	03-Aug-94 DPH	PGSt_IO_Gen_FileHandle changed from low-level to high-level.
			Added version argument to _Gen_Open call.
	12-Aug-94 DPH	Changed interface to not return version as an argument.
	10-Feb-95 TWA   Added <string.h> to eliminate compiler warnings

END_FILE_PROLOG:
******************************************************************************/

#ifndef  PGS_IO_Gen_Wrap_h  /* avoid re-inclusion */
#define  PGS_IO_Gen_Wrap_h

/*
 * #includes
 * should follow this form:
 *
 *        #include <PGS_IO_Gen_SampleHeader.h>
 */

#include <PGS_PC.h>
#include <stdio.h>
#include <string.h>


#ifdef __cplusplus
extern "C" {
#endif

/* old file handle supported low-level I/O for C & F77
 * typedef PGSt_integer	PGSt_IO_Gen_FileHandle;
 */
    
/* new file handle supports high-level I/O for C ONLY! */

typedef FILE		PGSt_IO_Gen_FileHandle;

typedef PGSt_integer	PGSt_IO_Gen_AccessType;
typedef PGSt_integer	PGSt_IO_Gen_Duration;

/*
 * Prototypes
 */

/* 
 * Old version (first) replaced with the one that immediately follows.
 *
pgs_status PGS_IO_Gen_Open(
	int 			type,
	PGSt_PC_Logical		filename,
	PGSt_IO_Gen_FileHandle 	*filehandle);
 */

/* 
 * Old version (last) replaced with the one that immediately follows.
 *
PGSt_SMF_status PGS_IO_Gen_Open(
	PGSt_PC_Logical        file_logical,
    	PGSt_IO_Gen_AccessType file_access,
    	PGSt_IO_Gen_FileHandle *file_handle);
*/

PGSt_SMF_status PGS_IO_Gen_Open(
	PGSt_PC_Logical        	file_logical,
    	PGSt_IO_Gen_AccessType 	file_access,
    	PGSt_IO_Gen_FileHandle 	**file_handle,
	PGSt_integer	        file_version);

/* 
 * These routines have been removed from the Toolkit. File access
 * is now direct to the POSIX/ANSI calls following a call to the
 * one of the 'open' toolkit calls
 *
PGSt_SMF_status PGS_IO_Gen_Fseek(
	PGSt_IO_Gen_FileHandle	fp,
	long			offset,
	long			origin);

PGSt_SMF_status PGS_IO_Gen_Flush(
	PGSt_IO_Gen_FileHandle	fp);

PGSt_SMF_status PGS_IO_Gen_Write(
	PGSt_IO_Gen_FileHandle	fp,
	void			*data,
	long			size,
	long			count);

PGSt_SMF_status PGS_IO_Gen_Rewind(
	PGSt_IO_Gen_FileHandle	fp);

PGSt_SMF_status PGS_IO_Gen_Read(
	PGSt_IO_Gen_FileHandle	fp,
	void			*data,
	long			size,
	long			count);
*/

PGSt_SMF_status PGS_IO_Gen_Temp_Open(
	PGSt_IO_Gen_Duration   	file_duration,
	PGSt_PC_Logical        	file_logical,
    	PGSt_IO_Gen_AccessType 	file_access,
    	PGSt_IO_Gen_FileHandle 	**file_handle);

PGSt_SMF_status PGS_IO_Gen_Temp_Reference(
	PGSt_IO_Gen_Duration   	file_duration,
	PGSt_PC_Logical        	file_logical,
    	PGSt_IO_Gen_AccessType 	file_access,
    	char	 		*file_reference,
	PGSt_boolean		*exist_flag);

PGSt_SMF_status PGS_IO_Gen_Temp_Delete(
	PGSt_PC_Logical        	file_logical);

PGSt_SMF_status PGS_IO_Gen_Close(
	PGSt_IO_Gen_FileHandle	*file_handle);

#ifdef __cplusplus
}
#endif

#endif             /*  PGS_IO_Gen_Wrap_h */
