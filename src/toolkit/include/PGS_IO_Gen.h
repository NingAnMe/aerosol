/*-------------------------------------------------------------------------*/
/*                                                                         */
/*  COPYRIGHT[copyright mark] 2000, Raytheon System Company, its vendors,  */
/*  and suppliers.  ALL RIGHTS RESERVED.                                   */
/*                                                                         */
/*-------------------------------------------------------------------------*/
/******************************************************************************

BEGIN_FILE_PROLOG:

FILENAME:

  PGS_IO_Gen.h

DESCRIPTION:

  This file contains typedefs, #defines, and #includes specific to the  
  Generic IO tools.

  It is included in PGS_IO.h, and depends on header files included therein.

AUTHOR:
  Charles Ruedi / Applied Research Corp.

HISTORY:
  	18-Mar-1994 CR  Initial version
	31-Mar-1994 MES Add conditional extern declaration support,
		        include <stdio.h> for tools source code only,
	 4-Apr-1994 DPH Moved typedefs to file PGS_IO_Gen_Wrap.h
	 6-Jan-1995 DPH Added Logical Identifier (PC) for IP address aquisition

END_FILE_PROLOG:
******************************************************************************/

#ifndef PGS_IO_Gen_h            	/* avoid re-inclusion */
#define PGS_IO_Gen_h

/*
 * typedefs 
 * should follow this form:
 *
 *         typedef <number-type> PGSt_IO_Gen_SampleTypedef
 */

/*
 * #defines 
 * should follow this form:
 *
 *        #define PGSd_IO_Gen_SampleDefine 0
 */

#ifdef  PGSd_IO_Gen_c          		/* prototypes are extern unless this is set */
#    define PGSd_IO_Gen_Extern
#else
#    define PGSd_IO_Gen_Extern extern
#endif

 
/*
 * Define valid PGS file access modes
 */
#define PGSd_IO_Gen_Write 1 		/* File will be opened for writing. */
#define PGSd_IO_Gen_Read 2 		/* File will be opened for reading, must exist.*/
#define PGSd_IO_Gen_Append 3 		/* File will be opened for append. */
#define PGSd_IO_Gen_Update 4 		/* Read/Write  all existing data is saved.*/
#define PGSd_IO_Gen_Trunc 5 		/* Truncate to zero and open file for write. */
#define PGSd_IO_Gen_AppendUpdate 6 	/*Read any place, but write to end-of-file.*/

/*
 * Define valid PGS temporary file creation modes
 */
#define PGSd_IO_Gen_NoEndurance 0	/* Force creation of temporary, non-static file */
#define PGSd_IO_Gen_Endurance 1 	/* Allow creation of intermediate, static file  */

/*
 * Process Control L.I.D.s
 */
#define PGSd_IO_Gen_HostAddress 10099   /* Logical Identifier for Auxiliary Host IP Address */

/*
 * Define temporary file creation dependencies
 */
#define PGSd_IO_GenDefaultPath	"./"	/* Default to current directory */

#define PGSd_IO_GEN_BUF_MAX	240	/* sized for popen calls */ 

#define PGSd_IO_GEN_REFERENCE_MAX	501	/* size limit for path & filename references  */ 
						/* 500 is the limit for IBM F77 */

#define PGSd_IO_INET_HOSTS_PATH         "PGS_HOST_PATH"

#define PGSd_IO_GEN_NAME_DELAY           1 /* seconds */

/*
 * #includes
 * should follow this form:
 *
 *        #include <PGS_IO_Gen_SampleHeader.h>
 */
#include <PGS_IO_Gen_Wrap.h>


#endif                          /* PGS_IO_Gen_h */
