/*-------------------------------------------------------------------------*/
/*                                                                         */
/*  COPYRIGHT[copyright mark] 2000, Raytheon System Company, its vendors,  */
/*  and suppliers.  ALL RIGHTS RESERVED.                                   */
/*                                                                         */
/*-------------------------------------------------------------------------*/
/******************************************************************************
BEGIN_FILE_PROLOG:

FILENAME:

  PGS_IO.h

DESCRIPTION:

  This is the master header file for the IO subsystem of the PGS toolkit.
  It contains typedefs, #defines, and #includes  required by the IO tools.

AUTHOR:
  Mike Sucher / Applied Research Corp.

HISTORY:
  18-Mar-1994 MES Initial version
  20-Apr-1994 MES Removed calls to PGS_IO_HDF.h & PGS_L0.h for this release
  08-Aug-1994 DPH pgstk2.5 branch: add definitions specific to the 2.5 release
  29-Aug-1994 MES main branch: removed the 2.5 release code additions
  23-Nov-1994 MES Add the #include for PGS_IO_L0.h

END_FILE_PROLOG:
******************************************************************************/

#ifndef PGS_IO_h        /* avoid re-inclusion */
#define PGS_IO_h


/*
 * typedefs 
 * should follow this form:
 *
 *         typedef <number-type> PGSt_IO_SampleTypedef
 */


/*
 * #defines 
 * should follow this form:
 *
 *        #define PGSd_IO_SampleDefine 0
 */


/*
 * general-purpose #includes
 */
#include <PGS_SMF.h>    /* Status Message Formatting (SMF) header */
#include <PGS_PC.h>     /* Process Control (PC) header */



/*
 * IO-specific #includes
 * should follow this form (SS is subsystem - IO, Gen ...):
 *
 *        #include <PGS_IO_SS_SampleHeader.h>
 */

#include <PGS_IO_1.h>       /* SMF return codes for all IO tools (seed = 1) */
#include <PGS_IO_Gen.h>     /* Generic tools header */
#include <PGS_IO_L0.h>      /* Level 0 tools header */

#endif                  /* PGS_IO_h */

