/*-------------------------------------------------------------------------*/
/*                                                                         */
/*  COPYRIGHT[copyright mark] 1999, Raytheon Systems Company, its vendors, */
/*  and suppliers.  ALL RIGHTS RESERVED.                                   */
/*                                                                         */
/*-------------------------------------------------------------------------*/
/*****************************************************************************
BEGIN_FILE_PROLOG:

FILENAME:

	PGS_SMF_Prototypes.h

DESCRIPTION:

	This file contains function prototypes that are specific to the
	SMF Tools

AUTHOR:
	Ray Milburn / Steven Myers & Associates

HISTORY:
	16-Feb-99 RM Initial version

END_FILE_PROLOG:
*****************************************************************************/

#ifndef PGS_SMF_Prototypes_h
#define PGS_SMF_Prototypes_h

#include <PGS_MEM1.h>

#ifdef __cplusplus
extern "C" {
#endif
  
/*****************************************************************
    Function prototypes.
*****************************************************************/

PGSt_SMF_status  PGS_SMF_GetShmSize(PGSMemShmSize *, PGSt_integer);

#ifdef __cplusplus
}
#endif

#endif
