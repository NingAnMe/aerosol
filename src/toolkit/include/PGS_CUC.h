/*-------------------------------------------------------------------------*/
/*                                                                         */
/*  COPYRIGHT[copyright mark] 2000, Raytheon System Company, its vendors,  */
/*  and suppliers.  ALL RIGHTS RESERVED.                                   */
/*                                                                         */
/*-------------------------------------------------------------------------*/
/*******************************************************************************
BEGIN_FILE_PROLOG:

FILENAME:

	PGS_CUC.h

DESCRIPTION:
	This file contains the function prototypes for the CUC tools.

AUTHOR:
	Richard Morris	EOSL

HISTORY:
	created 14/12/94
	update	5/01/95
	update  12/12/95 (GTSK)

END_FILE_PROLOG:
*******************************************************************************/

#ifndef PGSCUCD_h
#define PGSCUCD_h

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <CUC/odldef.h>
#include <CUC/udunits.h>
#include <PGS_IO.h>
#include <PGS_CUC_11.h>
#include <PGS_PC.h>

/*******
# defines
********/

/* logical ID of udunits.dat file in PCF */

#define PGSd_UDUNITS_DAT 10302

/*******
Declare function prototypes
*******/

#ifdef __cplusplus
extern "C" {
#endif

PGSt_SMF_status 
PGS_CUC_Conv(char inpUnit[50], char outUnit[50], PGSt_double *outSlope, PGSt_double *outIntercept);


PGSt_SMF_status 
PGS_CUC_Cons(int inpfileid, char *inpParameter, PGSt_double *outvalue);

#ifdef __cplusplus
}
#endif

#endif
