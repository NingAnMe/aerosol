/*-------------------------------------------------------------------------*/
/*                                                                         */
/*  COPYRIGHT[copyright mark] 2000, Raytheon System Company, its vendors,  */
/*  and suppliers.  ALL RIGHTS RESERVED.                                   */
/*                                                                         */
/*-------------------------------------------------------------------------*/
/*******************************************************************************
BEGIN_FILE_PROLOG:

NAME:
   PGS_TD.h

DESCRIPTION:
   This file contains the typedefs, special defines, include files, and function
   prototypes for the SDP (aka PGS) Toolkit Time and Date Tools.

END_FILE_PROLOG:
*******************************************************************************/

#ifndef _PGS_TD_H_
#define _PGS_TD_H_

/*  include files  */

#include <PGS_SMF.h>
#include <PGS_CSC_4.h>
#include <stdio.h>
/* error messages */

#include <PGS_TD_3.h>

/*  spacecraft ID flags  */

#define PGSd_EOS_AM        2222       
#define PGSd_EOS_PM        3333
#define PGSd_EOS_AM1       2222       
#define PGSd_EOS_PM1       3333
#define PGSd_TRMM          4444
#define PGSd_ADEOS_II      5555
#define PGSd_EOS_AURA      6666
#define PGSd_EOS_PM_GIRD   7777
#define PGSd_EOS_PM_GIIS   8888

/* geolocation "bad data" value */

#define PGSd_GEO_ERROR_VALUE 1.0E50
#define PGS_GEO_ERROR_VALUE 1.0E50 /* for backward compatibility */

/* special data types */

typedef PGSt_integer PGSt_flag;

/* old typedefs for backward compatibility */

typedef unsigned char PGSt_sc_timeType;
typedef unsigned char PGSt_scTime;      /* this is still in toolkit code */

#include <PGS_TD_Prototypes.h>

#endif

