/*-------------------------------------------------------------------------*/
/*                                                                         */
/*  COPYRIGHT[copyright mark] 2000, Raytheon System Company, its vendors,  */
/*  and suppliers.  ALL RIGHTS RESERVED.                                   */
/*                                                                         */
/*-------------------------------------------------------------------------*/
/****************************************************************************
BEGIN_FILE_PROLOG:

FILENAME:

        PGS_GCT.h

DESCRIPTION:

        This file contains typedefs, #defines, and #includes specific to the
        Geo-coordinate Transformation portion of the PGS Toolkit.  
AUTHOR:
        Alward N. Siyyid/ EOSL
        Carol S. W. Tsai / Space Applications Corp.
	Abe Taaheri / Emergent Information Tecnologies, Inc.

HISTORY:
        15-Dec-94 ANS  Initial version
	06-Jan-95 ANS  Changed UTM minimum value to -60
	21-July-95 ANS Introduced PGSd_GCT_IN_ERROR (ECSed00700)
        26-Sep-97 CSWT Added function prototype for PGS_GCT_SetGetrMajorrMinor()
	27-June-00 AT  added default radius and Ltruscale for BCEA projection
        23-Oct-00  AT  Updated for ISINUS projection, so that both codes
                       31 and 99 can be used for this projection. 

END_FILE_PROLOG:
*****************************************************************************/

#ifndef PGSGCT_h
#define PGSGCT_h
  
/* System Include files */

#include <stdio.h>  
#include <stdlib.h>
#include <string.h>




/* Toolkit include files */

#include "PGS_GCT_12.h"
#include "PGS_TYPES.h"
#include "PGS_SMF.h"
#include "PGS_PC.h"

/* Fortran interface include file */

#include <cfortran.h>

/* definitions */

#define PGSd_GCT_FORWARD 0         /* Forward direction from lat. long to x,y */
#define PGSd_GCT_INVERSE 1         /* Inverse direction from x,y to lat. long */
#define PGSd_GCT_UTM_SCALE_FACTOR 0.9996

#define PGSd_GCT_UTM_MINIMUM_ZONE -60    /* minimum UTM zone number */
#define PGSd_GCT_UTM_MAXIMUM_ZONE 60   /* maximum UTM zone number */

#define PGSd_GCT_OK OK 	           /* GCTP library return code for success */
#define PGSd_GCT_IN_BREAK 1.0e50       /* If return code is IN_BREAK from the
				    * GCTP library (for interrupted regions) 
				    * long, lat values to be returned are set
				    * to this value
				    */
#define PGSd_GCT_IN_ERROR 1.0e51    /* if the conversion encounters error then this value is returned as output */
#define PGSd_GCT_FILENAME_LENGTH 100 /* filename length including pathname */

/* the following definitions are for the data files id's needed for the 
 * state plane projection
 */

#define PGSd_GCT_NAD27 10200	/* for file nad27sp */
#define PGSd_GCT_NAD83 10201    /* for file nad83sp */

/* GCTP projection codes */
/* IMPORTANT: each of these codes has an analog in the file
              FW/proj.h.  These codes MUST have the same value
              as the codes in that file (e.g. PGSd_GEO = GEO). */

#define PGSd_GEO 0
#define PGSd_UTM 1
#define PGSd_SPCS 2
#define PGSd_ALBERS 3
#define PGSd_LAMCC 4
#define PGSd_MERCAT 5
#define PGSd_PS 6
#define PGSd_POLYC 7
#define PGSd_EQUIDC 8
#define PGSd_TM 9
#define PGSd_STEREO 10
#define PGSd_LAMAZ 11
#define PGSd_AZMEQD 12
#define PGSd_GNOMON 13
#define PGSd_ORTHO 14
#define PGSd_GVNSP 15
#define PGSd_SNSOID 16
#define PGSd_EQRECT 17
#define PGSd_MILLER 18
#define PGSd_VGRINT 19
#define PGSd_HOM 20
#define PGSd_ROBIN 21
#define PGSd_SOM 22
#define PGSd_ALASKA 23
#define PGSd_GOOD 24
#define PGSd_MOLL 25
#define PGSd_IMOLL 26
#define PGSd_HAMMER 27
#define PGSd_WAGIV 28
#define PGSd_WAGVII 29
#define PGSd_OBEQA 30
#define PGSd_ISINUS1 31
#define PGSd_CEA 97
#define PGSd_BCEA 98
#define PGSd_ISINUS 99
 
#define PGSd_CLARK66 0          /*  datum for CLARK 1866                     */
#define PGSd_GRS80_WGS84 8      /*  datum for GRS 1980 / WGS 84              */

#define PGSd_DEFAULT_BCEA_RADIUS 6371228.0 /*Earth radius for BCEA projection*/
#define PGSd_DEFAULT_BCEA_LTRUESCALE 30.0  /*Latitude of true scale 
					     (in degrees) for Behrmann 
					     Cylindrical Equal Area
					     Projection */

#ifdef __cplusplus
extern "C" {
#endif

PGSt_SMF_status PGS_GCT_Init(  
                        PGSt_integer projId,
                        PGSt_double projParam[],
                        PGSt_integer directFlag);

PGSt_SMF_status PGS_GCT_CheckRadii( 
                         PGSt_double  majorAxis,
                         PGSt_double  eccentricity);

PGSt_SMF_status PGS_GCT_SetGetrMajorrMinor( 
                         PGSt_boolean SWITCH,
                         double  *majorAxis,
                         double  *minorAxis);

PGSt_SMF_status
PGS_GCT_CheckLongitude(
        PGSt_double longitude);    /* input longitude */

PGSt_SMF_status
PGS_GCT_CheckLatitude(
        PGSt_double latitude);    /* input latitude */

PGSt_SMF_status PGS_GCT_Proj(
                        PGSt_integer projId,
                        PGSt_integer directFlag,
			PGSt_integer nPoints,
			PGSt_double  longitude[],
			PGSt_double  latitude[],
                        PGSt_double  mapX[],
                        PGSt_double  mapY[],
			PGSt_integer zone[]);

PGSt_SMF_status PGS_GCT_ValidateProjArgs(
			PGSt_integer projId,
                        PGSt_integer directFlag,
                        PGSt_integer nPoints);


#ifdef __cplusplus
#include <PGS_GCT_Prototypes.h>
}
#endif

#include <PGS_GCT_Prototypes.h>

#endif
