/*-------------------------------------------------------------------------*/
/*                                                                         */
/*  COPYRIGHT[copyright mark] 2000, Raytheon System Company, its vendors,  */
/*  and suppliers.  ALL RIGHTS RESERVED.                                   */
/*                                                                         */
/*-------------------------------------------------------------------------*/
/*****************************************************************************
BEGIN_FILE_PROLOG:

FILENAME:

	PGS_AA.h

DESCRIPTION:

	This file contains typedefs, #defines, and #includes specific to the
	Ancillary portion of the PGS Toolkit.  
AUTHOR:
	Graham Bland / EOSL

HISTORY:
	07-July-94 GJB  Initial version
	18-Aug-94  GJB  First configured version
	07-Feb-95  GJB  Add DEM specific parameter and prototype
				plus create PGS_AA.f
	11-July-95      ANS     Introduced prototype for PGS_AA_PeVA_stringF.c

END_FILE_PROLOG:
*****************************************************************************/

#ifndef PGSAA_h
#define PGSAA_h
  
#include <PGS_IO.h>
#include <PGS_AA_10.h>
  
/* these defines taben from PGS_AA_Tools. to allow user to have control */

#define PGSd_AA_MAXNOCACHES 2  /* defines number of memory buffers loaded by freeform */

#define PGSd_AA_MAXNOCHARS 355 /* defines path name size */

#define PGSd_AA_PEVMAXFILES 20 /* number of PeV files allowed */

#define PGSd_AA_OUT_OF_RANGE -1 /* this value is set when an input point is not found
				 * in any of the tiles 
				 */

/***********************************************************************

  Declare function prototypes.  All AA functions are included here as well 

  as Freeform modules since there is so much interaction between the 2d 
  and 3d tools
***********************************************************************/

/* main modules */

#ifdef __cplusplus
extern "C" {
#endif

PGSt_SMF_status PGS_AA_2DRead( 
			 char iparms[][100], 
			 PGSt_integer nParms,
			 PGSt_integer xStart,
			 PGSt_integer yStart,
			 PGSt_integer xDim,
			 PGSt_integer yDim,
			 PGSt_integer fileId,
			 PGSt_integer version,
			 PGSt_integer operation,
			 void *results);


PGSt_SMF_status PGS_AA_2Dgeo(  
			char iparms[][100], 
			PGSt_integer nParms,
			PGSt_double latitude[],
			PGSt_double longitude[],
			PGSt_integer nPoints,
			PGSt_integer fileId,
			PGSt_integer version,
			PGSt_integer operation, 
			void *results);

PGSt_SMF_status PGS_AA_3Dgeo( 
			char iparms[][100], 
			PGSt_integer nParms,
			PGSt_double latitude[],
			PGSt_double longitude[],
			PGSt_integer height[],
			PGSt_integer nPoints,
			PGSt_integer fileId,
			PGSt_integer version,
			PGSt_integer operation, 
			void *results);

PGSt_SMF_status PGS_AA_3DRead( 
			 char iparms[][100], 
			 PGSt_integer nParms,
			 PGSt_integer xStart,
			 PGSt_integer yStart,
			 PGSt_integer zStart,
			 PGSt_integer xDim,
			 PGSt_integer yDim,
			 PGSt_integer zDim,
			 PGSt_integer fileId,
			 PGSt_integer version,
			 PGSt_integer operation, 
			 void *results);


PGSt_SMF_status
PGS_AA_PeVA_real(
        PGSt_uinteger pevLogical,    /* input file logical */
        char *parameter,                        /* parameter name */
        PGSt_double *value );                        /* returned value */

PGSt_SMF_status
PGS_AA_PeVA_string(
        PGSt_uinteger pevLogical,    /* input file logical */
        char *parameter,                        /* parameter name */
        char *value[] );                          /* returned value */

PGSt_SMF_status
PGS_AA_PeVA_stringF(
                  PGSt_uinteger pevLogical,     /* input file logical */
                  char *parameter,              /* parameter name */
                  void *value,                  /* returned value */
                  unsigned int strLength);      /* size of string as defined in the
                                                 * fortran routine
                                                 */

PGSt_SMF_status
PGS_AA_PeVA_integer(
        PGSt_uinteger pevLogical,    /* input file logical */
        char *parameter,                        /* parameter name */
        PGSt_integer *value ); 

PGSt_SMF_status
PGS_AA_PeV_real(
        PGSt_uinteger PeV_Current_Logical,    /* input file logical */
        char *parameter,                        /* parameter name */
        PGSt_double *value );                        /* returned value */

PGSt_SMF_status
PGS_AA_PeV_string(
        PGSt_uinteger PeV_Current_Logical,    /* input file logical */
        char *parameter,                        /* parameter name */
        char *value );                          /* returned value */

PGSt_SMF_status
PGS_AA_PeV_integer(
        PGSt_uinteger PeV_Current_Logical,    /* input file logical */
        char *parameter,                        /* parameter name */
        PGSt_integer *value );                          /* returned value */

PGSt_SMF_status
PGS_AA_dem(                             /* fetches DEM values for the given set of points */
           char parm[][100],            /* parameter names (elevation, quality etc)
                                         * in a DEM data file
                                         */
           PGSt_integer nParms,      /* number of parameters */
           PGSt_double latitude[],  /* latitude vector of point with dimension equal to nPoints */
           PGSt_double longitude[], /* longitude vector of points with dimension equal to nPoints  */
           PGSt_integer versionFlag[],   /* DEM data flag may be tiled and this array contains
                                         * tile number for each point on return. If a particular
                                         * point is not in the DEM file then the associated
                                         * version flag would contain PGSd_DEM_OUT_OF_RANGE
                                         * With dimension equal to nPoints
                                         */
           PGSt_integer nPoints,        /* Number of points */
           PGSt_integer  fileId,      /* DEM data File id */
           PGSt_integer operation,      /* Mainly for future use but for now must be set to 1 */
           void *results);              /* result vector with dimension equal to nPoints */

#ifdef __cplusplus
}
#endif

#endif
