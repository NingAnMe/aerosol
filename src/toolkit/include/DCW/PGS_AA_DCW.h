/*****************************************************************************
BEGIN_FILE_PROLOG:

FILENAME:

	PGS_AA_DCW.h

DESCRIPTION:

	This file contains typedefs, #defines, and #includes specific to the
	DCW tool in the PGS Toolkit.  This file is almost identical
	to PGS_AA_Tools.h which the tool uses.  The only difference is 
	in the define statements saurrounding the freeform.h inclusion.  This
	is necessary so that freeform.h is included only once.

AUTHOR:
	Graham Bland / EOSL

HISTORY:
	07-July-94 GJB  Initial version
	18-Aug-94  GJB  First configured version

END_FILE_PROLOG:
*****************************************************************************/

#ifndef PGSAAD_h
#define PGSAAD_h
  
#include <cfortran.h>
#include <PGS_IO.h>
#include <PGS_AA_10.h>
  
/* various short term fixes */

/*#typedef unsigned int   PGSt_uinteger; */     /* PGS unsigned int */
typedef pgs_status PGSt_SMF_Status;
/* end of fixes */

  
/*****************************************************************

  #defines

*****************************************************************/


#define PGSd_AA_MAXNOPARMS 4
#ifndef PGSd_AA_MAXNOCHARS
#define PGSd_AA_MAXNOCHARS 80
#endif
#define PGSd_DCW_MAXTRIES 1

/* logical file id nos for ancillary data */

#define PGSd_AA_NOAMER 10991
#define PGSd_AA_SOAMAFR 10992
#define PGSd_AA_EURNASIA 10990
#define PGSd_AA_SASAUS 10993



/***********************************************************************

  Declare function prototypes.  All AA functions are included here as well 

  as Freeform modules since there is so much interaction between the 2d 
  and 3d tools
***********************************************************************/

/* main modules */

PGSt_SMF_status PGS_AA_dcw(char iparms[][100], 
			   PGSt_integer nParms,
			   PGSt_double latitude[],
			   PGSt_double longitude[],
			   PGSt_integer nPoints,
			   void *results);

#endif
