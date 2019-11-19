/*-------------------------------------------------------------------------*/
/*                                                                         */
/*  COPYRIGHT[copyright mark] 2000, Raytheon System Company, its vendors,  */
/*  and suppliers.  ALL RIGHTS RESERVED.                                   */
/*                                                                         */
/*-------------------------------------------------------------------------*/
/*****************************************************************************
BEGIN_FILE_PROLOG:

FILENAME:

	PGS_AA_Global.h

DESCRIPTION:

	This files holds definitions of global variables, which are 
        the support data.

AUTHOR:
	Graham Bland / EOSL

HISTORY:
	07-July-94 GJB  Initial version
	18-Aug-94  GJB  First configured version

END_FILE_PROLOG:
*****************************************************************************/

		       
char         cacheFormat1[PGSd_AA_MAXNOPARMS][PGSd_AA_MAXNOCHARS];

PGSt_integer cacheFormat2[PGSd_AA_MAXNOPARMS];

PGSt_integer cacheFormatBytes[PGSd_AA_MAXNOPARMS];

PGSt_integer  parmMemoryCache[PGSd_AA_MAXNOPARMS];

char parameterName[PGSd_AA_MAXNOCHARS];

PGSt_integer  parmMultiplier[PGSd_AA_MAXNOPARMS]; 
PGSt_integer  parmOffset[PGSd_AA_MAXNOPARMS];
char          parmUnits[PGSd_AA_MAXNOPARMS][PGSd_AA_MAXNOCHARS];
float         parmPrecision[PGSd_AA_MAXNOPARMS]; 
char          resolution  [PGSd_AA_MAXNOPARMS][PGSd_AA_MAXNOCHARS];   
float         parmAccuracy[PGSd_AA_MAXNOPARMS];
char          dataType[PGSd_AA_MAXNOPARMS][PGSd_AA_MAXNOCHARS];

PGSt_double   maxLat;
PGSt_double   minLat;
PGSt_double   maxLong;
PGSt_double   minLong;
PGSt_integer  maxHeight;
PGSt_integer  minHeight;
PGSt_integer	 xCells;
PGSt_integer	 yCells;
PGSt_integer	 zCells;
PGSt_integer funcIndex;
PGSt_integer logicalFile;
PGSt_integer autoOperation;
PGSt_integer physFileFormat;
PGSt_integer fileMemoryCache;
PGSt_double    lowerLeftLat;
PGSt_double     lowerLeftLong;
PGSt_double     meshLength;
PGSt_double     gridOrientation;



