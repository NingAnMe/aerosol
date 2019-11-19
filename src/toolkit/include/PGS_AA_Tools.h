/*-------------------------------------------------------------------------*/
/*                                                                         */
/*  COPYRIGHT[copyright mark] 2000, Raytheon System Company, its vendors,  */
/*  and suppliers.  ALL RIGHTS RESERVED.                                   */
/*                                                                         */
/*-------------------------------------------------------------------------*/
/*****************************************************************************
BEGIN_FILE_PROLOG:

FILENAME:

	PGS_AA_Tools.h

DESCRIPTION:

	This file contains typedefs, #defines, and #includes specific to the
	Ancillary portion of the PGS Toolkit.  This file is almost identical
	to PGS_AA_Private.h which the tool uses.  The only difference is 
	in the define statements saurrounding the freeform.h inclusion.  This
	is necessary so that freeform.h is included only once.

AUTHOR:
	Graham Bland / EOSL

HISTORY:
	07-July-94 GJB  Initial version
	18-Aug-94  GJB  First configured version

END_FILE_PROLOG:
*****************************************************************************/

#ifndef PGSAAt_h
#define PGSAAt_h
  
#include <PGS_IO.h>
#include <PGS_AA_10.h>
#include <PGS_AA.h>
  

/*****************************************************************

  #defines

*****************************************************************/

#define TOOL2D 2
#define TOOL3D 3
#define PGSd_AA_MAXNOPARMS 4
#define PGSd_AA_PEVMAXBUFSIZ 20480
#define PGSd_AA_ZERO 0
#define PGSd_AA_SUPPLIST 10900 

/* autoOperations */
#define PGSd_AA_AOP_PLATTECARRE 1
#define PGSd_AA_AOP_POLARSTEREO 2
#define PGSd_AA_AOP_GREENWICHSTART 4
#define PGSd_AA_AOP_IDLSTART 8
/* user defined operations, always 2^n */
#define PGSd_AA_OP_NEARESTCELL 1
#define PGSd_AA_OP_NINTCELL 2
#define PGSd_AA_OP_INTERP2BY2 4
#define PGSd_AA_OP_INTERP3BY3 8


/***********************************************************************

  Declare function prototypes.  All AA functions are included here as well 

  as Freeform modules since there is so much interaction between the 2d 
  and 3d tools
***********************************************************************/

/* modules other than top level (in PGS_AA.h) */



PGSt_SMF_status PGS_AA_Map( 
			     char *parms[], 
			     PGSt_integer nParms,
			     PGSt_integer fileId,
			     PGSt_integer version,
			     char *outputPhysFileFormat, 
			     char *physFileName,
			     PGSt_integer logSuppFile[PGSd_AA_MAXNOPARMS]);


PGSt_SMF_status PGS_AA_FF_Setup ( 
			    char *parms[], 
			    PGSt_integer nParms,
			    char *physFileName,
			    PGSt_integer logSuppFile[PGSd_AA_MAXNOPARMS], 
			    char *parmBuffer, 
			    char *outputFormat, 
			    char *outputPhysFileFormat);

PGSt_SMF_status PGS_AA_CheckFile(
			    char *physFileName, 
			    short *binCount);


PGSt_SMF_status PGS_AA_GetSupp( 
			       char *parms[], 
			       PGSt_integer nParms,
			       PGSt_integer logSuppFile[PGSd_AA_MAXNOPARMS], 
			       char *outputFormat, PGSt_integer *totalParmMemoryCache );

PGSt_SMF_status PGS_AA_2DReadGrid(  
			     char *parms[], 
			     PGSt_integer nParms,
			     PGSt_integer xStart,
			     PGSt_integer yStart,
			     PGSt_integer xDim,
			     PGSt_integer yDim, 
			     char *parmBuffer,
			     PGSt_integer totalParmMemoryCache,
			     void *results);

PGSt_SMF_status PGS_AA_2DReadGridF(
                             char *parms[],
                             PGSt_integer nParms,
                             PGSt_integer xStart,
                             PGSt_integer yStart,
                             PGSt_integer xDim,
                             PGSt_integer yDim,
                             char *parmBuffer,
                             PGSt_integer totalParmMemoryCache,
                             void *results);

PGSt_SMF_status PGS_AA_GEOGrid( 
			  char *parms[], 
			  PGSt_integer nParms,
			  PGSt_integer logSuppFile[],
			  PGSt_integer operation,
			  PGSt_double latitude[], 
			  PGSt_double longitude[],
			  PGSt_integer height[],
			  PGSt_integer nPoints, 
			  char *parmBuffer, 
			  short toolUsed,
			  PGSt_integer totalParmMemoryCache,
			  void *results); 

PGSt_SMF_status PGS_AA_GEOGridF(
                          char *parms[],
                          PGSt_integer nParms,
                          PGSt_integer logSuppFile[],
                          PGSt_integer operation,
                          PGSt_double latitude[],
                          PGSt_double longitude[],
                          PGSt_integer height[],
                          PGSt_integer nPoints,
                          char *parmBuffer,
                          short toolUsed,
                          PGSt_integer totalParmMemoryCache,
                          void *results);

PGSt_SMF_status PGS_AA_GetSuppGeo(PGSt_integer logSuppFile[]);

PGSt_SMF_status PGS_AA_AOP_GreenwichStart(
			PGSt_double latitude, 
			PGSt_double longitude, 
			PGSt_double *mlatitude, 
			PGSt_double *mlongitude);

PGSt_SMF_status PGS_AA_AOP_IDLStart(
			PGSt_double latitude, 
			PGSt_double longitude,
			PGSt_double *mlatitude, 
			PGSt_double *mlongitude);

PGSt_SMF_status PGS_AA_2DgeoF (
             		char iparms[][100],
             		PGSt_integer nParms,
             		PGSt_double latitude[],
            		PGSt_double longitude[],
           		PGSt_integer nPoints,
            		PGSt_integer fileId,
           		PGSt_integer version,
			PGSt_integer operation,
             		void *results);

PGSt_SMF_status PGS_AA_2DReadF (
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

PGSt_SMF_status PGS_AA_3DgeoF(
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

PGSt_SMF_status PGS_AA_3DReadF(
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

PGSt_SMF_status PGS_AA_demF(    
           		char parms[][100], 
           		PGSt_integer nParms,
           		PGSt_double latitude[],
           		PGSt_double longitude[],
           		PGSt_integer versionFlag[],
           		PGSt_integer nPoints,     
           		PGSt_integer fileId,     
           		PGSt_integer operation, 
           		void *results);

PGSt_SMF_status  PGS_AA_AOP_W3FB06(
				   PGSt_double latitude, 
				   PGSt_double longitude, 
				   PGSt_double *xRealStart,
				   PGSt_double *yRealStart);

PGSt_SMF_status PGS_AA_AOP_PLATTECARRE (   
			PGSt_double latitude, 
			PGSt_double longitude, 
			PGSt_double *xRealStart,
			PGSt_double *yRealStart);

PGSt_SMF_status PGS_AA_OP_NearestCell(
			 PGSt_double xRealStart,
			 PGSt_double yRealStart,
			 PGSt_integer *xStart,
			 PGSt_integer *yStart);

PGSt_SMF_status PGS_AA_OP_NINTCELL(
			 PGSt_double xRealStart,
			 PGSt_double yRealStart,
			 PGSt_integer *xStart,
			 PGSt_integer *yStart);

PGSt_SMF_status PGS_AA_BiLinearInterp(
                PGSt_double xDimention1[], /* first dimention */
                PGSt_double xDimention2[], /* second dimention */
                PGSt_double **yGridValues, /* grid values */
                PGSt_integer  rank1,	   /* grid size in 1st dimention */
		PGSt_integer  rank2, 	   /* grid size in 2nd dimention */
                PGSt_double xVal1,         /* first dimention value of the given point */
                PGSt_double xVal2,         /* seconfd dimention value of the given point */
                PGSt_double* yResult,      /* pointer to point to the result */
                PGSt_double* dyResult);     /* error estimate */

PGSt_SMF_status PGS_AA_LinearInterp(
                PGSt_double xDimention[], /* independent variable */
                PGSt_double yDimention[], /* dependent variable */
                PGSt_integer nPoints,     /* number of points */
                PGSt_double xVal,         /* x value where the y value is required */
                PGSt_double *yRes,        /* address to hold the result value */
                PGSt_double *dyRes);       /* error estimate */

PGSt_SMF_status PGS_AA_3DReadGrid( 
			     char *parms[], 
			     PGSt_integer nParms,
			     PGSt_integer xStart,
			     PGSt_integer yStart,
			     PGSt_integer zStart,
			     PGSt_integer xDim,
			     PGSt_integer yDim, 
			     PGSt_integer zDim, 
			     char *parmBuffer,
			     PGSt_integer totalParmMemoryCache,
			     void *results);

PGSt_SMF_status PGS_AA_3DReadGridF(
                             char *parms[],
                             PGSt_integer nParms,
                             PGSt_integer xStart,
                             PGSt_integer yStart,
                             PGSt_integer zStart,
                             PGSt_integer xDim,
                             PGSt_integer yDim,
                             PGSt_integer zDim,
                             char *parmBuffer,
                             PGSt_integer totalParmMemoryCache,
                             void *results);

PGSt_SMF_status
PGS_AA_Index(
	     char *parms[], 
	     PGSt_integer nParms,
	     PGSt_integer logSuppFile[PGSd_AA_MAXNOPARMS],
	     PGSt_integer *formatId);

PGSt_SMF_status
PGS_AA_PhysFile(
		char *parms[], 
		PGSt_integer nParms,
		PGSt_integer logSuppFile[PGSd_AA_MAXNOPARMS], 
		PGSt_integer formatId,
		PGSt_integer fileId,
		PGSt_integer version,
		char *outputPhysFileFormat, 
		char *physFileName);
	       
     
/* extern PGSt_SMF_status    val;      return value */
/* extern PGSt_SMF_status    errVal;   return values from SMF tool which is not further tested */
    
extern char         cacheFormat1[][PGSd_AA_MAXNOCHARS];

extern PGSt_integer cacheFormat2[];

extern PGSt_integer cacheFormatBytes[];

extern PGSt_integer  parmMemoryCache[];

extern char parameterName[];

extern PGSt_integer  parmMultiplier[]; 
extern PGSt_integer  parmOffset[];
extern char          parmUnits[][PGSd_AA_MAXNOCHARS];
extern float         parmPrecision[]; 
extern char          resolution  [][PGSd_AA_MAXNOCHARS];   
extern float         parmAccuracy[];
extern char          dataType[][PGSd_AA_MAXNOCHARS];

extern PGSt_double   maxLat;
extern PGSt_double   minLat;
extern PGSt_double   maxLong;
extern PGSt_double   minLong;
extern PGSt_integer  maxHeight;
extern PGSt_integer  minHeight;
extern PGSt_integer	 xCells;
extern PGSt_integer	 yCells;
extern PGSt_integer	 zCells;
extern PGSt_integer logicalFile;
extern PGSt_integer autoOperation;
extern PGSt_integer physFileFormat;
extern PGSt_integer fileMemoryCache;
extern PGSt_double    lowerLeftLat;
extern PGSt_double     lowerLeftLong;
extern PGSt_double     meshLength;
extern PGSt_double     gridOrientation;
extern PGSt_integer    funcIndex;

static PGSt_integer (*interpFunc[1])(double*, double*, double**, int, int, double, double, double*, double*) = 
                {PGS_AA_BiLinearInterp};

#endif













