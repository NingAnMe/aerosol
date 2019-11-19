/*-------------------------------------------------------------------------*/
/*                                                                         */
/*  COPYRIGHT[copyright mark] 2000, Raytheon System Company, its vendors,  */
/*  and suppliers.  ALL RIGHTS RESERVED.                                   */
/*                                                                         */
/*-------------------------------------------------------------------------*/
/*******************************************************************************
BEGIN_FILE_PROLOG:

NAME:   PGS_EPH.h

DESCRIPTION:

This file contains the common error handling defines, typedefs
and function prototypes.    

HISTORY:
        28-Sept-1998  AT Added prototype for auxilary function
	                 PGS_EPH_GetEphMetAux, added to the file
			 PGS_EPH_GetEphMet.c
        30-Dec-1998  PDN Changed names of certain arrays to correctly
                         identify the contents. yawPitchRoll -> eulerAngle,
                         and eulerAngleRates -> angularVelocity.
        10-Dec-2001  XW  Provided the actual attitude data upon request.
	10-Feb-2002  XW  Provided ephemeris data from HDF files.

END_PROLOG:                              
*******************************************************************************/

#ifndef _PGS_EPH_H_
#define _PGS_EPH_H_

/*  include files  */

#include <PGS_TD.h>

/* error messages */

#include <PGS_EPH_5.h>

/* logical identifiers for file and parameters defined in the Process Control
   File */
 
#define  PGSd_SC_EPHEM_DATA   10501
#define  PGSd_SC_ATTIT_DATA   10502
#define  PGSd_EPH_QFLAG_MASK  10507
#define  PGSd_ATT_QFLAG_MASK  10508
#define  PGSd_SC_TAG_INFO_FILE 10801
#define  PGSd_SC_EPHHDF_DATA   10501
#define  PGSd_SC_ATTHDF_DATA   10502

/* Fields for HDF stuff */

#define  DpDPrParentUrsField         "parentURs"
#define  DpDPrScIdField              "spacecraftId"
#define  DpDPrAsciiTimeField         "asciiTimeRange"
#define  DpDPrSourceField            "source"
#define  DpDPrVersionField           "version"
#define  DpDPrStartTimeField         "startTime"
#define  DpDPrEndTimeField           "endTime"
#define  DpDPrIntervalField          "interval"
#define  DpDPrNURsField              "nURs"
#define  DpDPrNRecordsField          "nRecords"
#define  DpDPrNOrbitsField           "nOrbits"
#define  DpDPrOrbitStartField        "orbitNumberStart"
#define  DpDPrOrbitEndField          "orbitNumberEnd"
#define  DpDPrKeplerFrameField       "keplerRefFrame"
#define  DpDPrKeplerElementsField    "keplerElements"
#define  DpDPrKeplerEpochField       "keplerEpochTai"
#define  DpDPrQaParamsField          "qaParameters"
#define  DpDPrQaStatsField           "qaStatistics"
#define  DpDPrOrbPeriodField         "orbitalPeriod"
#define  DpDPrDescPropField          "descNodePropagation"
#define  DpDPrFddReplaceField        "fddReplacement"
#define  DpDPrEphemerisHeaderFields  "spacecraftId,asciiTimeRange,source,version,startTime,endTime,interval,nURs,nRecords,nOrbits,orbitNumberStart,orbitNumberEnd,keplerRefFrame,keplerElements,keplerEpochTai,qaParameters,qaStatistics,orbitalPeriod,descNodePropagation,fddReplacement,parentURs"
#define  DpDPrEulerAngleOrderField    "eulerAngleOrder"
#define  DpDPrAttitudeHeaderFields    "spacecraftId,asciiTimeRange,source,version,startTime,endTime,interval,nURs,nRecords,eulerAngleOrder,qaParameters,qaStatistics,parentURs"
#define  DpDPrTimeField              "time"
#define  DpDPrXPosField              "xPos"
#define  DpDPrYPosField              "yPos"
#define  DpDPrZPosField              "zPos"
#define  DpDPrXVelField              "xVel"
#define  DpDPrYVelField              "yVel"
#define  DpDPrZVelField              "zVel"
#define  DpDPrQualityFlagField       "qFlagEph"
#define  DpDPrEphemerisDataFields    "time,xPos,yPos,zPos,xVel,yVel,zVel,qFlagEph"
#define  DpDPrFirstEulerAngleField   "firstEulerAngle"
#define  DpDPrSecondEulerAngleField  "secondEulerAngle"
#define  DpDPrThirdEulerAngleField   "thirdEulerAngle"
#define  DpDPrXRateField             "xRate"
#define  DpDPrYRateField             "yRate"
#define  DpDPrZRateField             "zRate"
#define  DpDPrAttQualityFlagField    "qFlagAtt"
#define  DpDPrAttitudeDataFields    "time,firstEulerAngle,secondEulerAngle,thirdEulerAngle,xRate,yRate,zRate,qFlagAtt"
#define  DpDPrOrbitNumberField       "orbitNumber"
#define  DpDPrAscendTimeField        "orbitAscendTime"
#define  DpDPrDescendTimeField       "orbitDescendTime"
#define  DpDPrDescendLongitudeField  "orbitDescendLongitude"
#define  DpDPrEphemerisOrbitFields   "orbitNumber,orbitAscendTime,orbitDescendTime,orbitDescendLongitude"

/* Size of UR field in ephemeris/attitude file headers */

#define PGSd_UR_FIELD_SIZE 256U

/* quality flag maskes */

#define PGSd_NO_DATA 0x1000
#define PGSd_INTERPOLATED_POINT 0x4000
#define PGSd_PLATFORM_FATAL 0x10000

/* special data types */

#ifdef __cplusplus
extern "C" {
#endif


typedef struct
{
    char          spacecraftID[24];     /* Spacecraft Name */
    char          asciiTimeRange[48];   /* start/stop times to nearest hour */
    char          source[32];           /* Source of the data */
    char          version[8];           /* Version number (default = 1) */
    PGSt_double   startTime;            /* Ephemeris dataset start time,
					   secTAI93 */
    PGSt_double   endTime;              /* Ephemeris dataset end time,
					   secTAI93 */
    PGSt_real     interval;             /* Standard interval between records */
    PGSt_uinteger nURs;
    PGSt_uinteger nRecords;             /* Number of ephemeris records */
    PGSt_uinteger nOrbits;              /* Number of orbits spanned */
    PGSt_uinteger orbitNumberStart;     /* Number of 1st orbit or part orbit */
    PGSt_uinteger orbitNumberEnd;       /* Number of last orbit or part orb. */
    char          keplerRefFrame[8];    /* Reference Frame: "TOD" or "J2000" */
    PGSt_double   keplerElements[6];    /* Osculating Keplerian elements at
					   epoch */
    PGSt_double   keplerEpochTAI;       /* TAI93 Epoch of Keplerian Elements */
    PGSt_real     qaParameters[16];     /* Ephemeris data quality processing
					   parameters */
    PGSt_real     qaStatistics[4];      /* Quality assurance statistics */
    char          spare[216];           /* Pad to 512 bytes */
} PGSt_ephemHeader;

typedef struct
{
    char          spacecraftID[24];     /* Spacecraft Name */
    char          asciiTimeRange[48];   /* start/stop times to nearest hour */
    char          source[32];           /* Source of the data */
    char          version[8];           /* Version number (default = 1) */
    PGSt_double   startTime;            /* Ephemeris dataset start time,
					   secTAI93 */
    PGSt_double   endTime;              /* Ephemeris dataset end time,
					   secTAI93 */
    PGSt_real     interval;             /* Standard interval between records */
    PGSt_uinteger nURs;                 /*  */
    PGSt_uinteger nRecords;             /* Number of ephemeris records */
    PGSt_uinteger eulerAngleOrder[3];   /* Order of rotations as a permutation
					   of 1=x, 2=y, 3=z */
    PGSt_real     qaParameters[16];     /* Ephemeris data quality processing
					   parameters */
    PGSt_real     qaStatistics[4];      /* Quality assurance statistics */
    char          spare[280];           /* Pad to 512 bytes */
} PGSt_attitHeader;

typedef struct
{
    PGSt_double   secTAI93;
    PGSt_double   position[3];
    PGSt_double   velocity[3];
    PGSt_uinteger qualityFlag;
    char          spare[4];
} PGSt_ephemRecord;

typedef struct
{
    PGSt_double   secTAI93;
    PGSt_double   eulerAngle[3];
    PGSt_double   angularVelocity[3];
    PGSt_uinteger qualityFlag;
    char          spare[4];
} PGSt_attitRecord;

typedef struct
{
    PGSt_integer  file_version_num;
    PGSt_double   startTAI93;
    PGSt_double   stopTAI93;
} PGSt_hdrSummary;

typedef struct
{
    PGSt_uinteger orbitNumber;           /* Orbit number, from mission start */
    char          spare[4];
    PGSt_double   orbitAscendTime;       /* Time of upward true equator
					    crossing, secTAI93 */
    PGSt_double   orbitDescendTime;      /* Time of downward true equator
					    crossing, secTAI93 */
    PGSt_double   orbitDescendLongitude; /* Orbit down-crossing terrestrial
					    longitude, radians */
} PGSt_ephemMetadata;

typedef struct
{
  PGSt_tag     spacecraftTag;
  char         spacecraftName[32];
  PGSt_integer eulerAngleOrder[3];
} PGSt_scTagInfo;

typedef enum
{
  PGSe_TAG_SEARCH=71,
  PGSe_NAME_SEARCH=72
} PGSt_searchTag;

/* function prototypes */

extern PGSt_SMF_status
PGS_EPH_interpolateAttitude(           
    PGSt_double,           
    PGSt_double [3],   
    PGSt_double [3],
    PGSt_double,             
    PGSt_double [3],      
    PGSt_double [3],  
    PGSt_integer [3],  
    PGSt_double,             
    PGSt_double [3],         
    PGSt_double [3]);    

extern PGSt_SMF_status
PGS_EPH_interpolatePosVel(
    PGSt_double,
    PGSt_double [3],
    PGSt_double [3],
    PGSt_double,
    PGSt_double [3],
    PGSt_double [3],
    PGSt_double,
    PGSt_double [3],
    PGSt_double [3]);

extern PGSt_SMF_status
PGS_EPH_EphemAttit(
    PGSt_tag,
    PGSt_integer,
    char *,
    PGSt_double [],
    PGSt_boolean,
    PGSt_boolean,
    void *,
    PGSt_double [][3],
    PGSt_double [][3],
    PGSt_double [][3],
    PGSt_double [][3],
    PGSt_double [][4]);

extern PGSt_SMF_status
PGS_EPH_EphAtt_unInterpolateAux(
    PGSt_tag,
    char *,
    char *,
    PGSt_boolean,
    PGSt_boolean,
    void *,
    PGSt_integer *,
    PGSt_integer,
    char [][28],
    PGSt_double [][3],
    PGSt_double [][3],
    PGSt_double [][3],
    PGSt_double [][3],
    PGSt_double [][4]);

extern PGSt_SMF_status
PGS_EPH_EphAtt_unInterpolate(
    PGSt_tag,
    char *,
    char *,
    PGSt_boolean,
    PGSt_boolean,
    void *,
    PGSt_integer *,
    char [][28],
    PGSt_double [][3],
    PGSt_double [][3],
    PGSt_double [][3],
    PGSt_double [][3],
    PGSt_double [][4]);


extern PGSt_SMF_status
PGS_EPH_UnInterpEphAtt(
    PGSt_tag,
    char *,
    char *,
    PGSt_boolean,
    PGSt_boolean,
    void *,
    PGSt_integer *,
    PGSt_integer *,
    char [][28],
    char [][28],
    PGSt_double [][3],
    PGSt_double [][3],
    PGSt_double [][3],
    PGSt_double [][3],
    PGSt_double [][4]);


extern PGSt_SMF_status
PGS_EPH_UnInterpEphAttAux(
    PGSt_tag,
    char *,
    char *,
    PGSt_boolean[2],
    void *,
    PGSt_integer [2],
    PGSt_integer,
    char [][28],
    char [][28],
    PGSt_double [][3],
    PGSt_double [][3],
    PGSt_double [][3],
    PGSt_double [][3],
    PGSt_double [][4]);

extern PGSt_SMF_status
PGS_EPH_GetEphMet(                          
    PGSt_tag,
    PGSt_integer,
    char [28],
    PGSt_double [],
    PGSt_integer *,
    PGSt_integer [],
    char [][28],
    char [][28],
    PGSt_double []);

extern PGSt_SMF_status
PGS_EPH_GetEphMetAux(                          
    PGSt_tag,
    PGSt_integer,
    PGSt_integer,
    char [28],
    PGSt_double [],
    PGSt_integer *,
    PGSt_integer [],
    char [][28],
    char [][28],
    PGSt_double []);

extern void 
PGS_EPH_file_split_path(
    char *,
    char *,
    char * );

extern PGSt_boolean
PGS_EPH_file_exists(
     char * );

PGSt_SMF_status
PGS_EPH_getEphemRecords(
    PGSt_scTagInfo*,
    PGSt_double,
    PGSt_integer,
    PGSt_ephemRecord*,
    PGSt_integer*,
    PGSt_integer*);

PGSt_SMF_status
PGS_EPH_getEphemRecords_UN(
    PGSt_scTagInfo*,
    PGSt_double,
    PGSt_integer,
    PGSt_ephemRecord*,
    PGSt_integer*,
    PGSt_integer*);

PGSt_SMF_status
PGS_EPH_getEphemHeaders(
    PGSt_scTagInfo*,
    PGSt_hdrSummary**,
    PGSt_integer*,
    PGSt_integer*);

PGSt_SMF_status
PGS_EPH_getAttitRecords(
    PGSt_scTagInfo*,
    PGSt_integer*,
    PGSt_double,
    PGSt_integer,
    PGSt_attitRecord*,
    PGSt_integer*);

PGSt_SMF_status
PGS_EPH_getAttitRecords_UN(
    PGSt_scTagInfo*,
    PGSt_integer*,
    PGSt_double,
    PGSt_integer,
    PGSt_attitRecord*,
    PGSt_integer*);

PGSt_SMF_status
PGS_EPH_getAttitHeaders(
    PGSt_scTagInfo*,
    PGSt_integer*,
    PGSt_hdrSummary**,
    PGSt_integer*);

PGSt_SMF_status
PGS_EPH_ManageMasks(
    PGSt_integer,
    PGSt_integer [2]);

PGSt_SMF_status
PGS_EPH_GetSpacecraftData(
    PGSt_tag,
    char*,
    PGSt_searchTag,
    PGSt_scTagInfo*);

char*
PGS_EPH_getToken(char*, const char*);

#ifdef __cplusplus
}
#endif

#endif
