/*-------------------------------------------------------------------------*/
/*                                                                         */
/*  COPYRIGHT[copyright mark] 2000, Raytheon System Company, its vendors,  */
/*  and suppliers.  ALL RIGHTS RESERVED.                                   */
/*                                                                         */
/*-------------------------------------------------------------------------*/
/*******************************************************************************
BEGIN_FILE_PROLOG:

NAME:
   PGS_CSC.h

DESCRIPTION:
   This file contains the common error handling defines, typedefs
   and function prototypes.

END_FILE_PROLOG:
*******************************************************************************/

#ifndef _PGS_CSC_H_
#define _PGS_CSC_H_

/*  include files  */

#include <PGS_SMF.h>
#include <PGS_TD.h>    /* defines spacecraft tags */
#include <PGS_CBP.h>   /* defines celestial body tags */

/* error messages */

#include <PGS_CSC_4.h>

/* logical ID (in the Process Control File) of UT1-UTC and polar motion file
   (utcpole.dat) */

#define PGSd_UTCPOLE 10401

/* max elements for array in PGS_CSC_UTC_UT1Pole()*/
#define MAX_RECS 2400


/* header length for the utcpole.dat file; 1st line
   is  99 characters and second is 69, including in
   each case the newline. Second line is hard coded */

#define PGSd_UTCPOLE_FIRST_TWO 168

/* record length for numerical data records is checked
  against actual write operation, to prevent problems
  in case of future format changes */

#define PGSd_UTCPOLE_RECORD 65


/* Input Vector Tags for ZenithAzimuth tool */


#define PGSd_CB   314159265   /* generic ECR vector to very distant
				 object (many many Earth radii)  or
				 any "free" vector pointing away  */
#define PGSd_LOOK 358979323   /* vector from SC to Look Point in ECR */

/* GetEarthFigure */
#define PGSd_DEFAULT_POLAR_RAD  6356752.314245 /* WGS84 Earth polar radius (m) */
#define PGSd_DEFAULT_EQUAT_RAD  6378137.0      /* WGS84 Earth equatorial radius (m) */
#define PGSd_DEFAULT_EARTH_TAG "THIS IS THE DEFAULT (WGS84) EARTH MODEL"

/* Day/Night Limit Tags */

#define PGSd_CivilTwilight      90
#define PGSd_CivilNight         96
#define PGSd_NauticalNight      102
#define PGSd_AstronNight        108

/* The sum of the squares of the components of a quaternion should be within 1E-7 of one */
#define PGSd_LOWER_LIMIT  .9999999
#define PGSd_UPPER_LIMIT 1.0000001

/* special data types */

typedef PGSt_integer PGSt_CSC_EarthTag;


/*     Function prototypes    */

#ifdef __cplusplus
extern "C" {
#endif

extern  PGSt_SMF_status
PGS_CSC_BorkowskiGeo(
    PGSt_double,
    PGSt_double,
    PGSt_double,
    PGSt_double,
    PGSt_double *,
    PGSt_double *);

extern PGSt_double
PGS_CSC_VecToVecAngle(
    PGSt_double,
    PGSt_double,
    PGSt_double,
    PGSt_double);

extern PGSt_SMF_status
PGS_CSC_GrazingRay(
    char        [20],
    PGSt_double [3],
    PGSt_double [3],
    PGSt_double *,
    PGSt_double *,
    PGSt_double *,
    PGSt_double *,
    PGSt_double [3],
    PGSt_double [3]);

extern PGSt_SMF_status
PGS_CSC_LookTwice(
    PGSt_double  [3],
    PGSt_double  [3],
    PGSt_double  ,
    PGSt_double  ,
    PGSt_double  ,
    PGSt_double  [2],
    PGSt_double  [2][3]);

extern PGSt_SMF_status
PGS_CSC_nutate2000(
    PGSt_integer,
    PGSt_double [2],
    PGSt_double [4],
    PGSt_boolean,
    PGSt_double []);

extern PGSt_SMF_status
PGS_CSC_rotat6(
    PGSt_double [6],
    PGSt_double [2],
    PGSt_integer,
    PGSt_double [6]);

extern PGSt_SMF_status
PGS_CSC_wahr2(
    PGSt_double [2],
    PGSt_double [4]);

extern PGSt_SMF_status
PGS_CSC_quickWahr(
    PGSt_double,
    PGSt_double [2],
    PGSt_double [4]);

extern PGSt_SMF_status
PGS_CSC_precs2000(
    PGSt_integer,
    PGSt_double [2],
    PGSt_boolean,
    PGSt_double []);

extern PGSt_SMF_status
PGS_CSC_rotat3(
    PGSt_double [3],
    PGSt_double [2],
    PGSt_integer,
    PGSt_double [3]);

extern PGSt_SMF_status
PGS_CSC_DayNight(
    PGSt_integer,
    char [28],
    PGSt_double [],
    PGSt_double [],
    PGSt_double [],
    PGSt_tag,
    PGSt_boolean []);

extern PGSt_SMF_status
PGS_CSC_TODtoJ2000(
    PGSt_integer,
    PGSt_double,
    PGSt_double [],
    PGSt_double []);

extern PGSt_SMF_status
PGS_CSC_J2000toTOD(
    PGSt_integer,
    PGSt_double,
    PGSt_double [],
    PGSt_double []);

extern PGSt_SMF_status
PGS_CSC_UTC_UT1Pole(
    PGSt_double [2],
    PGSt_double *,
    PGSt_double *,
    PGSt_double *,
    PGSt_double *);

extern PGSt_SMF_status
PGS_CSC_ECItoECR(
    PGSt_integer,
    char [28],
    PGSt_double [],
    PGSt_double [][6],
    PGSt_double [][6]);

extern PGSt_SMF_status
PGS_CSC_ECItoORB(
    PGSt_tag,
    PGSt_integer,
    char [28],
    PGSt_double [],
    PGSt_double [][3],
    PGSt_double [][3]);

extern PGSt_SMF_status
PGS_CSC_ECItoSC(
    PGSt_tag,
    PGSt_integer,
    char [28],
    PGSt_double [],
    PGSt_double [][3],
    PGSt_double [][3]);

extern PGSt_SMF_status
PGS_CSC_ORBtoECI(
    PGSt_tag,
    PGSt_integer,
    char [28],
    PGSt_double [],
    PGSt_double [][3],
    PGSt_double [][3]);

extern PGSt_SMF_status
PGS_CSC_ORBtoSC(
    PGSt_tag,
    PGSt_integer,
    char [28],
    PGSt_double [],
    PGSt_double [][3],
    PGSt_double [][3]);

extern void
PGS_CSC_RectToGeoCen(
    PGSt_double  [3],
    PGSt_double  *,
    PGSt_double  *,
    PGSt_double  *);

extern PGSt_double*
PGS_CSC_GeoCenToRect(
        PGSt_double,
        PGSt_double,
        PGSt_double,
        PGSt_double  *);

extern PGSt_SMF_status
PGS_CSC_SCtoORB(
    PGSt_tag,
    PGSt_integer,
    char [28],
    PGSt_double [],
    PGSt_double [][3],
    PGSt_double [][3]);

extern PGSt_SMF_status
PGS_CSC_EulertoOrbit_Quat(
    PGSt_double [3],
    PGSt_integer [3],
    PGSt_double [4]);

extern PGSt_SMF_status
PGS_CSC_SCtoECI(
    PGSt_tag,
    PGSt_integer,
    char [28],
    PGSt_double [],
    PGSt_double [][3],
    PGSt_double [][3]);

extern PGSt_SMF_status
PGS_CSC_ECRtoECI(
    PGSt_integer,
    char [28],
    PGSt_double [],
    PGSt_double [][6],
    PGSt_double [][6]);

extern PGSt_SMF_status
PGS_CSC_ECRtoGEO(
    PGSt_double [3],
    char *,
    PGSt_double *,
    PGSt_double *,
    PGSt_double *);

extern PGSt_SMF_status
PGS_CSC_GEOtoECR(
    PGSt_double,
    PGSt_double,
    PGSt_double,
    char *,
    PGSt_double [3]);

extern PGSt_SMF_status
PGS_CSC_GreenwichHour(
    PGSt_integer,
    char [28],
    PGSt_double [],
    PGSt_double []);

extern PGSt_SMF_status
PGS_CSC_quatRotate(
    PGSt_double [4],
    PGSt_double [3],
    PGSt_double [3]);

extern PGSt_SMF_status
PGS_CSC_SubSatPoint(
    PGSt_tag,
    PGSt_integer,
    char [28],
    PGSt_double [],
    char *,
    PGSt_boolean,
    PGSt_double [],
    PGSt_double [],
    PGSt_double [],
    PGSt_double [][3]);

extern PGSt_SMF_status
PGS_CSC_SubSatPointVel(
    PGSt_integer,
    PGSt_double [][6],
    char *,
    PGSt_double [],
    PGSt_double [],
    PGSt_double [],
    PGSt_double [][3]);


extern PGSt_SMF_status
PGS_CSC_GetFOV_Pixel(
    PGSt_tag,         /* spacecraftTag */
    PGSt_integer,     /* numOffsets */
    char [28],        /* UTC */
    PGSt_double [],   /* time offsets */
    char [20],        /* Earth tag */
    PGSt_boolean,     /* accuracy Flag */
    PGSt_double [][3],/* pixel Unit vectors in SC coords */
    PGSt_double [][3],/* instrument XYZ offset in SC coords */
    PGSt_double [],   /* latitudes  */
    PGSt_double [],   /* longitudes */
    PGSt_double [][3],/* ECR pixel unit vectors */
    PGSt_double [],   /* slant ranges */
    PGSt_double []);  /* Doppler velocities */

extern PGSt_SMF_status
PGS_CSC_GetEarthFigure(
    char *,
    PGSt_double *,
    PGSt_double *);

extern PGSt_SMF_status
PGS_CSC_LookPoint(
    PGSt_double [3],
    PGSt_double [3],
    PGSt_double,
    PGSt_double,
    PGSt_double,
    PGSt_double *,
    PGSt_double [3]);

extern PGSt_SMF_status
PGS_CSC_ZenithAzimuth(
    PGSt_double [3],
    PGSt_double,
    PGSt_double,
    PGSt_double,
    PGSt_tag,
    PGSt_boolean,
    PGSt_boolean,
    PGSt_double *,
    PGSt_double *,
    PGSt_double *);

extern PGSt_SMF_status
PGS_CSC_SpaceRefract(
    PGSt_double,
    PGSt_double,
    PGSt_double,
    PGSt_double *,
    PGSt_double *);

extern PGSt_double
PGS_CSC_Norm(
    PGSt_double *);

extern PGSt_SMF_status
PGS_CSC_getQuats(
    PGSt_double [3][3],
    PGSt_double [4]);

extern PGSt_SMF_status
PGS_CSC_QuatToEuler(
    PGSt_double [],
    PGSt_integer [],
    PGSt_double []);

extern PGSt_SMF_status
PGS_CSC_QuatToMatrix(
    PGSt_double [4],
    PGSt_double [3][3]);

extern PGSt_SMF_status
PGS_CSC_EulerToQuat(
    PGSt_double [],
    PGSt_integer [],
    PGSt_double []);

extern PGSt_double *
PGS_CSC_crossProduct(
    PGSt_double [],
    PGSt_double [],
    PGSt_double []);

extern PGSt_double
PGS_CSC_dotProduct(
    PGSt_double [],
    PGSt_double [],
    PGSt_integer);

extern PGSt_SMF_status
PGS_CSC_getECItoORBquat(
    PGSt_double [3],
    PGSt_double [3],
    PGSt_double [4]);

extern PGSt_SMF_status
PGS_CSC_getORBtoECIquat(
    PGSt_double [3],
    PGSt_double [3],
    PGSt_double [4]);

extern PGSt_SMF_status
PGS_CSC_getORBtoTIPquat(
    char [28],
    char *,
    PGSt_double [3],
    PGSt_double [4]);

extern PGSt_SMF_status
PGS_CSC_quatMultiply(
    PGSt_double [4],
    PGSt_double [4],
    PGSt_double [4]);

extern PGSt_SMF_status
PGS_CSC_TiltYaw(
    PGSt_double [3],
    PGSt_double [3],
    PGSt_double,
    PGSt_double,
    PGSt_double [4]);

extern PGSt_SMF_status
PGS_CSC_PointInFOVgeom(
    PGSt_integer,
    PGSt_double [3],
    PGSt_double [][3],
    PGSt_double  [3],
    PGSt_boolean *);

extern PGSt_SMF_status
PGS_CSC_Earthpt_FixedFOV(
    PGSt_integer,
    char [28],
    PGSt_double [],
    PGSt_tag,
    char  *,
    PGSt_double,
    PGSt_double,
    PGSt_double,
    PGSt_integer,
    PGSt_double [3],
    PGSt_double [][3],
    PGSt_boolean [],
    PGSt_double [][3]);

extern PGSt_SMF_status
PGS_CSC_Earthpt_FOV(
    PGSt_integer,
    char [28],
    PGSt_double [],
    PGSt_tag,
    char  *,
    PGSt_double,
    PGSt_double,
    PGSt_double,
    PGSt_integer,
    PGSt_double [][3],
    void *,
    PGSt_boolean [],
    PGSt_double [][3]);

extern PGSt_SMF_status
PGS_CSC_FOVconicalHull(
    PGSt_integer,
    PGSt_double [3],
    PGSt_double [][3],
    PGSt_double,
    PGSt_double [3],
    PGSt_double [][3],
    PGSt_boolean *,
    PGSt_double  *);

extern PGSt_SMF_status
PGS_CSC_EarthOccult(
    PGSt_double [3],
    PGSt_double,
    PGSt_double,
    PGSt_double [3],
    PGSt_double [3],
    PGSt_double [3],
    PGSt_boolean *);

/* GENERAL REFERENCES FOR ECItoECR and ECRtoECI transformations

Astronomical Almanac, published by U.S. Government Printing Office for
the U.S. Naval Observatory (annual - 1994 is referenced here).
(denoted "Astron. Almanac" or "A.A.")

Explanatory Supplement to the Astronomical Almanac
ed: Dr. P. Kenneth Seidelmann
Published by University Science Books, Mill Valley, CA 1992
(denoted "Explan. Suppl.")    */

#ifdef __cplusplus
}
#endif

#endif
