/*-------------------------------------------------------------------------*/
/*                                                                         */
/*  COPYRIGHT[copyright mark] 2000, Raytheon System Company, its vendors,  */
/*  and suppliers.  ALL RIGHTS RESERVED.                                   */
/*                                                                         */
/*-------------------------------------------------------------------------*/
/*******************************************************************************
BEGIN_FILE_PROLOG:

NAME:   PGS_SIM.h

DESCRIPTION:

This file contains the common error handling defines, typedefs
and function prototypes.    

END_PROLOG:                              
*******************************************************************************/

#ifndef _PGS_SIM_H_
#define _PGS_SIM_H_

/*  include files  */

#include <PGS_TD.h>

typedef struct
{
    char        time[28];        /* UTC time of record */
    PGSt_double pos[3];          /* position vector */
    PGSt_double vel[3];          /* velocity vector */
    PGSt_double ypr[3];          /* yaw, pitch, roll */
    PGSt_double yprRate[3];      /* yaw rate pitch rate roll rate */
    PGSt_double quaternion[4];   /* rotation from Spacecraft to ECI coordinates 
                                   (quaternion components) */
} scData;

typedef enum
{
  PGSe_ECItoORB=71,
  PGSe_ORBtoECI=72
} PGSt_directionTag;

/* function prototypes */

extern PGSt_SMF_status
PGS_EPH_callSim(
    PGSt_double,
    PGSt_tag,
    scData *);

extern PGSt_SMF_status
PGS_EPH_attOrbSim(
    PGSt_double,
    PGSt_double,
    PGSt_double,
    PGSt_double,
    PGSt_double,
    PGSt_double,
    PGSt_double,
    PGSt_tag   ,
    char *,
    PGSt_double [3],
    PGSt_double [3],
    PGSt_double [3],
    PGSt_double [3],
    PGSt_double [3][3],
    PGSt_double);

extern PGSt_SMF_status
PGS_EPH_attitudeNoise(
    PGSt_double,
    PGSt_double [3],
    PGSt_double [3],
    PGSt_double [3][3],
    char *);

extern PGSt_SMF_status
PGS_EPH_getQuats(
    PGSt_double [3][3],
    PGSt_double [4]);

extern  void 
PGS_EPH_matrixMultiply(
    PGSt_double [3][3],
    PGSt_double [3][3],
    PGSt_double [3][3]);

extern PGSt_SMF_status
PGS_EPH_orbSim(
    PGSt_double,
    PGSt_double,
    PGSt_double,
    PGSt_double,
    PGSt_double,
    PGSt_double,
    PGSt_double,
    PGSt_tag,
    PGSt_double *,
    PGSt_double *,
    PGSt_double);

extern PGSt_SMF_status
PGS_EPH_orbitalElements(
    PGSt_double,
    PGSt_tag,
    PGSt_double,
    PGSt_double,
    PGSt_double,
    PGSt_double,
    PGSt_double,
    PGSt_double,
    PGSt_double [10],
    PGSt_integer *,
    PGSt_double);

PGSt_SMF_status
PGS_EPH_TransformBodyRates(
    PGSt_integer ,
    PGSt_double  [3],
    PGSt_double  [3],
    PGSt_double  [3],
    PGSt_integer [3],
    PGSt_double  [3],
    PGSt_double  [3]);

#endif
