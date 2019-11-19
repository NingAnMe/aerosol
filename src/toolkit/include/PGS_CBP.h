/*-------------------------------------------------------------------------*/
/*                                                                         */
/*  COPYRIGHT[copyright mark] 2000, Raytheon System Company, its vendors,  */
/*  and suppliers.  ALL RIGHTS RESERVED.                                   */
/*                                                                         */
/*-------------------------------------------------------------------------*/
/*******************************************************************************
BEGIN_FILE_PROLOG:

NAME:
    PGS_CBP.h

DESCRIPTION;
    This file contains the typedefs, special defines, include files, and
    function prototypes for the PGS toolkit CBP tools.

END_FILE_PROLOG:
*******************************************************************************/

#ifndef _PGS_CBP_H_
#define _PGS_CBP_H_

/* include system include files */


/* include SMF header file */

#include <PGS_TD.h>
#include <PGS_CSC.h>

/* include error messages files */

#include <PGS_CBP_6.h>

/* celestial body tag definitions */

#define PGSd_MERCURY 1
#define PGSd_VENUS   2
#define PGSd_EARTH   3
#define PGSd_MARS    4
#define PGSd_JUPITER 5
#define PGSd_SATURN  6
#define PGSd_URANUS  7
#define PGSd_NEPTUNE 8
#define PGSd_PLUTO   9
#define PGSd_MOON   10
#define PGSd_SUN    11
#define PGSd_SSBARY 12
#define PGSd_EMBARY 13

#define PGSd_STAR 999

/* Function prototypes */

#ifdef __cplusplus
extern "C" {
#endif

extern PGSt_SMF_status
PGS_CBP_Earth_CB_Vector(
    PGSt_integer,
    char [28],
    PGSt_double [],
    PGSt_integer,
    PGSt_double [][3]);

extern PGSt_SMF_status
PGS_CBP_Sat_CB_Vector(
    PGSt_tag,
    PGSt_integer,
    char [28],
    PGSt_double [],
    PGSt_integer,
    PGSt_double [][3]);

extern PGSt_SMF_status
PGS_CBP_EphemRead(
    PGSt_double [2],
    PGSt_integer,
    PGSt_integer,
    PGSt_double [],
    PGSt_double [][3]);

extern PGSt_SMF_status
PGS_CBP_SolarTimeCoords(
    char [28],        
    PGSt_double,     
    PGSt_double *,   
    PGSt_double *,   
    PGSt_double *,   
    PGSt_double *,         
    PGSt_double *);

extern PGSt_SMF_status
PGS_CBP_body_inFOV(                      
    PGSt_integer,              
    char [28],           
    PGSt_double [],              
    PGSt_tag,          
    PGSt_integer,         
    PGSt_double [][3],       
    void *,  
    PGSt_integer,                   
    PGSt_boolean [],            
    PGSt_double [][3],         
    PGSt_double [][3]);      

#ifdef __cplusplus
}
#endif

#endif

