/*-------------------------------------------------------------------------*/
/*                                                                         */
/*  COPYRIGHT[copyright mark] 2000, Raytheon System Company, its vendors,  */
/*  and suppliers.  ALL RIGHTS RESERVED.                                   */
/*                                                                         */
/*-------------------------------------------------------------------------*/
/*******************************************************************************
BEGIN_FILE_PROLOG:

NAME:
   PGS_math.h

DESCRIPTION:
   This file just includes the system math.h file on all systems except the
   cray.  On the cray this file defines macros which redefine some of the
   standard C math functions to similar functions which take and return the
   type "long double" (rather than the type "double" which is normally the
   case).  The cray as a long double analog for each of its double math
   routines.  The long double version has the same name as the double version
   but with an "l" appended to the end of the function name (e.g. cos -> cosl).
   The cray man pages refer to these functions as being CRI extensions.  An
   alternate possibility for using functions which will correctly process the
   long double type is to call the cray FORTRAN double precision equivalents.
   If the CRI extensions are not available (i.e. if the code won't link and the
   linker is saying things like cosl is an undefined symbol) then the FORTRAN
   functions can be used.  In that case the line #define PGSd_USE_CRI_EXT
   (below) should be commented out and the FORTRAN functions will be used
   instead. 

   This is all necessary because on the cray the SDP Toolkit type PGSt_double,
   which is used throughout the toolkit, is a long double.

END_PROLOG:                              
*******************************************************************************/

#ifndef _PGS_MATH_H_

#define _PGS_MATH_H_

#ifndef UNICOS

/* if this is NOT a cray then just include the standard C math library header
   file here -- the rest of this file does not apply */

#include <math.h>

#else

/* if this IS a cray...redefine all the C math functions (which take and return
   double type to functions which can handle long double types */

#include <math.h>

/* comment out the following line if CRI extensions are NOT available */

#define PGSd_USE_CRI_EXT



#ifdef PGSd_USE_CRI_EXT

/* if CRI extensions are available then use them (these are extensions to the
   standard C math library which handle the long double type) */

#define sin(x) sinl(x)
#define cos(x) cosl(x)
#define tan(x) tanl(x)
#define asin(x) asinl(x)
#define acos(x) acosl(x)
#define atan(x) atanl(x)
#define atan2(x,y) atan2l((x),(y))
#define fmod(x,y) fmodl((x),(y))
#define fabs(x) fabsl(x)
#define sqrt(x) sqrtl(x)
#define log(x) logl(x)
#define log10(x) log10l(x)
#define exp(x) expl(x)
#define pow(x,y) powl((x),(y))

#undef PGSd_USE_CRI_EXT

#else

/* if CRI extensions are NOT available then call FORTRAN
   double precision functions */

extern long double DSIN();
extern long double DCOS();
extern long double DTAN();
extern long double DASIN();
extern long double DACOS();
extern long double DATAN();
extern long double DATAN2();
extern long double DMOD();
extern long double DABS();
extern long double DSQRT();
extern long double DLOG();
extern long double DLOG10();
extern long double DEXP();

#define sin(x) pgs_fortran_sin(x)
#define cos(x) pgs_fortran_cos(x)
#define tan(x) pgs_fortran_tan(x)
#define asin(x) pgs_fortran_asin(x)
#define acos(x) pgs_fortran_acos(x)
#define atan(x) pgs_fortran_atan(x)
#define atan2(x,y) pgs_fortran_atan2((x),(y))
#define fmod(x,y) pgs_fortran_fmod((x),(y))
#define fabs(x) pgs_fortran_fabs(x)
#define sqrt(x) pgs_fortran_sqrt(x)
#define log(x) pgs_fortran_log(x)
#define log10(x) pgs_fortran_log10(x)
#define exp(x) pgs_fortran_exp(x)
#define pow(x,y) pgs_fortran_pow((x),(y))


static long double pgs_fortran_sin(long double arg1)
{
    return DSIN(&arg1);
}


static long double pgs_fortran_cos(long double arg1)
{
    return DCOS(&arg1);
}


static long double pgs_fortran_tan(long double arg1)
{
    return DTAN(&arg1);
}


static long double pgs_fortran_asin(long double arg1)
{
    return DASIN(&arg1);
}


static long double pgs_fortran_acos(long double arg1)
{
    return DACOS(&arg1);
}


static long double pgs_fortran_atan(long double arg1)
{
    return DATAN(&arg1);
}


static long double pgs_fortran_atan2(long double arg1, long double arg2)
{
    return DATAN2(&arg1, &arg2);
}


static long double pgs_fortran_fmod(long double arg1, long double arg2)
{
    return DMOD(&arg1, &arg2);
}


static long double pgs_fortran_fabs(long double arg1)
{
    return DABS(&arg1);
}


static long double pgs_fortran_sqrt(long double arg1)
{
    return DSQRT(&arg1);
}

static long double pgs_fortran_log(long double arg1)
{
    return DLOG(&arg1);
}

static long double pgs_fortran_log10(long double arg1)
{
    return DLOG10(&arg1);
}

static long double pgs_fortran_exp(long double arg1)
{
    return DEXP(&arg1);
}

static long double pgs_fortran_pow(long double arg1, long double arg2)
{
    long double temp;
    
    temp = arg2*DLOG(&arg1);
    
    return DEXP(&temp);
}

#endif /* END: #ifdef PGSd_USE_CRI_EXTENSION */

#endif /* END: #ifndef UNICOS */

#endif /* END: #ifndef _PGS_MATH_H_ */

