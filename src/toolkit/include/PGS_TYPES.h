/*-------------------------------------------------------------------------*/
/*                                                                         */
/*  COPYRIGHT[copyright mark] 2000, Raytheon System Company, its vendors,  */
/*  and suppliers.  ALL RIGHTS RESERVED.                                   */
/*                                                                         */
/*-------------------------------------------------------------------------*/
#ifndef _PGS_TYPES_H_
#define _PGS_TYPES_H_

typedef unsigned char  PGSt_byte;          /* one byte (unsigned) */
typedef unsigned int   PGSt_uinteger;      /* PGS unsigned long int */
typedef int            PGSt_integer;       /* PGS long int          */
#ifndef UNICOS
typedef double         PGSt_double;        /* PGS double            */
#else
typedef long double    PGSt_double;
#endif
typedef PGSt_integer   PGSt_boolean;
typedef PGSt_integer   PGSt_tag;
typedef float          PGSt_real;          /* PGS real value should be
					      compatible with FORTRAN
					      REAL */
/*
 * The typdef for PGSt_PC_Logical was moved here from PGS_PC.h for TK5
 * because it is needed by PGS_SMF.h, which must be included before
 * other TK group header files.
 * 
 * 27-Apr-1995 Mike Sucher / A.R.C.
 */
typedef PGSt_uinteger PGSt_Logical;
typedef PGSt_Logical  PGSt_PC_Logical;

#endif

