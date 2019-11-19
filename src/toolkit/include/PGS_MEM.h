/*-------------------------------------------------------------------------*/
/*                                                                         */
/*  COPYRIGHT[copyright mark] 2000, Raytheon System Company, its vendors,  */
/*  and suppliers.  ALL RIGHTS RESERVED.                                   */
/*                                                                         */
/*-------------------------------------------------------------------------*/
/*****************************************************************
BEGIN_FILE_PROLOG:

FILENAME:
  PGS_MEM.h

DESCRIPTION:
  For memory toolkits.
        
AUTHOR:
  Kelvin K. Wan / Applied Research Corp.
        
HISTORY:
  25-Mar-1994 Standard Convention
        
END_FILE_PROLOG:
*****************************************************************/

#ifndef _PGS_MEM_H
#define _PGS_MEM_H

#ifdef __cplusplus
extern "C" {
#endif

/*
 * Debug Flag 
 */
#define PGSMEM  "pgsmem"

/*
 * System Headers 
 */
#include <stdlib.h>

/*
 * Toolkit Headers
 */
#include <PGS_SMF.h>
#include <PGS_MEM_7.h>
#include <PGS_IO_Gen.h>

/*
 * External Functions 
 */
extern PGSt_SMF_status PGS_MEM_Malloc         (void **addr,size_t numBytes);
extern PGSt_SMF_status PGS_MEM_Calloc         (void **addr,size_t num_elems,size_t num_bytes);
extern PGSt_SMF_status PGS_MEM_Realloc        (void **addr,size_t newsize);
extern void            PGS_MEM_Zero           (void *addr,size_t numBytes);
extern void            PGS_MEM_Free           (void *addr);
extern void            PGS_MEM_FreeAll        (void);

#ifdef __cplusplus
}
#endif


#endif /* end _PGS_MEM_H */


