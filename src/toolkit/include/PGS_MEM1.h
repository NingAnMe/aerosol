/*-------------------------------------------------------------------------*/
/*                                                                         */
/*  COPYRIGHT[copyright mark] 2000, Raytheon System Company, its vendors,  */
/*  and suppliers.  ALL RIGHTS RESERVED.                                   */
/*                                                                         */
/*-------------------------------------------------------------------------*/
/*****************************************************************
BEGIN_FILE_PROLOG:

FILENAME:
  PGS_MEM1.h

DESCRIPTION:
  For shared memory only.
        
AUTHOR:
  Kelvin K. Wan / Applied Research Corp.
        
HISTORY:
  25-Mar-1994 Standard Convention
   9-Dec-1994 Add -DSHHMEM flag to turn ON/OFF using shared memory
        
END_FILE_PROLOG:
*****************************************************************/

#ifndef _PGS_MEM1_H
#define _PGS_MEM1_H

/*
 * Debug Flag 
 */
#define PGSMEM  "pgsmem"

/*
 * System Headers 
 */
#include <stdlib.h>
#include <sys/types.h>

#ifdef SHMMEM
#include <sys/ipc.h>
#include <sys/shm.h>
typedef key_t PGSt_MEM_key;
#else
typedef long  PGSt_MEM_key;
#endif

/*
 * Toolkit Headers
 */
#include <PGS_SMF.h>
#include <PGS_MEM_7.h>
#include <PGS_IO_Gen.h>

#ifdef __cplusplus
extern "C" {
#endif

/*
 * Shared-Memory Constants 
 */
#define PGS_MEM_SHM_INIT    0
#define PGS_MEM_SHM_TERM    1
#define PGS_MEM_SHM_READ    0
#define PGS_MEM_SHM_WRITE   1

#define PGS_MEM_SHM_FLAG    0666
#define PGS_MEM_SHM_SYS     10
#define PGS_MEM_SHM_USER    11
#define PGS_MEM_SHM_PC      12
#define PGS_MEM_SHM_SMF     13

/*
 * Structure 
 */
typedef struct
{
    size_t          smf;          /* SMF   */
    size_t          pc;           /* PC    */
    size_t          user;         /* User  */
    size_t          total;        /* Total */
}PGSMemShmSize;

typedef struct
{
    PGSt_uinteger    smf;         
    PGSt_uinteger    pc;    
}PGSMemShmOffset;

typedef struct
{    
    PGSt_integer     shmid;
    PGSt_MEM_key     key;
    PGSt_SMF_boolean created;
}PGSMemShmUser;

typedef struct
{   
    PGSt_integer     shmid;
    PGSt_MEM_key     key;
}PGSMemShmSys;

typedef struct
{
    PGSMemShmSize   size;
    PGSMemShmOffset offset;
    PGSMemShmSys    sys;
    PGSMemShmUser   user;  
}PGSMemShm;

/*
 * External Functions 
 */
extern PGSt_SMF_status PGS_MEM_ShmCreate      (PGSt_uinteger size);
extern PGSt_SMF_status PGS_MEM_ShmAttach      (void **shm);
extern PGSt_SMF_status PGS_MEM_ShmDetach      (void);
extern PGSt_SMF_status PGS_MEM_Shmget         (PGSt_MEM_key key,PGSt_integer size,PGSt_integer flag,PGSt_integer *shmid);
extern PGSt_SMF_status PGS_MEM_Shmat          (PGSt_integer shmid,PGSt_integer flag,void **shmaddr);
extern PGSt_SMF_status PGS_MEM_Shmdt          (void *shmaddr);
extern PGSt_SMF_status PGS_MEM_Shmctl         (PGSt_integer shmid,PGSt_integer cmd,void *shmbuf);
extern PGSt_SMF_status PGS_MEM_ShmSysInit     (PGSMemShmSize *size);
extern PGSt_SMF_status PGS_MEM_ShmSysTerm     (void);
extern PGSt_SMF_status PGS_MEM_ShmSysAddr     (PGSt_integer keyAddr,void **addr,PGSt_uinteger *size);
extern void            PGS_MEM_ShmPrintBuf    (PGSMemShm *buf);
extern PGSt_SMF_status PGS_MEM_ShmReadF       (void *shm, PGSt_integer siz);
extern PGSt_SMF_status PGS_MEM_ShmWriteF      (void *shm, PGSt_integer siz);

#ifdef __cplusplus
}
#endif

#include <PGS_SMF_Prototypes.h>

#endif /* end _PGS_MEM1_H */


