/*-------------------------------------------------------------------------*/
/*                                                                         */
/*  COPYRIGHT[copyright mark] 1999, Raytheon Systems Company, its vendors, */
/*  and suppliers.  ALL RIGHTS RESERVED.                                   */
/*                                                                         */
/*-------------------------------------------------------------------------*/
/*******************************************************************************
BEGIN_FILE_PROLOG:

NAME:   PGS_TSF.h

DESCRIPTION:

This file contains the common error handling defines, typedefs
and function prototypes.    

HISTORY:
        06-Apr-1999 RM  Initial version
        22-Dec-1999 AT  Added keys in support of PM's GIIS and GIRD time formats
        5-June-2000 AT  Added keys in support of DEM's 3km resolution

END_PROLOG:                              
*******************************************************************************/

#ifndef _PGS_TSF_H_
#define _PGS_TSF_H_

#include <PGS_TYPES.h>
#include <PGS_SMF.h>
#include <PGS_TSF_15.h>


#ifdef _PGS_THREADSAFE

#include <pthread.h>

#define PGSd_TSF_LOCK_SUCCESS 0

#define PGSd_TSF_TOTAL_LOCKS 17

#define PGSd_TSF_SMFWRITELOCK 	0
#define PGSd_TSF_AALOCK         1
#define PGSd_TSF_CUCLOCK 	2
#define PGSd_TSF_CBPLOCK 	3
#define PGSd_TSF_PCLOCK 	4
#define PGSd_TSF_IOLOCK 	5	 
#define PGSd_TSF_DEMLOCK 	6
#define PGSd_TSF_LOCKODL 	7
#define PGSd_TSF_LOCKHDF 	8
#define PGSd_TSF_LOCKVPF 	9
#define PGSd_TSF_LOCKFREEFORM 	10
#define PGSd_TSF_LOCKUDUNITS 	11
#define PGSd_TSF_LOCKFREEWARE 	12
#define PGSd_TSF_EPHLOCK 	13
#define PGSd_TSF_GCTLOCK 	14
#define PGSd_TSF_MEMLOCK 	15
#define PGSd_TSF_SHMMEMLOCK 	16


#define PGSd_TSF_TOTAL_KEYS 57

#define PGSd_TSF_KEYSMFTHREADID 			0
#define PGSd_TSF_KEYSMFGLOBAL 				1
#define PGSd_TSF_KEYSMFLASTFUNC	 			2
#define PGSd_TSF_KEYSMFLASTMNE 				3
#define PGSd_TSF_KEYSMFLASTMSG 				4
#define PGSd_TSF_KEYAAOLDFILES 				5
#define PGSd_TSF_KEYAAPHYSFILEBIN 			6
#define PGSd_TSF_KEYCSCGETEARTHFIGUREOLDEARTHTAG 	7
#define PGSd_TSF_KEYCSCGEOTOECROLDEARTHTAG 		8
#define PGSd_TSF_KEYCSCECRTOGEOOLDEARTHTAG 		9
#define PGSd_TSF_KEYCSCECRTOECISETUP_MSG 		10
#define PGSd_TSF_KEYCSCUTC_UT1POLEACCURACY 		11
#define PGSd_TSF_KEYDEMINFO3ARC1 			12
#define PGSd_TSF_KEYDEMINFO3ARC2 			13
#define PGSd_TSF_KEYDEMINFO3ARC3 			14
#define PGSd_TSF_KEYDEMINFO30ARC1 			15
#define PGSd_TSF_KEYDEMINFO30ARC2 			16
#define PGSd_TSF_KEYDEMINFO30ARC3 			17
#define PGSd_TSF_KEYDEMINFO30TEST 			18
#define PGSd_TSF_KEYDEMSUBSET3ARC1 			19
#define PGSd_TSF_KEYDEMSUBSET3ARC2 			20
#define PGSd_TSF_KEYDEMSUBSET3ARC3 			21
#define PGSd_TSF_KEYDEMSUBSET30ARC1 			22
#define PGSd_TSF_KEYDEMSUBSET30ARC2 			23
#define PGSd_TSF_KEYDEMSUBSET30ARC3 			24
#define PGSd_TSF_KEYDEMSUBSET30TEST 			25
#define PGSd_TSF_KEYDEMINFOQUALITY 			26
#define PGSd_TSF_KEYDEMSUBSETINFOORIG 			27
#define PGSd_TSF_KEYDEMPOINTARRAY                       28
#define PGSd_TSF_KEYDEMSUMDEGPOINTARRAY                 29
#define PGSd_TSF_KEYIOL0FILEHDREOSPM 			30
#define PGSd_TSF_KEYIOL0FILEHDRADEOSII			31
#define PGSd_TSF_KEYIOL0TPTR 				32
#define PGSd_TSF_KEYIOL0FILETABLE 			33
#define PGSd_TSF_KEYIOL02PKTEOSAM 			34
#define PGSd_TSF_KEYIOL02PKTEOSPM 			35
#define PGSd_TSF_KEYIOL02PKTTRMM 			36
#define PGSd_TSF_KEYIOL02PKTADEOSII 			37
#define PGSd_TSF_KEYIOL0FILEHDRTRMM 			38
#define PGSd_TSF_KEYEPHHEADER_ARRAY 			39
#define PGSd_TSF_KEYEPHATTITHEADER_ARRAY                40
#define PGSd_TSF_KEYEPHSCINFOARRAY 			41
#define PGSd_TSF_KEYEPHSTATICHDRARRAY 			42
#define PGSd_TSF_KEYEPHEPHEMRECORDARRAY 		43
#define PGSd_TSF_KEYEPHATTITRECORDARRAY 		44
#define PGSd_TSF_KEYMETMASTERNODE 			45
#define PGSd_TSF_KEYMETAGGLIST 				46
#define PGSd_TSF_KEYMETATTRHANDLE 			47
#define PGSd_TSF_KEYIOL02PKTEOSPMGIIS                   48
#define PGSd_TSF_KEYIOL02PKTEOSPMGIRD                   49
#define PGSd_TSF_KEYDEMINFO90ARC1 			50
#define PGSd_TSF_KEYDEMINFO90ARC2 			51
#define PGSd_TSF_KEYDEMINFO90ARC3 			52
#define PGSd_TSF_KEYDEMSUBSET90ARC1 			53
#define PGSd_TSF_KEYDEMSUBSET90ARC2 			54
#define PGSd_TSF_KEYDEMSUBSET90ARC3 			55
#define PGSd_TSF_KEYIOL02PKTEOSAURA			56
typedef struct
{
    pthread_mutex_t lockArray[PGSd_TSF_TOTAL_LOCKS];
    pthread_key_t keyArray[PGSd_TSF_TOTAL_KEYS];

    pthread_key_t keyTSFonce;
    pthread_key_t keyTSFMasterIndex;

} PGSt_TSF_MasterStruct;

#else  /* -D_PGS_THREADSAFE */

typedef struct
{
    int dumVal;
} PGSt_TSF_MasterStruct;

#endif  /* -D_PGS_THREADSAFE */

#define PGSd_TSF_BAD_MASTER_INDEX -99

#ifdef __cplusplus
extern "C" {
#endif

/*******************************************************************************
  Function prototypes
*******************************************************************************/
PGSt_SMF_status PGS_TSF_GetTSFMaster(PGSt_TSF_MasterStruct **threadMaster);
PGSt_SMF_status PGS_TSF_LockIt(int lockFlag);
PGSt_SMF_status PGS_TSF_UnlockIt(int lockFlag);
void PGS_TSF_ThreadInit();
void PGS_TSF_ThreadEnd(void *tag);
int PGS_TSF_GetMasterIndex();

void PGS_TSF_ThreadInitAA(PGSt_TSF_MasterStruct *threadMaster,int index);
void PGS_TSF_ThreadEndAA(PGSt_TSF_MasterStruct *threadMaster);

void PGS_TSF_ThreadInitCBP(PGSt_TSF_MasterStruct *threadMaster,int index);

void PGS_TSF_ThreadInitCSC(PGSt_TSF_MasterStruct *threadMaster,int index);
void PGS_TSF_ThreadEndCSC(PGSt_TSF_MasterStruct *threadMaster);

void PGS_TSF_ThreadInitDEM(PGSt_TSF_MasterStruct *threadMaster,int index);

void PGS_TSF_ThreadInitSMF(PGSt_TSF_MasterStruct *threadMaster,int index);
void PGS_TSF_ThreadEndSMF(PGSt_TSF_MasterStruct *threadMaster);

void PGS_TSF_ThreadInitIO(PGSt_TSF_MasterStruct *threadMaster,int index);
void PGS_TSF_ThreadEndIO(PGSt_TSF_MasterStruct *threadMaster);

void PGS_TSF_ThreadInitTD(PGSt_TSF_MasterStruct *threadMaster,int index);

void PGS_TSF_ThreadInitEPH(PGSt_TSF_MasterStruct *threadMaster,int index);
void PGS_TSF_ThreadEndEPH(PGSt_TSF_MasterStruct *threadMaster);

void PGS_TSF_ThreadInitMET(PGSt_TSF_MasterStruct *threadMaster,int index);
void PGS_TSF_ThreadEndMET(PGSt_TSF_MasterStruct *threadMaster);

void PGS_TSF_ThreadInitGCT(PGSt_TSF_MasterStruct *threadMaster,int index);

#ifdef __cplusplus
}
#endif


#endif
