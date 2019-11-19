/*-------------------------------------------------------------------------*/
/*                                                                         */
/*  COPYRIGHT[copyright mark] 2000, Raytheon System Company, its vendors,  */
/*  and suppliers.  ALL RIGHTS RESERVED.                                   */
/*                                                                         */
/*-------------------------------------------------------------------------*/
/******************************************************************************
BEGIN_FILE_PROLOG:

FILENAME:

  	PGS_IO_L0_Wrap.h

DESCRIPTION:

  	This file contains ANSI C prototypes of the PGS Toolkit 
  	Level 0 I/O tools.

AUTHOR:
  	Mike Sucher / Applied Research Corp.

HISTORY:
  23-Dec-1994 MES  Initial version
  20-Jan-1995 TWA  Added TRMM_HdrInfo prototype

END_FILE_PROLOG:
******************************************************************************/

#ifndef  PGS_IO_L0_Wrap_h  /* avoid re-inclusion */
#define  PGS_IO_L0_Wrap_h

/*
 * #includes
 * should follow this form:
 *
 *        #include <PGS_IO_L0_SampleHeader.h>
 */
#include <stdio.h>


/*
 * Prototypes
 */

#ifdef __cplusplus
extern "C" {
#endif

PGSt_SMF_status 
PGS_IO_L0_Close(
    PGSt_IO_L0_VirtualDataSet	
    );

PGSt_SMF_status 
PGS_IO_L0_FileVersionInfo(
    PGSt_PC_Logical	,
    PGSt_integer	,
    PGSt_tag		,
    PGSt_IO_L0_VersionTable *
    );

PGSt_SMF_status 
PGS_IO_L0_GetHeader(
    PGSt_IO_L0_VirtualDataSet ,
    PGSt_integer	  ,
    PGSt_IO_L0_Header     *,
    PGSt_integer	  ,
    PGSt_IO_L0_Footer     *	  
    );

PGSt_SMF_status 
PGS_IO_L0_GetPacket(
    PGSt_IO_L0_VirtualDataSet ,
    PGSt_integer              ,
    PGSt_IO_L0_Packet         *  
    );

PGSt_SMF_status 
PGS_IO_L0_ManageTable(
    PGSt_integer,
    PGSt_IO_L0_VirtualDataSet *,
    PGSt_IO_L0_FileTable      *,
    PGSt_IO_L0_FileTable      **		
    );

PGSt_SMF_status 
PGS_IO_L0_MapVersions(
    PGSt_PC_Logical	,
    PGSt_tag		,
    PGSt_integer        *,
    PGSt_IO_L0_VersionTable  **
    );

PGSt_SMF_status 
PGS_IO_L0_NextPhysical(
    PGSt_IO_L0_VirtualDataSet	
    );

PGSt_SMF_status 
PGS_IO_L0_Open(
    PGSt_PC_Logical	,
    PGSt_tag 		,
    PGSt_IO_L0_VirtualDataSet *,
    PGSt_double		*,
    PGSt_double		*
    );

PGSt_SMF_status 
PGS_IO_L0_SeekPacket(
    PGSt_IO_L0_VirtualDataSet ,
    PGSt_integer	  ,
    PGSt_integer         *,
    PGSt_double		  
    );

PGSt_SMF_status 
PGS_IO_L0_SetStart(
    PGSt_IO_L0_VirtualDataSet  ,
    PGSt_double                
    );

PGSt_SMF_status 
PGS_IO_L0_SetStartCntPkts(
    PGSt_IO_L0_VirtualDataSet  ,
    PGSt_double ,
    PGSt_integer *               
    );

PGSt_SMF_status 
PGS_IO_L0_TRMM_HdrInfo(
    PGSt_IO_Gen_FileHandle *,
    PGSt_IO_L0_VersionTable *
    );

PGSt_SMF_status
PGS_IO_L0_File_Sim(    
    PGSt_tag         , 
    PGSt_integer     [], 
    PGSt_integer     , 
    char             [28],
    PGSt_integer     , 
    PGSt_double      , 
    PGSt_integer     [], 
    PGSt_integer     [], 
    char             *,
    void             *,
    PGSt_uinteger    [2],
    void             *,
    void             *
    ); 

PGSt_SMF_status
PGS_IO_L0_sortArrayIndices(
    PGSt_integer     [],
    PGSt_integer     ,
    PGSt_integer     []
    );

PGSt_SMF_status
PGS_IO_L0_SFDU_Sim(
    PGSt_tag         ,
    char             *,
    char             *,
    PGSt_integer     ,        
    char             [28], 
    char             [28], 
    char
    );

PGSt_SMF_status
PGS_IO_L0_VersionInfoCheck(
    PGSt_IO_L0_VersionTable  *,
    PGSt_tag
    );

PGSt_SMF_status
PGS_IO_L0_EDOS_hdr_Sim(
    unsigned char,
    unsigned char,
    PGSt_integer[],
    PGSt_integer[],
    unsigned char,
    unsigned long,
    unsigned int,
    PGSt_double,
    PGSt_double,
    PGSt_double,
    char*,
    PGSt_tag);

extern PGSt_SMF_status
PGS_IO_L0_GetEOSAMfileTimes(
    unsigned char*,
    PGSt_double [][2],
    PGSt_integer*,
    unsigned long*);

extern PGSt_SMF_status
PGS_IO_L0_GetEOSAURAfileTimes(
    unsigned char*,
    PGSt_double [][2],
    PGSt_integer*,
    unsigned long*);

PGSt_SMF_status
PGS_IO_L0_GetEOSPMGIISfileTimes(
    unsigned char*,
    PGSt_double [][2],
    PGSt_integer*,
    unsigned long*);

PGSt_SMF_status
PGS_IO_L0_GetEOSPMGIRDfileTimes(
    unsigned char*,
    PGSt_double [][2],
    PGSt_integer*,
    unsigned long*);

extern PGSt_uinteger
PGS_IO_L0_BYTEtoINT(
    void*,
    PGSt_uinteger);

#ifdef __cplusplus
}
#endif

#endif             /*  PGS_IO_L0_Wrap_h */

