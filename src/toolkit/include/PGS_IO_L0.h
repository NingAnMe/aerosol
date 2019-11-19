/*-------------------------------------------------------------------------*/
/*                                                                         */
/*  COPYRIGHT[copyright mark] 2000, Raytheon System Company, its vendors,  */
/*  and suppliers.  ALL RIGHTS RESERVED.                                   */
/*                                                                         */
/*-------------------------------------------------------------------------*/
/*****************************************************************************
BEGIN_FILE_PROLOG

FILENAME:
  PGS_IO_L0.h

DESCRIPTION:
  This file contains typedefs, #defines, and #includes specific to the 
  Level 0 IO tools.

AUTHOR:
  Mike Sucher / Applied Research Corp.

HISTORY:
  30-Jan-1995 TWA  Initial version
  06-Dec-2000 AT   defined PGSd_TD_GIRD_GIIS_EPSILON for L0 tools
  30-Oct-2001 XW   Modified to support AURA spacecraft

END_FILE_PROLOG
*****************************************************************************/

#ifndef PGS_IO_L0_h     /* avoid re-inclusion */
#define PGS_IO_L0_h

#ifndef PGS_IO_Gen_h	/* make sure Generic IO definitions are set */
#include <PGS_IO_Gen.h>
#endif /* PGS_IO_Gen_h */

#include <PGS_TD.h>    /* this is needed for spacecraft tag definitions */
#include <PGS_MEM.h>   /* this is needed for memory allocation/deallocation */

#define TRMM_HDR_VAR_LEN 520
#define PGSd_TD_GIRD_GIIS_EPSILON 1.e-6

/*
 * typedefs 
 * should follow this form:
 *
 *         typedef <number-type> PGSt_IO_L0_SampleTypedef
 */
	

typedef PGSt_integer PGSt_IO_L0_VirtualDataSet;


/*
 *    File Header Definitions
 */

typedef unsigned char PGSt_IO_L0_Header;

typedef struct			/* ADEOS-II File Header structure */
{
    /*
     *    !!! IMPORTANT TK6 PROTOTYPE NOTE (25-Mar-1996):
     *    !!! this is a dummy header structure for use in the prototype code
     *    !!! it must be replaced in the working version 
     */
    unsigned char spacecraftID[2];           /*  6 bits reserved, 10 Spacecraft ID */
    unsigned char spacecraftClockFirst[9];   /* 72 bits clock - first packet */
    unsigned char spare1;                    /*  8 bits spare */
    unsigned char spacecraftClockLast[9];    /* 72 bits clock - last packet */
    unsigned char spare2;                    /*  8 bits spare */
    unsigned char packetCount[4];            /* 32 data packet count */
} PGSt_IO_L0_FileHdrADEOS_II;

typedef struct			/* TRMM File Header structure */
{
    unsigned char spacecraftID[2];           /*  6 bits reserved, 10 Spacecraft ID */
    unsigned char spacecraftClockFirst[9];   /* 72 bits clock - first packet */
    unsigned char spare1;                    /*  8 bits spare */
    unsigned char spacecraftClockLast[9];    /* 72 bits clock - last packet */
    unsigned char spare2;                    /*  8 bits spare */
    unsigned char packetCount[4];            /* 32 bits data packet count */
    unsigned char processingOptions;         /*  8 bits processing options
                                                  bit 3: Redundant Data Deleted
                                                  bit 6: Data Merging
                                                  bit 7: RS Decoding */
    unsigned char dataTypeFlags;             /*  8 bits data type flags
                                                  =1, Routine Production (RP)
                                                  =2, Quicklook Data (QL) */
    unsigned char timeOfReceipt[7];          /* 56 bits Time of Receipt
                                                  at originating node */
    unsigned char spare3;                    /*  8 bits spare */
    unsigned char spare4;                    /*  8 bits spare */
    unsigned char spare5;                    /*  8 bits spare */
    unsigned char selectOptions;             /*  8 bits select options */
    unsigned char numAPID;                   /*  8 bits no. of selected groups
                                                  =1, one file per APID 
                                                  >1, no. APIDs [FUTURE] */
    unsigned char varLenBuf[TRMM_HDR_VAR_LEN]; /*  variable length buffer

Contents of varLenBuf:
   Length depends on the no. APIDs N in the file

   (1) For TRMM science APID files, no. APIDs is always 1, and the
       content of the buffer is as follows:  

         bytes     1 -    2 : APID 
                                CERES: 54, 55 or 56
                                LIS: 61
         byte             3 : spare
         byte             4 : no.QAC lists in file; always 1 for TRMM
         bytes     5 -    8 : offset in bytes to QAC list, as counted
                               from the last byte of this field;
                               equal to total bytes contained in data packets


   (2) ***THIS IS NOT IMPLEMENTED IN TK4 DELIVERY (Mar. 1995)***
       For TRMM housekeeping APID files, the no. APIDs N varies.
       The content of the buffer is as follows, in this general case:
         bytes     1 -    2 : 1st APID 
         bytes     3 -    4 : 2nd APID 
                .
                .
                .
         bytes  2N-1 -   2N : Nth APID (max value of N is 255)
                                 The APIDs can take on many different values
                                 between 1 and 255
         byte          2N+1 : spare
         byte          2N+2 : no.QAC lists in file; always 1 for TRMM
         bytes  2N+3 - 2N+6 : offset in bytes to QAC list, as counted
                               from the last byte of this field;
                               equal to total bytes contained in data packets*/

}  PGSt_IO_L0_FileHdrTRMM;


/*
 *    Packet Header Definitions
 */

typedef unsigned char PGSt_IO_L0_Packet;

typedef struct			/* Primary Packet Header structure */
{
    unsigned char packetID[2];  /* bits 0-2:  Version Number
				   bit 3:     Type
				   bit 4:     Secondary Header Flag
				   bits 5-15: Application Process ID */
    unsigned char pktSeqCntl[2];/* bits 0-1:  Sequence Flags
				   bits 2-15: Packet Sequence Count */
    unsigned char pktLength[2]; /* Packet Length */
} PGSt_IO_L0_PriPktHdr;

typedef struct			/* TRMM Secondary Packet Header structure */
{
    PGSt_scTime   scTime[8];    /* Time Stamp */
} PGSt_IO_L0_SecPktHdrTRMM;

typedef struct			/* EOS_AM Secondary Packet Header structure */
{
    PGSt_scTime   scTime[8];    /* bit 0:     Secondary Header ID Flag
				   bits 1-63: Time Stamp */
    unsigned char flags;        /* bit 0:     Quick Look Flag
				   bits 1-7:  User Flags */
} PGSt_IO_L0_SecPktHdrEOS_AM;

typedef struct                  /* EOS_AURA Secondary Packet Header structure */
{
    unsigned char flags;        /* bit 0:     CCSDS Flag
                                   bit 1:     Quick Look Flag
                                   bits 2-7:  User Flags */
    PGSt_scTime   scTime[8];    /* Time Stamp */
} PGSt_IO_L0_SecPktHdrEOS_AURA;

typedef struct			/* EOS_PM_GIIS Secondary Packet Header structure */
{
    PGSt_scTime   scTime[8];    /* bits 0:     Secondary Header ID Flag
				   bits 1-63: Time Stamp */
    unsigned char flags;        /* bit 0:     CCSDS Flag
                                   bit 1:     Quick Look Flag
				   bits 2-7:  User Flags */
} PGSt_IO_L0_SecPktHdrEOS_PM_GIIS;

typedef struct			/* EOS_PM_GIRD Secondary Packet Header structure */
{
    unsigned char flags;        /* bit 0:     CCSDS Flag
				   bit 1:     Quick Look Flag
				   bits 2-7:  User Flags */
    PGSt_scTime   scTime[8];    /* Time Stamp */
} PGSt_IO_L0_SecPktHdrEOS_PM_GIRD;

typedef struct			/* ADEOS_II Secondary Packet Header structure */
{
    PGSt_scTime   scTime[5];    /* Time Stamp
				   bits 0-31:   instrument time
				   bits 32-47:  pulse time */
    PGSt_scTime   orbitTime[4];
} PGSt_IO_L0_SecPktHdrADEOS_II;

/*
 *    File Footer Definitions
 */

typedef unsigned char PGSt_IO_L0_Footer;

/*
 *    File Mapping Table Structure Definitions
 */

typedef struct			/* Entry in the file version mapping array */

/* This structure describes items in a particular physical file */
{
    PGSt_integer   file_version;      /* file version for Gen_Open */
    unsigned long  packet_count;      /* No. packets in file */
    unsigned long  num_pkts_read;     /* No. packets already read */
    PGSt_double    start_time;        /* time of first packet (TAI secs) */
    PGSt_double    stop_time;         /* time of  last packet (TAI secs) */
    unsigned long  stat_hdr_size;     /* size of static part of header (bytes)*/
    unsigned long  var_hdr_start;     /* ***TRMM ONLY*** start byte of variable
                                         length part of file header */
    unsigned long  var_hdr_size;      /* ***TRMM ONLY*** size of variable 
                                         length part of header (bytes) */
    unsigned long  pkts_start;        /* start byte of 1st packet */
    unsigned long  pkts_size;         /* total size of packets (bytes) */
    unsigned long  footer_start;      /* ***TRMM ONLY*** start byte of footer */
    unsigned long  footer_size;       /* ***TRMM ONLY*** size of footer(bytes)*/
    unsigned long  file_size;         /* Total size of file (bytes) */

} PGSt_IO_L0_VersionTable;

typedef struct 			/* Entry in the virtual data set file table */

/* This structure describes items in a particular virtual data set */
{
    PGSt_PC_Logical 	       file_logical;      /* file logical for Gen_Open */
    PGSt_integer               version_count;     /* number of physical file versions */
    PGSt_tag                   spacecraft_tag;    /* spacecraft tag for this data set  */
    PGSt_integer               open_file_seq_no;  /* which time-sequenced file is open */
    PGSt_IO_Gen_FileHandle     *open_file_stream; /* file pointer for Unix stream I/O */
    PGSt_IO_L0_VersionTable    *version_array;    /* pointer to version mapping array */
} PGSt_IO_L0_FileTable;



/*
 * #defines 
 * should follow this form:
 *
 *        #define PGSd_IO_L0_SampleDefine 0
 */


#ifdef  PGSd_IO_L0_c          		/* prototypes are extern unless this is set */
#    define PGSd_IO_L0_Extern
#else
#    define PGSd_IO_L0_Extern extern
#endif


#define PGSd_IO_L0_MaxOpenFiles 20     /* max number of open virtual data sets */


#define PGSd_IO_L0_FileHdrSizeEOS_PM 26
#define PGSd_IO_L0_FileHdrSizeEOS_AURA 26
#define PGSd_IO_L0_FileHdrSizeADEOS_II 26

#define PGSd_IO_L0_PrimaryPktHdrSize 6
#define PGSd_IO_L0_SecPktHdrSizeTRMM 8
#define PGSd_IO_L0_SecPktHdrSizeEOS_AM 9
#define PGSd_IO_L0_SecPktHdrSizeEOS_PM 9
#define PGSd_IO_L0_SecPktHdrSizeEOS_AURA 9
#define PGSd_IO_L0_SecPktHdrSizeADEOS_II 9


/* #define PGSd_IO_L0_PriPktHdrSize sizeof(PGSt_IO_L0_PriPktHdr) */

#define PGSd_IO_L0_PriPktHdrSize 6

/*
 * Define valid commands for PGS_IO_L0_ManageTable
 */
#define PGSd_IO_L0_InitTable		1
#define PGSd_IO_L0_AddTableEntry        2
#define PGSd_IO_L0_DeleteTableEntry	3
#define PGSd_IO_L0_UpdateTableEntry	4
#define PGSd_IO_L0_GetTableEntry	5
#define PGSd_IO_L0_GetTablePointer	6


/*
 * #includes
 * should follow this form:
 *
 *        #include <PGS_IO_L0_SampleHeader.h>
 */
#include <PGS_IO_L0_Wrap.h>


#endif                          /* PGS_IO_L0_h */
