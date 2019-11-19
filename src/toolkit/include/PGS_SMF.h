/*-------------------------------------------------------------------------*/
/*                                                                         */
/*  COPYRIGHT[copyright mark] 2000, Raytheon System Company, its vendors,  */
/*  and suppliers.  ALL RIGHTS RESERVED.                                   */
/*                                                                         */
/*-------------------------------------------------------------------------*/
/*****************************************************************
BEGIN_FILE_PROLOG:

FILENAME:
  PGS_SMF.h

DESCRIPTION:
        
AUTHOR:
  Kelvin K. Wan      / Applied Research Corp.
  Michael E. Sucher  / Applied Research Corp.
  David P. Heroux    / Applied Research Corp.
  Guru Tej S. Khalsa / Applied Research Corp.
  Carol S. W. Tsai   / Space Applications Corporation
  Adura Adekunjo     / L3 Communications EER Systems inc.

HISTORY:
  25-Mar-1994 Standard Convention

  18-Jul-1994 Modify to include PGS_TYPES.h

  15-Aug-1994 No longer need PGS_SMF_2.h and PGS_0.h

  23-Aug-1994 The level mask has been reordered for severity

  22-Mar-1995 MES Created new typedef struct: PGSSmfOtherGlb and
                  added member of this type to PGSSmfGlbVar.
                  This is to encapsulate the former static globals
                  (callerID, writeLogFile, useMemType, whichProc) 
                  used by the 1-file version of SMF 

  23-Mar-1995 MES Added PGSSMF_W_NO_MESSAGE_FILE

  24-Mar-1995 MES Added PGSSMF_M_NOT_IN_CACHE

  28-Mar-1995 MES Created PGS_SMF_MsgCacheInfo structure typedef
                  and added to PGSSmfShm to give shared memory
                  support for the message buffer.

  23-Mar-1995 MES Added command definitions for PGS_SMF_MsgLevel

  28-Apr-1995 MES Added function prototypes for new high-level 
                  tools PGS_SMF_Begin and PGS_SMF_End.

  27-Sep-1995 MES Added return status codes: PGSSMF_M_NOT_ACTION, 
                  PGSSMF_M_REGULAR_ACTION and PGSSMF_M_EMAIL_ACTION

  12-Oct-1995 DPH Added contant identifier PGSd_SMF_TOOLKIT_SMFS.

  13-Oct-1995 DPH Added logical identifier PGSd_SMF_LOGICAL_SNMP_CONNECT
		  to switch CSMS event logging on and off.
		 
                  Added logical identifier PGSd_SMF_LOGICAL_EVENTLOG
		  to define the PCF address for the local host Event
		  Logger file.

		  Added constant ON & OFF values.

  16-Oct-1995 DPH Added another level 'C' to handle SNMP channel 
		  communications. All action codes with level C
		  will trigger code to handle MSS dispositions.
		  Also added placeholders for future SMF levels.

  19-Oct-1995 DPH Added shell return value PGS_SH_SMF_MSSLOGFILE
		  to report problem with MSS environment values

  20-Oct-1995 DPH Moved the following routine declarations to the
                  include file: PGS_SMF_LogEvent()
                                PGS_SMF_GetActionType()
                                PGS_SMF_GetActionMneByCode()
				PGS_SMF_GetMSSGlobals()

		  Added additional MSS Disposition code to support
		  non-email communications; added adjunct returnStatus 
		  also.

  31-Oct-1995 GSK Removed "L" suffix on hexidecimal constants, the "L" was
                  unnecessary and was causing some compilers to warn that an
		  integer constant was being promoted to an unsigned long.

  05-Mar-1997 CST Added shell return value PGS_SH_PC_BUFSIZEERROR
                  to report problem with the size of file listed
                  in the PCF.

  12-Feb-1998 CST Changed definition of PGSd_TOOLKIT_MAJOR_VERSION from
                  B.0 TK5.2.1 to B.0 TK5.2.1.1

  17-Feb-1998 EML Changed definition of PGSd_TOOLKIT_MAJOR_VERSION from
                  B.0 TK5.2.1.1 to B.0 TK5.2.2 for ECS drop 4
  19-May-1998 CST Changed the length of character string from PGS_SMF_MAX_MSG_SIZE
                  defined as 241 to PGS_SMF_MAX_MSGBUF_SIZE defined as 481 for the
                  element, msg that defined to hold the message, of the structure
                  PGS_SMF_MsgData, a variable Data Structure (This changing is for
                  ECSed14746 about SDPTK5.2.1 limits on user-defined log messages
                  greater than 275 chars)

  28-May-1998 CST Changed definition of PGSd_TOOLKIT_MAJOR_VERSION from
                  B.0 TK5.2.2 to B.0 TK5.2.3 for ECS drop 4

  25-Jun-1998 CST Changed definition of PGSd_TOOLKIT_MAJOR_VERSION from
                  B.0 TK5.2.3 to B.0 TK5.2.4 for ECS drop 4

  28-May-1998 CST Changed definition of PGSd_TOOLKIT_MAJOR_VERSION from
                  B.0 TK5.2.2 to B.0 TK5.2.3 for ECS drop 4


  19-Aug-1998 MEDS Changed definition of PGSd_TOOLKIT_MAJOR_VERSION from
                  B.0 TK5.2.3 to B.0 TK5.2.4 for ECS drop 4PL
  29-Apr-1999 MEDS Changed definition of PGSd_TOOLKIT_MAJOR_VERSION from
                  B.0 TK5.2.4 to B.0 TK5.2.5 for ECS drop 5A
  11-Jul-2000 AT   Changed definition of PGSd_TOOLKIT_MAJOR_VERSION from
                  B.0 TK5.2.6 to B.0 TK5.2.6.1 for ECS drop 5B and 6A
  08-AUG-2000 AT   Changed definition of PGSd_TOOLKIT_MAJOR_VERSION from
                  B.0 TK5.2.6.1 to B.0 TK5.2.6.2 for ECS drop 5B and 6A
  22-Sep-2000 AT   Changed definition of PGSd_TOOLKIT_MAJOR_VERSION from
                  B.0 TK5.2.6.2 to TK5.2.7 for ECS drop 6A.
  05-Jan-2001 AT   Changed definition of PGSd_TOOLKIT_MAJOR_VERSION from
                  B.0 TK5.2.7 to TK5.2.7.1 for ECS drop 6A.
  27-Feb-2001 AT   Modified for linux
  26-Mar-2001 AT   Changed definition of PGSd_TOOLKIT_MAJOR_VERSION from
                  B.0 TK5.2.7.1 to TK5.2.7.2 for ECS drop 6A.
  26-Mar-2001 AT   Changed definition of PGSd_TOOLKIT_MAJOR_VERSION from
                  B.0 TK5.2.7.2 to TK5.2.7.3 for ECS drop 6A.
  26-Sep-2001 PTN   Changed definition of PGSd_TOOLKIT_MAJOR_VERSION from
                  TK5.2.7.3 to TK5.2.7.4 for ECS drop 6A.
  26-Feb-2002 PTN   Changed definition of PGSd_TOOLKIT_MAJOR_VERSION from
                  TK5.2.7.4 to TK5.2.8 for ECS drop 6A.
  10-Jun-2002 AT    Changed definition of PGSd_TOOLKIT_MAJOR_VERSION from
                  TK5.2.8 to TK5.2.8.1 for ECS drop 6A.
  16-Jul-2002 PTN   Changed definition of PGSd_TOOLKIT_MAJOR_VERSION from
                  TK5.2.8.1 to TK5.2.8.2 for ECS drop 6A.
  30-Oct-2002 PTN   Changed definition of PGSd_TOOLKIT_MAJOR_VERSION from
                  TK5.2.8.2 to TK5.2.8.3 for ECS drop 6A.
  17-Mar-2003 AA   Changed definition of PGSd_TOOLKIT_MAJOR_VERSION from
                  TK5.2.8.3 to TK5.2.9 for ECS drop 6A.
  20-Oct-2003 AT   Changed definition of PGSd_TOOLKIT_MAJOR_VERSION from
                  TK5.2.9 to TK5.2.10 for ECS drop 6A.
  18-Dec-2003 AA  Added definitions for EPS_32 and EPS_64
  12-Apr-2004 AA  Changed definition of PGSd_TOOLKIT_MAJOR_VERSION from
		  TK5.2.10 to TK5.2.11 for ECS drop 7A
  08-Jul-2004 AA  Changed Changed definition of PGSd_TOOLKIT_MAJOR_VERSION from
                  TK5.2.11 to TK5.2.12 for ECS drop 7A
  23-Mar-2005 AT  Changed definition of PGSd_TOOLKIT_MAJOR_VERSION from
                  TK5.2.12 to TK5.2.13 for ECS drop 7A
  31-Mar-2005 MP  Changed so sys_nerr not declared for Linux (NCR_42358)
  08-Sep-2005 AT  Changed definition of PGSd_TOOLKIT_MAJOR_VERSION from
                  TK5.2.13 to TK5.2.14 for ECS drop 7A
END_FILE_PROLOG:
*****************************************************************/

#ifndef _PGS_SMF_H
#define _PGS_SMF_H

#ifdef __cplusplus
extern "C" {
#endif

#define EPS_32      1.0E-37

#define EPS_64      1.0E-307
#define EPSREL_64      1.0E-10
#define EPSABS_64      1.0E-307

/*
 * Debug Flag 
 */
#define PGSSMF  "pgssmf" 

/*
 * Toolkit return status 
 */
#ifndef NULL
#define NULL               0
#endif

#define PGS_TRUE           1          
#define PGS_FALSE          0
#define PGS_NULL_STR       "NULL"

#define PGSd_CALLERID_SMF     10   /* SMF is starting to init. */
#define PGSd_CALLERID_USR     11   /* User calling SMF */

/*
 * System Headers
 */
#include <limits.h>
#include <stdio.h>
#include <errno.h>
extern int   errno;            /* not POSIX definition but needed to support */

#if !( defined(LINUX) || defined(MACINTOSH) ) /* for linux the following 
						 is not needed */
extern int   sys_nerr;         /* UNIX error/message                         */
extern char *sys_errlist[];

#endif

/*
 * Toolkit Headers 
 */
#include <PGS_TYPES.h>

typedef PGSt_integer      PGSt_SMF_status;
typedef PGSt_SMF_status   PGSt_SMF_code;
typedef PGSt_integer      PGSt_SMF_boolean;
typedef PGSt_SMF_status   pgs_status;
typedef PGSt_SMF_code     pgs_code;
typedef PGSt_SMF_boolean  pgs_boolean;

/*
 * Contants
 */
#define PGSd_SMF_CHAR_FORMFEED            0x0c   /* form feed character: Ctrl-L */
#define PGSd_SMF_NUM_OPENFILE                5   /* maximum number of opened files */
#define PGSd_SMF_TOOLKIT_SMFS		    16   /* number of Toolkit defined SMF files */
#define PGSd_SMF_USE_SHAREDMEM               0   /* use shared memory */
#define PGSd_SMF_USE_ASCIIFILE               1   /* use ascii file */
#define PGSd_SMF_WRITELOG_OFF                0   /* write to log file OFF */
#define PGSd_SMF_WRITELOG_ON                 1   /* write to log file ON */
#define PGSd_SMF_PROC_INIT                 0x2   /* initialization process */
#define PGSd_SMF_PROC_TERM                 0x4   /* termination process */
#define PGSd_SMF_PROC_USER                 0x8   /* user (PGE) process */

#define PGSd_SMF_TAG_LENGTH_MAX             61   /* message tag; 1 char. for '\0' */
#define PGSd_SMF_PATH_MAX                  256   /* maximumm path+filename; 1 char. for '\0' */
#define PGSd_SMF_MAILMSG_LEN             10001   /* mail message length */

#define PGSd_SMF_LOGICAL_LOGSTATUS       10100   /* status log file */
#define PGSd_SMF_LOGICAL_LOGREPORT       10101   /* report log file */
#define PGSd_SMF_LOGICAL_LOGUSER         10102   /* user log file */
#define PGSd_SMF_LOGICAL_TMPSTATUS       10103   /* temporary log status file */
#define PGSd_SMF_LOGICAL_TMPREPORT       10104   /* temporary log report file */
#define PGSd_SMF_LOGICAL_TMPUSER         10105   /* temporary log user file */

#define PGSd_SMF_LOGICAL_REMOTEHOSTNAME  10106   /* remote host name */
#define PGSd_SMF_LOGICAL_REMOTEPATH      10107   /* remote path to send the file */
#define PGSd_SMF_LOGICAL_MAILUSER        10108   /* send the mail to user */
#define PGSd_SMF_LOGICAL_TRANSMIT        10109   /* transmit file */

#define PGSd_SMF_LOGICAL_TMPFILE         10110   /* temporary file */
#define PGSd_SMF_LOGICAL_SHMMEM          10111   /* simulation of SMF shared memory */
#define PGSd_SMF_LOGICAL_SNMP_CONNECT    10112   /* enable/diable event log transactions */
#define PGSd_SMF_LOGICAL_EVENTLOG        10113   /* Local host event log */

#define PGSd_SMF_LOGICAL_LOGGING         10114
#define PGSd_SMF_LOGICAL_TRACE           10115
#define PGSd_SMF_LOGICAL_PID             10116
#define PGSd_SMF_LOGICAL_LEVEL           10117
#define PGSd_SMF_LOGICAL_SEED            10118
#define PGSd_SMF_LOGICAL_CODE            10119

#define PGSd_TOOLKIT_VERSION_LOGICAL     10220
#define PGSd_TOOLKIT_MAJOR_VERSION       "TK5.2.14"
#ifdef PGS_DAAC
#    define PGSd_TOOLKIT_MINOR_VERSION   "DAAC"
#else
#    define PGSd_TOOLKIT_MINOR_VERSION   "SCF"
#endif
  /*#define PGSd_TOOLKIT_VERSION_STRING \
    PGSd_TOOLKIT_MINOR_VERSION##" "##PGSd_TOOLKIT_MAJOR_VERSION*/
#ifdef PGS_DAAC
#    define PGSd_TOOLKIT_VERSION_STRING "DAAC  TK5.2.14"
#else
#    define PGSd_TOOLKIT_VERSION_STRING  "SCF  TK5.2.14"
#endif

/*
 * Defined contants with seed = 0 (system code)
 */
#define PGS_S_SUCCESS                                  0 
#define PGS_SH_PC_BUFSIZEERROR                       222 /* 0x000000de */
#define PGS_SH_SMF_MSSLOGFILE			     242 /* 0x000000f2 */
#define PGS_SH_PC_TRUNC                              243 /* 0x000000f3 */
#define PGS_SH_PC_TOOLERROR                          244 /* 0x000000f4 */
#define PGS_SH_PC_NODATA                             245 /* 0x000000f5 */
#define PGS_SH_SYS_PARAM                             246 /* 0x000000f6 */
#define PGS_SH_MEM_INIT                              247 /* 0x000000f7 */
#define PGS_SH_PC_DELETETMP                          248 /* 0x000000f8 */
#define PGS_SH_SMF_SENDRUNTIME                       249 /* 0x000000f9 */
#define PGS_SH_SMF_SENDLOGFILE                       250 /* 0x000000fa */
#define PGS_SH_MEM_TERM                              251 /* 0x000000fb */
#define PGS_SH_SMF_LOGFILE                           252 /* 0x000000fc */
#define PGS_SH_PC_LOADDATA                           253 /* 0x000000fd */
#define PGS_SH_PC_ENV                                254 /* 0x000000fe */
#define PGS_SH_SMF_SHMMEM                            255 /* 0x000000ff */
#define PGS_SH_SIGFPE                                260 /* 0x00000104 */

#define PGS_M_NULL                                  1797 /* 0x00000705 */
#define PGS_M_UNIX                                  1798 /* 0x00000706 */
#define PGS_E_HDF                                   3847 /* 0x00000f07 */
#define PGS_E_UNIX                                  3848 /* 0x00000f08 */
#define PGS_E_ECS                                   3849 /* 0x00000f09 */
#define PGS_E_TOOLKIT                               3850 /* 0x00000f0a */
#define PGS_F_TOOLKIT                               4363 /* 0x0000110b */
#define PGS_E_GEO                                   3852 /* 0x00000f0c */
#define PGS_E_DCE                                   3853 /* 0x00000f0d */
#define PGS_E_MATH                                  3854 /* 0x00000f0e */
#define PGS_E_ENV                                   3855 /* 0x00000f0f */

/*
 * Defined constants with seed = 2 (SMF code)
 * I don't believe that order matters here since these entries are not in 
 * an actual SMF file. However, since only 512 codes can be supported for
 * each seed number, the total number of entries here MUST NOT EXCEED THOSE
 * BOUNDS!
 */
#define PGSSMF_E_UNDEFINED_CODE                    19968 /* 0x00004e00 */
#define PGSSMF_E_UNDEFINED_UNIXERRNO               19969 /* 0x00004e01 */
#define PGSSMF_E_CANT_OPEN_FILE                    19970 /* 0x00004e02 */
#define PGSSMF_E_MSG_TOOLONG                       19971 /* 0x00004e03 */
#define PGSSMF_E_SIGFPE                            19972 /* 0x00004e04 */
#define PGSSMF_E_SIGACTION                         19973 /* 0x00004e05 */
#define PGSSMF_E_INVALID_FORMAT                    19974 /* 0x00004e06 */
#define PGSSMF_E_BAD_REFERENCE                     19975 /* 0x00004e07 */
#define PGSSMF_E_INVALID_FILE                      19976 /* 0x00004e08 */
#define PGSSMF_E_SENDFILE                          19977 /* 0x00004e09 */
#define PGSSMF_E_NOMAIL_ADDR                       19978 /* 0x00004e0a */
#define PGSSMF_E_NONETRC_FILE                      19979 /* 0x00004e0b */
#define PGSSMF_E_NETRC_MODE                        19980 /* 0x00004e0c */
#define PGSSMF_E_SENDMAIL                          19981 /* 0x00004e0d */
#define PGSSMF_E_REMOTEPATH                        19982 /* 0x00004e0e */
#define PGSSMF_E_NOHOSTNAME                        19983 /* 0x00004e0f */
#define PGSSMF_E_LOGFILE                           19984 /* 0x00004e10 */
#define PGSSMF_E_SENDRUNTIME_DATA                  19985 /* 0x00004e11 */
#define PGSSMF_E_SENDSTATUS_LOG                    19986 /* 0x00004e12 */
#define PGSSMF_E_BAD_EVENTLOG_ACCESS		   19987 /* 0x00004e13 */
#define PGSSMF_E_MSSLOGFILE                        19988 /* 0x00004e14 */
#define PGSSMF_E_INVALID_SWITCH                    19989 /* 0x00004e15 */

#define PGSSMF_W_NOACTION                          19475 /* 0x00004c13 */
#define PGSSMF_W_SENDRUNTIME_DATA                  19476 /* 0x00004c14 */
#define PGSSMF_W_NO_CONSTRUCT_TAG                  19477 /* 0x00004c15 */
#define PGSSMF_W_NO_MESSAGE_FILE                   19478 /* 0x00004c16 */

#define PGSSMF_M_TRANSMIT_DISABLE                  17942 /* 0x00004616 */
#define PGSSMF_M_UNIX                              17943 /* 0x00004617 */
#define PGSSMF_M_NOT_IN_CACHE                      17944 /* 0x00004618 */
#define PGSSMF_M_NOT_ACTION                        17945 /* 0x00004619 */
#define PGSSMF_M_REGULAR_ACTION                    17946 /* 0x0000461a */
#define PGSSMF_M_EMAIL_ACTION                      17947 /* 0x0000461b */
#define PGSSMF_M_INFO_ACTION                       17948 /* 0x0000461c */
#define PGSSMF_M_LOGGING_DISABLED                  17949 /* 0x0000461d */
#define PGSSMF_M_LOGGING_ENABLED                   17950 /* 0x0000461e */
#define PGSSMF_M_TRACE_DISABLED                    17951 /* 0x0000461f */
#define PGSSMF_M_ERROR_TRACE_ENABLED               17952 /* 0x00004620 */
#define PGSSMF_M_FULL_TRACE_ENABLED                17953 /* 0x00004621 */
 
/*
 * SMF Limitations 
 */
#define PGS_SMF_MAX_SEED_NO            524287   /* 2^19 - 1; 19 bits                */
#define PGS_SMF_MAX_MNEMONIC           510      /* 2^9 - 1; 9 bits                  */
#define PGS_SMF_MAX_INSTR_SIZE         11       /* max. 10 chars, 1 char for '\0'   */           
#define PGS_SMF_MAX_LABEL_SIZE         11       /* max. 10 chars, 1 char for '\0'   */
#define PGS_SMF_MAX_MNEMONIC_SIZE      32       /* instr_size + _ + SH + _ + 17, 1 char for '\0'     */
#define PGS_SMF_MAX_MSG_SIZE           241      /* max. 240 chars, 1 char for '\0'  */
#define PGS_SMF_MAX_ACT_SIZE           241      /* max. 240 chars, 1 char for '\0'  */
#define PGS_SMF_MAX_MSGBUF_SIZE        481      /* max. 480 chars, 1 char for '\0'  */
#define PGS_SMF_MAX_FUNC_SIZE          101      /* max. 100 chars, 1 char for '\0'  */
#define PGS_SMF_MAX_MSGTAG_SIZE        61       /* max. 60 chars, 1 char for '\0'   */

/*
 * SMF Status Level 
 */
#define PGS_SMF_STAT_LEV_SH            "_SH_"
#define PGS_SMF_STAT_LEV_M             "_M_"
#define PGS_SMF_STAT_LEV_U             "_U_"
#define PGS_SMF_STAT_LEV_S             "_S_"
#define PGS_SMF_STAT_LEV_N             "_N_"
#define PGS_SMF_STAT_LEV_W             "_W_"
#define PGS_SMF_STAT_LEV_F             "_F_"
#define PGS_SMF_STAT_LEV_E             "_E_"
#define PGS_SMF_STAT_LEV_A             "_A_"
#define PGS_SMF_STAT_LEV_X0            "_?_"
#define PGS_SMF_STAT_LEV_C             "_C_"
#define PGS_SMF_STAT_LEV_X2            "_?_"
#define PGS_SMF_STAT_LEV_X3            "_?_"
#define PGS_SMF_STAT_LEV_X4            "_?_"
#define PGS_SMF_STAT_LEV_X5            "_?_"
#define PGS_SMF_STAT_LEV_X6            "_?_"
#define PGS_SMF_LEV_DELIMIT            "_"
#define PGS_SMF_LEV_DISPLAY            3 

#define PGS_SMF_MASK_LEV_SH            0x00000000      /* _SH_ : shell     */
#define PGS_SMF_MASK_LEV_S             0x00000200      /* _S_  : success   */
#define PGS_SMF_MASK_LEV_A             0x00000400      /* _A_  : action    */
#define PGS_SMF_MASK_LEV_M             0x00000600      /* _M_  : message   */
#define PGS_SMF_MASK_LEV_U             0x00000800      /* _U_  : user info */
#define PGS_SMF_MASK_LEV_N             0x00000a00      /* _N_  : notice    */
#define PGS_SMF_MASK_LEV_W             0x00000c00      /* _W_  : warning   */
#define PGS_SMF_MASK_LEV_E             0x00000e00      /* _E_  : error     */
#define PGS_SMF_MASK_LEV_F             0x00001000      /* _F_  : fatal     */
#define PGS_SMF_MASK_LEV_X0            0x00001200      /* _?_  : unused    */
#define PGS_SMF_MASK_LEV_C             0x00001400      /* _C_  : channel   */
#define PGS_SMF_MASK_LEV_X2            0x00001600      /* _?_  : unused    */
#define PGS_SMF_MASK_LEV_X3            0x00001800      /* _?_  : unused    */
#define PGS_SMF_MASK_LEV_X4            0x00001a00      /* _?_  : unused    */
#define PGS_SMF_MASK_LEV_X5            0x00001c00      /* _?_  : unused    */
#define PGS_SMF_MASK_LEV_X6            0x00001e00      /* _?_  : unused    */
#define PGS_SMF_MASK_CONSTANT          0x000001ff      /* Bit: 8  - 0  */
#define PGS_SMF_MASK_LEVEL             0x00001e00      /* Bit: 12 - 9  */
#define PGS_SMF_MASK_SEED              0xffffe000      /* Bit: 31 - 13 */


/*
 * SMF System Info
 */
#define PGS_SMF_SYS_FILEPREFIX     "PGS"         
#define PGS_SMF_SYS_INSTR          "PGSTK"            
#define PGS_SMF_SYS_LABEL          "PGS" 
#define PGS_SMF_SYS_SEED           0            

#define PGS_SMF_ASSIGN_LABEL       "PGSSMF"            
#define PGS_SMF_ASSIGN_SEED        2            

#define PGSd_SMF_ON		   "1"
#define PGSd_SMF_OFF           	   "0"

/*
 * Messaging control
 */
#define PGSd_QUERY                   2
#define PGSd_QUERY_ALL               3
#define PGSd_ON		             1
#define PGSd_OFF           	     0
#define PGSd_DISABLE_STATUS_LEVEL  -10
#define PGSd_DISABLE_SEED          -20
#define PGSd_DISABLE_CODE          -30
#define PGSd_DISABLE_ALL           -40
#define PGSd_ENABLE_STATUS_LEVEL    10
#define PGSd_ENABLE_SEED            20
#define PGSd_ENABLE_CODE            30
#define PGSd_ENABLE_ALL             40
#define PGSd_FLUSH                  86
#define PGSd_ADD_ELEMENT             1
#define PGSd_DELETE_ELEMENT         -1
#define PGSd_FREE_LIST              86
#define PGSd_SET_NO_TRACE          100
#define PGSd_SET_ERROR_TRACE       101 
#define PGSd_SET_FULL_TRACE        102
#define PGSd_ENABLE_PID_LOGGING    111
#define PGSd_DISABLE_PID_LOGGING  -111

/* special command flags */

#define PGSd_GET  1
#define PGSd_SET -1

/*
 * Old SMF Globals
 */

#define PGSd_CALLER_ID         1
#define PGSd_WRITE_LOG_FILE    2
#define PGSd_USE_MEM_TYPE      3
#define PGSd_WHICH_PROC        4

/*
 * MSS Disposition Codes
 */
#define PGS_SMF_MSSCODE_EMAIL      "PGSEMAIL"            
#define PGS_SMF_MSSCODE_INFO       "PGSINFO"            

/*
 * Command Definitions Used By PGS_SMF_CacheMsgShm
 */
#define PGSd_SMF_FindByCode        1
#define PGSd_SMF_FindByMnemonic    2
#define PGSd_SMF_AddRecord         3
#define PGSd_SMF_LastFindStatus    4
#define PGSd_SMF_DisplayBuffer     5
#define PGSd_SMF_InitCacheInfo1    6
#define PGSd_SMF_InitCacheInfo2    7


/*
 * Definitions Used By PGS_SMF_MsgLevel
 */
#define PGSd_SMF_IncLevel          1    /* command */
#define PGSd_SMF_DecLevel          2    /* command */
#define PGSd_SMF_GetLevel          3    /* command */
#define PGSd_SMF_SetIndentString   4    /* command */
#define PGSd_SMF_EnableLevel       5    /* command */
#define PGSd_SMF_DisableLevel      6    /* command */
#define PGSd_SMF_GetTracePath      7    /* command */

#define PGSd_SMF_MaxMsgLevel      20    /* max value */
 
/*
 * Structure Defintions
 */ 
typedef struct
{
    PGSt_SMF_code           seed;                                   /* unique seed no  */
    char                    instr[PGS_SMF_MAX_INSTR_SIZE];          /* organization    */
    char                    label[PGS_SMF_MAX_LABEL_SIZE];          /* instrument name */
}PGS_SMF_FileInfo;

typedef struct 
{
    PGSt_SMF_code           code;                                    /* unique code              */
    char                    mnemonic[PGS_SMF_MAX_MNEMONIC_SIZE];     /* mnemonic label           */
    char                    msg[PGS_SMF_MAX_MSGBUF_SIZE];               /* mnemonic message string  */
    char                    action[PGS_SMF_MAX_MNEMONIC_SIZE];       /* action label             */
}PGS_SMF_MsgData;
                         
typedef struct
{
    PGSt_SMF_code           seed;                /* max unique seed (19 bits)  */ 
    short                   levels;              /* max levels (4 bits)        */
    short                   unique_code;         /* max unique code (9 bits)   */
}PGS_SMF_CodeInfo;

typedef struct                                  
{                                       
    PGS_SMF_FileInfo        fileinfo;
    PGS_SMF_MsgData         msgdata;
    char                    funcname[PGS_SMF_MAX_FUNC_SIZE];
}PGS_SMF_MsgInfo;

typedef struct
{
    void (*func)(int signo);                              /* signal handler */
    char funcname[PGS_SMF_MAX_FUNC_SIZE];                 /* function name */
}PGSSmfSigHandler;

typedef struct
{
    char               logStatus[PGSd_SMF_PATH_MAX];       /* status log file */
    char               logReport[PGSd_SMF_PATH_MAX];       /* report log file */
    char               logUser[PGSd_SMF_PATH_MAX];         /* user log file */ 
    FILE              *fptrStatus;                         /* status log file pointer */
    FILE              *fptrReport;                         /* report log file pointer */
    FILE              *fptrUser;                           /* user log file pointer */
    char               msgTag[PGSd_SMF_TAG_LENGTH_MAX];    /* message tag */   
    PGSt_SMF_boolean   record;                             /* flag to indicate if errors are to be captured into log files */
    PGSt_SMF_boolean   transmit;                           /* flag to indicate if allow to send the log files */    
}PGSSmfLog;

typedef struct
{
    PGSt_SMF_boolean   transmit;            /* flag to indicate if allow to send the files */
}PGSSmfRunData;

typedef struct
{
    PGSt_SMF_code     fileNo[PGSd_SMF_NUM_OPENFILE];     /* file number */
    FILE             *filePtr[PGSd_SMF_NUM_OPENFILE];    /* opened file pointer */
    int               num;                               /* number of file opened */
}PGSSmfOpenFile;

/*
 *  Former Static Global Variables
 */

typedef struct
{
    PGSt_SMF_status    callerID;            /* callerID to stop cyclic situation */
    PGSt_SMF_status    writeLogFile;        /* flag to capture errors to log file */
    PGSt_SMF_status    useMemType;          /* flag to use either shared memory or ascii file */
    PGSt_SMF_status    whichProc;           /* which process are we in? (init., PGE, or term.) */
}PGSSmfOtherGlb;

typedef struct                     /* message cache: indices, record count */
{                                       
    PGSt_integer            newest;        /* index to newest entry */
    PGSt_integer            oldest;        /* index to oldest entry */
    PGSt_uinteger            count;        /* count of records allocated */
}PGS_SMF_MsgCacheInfo;

/*
 *  SMF Global Variable Data Structure
 */

typedef struct
{
    PGS_SMF_MsgInfo   msginfo;                      /* code buffer */
    char              msg[PGS_SMF_MAX_MSGBUF_SIZE]; /* current message buffer */
    PGSSmfSigHandler  signal;                       /* signal handling */
    PGSSmfLog         log;                          /* log files */
    PGSSmfRunData     rundata;                      /* runtime files */
    PGSSmfOpenFile    open;                         /* opened files */
    PGS_SMF_MsgCacheInfo msgCacheInfo;              /* message cache info */
    PGSSmfOtherGlb    other;                        /* other globals */
    PGSt_SMF_code     unixErrno;                    /* Unix errno */
}PGSSmfGlbVar;

/*
 * Current Static Global Variables
 */ 


/*
 *  Shared Memory Data Structure
 */

typedef struct
{
    char  logStatus[PGSd_SMF_PATH_MAX];      /* status log file */
    char  logReport[PGSd_SMF_PATH_MAX];      /* report log file */
    char  logUser[PGSd_SMF_PATH_MAX];        /* user log file */ 
    char  msgTag[PGSd_SMF_TAG_LENGTH_MAX];   /* message tag */  
    int   writeLogFile;                      /* write to log file */
    PGS_SMF_MsgCacheInfo msgCacheInfo;       /* message cache info */
}PGSSmfShm;


/*
 * PGS_SMF.c: External Functions 
 */
extern PGSt_SMF_status       PGS_SMF_SetHDFMsg             (PGSt_SMF_code code,char *msg,char *funcname);       
extern PGSt_SMF_status       PGS_SMF_SetUNIXMsg            (PGSt_integer unix_errcode,char *msg,char *funcname);
extern PGSt_SMF_status       PGS_SMF_SetStaticMsg          (PGSt_SMF_code code,char *funcname);
extern PGSt_SMF_status       PGS_SMF_SetDynamicMsg         (PGSt_SMF_code code,char *msg,char *funcname);
extern PGSt_SMF_status       PGS_SMF_GetActionByCode       (PGSt_SMF_code code,char action[]);
extern PGSt_SMF_status       PGS_SMF_GetMsgByCode          (PGSt_SMF_code code,char msg[]);
extern void                  PGS_SMF_GetMsg                (PGSt_SMF_code *code,char mnemonic[],char msg[]);
extern PGSt_SMF_status       PGS_SMF_CreateMsgTag          (char *systemTag);
extern PGSt_SMF_status       PGS_SMF_GetInstrName          (PGSt_SMF_code code,char instr[]);
extern PGSt_SMF_status       PGS_SMF_GenerateStatusReport  (char *report);
extern PGSt_SMF_boolean      PGS_SMF_TestErrorLevel        (PGSt_SMF_status code);
extern PGSt_SMF_boolean      PGS_SMF_TestFatalLevel        (PGSt_SMF_status code);
extern PGSt_SMF_boolean      PGS_SMF_TestMessageLevel      (PGSt_SMF_status code);
extern PGSt_SMF_boolean      PGS_SMF_TestWarningLevel      (PGSt_SMF_status code);
extern PGSt_SMF_boolean      PGS_SMF_TestUserInfoLevel     (PGSt_SMF_status code);
extern PGSt_SMF_boolean      PGS_SMF_TestSuccessLevel      (PGSt_SMF_status code);
extern PGSt_SMF_boolean      PGS_SMF_TestNoticeLevel       (PGSt_SMF_status code);
extern PGSt_SMF_status       PGS_SMF_TestStatusLevel       (PGSt_SMF_status code);
extern PGSt_SMF_status       PGS_SMF_GetGlobalVar          (PGSSmfGlbVar **global_var);
extern void                  PGS_SMF_GetSystemCode         (PGSt_SMF_code code,char *mnemonic,char *msg);
extern void                  PGS_SMF_GetSMFCode            (PGSt_SMF_code code,char *mnemonic,char *msg);
extern void                  PGS_SMF_ExtractMsgInfo        (PGS_SMF_MsgInfo *codeinfo,char *line);
extern void                  PGS_SMF_ExtractFileInfo       (PGS_SMF_MsgInfo *codeinfo,char *line);
extern PGSt_SMF_status       PGS_SMF_GetEnv                (PGS_SMF_MsgInfo *codeinfo,char *funcname);
extern PGSt_SMF_status       PGS_SMF_CallerID              (void);
extern PGSt_SMF_status       PGS_SMF_Get_WriteLogFile      (void);
extern PGSt_SMF_status       PGS_SMF_DecodeCode            (PGSt_SMF_code code,char *mnemonic,PGS_SMF_MsgInfo *codeinfo,char *func,short op);
extern PGSt_SMF_status       PGS_SMF_GetSysShm             (PGSSmfGlbVar *global_var);
extern void                  PGS_SMF_InitProc              (PGSt_SMF_status useMem,PGSt_SMF_status writeLog);
extern void                  PGS_SMF_TermProc              (PGSt_SMF_status useMem,PGSt_SMF_status writeLog);
extern void                  PGS_SMF_TermSMF               (void);
extern PGSt_SMF_status       PGS_SMF_Begin                 (char *);
extern PGSt_SMF_status       PGS_SMF_End                   (char *);
extern PGSt_SMF_status 	     PGS_SMF_LogEvent		   (PGSt_SMF_code statusCode, char *messageString, char *messageLabel, char *actionString, PGSt_SMF_code mssDisposition);
extern PGSt_SMF_status 	     PGS_SMF_GetActionMneByCode(PGSt_SMF_code code, char action[], char mnemonic[]);
 
extern PGSt_SMF_status 	     PGS_SMF_GetActionType(char *actionLabel);
extern void 		     PGS_SMF_GetMSSGlobals(char **eventLogRef, char **comm_snmp);
extern PGSt_SMF_status       PGS_SMF_Logging(PGSt_integer, char*);
extern PGSt_SMF_status       PGS_SMF_LoggingControl(PGSt_integer, PGSt_SMF_code);
extern PGSt_integer          PGS_SMF_ManageLogControlList(PGSt_SMF_status**,
							  size_t*,
							  PGSt_SMF_status,
							  PGSt_SMF_status);
extern PGSt_SMF_status       PGS_SMF_TraceControl(int);
extern void                  PGS_SMF_SetCallerID(PGSt_SMF_status);
extern char*                 PGS_SMF_LogPID(PGSt_integer);
extern void                  PGS_SMF_InitializeLogging(void);
extern void                  PGS_SMF_GetToolkitVersion(char [21]);



/*
 * PGS_SMF_Util.c: External Functions
 */
extern PGSt_SMF_status       PGS_SMF_PrintFile             (char *file);
extern PGSt_SMF_status       PGS_SMF_SetRunTimeData_Int    (char msg[],PGSt_integer value);
extern PGSt_SMF_status       PGS_SMF_SetRunTimeData_Short  (char msg[],short value);
extern PGSt_SMF_status       PGS_SMF_SetRunTimeData_Double (char msg[],PGSt_double value);
extern PGSt_SMF_status       PGS_SMF_SetRunTimeData_Float  (char msg[],float value);
extern void                  PGS_SMF_PrintMsgInfo          (PGS_SMF_MsgInfo *msginfo);
extern PGSt_SMF_status       PGS_SMF_SetArithmeticTrap     (void (*func)(int signo));
extern void                  PGS_SMF_SysSignalHandler      (int signo);
extern PGSt_SMF_status       PGS_SMF_Sigaction             (void (*func)(int signo),char *funcname);

/*
 * PGS_SMF_Comm.c: External Functions
 */
extern PGSt_SMF_status       PGS_SMF_System                (char *cmdStr);
extern PGSt_SMF_status       PGS_SMF_SendFile              (char *LocalFileName,char *RemoteHostName,char *RemoteFileName);  
extern PGSt_SMF_status       PGS_SMF_SendMail              (char *toAddresses,char *ccAddresses,char *bccAddresses,char *subText,char *msgText);
extern PGSt_SMF_status       PGS_SMF_CheckNetrcFile        (char *RemoteHostName);
extern void                  PGS_SMF_RemoveSpace           (char *str);

/*
 * PGS_SMF_SendStatusReport.c: External Functions
 */
extern PGSt_SMF_status       PGS_SMF_SendStatusReport        (void);
extern PGSt_SMF_status       PGS_SMF_SysSendStatusReport     (void);
extern PGSt_SMF_status       PGS_SMF_SysTermSendStatusReport (void);

/*
 * PGS_SMF_SendRuntimeData.c: External Functions
 */
extern PGSt_SMF_status       PGS_SMF_SendRuntimeData         (PGSt_integer numfiles,PGSt_PC_Logical files[],PGSt_integer version[]);
extern PGSt_SMF_status       PGS_SMF_SysSendRuntimeData      (void);
extern PGSt_SMF_status       PGS_SMF_SysTermSendRuntimeData  (void);

/*
 * PGS_SMF_Popen.c: External Functions
 */
extern FILE*                 PGS_SMF_Popen                 (const char *cmdStr,const char *type);
extern int                   PGS_SMF_Pclose                (FILE *fp);
extern int                   PGS_SMF_OpenMax               (void);

/*
 * PGS_SMF1.c: External Functions
 */
extern void                  PGS_SMF_SetUnknownMsg         (PGSt_SMF_status code,char *funcname);



#ifdef __cplusplus
}
#endif

#endif /* end _PGS_SMF_H */

