/*
 * NAME: err.h
 *              
 * PURPOSE: contains error defined constants and prototypes for error.c 
 *
 * USAGE:       
 *
 * RETURNS:     
 *
 * DESCRIPTION: include file for error.c
 *
 * SYSTEM DEPENDENT FUNCTIONS:  
 *
 * AUTHOR:      Mark Van Gorp at NGDC (based upon Ron White's version) 497-6221
 *
 * COMMENTS:    
 *
 * KEYWORDS:    
 *
 */
/*
 * HISTORY:
 *	Rich Fozzard	7/31/95		-rf01
 *		CodeWarrior for Mac is picky about redefines of TRUE,FALSE
*/


/* Avoid multiple includes */
#ifndef MAX_ERRSTR_BUFFER

#include <errno.h>

#define MAX_ERRSTR_BUFFER       _MAX_PATH

/* General Errors */

#define ERR_GENERAL             500
#define ERR_OPEN_FILE           501
#define ERR_READ_FILE           502
#define ERR_WRITE_FILE          503
#define ERR_PTR_DEF             504
#define ERR_MEM_LACK            505
#define ERR_UNKNOWN             506
#define ERR_FIND_FILE           507
#define ERR_FILE_DEFINED        508
#define ERR_CLOSE_FILE          509
#define ERR_OUT_OF_RANGE        510
#define ERR_MEM_CORRUPT         511
#define ERR_STRUCT_FIELD        512
#define ERR_MAX_LT_MIN          513
#define ERR_MAKE_HISTOGRAM      514
#define ERR_PROCESS_DATA        515
#define ERR_DATA_GT_BUFFER      516
#define ERR_WRITE_DATA          517
#define ERR_FIND_INTERSECT      518
#define ERR_NUM_TOKENS          519
#define ERR_COMMAND_LINE        520
#define ERR_STRING_TOO_LONG     521
#define ERR_FILE_EXISTS         522
#define ERR_FILE_NOTEXIST       523
#define ERR_CREATE_FILE         524

/* Freeform Errors */
#define ERR_UNKNOWN_VAR_TYPE    1000
#define ERR_UNKNOWN_FORM_TYPE   1001
#define ERR_HEAD_FORM           1002
#define ERR_CONVERT             1003
#define ERR_MAKE_FORM           1004
#define ERR_FREE_FORM           1005
#define ERR_SHOW_FORM           1006
#define ERR_GET_DELIMITER       1007
#define ERR_PROC_LIST           1008
#define ERR_GET_VALUE           1009
#define ERR_UNEXPECTED_CR       1010
#define ERR_BINARY_OVERFLOW     1011
#define ERR_VARIABLE_NOT_FOUND  1012
#define ERR_SHOW_VARS           1013
#define ERR_VAR_REPLACE         1014
#define ERR_HEAD_LENGTH         1015
#define ERR_MAKE_INDEX          1017
#define ERR_SKIP_HEADER         1018
#define ERR_VAR_POSITIONS       1019
#define ERR_NO_VARS_SHARED      1020
#define ERR_FIND_FORM           1021
#define ERR_CONVERT_VAR         1022

/* Binview Errors */

#define ERR_DBIN_EVENT          1500
#define ERR_BIN_NOT_DEFINED     1501
#define ERR_MAX_BIN_REACHED     1502
#define ERR_CACHE_DEFINED       1503
#define ERR_BINFORM_DEFINED     1504
#define ERR_HEAD_DEFINED        1505
#define ERR_UNKNOWN_SECTION     1506
#define ERR_UNKNOWN_OBJECT      1507
#define ERR_MAKE_DBIN           1508
#define ERR_SET_DBIN            1509
#define ERR_SHOW_DBIN           1510
#define ERR_SET_VIEW            1511
#define ERR_MAKE_VIEW           1512
#define ERR_MAX_VIEW_REACHED    1513
#define ERR_UNKNOWN_DATA_TYPE   1514
#define ERR_SHOW_VIEW           1515
#define ERR_FORMAT_LIST_DEF     1516
#define ERR_FILE_LENGTH         1517
#define ERR_DATA_ROUNDUP        1518
#define ERR_OVERFLOW_UNCHANGE   1519
#define ERR_NT_DEFINE           1520
#define ERR_DVIEW_EVENT         1521
#define ERR_MAKE_PLIST          1522
#define ERR_VIEW_UNDEFINED      1523
#define ERR_UNKNOWN_MESSAGE     1524
#define ERR_UNKNOWN_FORMAT_TYPE 1525
#define ERR_NAME_TABLE			1526
#define ERR_EVENT_RETIRED		1527

/* XVT Errors */

#define ERR_OPEN_DIALOG         2000
#define ERR_OPEN_SLIST          2001
#define ERR_ADD_SLIST           2002

/* String database and menu systems */

#define ERR_MAKE_MENU_DBASE     3000
#define ERR_NO_SUCH_SECTION     3001
#define ERR_GETTING_SECTION     3002
#define ERR_MENU                3003

#define ERR_MN_BUFFER_TRUNCATED 3500
#define ERR_MN_SEC_NFOUND       3501
#define ERR_MN_FILE_CORRUPT     3502
#define ERR_MN_REF_FILE_NFOUND  3503

/* Parameters and parsing */

#define ERR_SYNTAX              4000
#define ERR_MISSING_TOKEN       4001
#define ERR_MISSING_VAR         4002
#define ERR_PARAM_OVERFLOW      4005
#define ERR_PARAM_VALUE         4006
#define ERR_WARNING1            4010
#define ERR_BAD_NUMBER_ARGV     4011
#define ERR_STR_TO_NUMBER       4012
#define ERR_UNKNOWN_OPTION      4013
#define ERR_IGNORED_OPTION      4014
#define ERR_VARIABLE_DESC       4015

/* HDF errors */

#define ERR_MAKE_HDF            5000
#define ERR_DIMENSION           5001
#define ERR_MAKE_VSET           5002
#define ERR_SET_SDS_RANGE       5003
#define ERR_WRITE_HDF           5004
#define ERR_READ_HDF            5005
#define ERR_GET_HDF_FMT         5006
#define ERR_GET_HDF_OBJECT      5007

/* ADTLIB errors */

#define ERR_MAKE_MAX_MIN        6000
#define ERR_FIND_MAX_MIN        6001
#define ERR_PARSE_EQN           6002
#define ERR_EE_VAR_NFOUND       6003
#define ERR_CHAR_IN_EE          6004
#define ERR_EE_DATA_TYPE        6005
#define ERR_NDARRAY             6006
#define ERR_GEN_QUERY           6007

/* NAME_TABLE errors */

#define ERR_EXPECTING_SECTION_START  7001
#define ERR_EXPECTING_SECTION_END    7002
#define ERR_MISPLACED_SECTION_START  7003
#define ERR_MISPLACED_SECTION_END    7004
#define ERR_NT_MERGE                 7005

/* internal messaging errors */

#define ERR_MAX_STACK_EXCEEDED       7900
#define ERR_SWITCH_DEFAULT           7901

#define ERR_DEBUG_MSG                8000 /* Don't EVER change this number! */

typedef char *pPSTR;
typedef int  ERR_BOOLEAN;

#ifdef TRUE /* CodeWarrior for Mac is picky about this -rf01 */
#undef TRUE
#endif	/* end #ifdef TRUE -rf01 */
#define TRUE                    1
#ifdef FALSE /* CodeWarrior for Mac is picky about this -rf01 */
#undef FALSE
#endif	/* end #ifdef FALSE -rf01 */
#define FALSE                   0

#ifdef PROTO
void    err_clear(void);
void    err_disp(void);
void    err_end(void);
#ifdef ERR_ROUTINE_NAME
int    err_push_unwrapped(pPSTR,int,pPSTR);
#define err_push(a,b,c) err_push_unwrapped(a,b,c)
#else
int    err_push_unwrapped(int, pPSTR);
#define err_push(a,b,c) err_push_unwrapped(b,c)
#endif

ERR_BOOLEAN err_state(void);
#else
void    err_clear();
void    err_disp();
void    err_end();
#ifdef ERR_ROUTINE_NAME
int    err_push_unwrapped();
#define err_push(a,b,c) err_push_unwrapped(a,b,c)
#else
int    err_push_unwrapped();
#define err_push(a,b,c) err_push_unwrapped(b,c)
#endif

ERR_BOOLEAN err_state();
#endif


#endif



