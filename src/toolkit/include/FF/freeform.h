/*
 * NAME:        freeform.h
 *              
 * PURPOSE: Define freeform constants and data structures
 *
 * AUTHOR:      T. Habermann, NGDC, (303) 497 - 6472, haber@ngdc1.colorado.edu
 *
 * USAGE:       #include <freeform.h>
 *
 * COMMENTS:
 *
 */
/*
 * HISTORY:
 *	r fozzard	4/21/95		-rf01 
 *		fix MAX_EOL_1stCHAR typo (should be MAC_EOL_1stCHAR)
 *	r fozzard	7/25/95		-rf02 
 *		CodeWarrior for Mac is picky about multiply defined macros
 *	r fozzard	7/28/95		-rf03
 *		CodeWarrior doesn't need unix.h
 *	r fozzard	8/3/95		-rf04
 *		Redefine FF_TYPES_t to unsigned int to solve var_args stack problem; apparently
 *		on the Mac, (and maybe Unix), a 4-byte value is pushed, even when you request only
 *		2 bytes.
 *		Also, a fix to the TRUE/FALSE definitions to properly define them if not defined.
 *  Adura Adekunjo 08/14/01  Modified for solaris8
*/


/* First:
	Check to see if this file has been included already.
	If it has skip the definitions. */
#ifndef FREEFORM
#define FREEFORM
#define FF_LIB_VER "3.2"

/* Second:
	Some arrays are initialized in this include file. This process
	can only take place once. Whether or not it takes place is
	controlled by DEFINE_DATA. This constant is defined in the main
	program where the initialzation is done. */

/* MAO -- menu_dir_sep has been removed, and replaced with the #define
   "MENU_DIR_SEPARATOR" -- os_str_native_path() is being used unconditionally
   rather than a test on menu_dir_separator and then a call to nt_replace()
*/

#ifdef DEFINE_DATA
#define EXTERN
#else
#define EXTERN extern
#endif

/* common include files, for proper prototyping of all functions
	- they are included here to ensure prototyping.
	These might be better off in the individual files.
*/

#include <ctype.h>
#include <stdarg.h>
#include <stddef.h>
#include <stdio.h>
#include <string.h>
#if defined(XVT) && !defined(NDEBUG)
#define assert(exp) \
    ( (exp) ? (void) 0 : err_assert(#exp, __FILE__, __LINE__) )
void err_assert(void *, void *, unsigned);
#else
#include <assert.h>
#endif
#include <errno.h>

/* Operating System Dependent Definitions:

	The identifier for the operating system must be defined in the
	command line for the compiler on that system. Present options
	include:
		CCLSC   for the MacIntosh
		CCMSC   for Microsoft on the PC
		SUNCC   for ANSI C on the Sun-3 or Sun-4, or other UNIX (NOTE: DEC-ALPHA
	has different data type sizes than does the Sun -- this causes problems
	which remain to be fixed)
*/

/* Following are to be used when converting a floating point type to an
   integer type -- this should avoid the occasional truncation error */

#include <float.h>

#define DOUBLE_UP DBL_EPSILON
#define FLOAT_UP FLT_EPSILON
#define STR_END '\0' /* NULL-terminator for the C-string data type */

/* Directory Separators: specifically for the three currently supported
   general operating systems, for path names in menu files (same as DOS) -- 
   NATIVE_DIR_SEPARATOR is set to one of the three os-specific separators
   at compile-time, according to whether SUNCC, CCLSC, or CCMSC is defined.
*/

#define UNIX_DIR_SEPARATOR '/'
#define UNIX_EOL_STRING "\n"
#define UNIX_EOL_1stCHAR '\n'
#define UNIX_EOL_SPACE 1

#define MAC_DIR_SEPARATOR ':'
#define MAC_EOL_STRING "\r"
#define MAC_EOL_1stCHAR '\r'
#define MAC_EOL_SPACE 1

#define DOS_DIR_SEPARATOR '\\'
#define DOS_EOL_STRING "\r\n"
#define DOS_EOL_1stCHAR '\r'
#define DOS_EOL_2ndCHAR '\n'
#define DOS_EOL_SPACE 2

#define NUM_DIR_SEPARATORS 3
#define UNION_DIR_SEPARATORS "/:\\"
#define UNION_EOL_CHARS DOS_EOL_STRING
#define MENU_DIR_SEPARATOR DOS_DIR_SEPARATOR 

#ifdef SUNCC    /* ANSI C for the SUN, or other UNIX platforms */
#include <stdlib.h>
#include <fcntl.h>
#include <sys/uio.h>
#include <sys/types.h>
#include <sys/stat.h>

#ifndef _MAX_PATH
#define _MAX_PATH 260
#endif
		  
#define O_BINARY 0
#define EOL_SPACE UNIX_EOL_SPACE
#define EOL_1STCHAR UNIX_EOL_1stCHAR
#define NATIVE_EOL_1stCHAR UNIX_EOL_1stCHAR
#define NATIVE_EOL_SPACE UNIX_EOL_SPACE
#define NATIVE_DIR_SEPARATOR UNIX_DIR_SEPARATOR
#define NATIVE_EOL_STRING UNIX_EOL_STRING
#define IS_EOL(s) ((s) ? (*(s) == UNIX_EOL_1stCHAR) : 0) 

#define _fmemcmp memcmp
#define _fmemcpy memcpy
#define _fmemmove memmove
#define _fmemset fmemset
#define _fstrncmp strncmp
#define _fstrcpy fstrcpy
#define _fstrcspn strcspn

#ifndef INCL_XVTH
#ifndef PROTO
#define PROTO /* ANSI declarations required */ 
#endif
#endif

#endif          /* End of Sun Includes */

#ifdef CCLSC    /* LightSpeed C Macintosh, or other Macintosh */
/* #include <unix.h> -rf03 */
#include <fcntl.h>

#ifndef _MAX_PATH
#define _MAX_PATH 260
#endif
		  
#define EOL_SPACE MAC_EOL_SPACE
#define EOL_1STCHAR MAC_EOL_1stCHAR /* fix typo; was "MAX_EOL_1stCHAR" -rf01*/
#define NATIVE_DIR_SEPARATOR MAC_DIR_SEPARATOR
#define NATIVE_EOL_STRING MAC_EOL_STRING
#define NATIVE_EOL_1stCHAR MAC_EOL_1stCHAR
#define NATIVE_EOL_SPACE MAC_EOL_SPACE
#define IS_EOL(s) ((s) ? (*(s) == MAC_EOL_1stCHAR) : 0)  /* fix typo; was "MAX_EOL_1stCHAR" -rf01*/
#endif          /* End of LightSpeed C */

#ifdef CCMSC    /* Microsoft C PC */
#include <malloc.h>
#include <io.h>

#include <fcntl.h>
#include <sys\types.h>
#include <sys\stat.h>

#ifndef _MAX_PATH
#define _MAX_PATH 260
#endif
		  
#define EOL_SPACE DOS_EOL_SPACE
#define EOL_1STCHAR DOS_EOL_1stCHAR
#define NATIVE_DIR_SEPARATOR DOS_DIR_SEPARATOR
#define NATIVE_EOL_STRING DOS_EOL_STRING
#define NATIVE_EOL_1stCHAR DOS_EOL_1stCHAR
#define NATIVE_EOL_SPACE DOS_EOL_SPACE
#define IS_EOL(s) ((s) ? ((*(s) == DOS_EOL_1stCHAR) && \
                   (*(s+1) == DOS_EOL_2ndCHAR)) : 0) 

#ifndef INCL_XVTH
#ifndef PROTO
#define PROTO /* ANSI declarations required */ 
#endif
#endif

#endif          /* End of Microsoft C */

#include <err.h>
#include <dl_lists.h>   /* Included for definition of DLL_NODE */
#include <os_utils.h>   /* operating system dependent functions */
#include <memtrack.h>
#include <adtype.h>

#include <ff_types.h>

/* MEMORY MODELS

	The FREEFORM system deals with two types of buffers:

	Data buffers are thought to be sections of memory which
	are used for storing large amounts of data. These buffers
	can be larger than the PC limit of 64K, so they are defined
	as huge data objects. These objects are refered to as
	FF_DATA_BUFFERs.

	Scratch buffers are small sections of memory that are used for
	a number of string manipulation tasks. They are located in the
	near heap. Generally the application needs to make one of these
	things and then pass it's address to the freeform functions. This
	is a near address.
	
	The definitions of these buffers are given here.        

*/

#ifdef MEDIUM           /* Definitions for Medium memory model */

#ifndef INCL_XVTH
#define FAR     _far
#endif
#define HUGE    _huge

/* Caution: This function list should contain only those that will
	operate on FAR data objects.    

   Functions that need memory: strchr, strcmp
*/

#define memcpy  _fmemcpy
#define memcmp  _fmemcmp
#define memmove _fmemmove
#define memset  _fmemset
#define strncmp _fstrncmp
#define strcpy  _fstrcpy
#define strcspn _fstrcspn

#else           /* Non Medium Memory Model definitions */

#ifndef INCL_XVTH
#define FAR
#endif
#ifdef HUGE
#undef HUGE
#endif
#define HUGE

#endif          /* End of Non-Medium memory model definitions */

#define FF_SCRATCH_BUFFER       char *
#define FF_DATA_BUFFER          char HUGE *
#define FF_DATA_PTR             void HUGE *

/* Utility macros */
#define CH_TO_END(p) (p+=ok_strlen(p))
#define FFV_VARIABLE(v) ( (v) ? ((VARIABLE_PTR)dll_data(v)) : NULL)
#define FFF_FORMAT_FMT(f) ( (f) ? ((FORMAT_PTR)dll_data(f)) : NULL)
#define FFF_FIRST(fl) ( (fl) ? (dll_first(fl)) : NULL)
#define FFF_NEXT(fl) ( (fl) ? (dll_next(fl)) : NULL)
#define FD_FIRST(fdl) ( (fdl) ? (dll_first(fdl)) : NULL)
#define FD_LAST(fdl) ( (fdl) ? (dll_last(fdl)) : NULL)
#define FD_FORMAT_DATA(fd) ( (fd) ? ((FORMAT_DATA_PTR)dll_data(fd)) : NULL)
#define FFV_FIRST_VARIABLE(vl) ( (vl) ? (dll_first (vl->variables)) : NULL)
#define FFV_NEXT_VARIABLE(vl) ( (vl) ? (dll_next(vl)) : NULL)
#define FORMAT_LENGTH(f) ( (f) ? ( (((f)->type) & FFF_ASCII) ? ((f)->max_length) + EOL_SPACE : ((f)->max_length) ) : (FF_TYPES_t)0 )
#define FF_VAR_LENGTH(v) ( (v) ? (v->end_pos - v->start_pos + (FF_TYPES_t)1) : (FF_TYPES_t)0)

#define FFV_DATA_TYPE_TYPE(t) (t & FFV_DATA_TYPES)
#define FFV_DATA_TYPE(v) ((v) ? FFV_DATA_TYPE_TYPE(v->type) : FFV_NULL)
#define FFV_TYPE(v) FFV_DATA_TYPE(v)
#define FFF_TYPE(f) ((f) ? (f->type) : FFF_NULL)
#define FFF_FILE_TYPE(f) (FFF_TYPE(f) & FFF_FILE_TYPES)
#define FD_TYPE(fd) (fd->format->type & FD_TYPES)

/* constants */
#define END_ARGS        0
#define MAX_PV_LENGTH 355 /* Maximum parameter or parameter-value length */
#define MAX_NAME_LENGTH MAX_PV_LENGTH
#define TMP_BUFFER_SIZE 1024

/* Types and struct definitions */

typedef DLL_NODE         FORMAT_LIST,
                         VARIABLE_LIST,
                         FORMAT_DATA_LIST;

typedef DLL_NODE_PTR     FORMAT_LIST_PTR,
                         VARIABLE_LIST_PTR,
                         FORMAT_DATA_LIST_PTR;

typedef DLL_NODE_PTR    *DLL_NODE_HANDLE; /* until I move this into dl_lists.h */

typedef DLL_NODE_HANDLE  FORMAT_LIST_HANDLE,
                         VARIABLE_LIST_HANDLE,
                         FORMAT_DATA_LIST_HANDLE;

typedef struct
{
	char  *input_file;
	char  *output_file;
	char  *input_format_file;
	char  *input_format_title;
	char  *output_format_file;
	char  *output_format_title;
	char  *var_file;
	char  *query_file;
	char  *local_buffer;
	long   local_buffer_size;
	long   cache_size;
	long   records_to_read;
	
	/* Checkvar specific option flags */
	int     cv_precision;
	double  cv_missing_data;
	int     cv_maxbins;
	
	struct user_struct_t
	{
		unsigned int set_input_file : 1;
		unsigned int set_output_file : 1;
		unsigned int set_input_format_file : 1;
		unsigned int set_input_format_title : 1;
		unsigned int set_output_format_file : 1;
		unsigned int set_output_format_title : 1;
		unsigned int set_var_file : 1;
		unsigned int set_query_file : 1;
		unsigned int set_local_buffer_size : 1;
		unsigned int set_records_to_read : 1;
		unsigned int set_cv_precision : 1;
		unsigned int set_cv_missing_data : 1;
		unsigned int set_cv_maxbins : 1;
		unsigned int set_cv_maxmin_only : 1;
		unsigned int set_unused_1 : 1;
		unsigned int set_unused_2 : 1;
	} user;
} FFF_STD_ARGS, *FFF_STD_ARGS_PTR, **FFF_STD_ARGS_HANDLE;

typedef struct struct_variable_t
{
	void           *check_address;           /* assert(variable->check_address == variable); */
	void           *info;                    /* Pointer to a type dependent structure with supplementary information about the variable */
	FFV_TYPE_type   type;                    /* variable type, see FFF_LOOKUP variable_types */
	unsigned short  start_pos;               /* starting byte of variable in a record, counting from 1 (one) */
	unsigned short  end_pos;                 /* ending byte of variable in a record */
	short           precision;               /* implied (if integer) or real (if floating point) precision, i.e., number of decimal places */
	char            name[MAX_PV_LENGTH + 2]; /* the variable name */
} VARIABLE, *VARIABLE_PTR, **VARIABLE_HANDLE;

typedef struct struct_format_t
{
	void              *check_address;           /* assert(format->check_address == format); */
	VARIABLE_LIST_PTR  variables;               /* the DLL of variable structures */
	FFF_TYPE_type      type;                    /* format type, see FFF_LOOKUP format_types */
	unsigned short     num_in_list;             /* number of variables in format */
	unsigned short     max_length;              /* record length in bytes */
	char               name[MAX_PV_LENGTH + 2]; /* title if given in a format file or menu section */
	char               locus[_MAX_PATH];        /* path-name or menu section title of format */
} FORMAT, *FORMAT_PTR, **FORMAT_HANDLE;

#define FORMAT_NAME_INIT "Format Was Untitled"
#define FORMAT_LOCUS_INIT "Unknown Origin"

typedef struct
{
	char   *buffer;
	size_t  bytes_used;
	size_t  total_bytes;
} FF_BUFSIZE, *FF_BUFSIZE_PTR, **FF_BUFSIZE_HANDLE;

#ifndef INT_MAX
#include <limits.h>
#endif

#define BUFSIZE_TOTAL_BYTES_UNKNOWN INT_MAX

typedef struct
{
/* generic io_element suitable for name tables, headers, and caches --
   anything that contains data in a dynamic buffer described by a format
*/
	void *check_address;
	FORMAT_PTR format;
	FF_BUFSIZE_PTR data;
} FORMAT_DATA, *FORMAT_DATA_PTR, **FORMAT_DATA_HANDLE;

typedef FORMAT_DATA   NAME_TABLE,
                     *NAME_TABLE_PTR,
                    **NAME_TABLE_HANDLE;

typedef FORMAT_DATA_LIST   NAME_TABLE_LIST,
                          *NAME_TABLE_LIST_PTR,
                         **NAME_TABLE_LIST_HANDLE;

typedef enum pp_object_enum
{
	PPO_FORMAT_LIST = 0,
	PPO_NT_LIST     = 1,
	ADD_YOUR_TYPE_HERE = 2
} PP_OBJECT_TYPES;

typedef struct pp_object_struct
{
	union object_union
	{
		FORMAT_LIST_HANDLE hf_list;
		struct NT_LIST_OBJ_struct
		{
			FF_TYPES_t nt_io_type;
			NAME_TABLE_LIST_HANDLE hnt_list;
		} nt_list;
		void *add_your_object_here;
	} ppo_object;
	PP_OBJECT_TYPES ppo_type;
} PP_OBJECT, *PP_OBJECT_PTR;

/* Format Attributes and object types */
#define FFF_NAME                2
#define FFF_FORMAT              4
#define FFF_INDEX_ARRAY 5

#define FFF_VARIABLES   (FF_TYPES_t)0X0001
#define FFF_INFO        (FF_TYPES_t)0X0002
#define FFF_FMT_INFO    (FF_TYPES_t)0X0004

typedef struct {
	char *name;
#ifdef PROTO
	int (*convert_func)(VARIABLE_PTR, double *, FORMAT_PTR, FF_DATA_BUFFER);
#else
	int (*convert_func)();
#endif
} CONVERT_VARIABLE;

#ifdef PROTO

/* Conversion Utility Function Prototypes */
extern int cv_multiply_value(VARIABLE *, double *, double, char *, FORMAT *, FF_DATA_BUFFER);

/* Conversion Function Prototypes */
extern int cv_abs       (VARIABLE_PTR, double *, FORMAT_PTR, FF_DATA_BUFFER);
extern int cv_abs_sign_to_value(VARIABLE_PTR, double *, FORMAT_PTR, FF_DATA_BUFFER);
extern int cv_units     (VARIABLE_PTR, double *, FORMAT_PTR, FF_DATA_BUFFER);
extern int cv_date_string       (VARIABLE_PTR, double *, FORMAT_PTR, FF_DATA_BUFFER);
extern int cv_time_string       (VARIABLE_PTR, double *, FORMAT_PTR, FF_DATA_BUFFER);
extern int cv_deg       (VARIABLE_PTR, double *, FORMAT_PTR, FF_DATA_BUFFER);
extern int cv_deg_nsew  (VARIABLE_PTR, double *, FORMAT_PTR, FF_DATA_BUFFER);
extern int cv_degabs    (VARIABLE_PTR, double *, FORMAT_PTR, FF_DATA_BUFFER);
extern int cv_degabs_nsew(VARIABLE_PTR, double *, FORMAT_PTR, FF_DATA_BUFFER);
extern int cv_deg_abs   (VARIABLE_PTR, double *, FORMAT_PTR, FF_DATA_BUFFER);
extern int cv_dms       (VARIABLE_PTR, double *, FORMAT_PTR, FF_DATA_BUFFER);
extern int cv_feet_to_meters(VARIABLE_PTR, double *, FORMAT_PTR, FF_DATA_BUFFER);
extern int cv_geo44tim(VARIABLE_PTR, double *, FORMAT_PTR, FF_DATA_BUFFER);
extern int cv_geog_quad (VARIABLE_PTR, double *, FORMAT_PTR, FF_DATA_BUFFER);
extern int cv_geog_sign (VARIABLE_PTR, double *, FORMAT_PTR, FF_DATA_BUFFER);
extern int cv_ipe2ser   (VARIABLE_PTR, double *, FORMAT_PTR, FF_DATA_BUFFER);
extern int cv_lon_east  (VARIABLE_PTR, double *, FORMAT_PTR, FF_DATA_BUFFER);
extern int cv_long2mag  (VARIABLE_PTR, double *, FORMAT_PTR, FF_DATA_BUFFER);
extern int cv_mag2long  (VARIABLE_PTR, double *, FORMAT_PTR, FF_DATA_BUFFER);
extern int cv_meters_to_feet(VARIABLE_PTR, double *, FORMAT_PTR, FF_DATA_BUFFER);
extern int cv_nsew      (VARIABLE_PTR, double *, FORMAT_PTR, FF_DATA_BUFFER);
extern int cv_ser2ymd   (VARIABLE_PTR, double *, FORMAT_PTR, FF_DATA_BUFFER);
extern int cv_ser2ipe   (VARIABLE_PTR, double *, FORMAT_PTR, FF_DATA_BUFFER);
extern int cv_sea_flags (VARIABLE_PTR, double *, FORMAT_PTR, FF_DATA_BUFFER);
extern int cv_slu_flags (VARIABLE_PTR, double *, FORMAT_PTR, FF_DATA_BUFFER);
extern int cv_ymd2ser   (VARIABLE_PTR, double *, FORMAT_PTR, FF_DATA_BUFFER);
extern int cv_ydec2ymd 	(VARIABLE_PTR, double *, FORMAT_PTR, FF_DATA_BUFFER);
extern int cv_ymd2ipe   (VARIABLE_PTR, double *, FORMAT_PTR, FF_DATA_BUFFER);
extern int cv_noaa_eq   (VARIABLE_PTR, double *, FORMAT_PTR, FF_DATA_BUFFER);

extern char *ff_lookup_string(FFF_LOOKUP_PTR, FF_TYPES_t);
extern FF_TYPES_t ff_lookup_number(FFF_LOOKUP_PTR, char *);
unsigned int ff_file_to_buffer(char *, char *);  
int ff_file_to_bufsize(char *, FF_BUFSIZE_PTR);
int ff_make_newline(char *);

extern int parse_command_line(int, char **, FFF_STD_ARGS_PTR);

/* FREEFORM Functions Which Deal With FF_SCRATCH_BUFFER */
extern int ff_show_format(FORMAT_PTR, FF_BUFSIZE_PTR);
extern int ff_show_format_list(FORMAT_LIST *, FF_SCRATCH_BUFFER);
extern FF_DATA_BUFFER ff_strnstr(char *pcPattern, FF_DATA_BUFFER pcText, size_t uTextLen);

/* FREEFORM Functions Which Deal With FF_SCRATCH_BUFFER */

extern int ff_get_double(VARIABLE_PTR, FF_DATA_PTR, double *, FF_TYPES_t);
extern int ff_string_to_binary(char *, FF_TYPES_t, FF_DATA_BUFFER);
extern int ff_binary_to_string(void *source, FF_TYPES_t data_type, char *target, int optional_size);
extern int ff_process_variable_list(FF_DATA_BUFFER, FF_DATA_BUFFER, FORMAT_PTR, FORMAT_PTR, long *);

extern int ff_show_variables(FF_SCRATCH_BUFFER, FF_SCRATCH_BUFFER, FORMAT_PTR);
extern int ff_get_value(VARIABLE_PTR, FF_DATA_PTR, FF_DATA_PTR, FF_TYPES_t);
extern int ff_get_string(VARIABLE_PTR, FF_DATA_PTR, FF_SCRATCH_BUFFER, FF_TYPES_t);
extern FORMAT_PTR ff_dup_format(FORMAT_PTR);
extern unsigned ff_skip_header(FORMAT_PTR, int);
extern int ff_text_pre_parser(char *, FF_BUFSIZE_PTR, PP_OBJECT_PTR pp_object);
extern VARIABLE_PTR ff_make_variable(VARIABLE_PTR);
extern FORMAT_PTR ff_create_format(void);
extern void ff_free_format(FORMAT_PTR);
extern void ff_free_variable(VARIABLE_PTR);
extern FORMAT_PTR ff_find_format(FORMAT_LIST_PTR, ...);
extern int ff_find_files(char *file_base, char *ext, char *first_dir, char ***found_files);
extern FORMAT_PTR ff_afm2bfm(FORMAT_PTR);
extern VARIABLE_PTR ff_find_variable(char *, FORMAT_PTR);
extern VARIABLE_PTR ff_new_name(FORMAT_PTR, char *, char *);
extern unsigned char ff_var_length(VARIABLE_PTR);
extern int ff_header_to_format(FORMAT *, char *, int, int, int);
extern int btype_to_btype(void *src_value, FF_TYPES_t src_type, void *dest_value, FF_TYPES_t dest_type);
extern BOOLEAN type_cmp(FF_TYPES_t type, void *value0, void *value1);

/* Functions which DO NOT take huge pointers */
extern int ff_list_vars(FORMAT_PTR, FF_BUFSIZE_PTR);
extern int ff_format_info(FORMAT_PTR, FF_BUFSIZE_PTR);
extern void ff_show_format_description(FORMAT_PTR format, FF_BUFSIZE_PTR bufsize);

/* Other */
extern void byte_swap(char *dataptr, FF_TYPES_t vartype);
extern FF_BUFSIZE_PTR ff_create_bufsize(size_t total_bytes);
extern int ff_resize_bufsize(size_t new_size, FF_BUFSIZE_HANDLE hbufsize);
extern void ff_destroy_bufsize(FF_BUFSIZE_PTR bufsize);
#ifdef IBM6000
extern int putenv(char *);
#endif
#if defined(SUNOS58X) || defined(SUNOS59X) || defined(SUNOS510X)
extern int putenv(char *);
#else
#ifdef LINUX
;
#else
extern int putenv(const char *);
#endif
#endif
#ifndef LINUX
extern char *strdup(const char *);
extern int strncasecmp(const char *, const char *, size_t);
extern int strcasecmp(const char *, const char *);
#endif

#else
/* Conversion Utility Function Prototypes */
extern int cv_multiply_value();

/* Conversion Function Prototypes */
extern int cv_abs       ();
extern int cv_abs_sign_to_value();
extern int cv_units     ();
extern int cv_date_string       ();
extern int cv_time_string       ();
extern int cv_deg       ();
extern int cv_deg_nsew  ();
extern int cv_degabs    ();
extern int cv_degabs_nsew();
extern int cv_deg_abs   ();
extern int cv_dms       ();
extern int cv_feet_to_meters();
extern int cv_geo44tim();
extern int cv_geog_quad ();
extern int cv_geog_sign ();
extern int cv_ipe2ser   ();
extern int cv_lon_east  ();
extern int cv_long2mag  ();
extern int cv_mag2long  ();
extern int cv_meters_to_feet();
extern int cv_nsew      ();
extern int cv_ser2ymd   ();
extern int cv_ser2ipe   ();
extern int cv_sea_flags ();
extern int cv_slu_flags ();
extern int cv_ydec2ymd   ();
extern int cv_ymd2ser   ();
extern int cv_ymd2ipe   ();
extern int cv_noaa_eq   ();

extern char *ff_lookup_string();
extern FF_TYPES_t ff_lookup_number();
unsigned int ff_file_to_buffer();
int ff_file_to_bufsize(char *, FF_BUFSIZE_PTR);
int ff_make_newline();


extern void ff_replace_percents_in_variable_names();

/* FREEFORM Functions Which Deal With FF_SCRATCH_BUFFER */

extern int ff_show_format();
extern int ff_show_format_list();
extern FF_DATA_BUFFER ff_strnstr();

/* FREEFORM Functions Which Deal With FF_SCRATCH_BUFFER */

extern int ff_get_double();
extern int ff_string_to_binary();
extern int ff_binary_to_string();
extern int ff_process_variable_list();

extern int ff_show_variables();
extern int ff_get_value();
extern int ff_get_string();
extern FORMAT_PTR ff_dup_format();
extern unsigned ff_skip_header();
extern FORMAT_LIST_PTR ff_pre_parse();
extern FORMAT_PTR ff_afm2bfm();
extern VARIABLE_PTR ff_make_variable();
extern FORMAT_PTR ff_create_format();
extern FORMAT_PTR ff_free_format();
extern FORMAT_PTR ff_find_format();
extern int ff_find_files();
extern int ff_find_format_files();
extern VARIABLE_PTR ff_find_variable();
extern VARIABLE_PTR ff_new_name();
extern unsigned char ff_var_length();
extern BOOLEAN ff_header_to_format();
extern BOOLEAN btype_to_btype();
extern BOOLEAN type_cmp();

/* Functions which DO NOT take huge pointers */
extern int ff_list_vars();
extern int ff_format_info(); 
extern void ff_show_format_description();
/* Other */
extern void byte_swap();
extern FF_BUFSIZE_PTR ff_create_bufsize();
extern FF_BUFSIZE_PTR ff_resize_bufsize();
extern void ff_destroy_bufsize();

#endif

#ifdef DEFINE_DATA
	/* MAO:d changed "cd_driver" to "cd_device" */
	char cd_device[MAX_PV_LENGTH]={'\0'};
#else
	EXTERN char cd_device[MAX_PV_LENGTH];
#endif

/* external variable key_var is needed for the freeform function compvar() */
EXTERN VARIABLE *key_var;

#endif  /* End of ifndef FREEFORM */

