/*
 *
 * NAME:	databin.h
 *		
 * PURPOSE: Define data bin constants and data structures
 *			This include file defines the databin structures
 * 			This is a working version of the program which correctly sets
 * 			up the bins and views and can get data from both bins.
 * 			It does not process the data in each segment. In order to do this
 * 			a list of headers and output bins are needed.
 *
 * AUTHOR:	T. Habermann, NGDC, (303) 497 - 6472, haber@mail.ngdc.noaa.gov
 *
 * USAGE:	#include <databin.h>
 *
 * COMMENTS:
 *
 * 
 * 
 */
/*
 * HISTORY:
 * 	R Fozzard	12/6/94		-rf01
 *		remove some redefinitions for CCLSC (Mac)
 *		Also, be sure to define SCATCH_BUFFER and DATA_BUFFER
 */
 
/* This chunk of preprocessor code is for the inclusion of the
 * new menu stuff into geovu.  */
#ifndef NEW_MENU
#ifndef SDB_MENU
#define SDB_MENU
#endif
#endif

/* Define a constant to avoid multiple inclusions */
#ifndef DB_INCLUDED
#define DB_INCLUDED

#include <index.h>
#include <name_tab.h>
#include <data_par.h>
#include <dl_lists.h>

#include <menuindx.h>

/* MEMORY MODELS

	The FREEFORM system deals with two types of buffers:

	Data buffers are sections of memory which
	are used for storing large amounts of data. These buffers
	can be larger than the PC limit of 64K, so they are defined
	as huge data objects. These objects are refered to as
	DATA_BUFFERs.

	Scratch buffers are small sections of memory that are used for
	a number of string manipulation tasks. They are located in the
	near heap. Generally the application needs to make one of these
	things and then pass it's address to the freeform functions. This
	is a near address.
	
	The definitions of these buffers are given here.	

*/

/* The medium memory model is used with MS windows and XVT */
#ifdef CCLSC
#define HUGE
#endif

#ifdef MEDIUM
#define memcpy	_fmemcpy
#define memcmp	_fmemcmp
#define memmove _fmemmove
#define memset	_fmemset
#define BUFFER_PTR		char *
/* (MAO:d use FF_DATA_PTR instead) #define DATA_PTR void huge * */

/* these defs are repeated from freeform.h, and are not needed here? -rf01 */
/* #ifdef CCLSC							-rf01 */
/*	#define FF_SCRATCH_BUFFER		char *		-rf01 */
/*	#define FF_DATA_BUFFER		char huge *		-rf01 */
/* #else							-rf01 */
	#define SCRATCH_BUFFER		char *
	#define DATA_BUFFER		char huge *
/* #endif							-rf01 */

/* There are a number of functions which are used to operate on the data
caches which do not work with huge pointers. These must be selectively
nt_replaced in the medium memory model with huge pointers.
*/
#define READ_DATA _lread

#else

#define FAR
#define BUFFER_PTR		char *
/* (MAO:d use FF_DATA_PTR instead) #define DATA_PTR void * */
/* these defs are repeated from freeform.h, and are not needed here? -rf01 */
/* #ifdef CCLSC							-rf01 */
/*	#define FF_SCRATCH_BUFFER		char *		-rf01 */
/*	#define FF_DATA_BUFFER		char *			-rf01 */
/* #else							-rf01 */
#define SCRATCH_BUFFER		char *
#define DATA_BUFFER		char *
/* #endif							-rf01 */
#define READ_DATA memRead
#endif

/* define data type */
#define DATA_TYPE_IMAGE           1
#define DATA_TYPE_VECTOR          2
#define DATA_TYPE_POINT           3

/* define header type */
#define HEADER_NONE               0         /* no header */
#define HEADER_EMBEDDED			1		/* enbeded header, fixed item position */
#define HEADER_EMBEDDED_VARIED    2		/* enbeded header, varied item position */
#define HEADER_SEPARATED          3		/* seperated header, fixed item position */
#define HEADER_SEPARATED_VARIED   4		/* seperated header, varied item position */

/* Define a string to indicate the format buffer contains a format title */
#define FORMAT_BUFFER_IS_A_TITLE   ":/\\.NOT_A_FILE.\\/:"

/* Define BIN structures */

typedef struct databin_struct_t{
	void*	check_address;
	char 	*title;
	char	*file_name; 	   /* data file name */
	char	*header_file_name; /* header file name */
	char	*header; 	   /* header data */

	void	*data_parameters;  /* pointer to data parameters related to data type */

	FF_SCRATCH_BUFFER	buffer;	   /* Scratch Buffer */

	FF_DATA_BUFFER	cache;		   /* begining of the data cache */
	FF_DATA_BUFFER	cache_end; 	   /* end of cache */
	FF_DATA_BUFFER	data_ptr;	   /* pointer to data */

	struct state_struct_t {
		unsigned int open_file        : 1;
		unsigned int std_input        : 1;
		unsigned int cache_defined    : 1;
		unsigned int cache_filled     : 1;
		unsigned int header_defined   : 1;
		unsigned int header_delimited : 1;
		unsigned int input_delimited  : 1;
		unsigned int new_record       : 1;
		unsigned int read_only        : 1;
		unsigned int format_key       : 1;
		unsigned int byte_order       : 1;
		unsigned int empty2           : 5;
	} state;
	unsigned char	type;
#ifdef CCMSC
	char db_pc_padding; /* explicitly force 2-byte alignment */
#endif
	unsigned char	*header_delimited_fields;
	unsigned char	*input_delimited_fields;

	unsigned int	bytes_to_read; /* usually input record length */
	unsigned int	num_input_vars;
	unsigned int	num_header_vars;

	int		header_type;
	short		projection;     /* georeferred ? : 0=none */

	long		cache_size;
	long		records_in_cache;
	long 		file_location;
	long		bytes_available; /* bytes remaining in file for processing */
	long		data_available; /* bytes remaining in cache for processing */

	FORMAT_PTR	input_format;
	FORMAT_PTR	output_format;
	FORMAT_PTR	header_format;
	INDEX		*index;
#ifdef XVT
	MENU_INDEX_PTR mindex;			/* Menu index */
	MENU_OPEN_PTR gvmopen;			/* Menu open struct */
	MENU_SELECTION_PTR gvmselection;/* Menu selection struct */
#endif
	FORMAT_LIST_PTR	format_list;
	NAME_TABLE_LIST_PTR table_list;	/* define the name equivalence and constants */
	int		data_file;
	int		header_file;
} DATA_BIN, *DATA_BIN_PTR, **DATA_BIN_HANDLE;

/* Define Defaults */
#define DEFAULT_BUFFER_SIZE	32768U
#ifdef SUNCC
#define DEFAULT_CACHE_SIZE	20971520L
#else
#define DEFAULT_CACHE_SIZE	16384L
#endif

/* Define Generic attributes */
#define BUFFER				 1
#define INPUT_FORMAT 		 2
#define OUTPUT_FORMAT		 3
#define HEADER_FORMAT		 4
#define SHOW_FORMAT_LIST			 5
#define TITLE				 6
#define BUFFER_IN			 7
#define PROCESS_FORMAT_LIST	 8
#define DEFINE_HEADER		 9
#define PROCESS_HEADERS		10
#define MAKE_NAME_TABLE		11
#define CHECK_FILE_SIZE		12
#define DBIN_BUFFER_SIZE	13
#define DBIN_HEADER		14
#define DBIN_INPUT_CACHE	15
#define DBIN_OUTPUT_CACHE	16
#define DBIN_FORMAT		17	

/* Define BIN attributes */
#define DBIN_FILE_NAME		101
#define DBIN_FILE_HANDLE	102
#define DBIN_CACHE			103
#define DBIN_CACHE_SIZE		104
#define DBIN_NEXT_RECORD	105
#define DBIN_FILL_CACHE		106
#define DBIN_GET_VECTOR		107
#define DBIN_BUFFER_TO_INDEX	108
#define DBIN_BOX_NUMBER		109
#define DBIN_HEADER_FILE_NAME	110
#define DBIN_CONVERT_CACHE	111
#define DBIN_FLAGS			112
#define DBIN_SET_HEADER		113
#define DBIN_OUTPUT_SIZE	114
#define DBIN_RECORD_COUNT	115
#define DBIN_BYTE_COUNTS	116
#define DBIN_FILE_POSITION	117
#define DBIN_INDEX			118
#define DBIN_DATA_AVAILABLE	119
#define DBIN_SET_SCALE		120
#define DBIN_SCALE_CACHE	121
#define DBIN_CACHE_TO_HUGE	122
#define DBIN_HUGE_TO_CACHE	123
#define DBIN_DATA_TO_NATIVE	124
#define DBIN_CONVERT_HEADER	125
/* DBIN_SET_STRDB has retired!!! */
#define DBIN_SET_STRDB		126
#define DBIN_STD_FORMAT 	127
#define DBIN_BYTE_ORDER		128
#define DBIN_DEMUX_CACHE	129
#define DBIN_NEXT_REC_IN_CACHE	130
#define DBIN_CONVERT_ROWSIZES	131
#define DBIN_SET_MENU_INDEX		132
#define DBIN_SET_TOTAL_RECORDS	133
#define DBIN_SET_MENU_SELECTION	134
				      
/* Define BIN states */
#define DBS_OPEN_FILE		(unsigned short)0X0001
#define DBS_CACHE_DEFINED	(unsigned short)0X0002
#define DBS_FILLED_CACHE	(unsigned short)0X0004
#define DBS_HEADER_DEFINED	(unsigned short)0X0008
#define DBS_HEADER_DELIMITED	(unsigned short)0X0010
#define DBS_INPUT_DELIMITED	(unsigned short)0X0020
#define DBS_NEW_RECORD		(unsigned short)0X0040
#define DBS_READ_ONLY		(unsigned short)0X0080
#define DBS_FORMAT_KEY		(unsigned short)0X0100
#define DBS_BYTE_ORDER       (unsigned short)0X0200

#ifdef PROTO
/* data bin function prototypes */

DATA_BIN_PTR	db_make(char *);
int		db_set(DATA_BIN_PTR, ...);
int		db_show(DATA_BIN_PTR, ...);
int		db_events(DATA_BIN_PTR, ...);

/* Format List Functions */
extern FORMAT_LIST_PTR db_merge_format_lists(FORMAT_LIST_PTR listone, FORMAT_LIST_PTR listtwo);
extern void db_format_list_mark_io(FORMAT_LIST_PTR f_list, char *file_name);
extern int db_delete_all_formats(FORMAT_LIST_PTR list, FF_TYPES_t format_type);
extern void db_delete_rivals(FORMAT_LIST_PTR f_list, FORMAT_PTR save_format);
extern FORMAT_LIST_PTR db_replace_format(FORMAT_LIST_PTR, FORMAT_PTR);
extern FORMAT_PTR db_find_format(FORMAT_LIST_PTR, ...);
extern FORMAT_PTR db_find_format_is_isnot(FORMAT_LIST_PTR, ...);
extern int db_show_format_list(FORMAT_LIST_PTR, FF_TYPES_t, FF_SCRATCH_BUFFER);
extern int db_delete_format(FORMAT_LIST_PTR, FF_TYPES_t);
extern void db_free_format_list(FORMAT_LIST_PTR);
extern long db_embedded_header_length(DATA_BIN *);


/* name table function proto related to dbin*/
extern BOOLEAN nt_askvalue(DATA_BIN_PTR, char *, FF_TYPES_t, void *, char *);
extern BOOLEAN nt_putvalue(DATA_BIN_PTR, char *, FF_TYPES_t, void *, short);
extern BOOLEAN nt_askexist(DATA_BIN_PTR, char *);

extern int get_var_related_info(DATA_BIN *, VARIABLE *, short, short, unsigned int, void *);

FORMAT *make_head_format(FORMAT *, char *);
int put_var_related_info(DATA_BIN *, VARIABLE *, short, short, unsigned int, void *, short);
extern int set_point(DATA_BIN *);
extern short classname(DATA_BIN *, short, char *);
DLL_NODE_PTR db_format_to_view_list(FORMAT_PTR, char *);
int make_standard_dbin(FFF_STD_ARGS_PTR, DATA_BIN_HANDLE, int (*pfi)(int));
int db_hdf_fmt_to_buffer(DATA_BIN_PTR, char*, char*, unsigned short *, unsigned short*);

long do_get_count(FF_TYPES_t, ...);

int set_var(VARIABLE *, char *, char *, FORMAT *);
BOOLEAN initialize_image(DATA_BIN *);
VARIABLE_PTR get_display_var(FORMAT_PTR, short);
int number_of_var(FORMAT_PTR);
void db_free(DATA_BIN_PTR);
void db_free_format_list(FORMAT_LIST_PTR );
char *stdform(char *);
extern char *delete_white_space(char *);
int endian(void);

extern int db_find_format_files(DATA_BIN_PTR dbin, char *input_file, char ***found_files);

#else

/* data bin function prototypes */

extern DATA_BIN_PTR	db_make();
extern int		db_set();
extern int		db_show();
extern int		dbin_event();
extern BOOLEAN		nt_askvalue();
extern BOOLEAN		nt_askexist();
extern BOOLEAN          nt_putvalue();
extern int get_var_related_info();
extern FORMAT		*make_head_format();
extern int put_var_related_info();
extern int set_point();
extern long db_embedded_header_length();
extern DLL_NODE_PTR db_format_to_view_list();
extern DATA_BIN_PTR make_standard_dbin();
extern int db_hdf_fmt_to_buffer();


extern char        *delete_white_space(); 
extern short classname();

extern long do_get_count();

/* Format List Functions */
extern FORMAT_LIST_PTR	db_replace_format();
extern FORMAT_PTR	db_find_format();
extern int		db_show_format_list();
extern int		db_delete_format();
extern void		db_free_format_list();

extern int		set_var();
extern BOOLEAN		initialize_image();
extern int		number_of_var();
extern void		db_free();
extern char		*stdform(); 
extern VARIABLE_PTR	get_display_var();
extern int endian();

#endif

/* End of ifndef DB_INCLUDED */
#endif
