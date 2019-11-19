/* 

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

/* Avoid multiple includes */
#ifndef VIEW_TYPE

/* This is included for the EQUATION_INFO struct */
#include <eval_eqn.h>
     
/* The medium memory model is used with MS windows and XVT */

#ifdef MEDIUM
#define memcpy	_fmemcpy
#define memcmp	_fmemcmp
#define memmove _fmemmove
#define memset	_fmemset

#define SCRATCH_BUFFER	char *
#define BUFFER_PTR	char *
#define DATA_BUFFER		char huge *
/* (MAO:d use FF_DATA_PTR instead) #define DATA_PTR void huge * */

#else

#define FAR
#define SCRATCH_BUFFER	char *
#define BUFFER_PTR		char *
#define DATA_BUFFER		char *
/* (MAO:d use FF_DATA_PTR instead) #define DATA_PTR void * */

#endif

/* Define the ROW_SIZES structure, if it has not already been defined */
/**********************************************************************/
/** NOTE: THIS STRUCTURE IS ALSO DEFINED IN MENUINDX.H!!!!!!!!!!!!!! **/ 
/**********************************************************************/
#ifndef ROWSIZE_STRUCT_DEFINED
#define ROWSIZE_STRUCT_DEFINED
typedef	struct {
		long start;
		long num_bytes;
}ROW_SIZES, *ROW_SIZES_PTR;
#endif

struct pl{
	VARIABLE_PTR var;
	void *minimum;
	void *maximum;
	struct pl *next;
};

typedef struct pl PARAM_LIST, *PARAM_LIST_PTR;


/* Define VIEW structures */

typedef struct {
	void *check_address;
	char *title;
	DATA_BUFFER data;
	DATA_BUFFER first_pointer;
	SCRATCH_BUFFER buffer;
	void *info;
	
	int palette;

	unsigned char type;
	unsigned x_lower_right;
	unsigned y_lower_right;
	unsigned x_upper_left;
	unsigned y_upper_left;
	unsigned num_pointers;
	unsigned length;
	int	direction;
	int increment;
	long size;
	double user_xmin, user_xmax, user_ymin, user_ymax;

	PARAM_LIST_PTR p_list;
	ROW_SIZES_PTR row_sizes;
	DATA_BIN_PTR dbin;
	FORMAT_PTR output_format;
	VARIABLE_PTR var_ptr;
	EQUATION_INFO_PTR eqn_info;

}DATA_VIEW, *DATA_VIEW_PTR;

/* the code handling "cdmenu.lst" is replaced by that using SOURCE_MENU_LIST */
#define SOURCE_MENU_LIST "cdm_serv.lst"

/* Define VIEW types */
#define V_TYPE_INITIALIZED	0
#define V_TYPE_RECORD		1
#define V_TYPE_VECTOR		2
#define V_TYPE_ARRAY		3

/* Define VIEW Attributes */
#define VIEW_TYPE               101
#define VIEW_UPPER_LEFT         102
#define VIEW_LOWER_RIGHT        103
#define VIEW_NUM_POINTERS       104
#define VIEW_ROW_SIZES          105
#define VIEW_DIRECTION          106
#define VIEW_INCREMENT          107
#define VIEW_DATA_SIZE          108
#define VIEW_STARTS             109
#define VIEW_USER_XMIN          110
#define VIEW_USER_XMAX          111
#define VIEW_USER_YMIN          112
#define VIEW_USER_YMAX          113
#define VIEW_DATA_BIN           114
#define VIEW_GET_DATA		115
#define NEXT_RECORD		116
#define VIEW_LENGTH		117
#define VIEW_FIRST_POINTER 	118
#define VIEW_PARAMETER_LIST	119
#define VIEW_INFO		120
#define VIEW_PLIST_TO_VIEW_AREA 121
#define VIEW_QUERY_RESTRICTION  122
#define VIEW_GET_NEXT_REC 123

/* data view events */
#define	VIEW_PLIST_TO_ROWSIZE		200
#define MAKE_ROWSIZE_AREA_INTERSECT	201
#define CLIP_VIEW_INTERSECT			202
#define PLIST_CACHE_TO_ROWSIZE		203
#define MAXMIN_CACHE_TO_ROWSIZE		204
#define CLIP_VIEW_ON_QUERY			205
#define VIEW_QUERY_TO_ROWSIZE		206

/* define view direction */
#define DIR_LEFT_RIGHT            1
#define DIR_RIGHT_LEFT            2
#define DIR_LEFT_RIGHT_DOWN_UP    3
#define DIR_RIGHT_LEFT_DOWN_UP    4

/* data view prototypes */

DATA_VIEW_PTR	dv_make(char *, SCRATCH_BUFFER);
int				dv_set(DATA_VIEW_PTR, ...);
int      		dv_events(DATA_VIEW_PTR, ...);
int				dv_show(DATA_VIEW_PTR, ...);
PARAM_LIST_PTR	make_param_list(char *, FORMAT *);
int	show_param_list(PARAM_LIST *, char *);
void dv_free(DATA_VIEW_PTR);
long view_size(DATA_VIEW *, INDEX *, int , FORMAT *);
long retrieve_row(ROW_SIZES *, int, char *, int, unsigned int);
extern short get_limit_from_plist(PARAM_LIST *, char *, void *, void *);
int db_hdf_tag_to_rowsize(DATA_BIN_PTR dbin, unsigned short hdf_tag, unsigned short *hdf_ref, unsigned short exact_hdf_ref, ROW_SIZES_PTR row_size_ptr);

/* Can remove wrapper below when gvpoints.c gets checked in */
#endif
