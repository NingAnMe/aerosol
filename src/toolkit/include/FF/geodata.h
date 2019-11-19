/*
 * FILENAME:    geodata.h
 *      
 * CONTAINS:    Globals, data structures, constants, and prototypes
 *
 * AUTHOR:  Liping Di, NGDC, (303) 497 - 6284, lpd@ngdc1.colorado.edu
 *
 */
#ifndef GEODATA_H
#define GEODATA_H

/* Make sure the new menu stuff gets included if appropriate */
#ifndef NEW_MENU
#ifndef SDB_MENU
#define SDB_MENU
#endif
#endif

#include <menuindx.h>

/* Define the maximum menu index buffer size */
#define MAX_MENU_BUFFER_SIZE (size_t)64000

/* Define the menu_stack struct */
typedef struct menu_stack_struct{
	MENU_OPEN_PTR mopen; /* A pointer to our MENU_OPEN struct */
	char *section_name; /* The section name */
	int depth;          /* The depth in the navigation */
	MENU_SECTION_PTR msection; /* A pointer to the section, if it exists */
} MENU_STACK, *MENU_STACK_PTR;

/* Define the menu_sect_placeholder struct */
typedef struct menu_sect_phstruct{
	int highlighted_item; /* The number of the currently selected item in the list box */
	char button_clicked;  /* Which button was cliked (for return information) */
	char multiple_select; /* If TRUE, allow multiple items to be selected (SLIDE SHOW) */
	int error; /* 0 if no errors occured */
	MENU_SELECTION_PTR selected_items; /* For slide show only- a DLL of selected items */
	MENU_SELECTION_PTR help_selection; /* For use with help only */
} MENU_SECT_PLACEHOLDER, *MENU_SECT_PLACEHOLDER_PTR;

/* Function declarations */
int show_text_as_help(MENU_INDEX_PTR mindex, char *title, char *section, char *text);
int get_palmen_sec_titles(MENU_INDEX_PTR mindex, char **titles, short *count);
MENU_SELECTION_PTR get_menu_selection(char *menufilename, char multiple_select);
int create_menu_of_menus(void);
int menu_open_op(char action, char *menufilename, void *apptr);
int menu_open_dump(void);

#define MENU_LISTBOX_WIDTH 80
#define MENU_PATHBOX_WIDTH 60
/* MENU_SCRATCH_SIZE has to be bigger than MAX_MENU_FILENAME_LENGTH and MENU_LISTBOX_WIDTH */
#define MENU_SCRATCH_SIZE 266
#define MENU_PATHBOXTITLE_WIDTH 60
#define MENU_HELP_TITLE_WIDTH 200
#define MAX_MENU_FILENAME_LENGTH 256

/* Actions for get_menu_selection */
#define MENU_SELECTION_SET_MOM 42

/* Actions for menu_open_op */
#define MENU_OPEN_GET 0
#define MENU_OPEN_INIT 1
#define MENU_OPEN_SET_PATH 2
#define MENU_OPEN_CHECK 3
#define MENU_OPEN_FIND_OWAP 4

/* MAIN_MENU_OF_MENUS must have an extension (a ".") */
#define MAIN_MENU_OF_MENUS "mainmenu.dsl"

/********* END of new menu stuff **********/




#define INPUT_BUFFER_SIZE      40960
#define OUTPUT_BUFFER_SIZE     40960

#define MAX_NUM_POINTS        10240
      
#define MAX_NUMBER_SOURCES       64 /* define the maximum number of sources */
#define MAX_NUMBER_VIEWS         64 /* define the maximum number of views */

#define DLG_FORM        103 /* define headview dialog id */ 
#define DLG_RES         104 /* define resource id */
#define DLG_DTYPE       124 /* define data types dialog id */
#define DLG_IMAGE               105 /* define resource id */
#define DLG_MOVE_LIST_BOX       106 /* define resource id */
#define DLG_RES_MULTIPLE    121 /* multiple selection */
#define DLG_GEO_RESP    126  /* get response dialog */

#define GEO_RESP_1  1   /* response 1*/
#define GEO_RESP_2      2
#define GEO_RESP_3      3
#define GEO_MESSAGE     4

#define  DLG_PGUP       4   /* page up button id in the show header dialog */
#define  DLG_PGDWN      5   /* page down button id in the show header dialog */

#define VIEW_BITMAP_IMAGE       1
#define VIEW_CONTOUR            2
#define VIEW_VECTOR             3
#define VIEW_XYPLOT             4
#define VIEW_TABLE              5

#define NUM_MAP_PROJECTION        23 
#define MAP_PROJECTION_NONE       0
#define MAP_PROJECTION_UTM        1
#define MAP_PROJECTION_LATLON     21

#define IMAGE_BUFFER_MAX_COLUMN   2048
#define IMAGE_BUFFER_MAX_ROW      2048

typedef struct {
		float x;
		float y;
	}XY;

typedef struct {
	char *title;
	RCT grctp;
	int nitem;
	int select;
	char **item;
	} MOVE_LIST_BOX;
		 
typedef struct {                    /* define a DLG res cb data structure */
	char *title;               
	char *message;
	int  select;
	BOOLEAN cancel_enabled;
	SLIST x;
	}DLG_RES_DATA;

typedef struct {
	char *file;         /* file name */
	FILE *stream;           /* open stream */
	int lpp;            /* line per page */
	int npage;          /* number of pages */
	int dpage;          /* current page number */
	long far *position;     /* page position */
}PAGE, *PAGE_PTR;                   


typedef struct {            /* define a DLG cb_form data structure */ 
	DATA_BIN *dbin;
	int npage;          /* number of pages to show the header */
	int nitem;          /* number of item in the header */
	char *bbuffer;          /* buffer to keep modified header */
	char *abuffer;          /* buffer to keep header value as ASCII */
	}CB_FORM_DATA;
	

#ifdef DEFINE_GEODATA
  DATA_BIN *data_bin[MAX_NUMBER_SOURCES];       /* define 64 DATA_BIN pointers */
  int NSource=0;                    /* define current number of DATA_BIN */
  int NView=0;                      /* define current number of views */

  DATA_VIEW *view_list[MAX_NUMBER_VIEWS];       /* define view list */
  char *input_buffer = NULL;
  char *output_buffer = NULL;        /* I/O buffer */
  FILE_SPEC fs_in;                      /* XVT file structure */
  char *proj[]={"none",
		  "UTM",
		  "State plane",
		  "Albers conical equal area",
		  "Lambert conformal conic",
		  "Mercator",
		  "Polar stereographic",
		  "Polyconic",
		  "Equidistant conic",
		  "Transverse Mercator",
		  "Stereographic",
		  "Lambert azimuthal equal area",
		  "Azimuthal equidistant",
		  "Gnomonic",
		  "Orthographic",
		  "General vertical near_side perspective",
		  "Sinusoidal",
		  "Equirectangular",
		  "Miller cylindrical",
		  "Van der Grinten",
		  "Oblique Mercator",
		  "Lat/Lon",
		  "User-Defined Projection                                          "};
  char *format_text[8]={"BSQ", "BIL", "BIP","BIP2", "PCX", "BMP", "DIB", "GIF"};
  char *direction_text[4]={"NORMAL", "INVERSE COLUMN",
			"INVERSE ROW", "INVERSE ROW AND COLUMN"};

#else
  extern int NSource, NView;
  extern DATA_BIN *data_bin[];
  extern DATA_VIEW *view_list[];

  extern char *input_buffer, *output_buffer;
  extern FILE_SPEC fs_in;
  extern char *proj[], *format_text[], *direction_text[];
#endif

#ifdef PROTO
	long    cb_image(WINDOW, EVENT *);
	long    cb_image_param(WINDOW, EVENT *);
	DATA_BIN *db_create(char *, char *, MENU_SELECTION_PTR);
	
	long    cb_page(WINDOW, EVENT *);
	long    cb_move_list_box(WINDOW, EVENT *);
	long    cb_res(WINDOW, EVENT *);
	long    cb_res_multiple(WINDOW, EVENT *);
	long    cb_plot(WINDOW, EVENT *);
	long    cbViewParam (WINDOW, EVENT *);
	long    cb_variable(WINDOW, EVENT *);
	long    cb_form(WINDOW, EVENT *);

	float   *color_ranges(short, float, float, unsigned char *, short, short);  

	void    ImageDisplayDlg(DATA_VIEW *, short, WINDOW);

	BOOLEAN find_item(char *, char *, char, int, char *);
	MOVE_LIST_BOX *format_to_mlist(FORMAT *, BOOLEAN);
	short geo_get_response(char *, char *, char *, char *, char *, short);
	DATA_BIN *initialize_dbin(DATA_BIN *);

	VARIABLE_PTR get_default_var(FORMAT_PTR);
/* MAO:d0 removed get_dir_separator -- function no longer used
	char    get_dir_separator(DLL_NODE_PTR);
*/
	char    *get_header_type(DATA_BIN *);
	BOOLEAN GetViewParam (DATA_VIEW *, int);

	BOOLEAN set_image_limit(DATA_BIN *, short);
	BOOLEAN set_param(DATA_BIN *, short);
	char    *stdform(char *);
	void    ShowHeader(DATA_BIN *); 
	BOOLEAN show_file(char *);
#else
	DATA_BIN *db_create();
	long    cb_image( );
	long    cb_image_param();
	long    cb_page();
	long    cb_move_list_box( );
	long    cb_res();
	long    cb_res_multiple( );
	long    cb_plot();
	long    cbViewParam();
	long    cb_variable();
	long    cb_form();

	float   *color_ranges();   
	void    ImageDisplayDlg();

	BOOLEAN find_item();
	MOVE_LIST_BOX *format_to_mlist();
	short geo_get_response();
	DATA_BIN *initialize_dbin();

	VARIABLE_PTR get_default_var();
	char    get_dir_separator();
	char    *get_header_type();
	BOOLEAN GetViewParam ();
	BOOLEAN set_image_limit();
	BOOLEAN set_param();
	char    *stdform();
	void    ShowHeader(); 
	BOOLEAN show_file();
#endif

#endif
