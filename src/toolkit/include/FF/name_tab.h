
/*
 *	FILENAME: name_tab.h
 *
 *	CONTAINS: This is the include file for much of the name table
 *		  code: symbols, structures, and prototypes. 
 *			More is found in include file
 *		  databin.h, which includes this file.
 *
 *
 */
 
#ifndef NAME_TAB_INCLUDED
#define NAME_TAB_INCLUDED
#define NT_COPY_EVERYTHING 0
#define NT_COPY_EQ_ONLY 1
#define NT_COPY_CONSTANT_ONLY 2
#define NT_ANYWHERE 0
#define NT_HEADER_ONLY	1

/* NAME_TABLE values' buffer is in multiples of 256 */
#define NAME_TABLE_QUANTA 256

#define INPUT_NAME_TABLE_EXISTS(dbin) ( (dbin) ? !fd_get_format_data(dbin->table_list, FFF_INPUT | FFF_TABLE, NULL) : 0)

/* define the conversion between user's variable name and value to geovu name and value */

typedef struct t_table /* define value translation table */
{
	void           *check_address;
	FF_TYPES_t    gtype;  /* corresponding geovu data type */
	void           *gvalue;	/* corresponding geovu data value */
	FF_TYPES_t    utype;  /* define the user's data type */
	void           *uvalue; /* define the user's data value */
	struct t_table *next;
} TRANSLATOR, *TRANSLATOR_PTR;

/* Name Table Prototypes */
#ifdef PROTO				
	char *nt_find_user_name(NAME_TABLE *, char *);
	int nt_add_constant(NAME_TABLE_HANDLE, char *, FF_TYPES_t, void *);
	NAME_TABLE_PTR nt_create(char *, char *);
	BOOLEAN nt_get_geovu_value(NAME_TABLE_PTR, char *, void *, FF_TYPES_t, void *, FF_TYPES_t *);
	BOOLEAN nt_get_user_value(NAME_TABLE_PTR, char *, void *, FF_TYPES_t, void *, FF_TYPES_t *);
	void nt_free_name_table(NAME_TABLE_PTR table);
	int nt_merge(NAME_TABLE_PTR update_table, NAME_TABLE_HANDLE table);
	int nt_delete_constant(NAME_TABLE_PTR table, char *name);
	int nt_show(NAME_TABLE_PTR table, char *ch);

int fd_get_format_data(FORMAT_DATA_LIST_PTR fd_list, FF_TYPES_t fd_type,
    FORMAT_DATA_HANDLE format_data);
int fd_put_format_data(FORMAT_DATA_LIST_HANDLE fd_list_hdl, FF_TYPES_t fd_type,
    FORMAT_DATA_PTR format_data);
int fd_remove_format_data(FORMAT_DATA_LIST_HANDLE fd_list_hdl, FF_TYPES_t fd_type,
    char *f_name);
void fd_free_format_data(FORMAT_DATA_PTR fd);
void fd_free_format_data_list(FORMAT_DATA_LIST_PTR);

extern NAME_TABLE_PTR nt_get_name_table(NAME_TABLE_LIST_PTR, FF_TYPES_t);
int nt_put_name_table(NAME_TABLE_LIST_HANDLE, FF_TYPES_t, NAME_TABLE_PTR);
extern NAME_TABLE_PTR nt_remove_name_table(NAME_TABLE_LIST_HANDLE, NAME_TABLE_PTR);

#else
	char *nt_find_user_name();
	int nt_add_constant();
	NAME_TABLE_PTR nt_create();
	void nt_free_name_table();
	BOOLEAN nt_askvalue();
	BOOLEAN nt_get_geovu_value(); 
	BOOLEAN nt_get_user_value();
	int nt_merge();
	int nt_delete_constant();
	int nt_show();

int fd_get_format_data();
int fd_put_format_data();
int fd_remove_format_data();
void fd_free_format_data();
void fd_free_format_data_list();

extern NAME_TABLE_PTR nt_get_name_table();
int nt_put_name_table();
extern NAME_TABLE_PTR nt_remove_name_table();

#endif /* PROTO */
#endif /* (not defined) NAME_TAB_INCLUDED */

