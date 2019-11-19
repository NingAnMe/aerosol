
#ifndef __VPFTIDX_H__
#define __VPFTIDX_H__ 1

#include <stdio.h>
#include "set.h"
 
typedef struct {                        /* Total of 60 bytes */
  int      nbytes ,                /* 60 + directory length */
                nbins ,                 /* Directory size */
                table_nrows ;           /* Num rows in original table */
  char          index_type ,            /* T = thematic, G = gazetteer */
                column_type ;           /* T, I, R, S, F, K */
  int      type_count ;            /* usually 1, but more for T */
  char          id_data_type ,          /* I if > 32767, else S */
		vpf_table_name[13] ,
		vpf_column_name[25] ,   /* 16 bytes + 9 for TILE_ID */
		sort ,                  /* 'S' if directory sorted */
		padding[3] ;            /* To make it a nice 60 bytes */
} ThematicIndexHeader ;
  
#define         THEMATIC_INDEX_HEADER_SIZE    60
 
#define         DIRECTORY_SIZE                (sizeof(int)*2)
 
#define         MAX_ID          32767   /* Threshold between S and I ids */ 

typedef union {
    char        cval ,
		*strval ;
    int    ival ;
    short int   sval ;
    float       fval ;
    double      dval ;
} ThematicIndexValue ;

typedef struct {                        /* length = sizeof (datatype) + */
  ThematicIndexValue value;             /*          8 * nbins           */
  int      binid,
                start_offset ,
                num_items ;             /* For each value, count the ids */  
} ThematicIndexDirectory ;
 
typedef struct {
   ThematicIndexHeader h;
   ThematicIndexDirectory *d, *gid;
   FILE *fp;
} ThematicIndex;
 


int create_thematic_index( char indextype, char *tablename, 
                                char *idxname, char *columnname, 
                                char *idx_set );

set_type read_thematic_index( char *idxname, char *value );

ThematicIndex open_thematic_index( char *idxname, int storage );

set_type search_thematic_index( ThematicIndex *idx, char *value );

void close_thematic_index( ThematicIndex *tdx );

int create_gazetteer_index( char *tablename, char *idxname,
				 char *columnname, char *idx_set );

set_type search_gazetteer_index( ThematicIndex *idx, char *query_str );

set_type read_gazetteer_index( char *idxname, char *query_str );

#endif


