
#ifndef __VVUTIL_H__
#define __VVUTIL_H__ 1

#include "vpfview.h"

typedef struct {
   char db[255];
   char lib[9];
} db_lib_type;

int create_view( char *workspace,
                 char *view_name,
                 db_lib_type *db_lib_list,
                 int num_db_lib,
                 char *sympath );

int delete_view( char *viewpath );

int copy_view( char *source, 
               char *dest );

int rename_view( char *inpath,
                 char *newname );

#endif

