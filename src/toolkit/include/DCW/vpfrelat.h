
#ifndef __VPFRELAT_H__
#define __VPFRELAT_H__ 1

#include "linklist.h"
#include "vpftable.h"
#include "vpftidx.h"

typedef struct {
   char table1[40];
   char key1[40];
   char table2[40];
   char key2[40];
} vpf_relate_struct;


typedef struct {
   int nchain;
   vpf_table_type *table;
   linked_list_type relate_list;
} feature_class_relate_type, fcrel_type;



int num_relate_paths( char *start_table, char *end_table,
                      char *fcname, vpf_table_type fcs );

linked_list_type fcs_relate_list( char *fcname, char *start_table,
				  char *end_table, vpf_table_type fcs,
                                  int npath );

int related_row( void *keyval, vpf_table_type table, char *keyfield,
		      int tile_id );

linked_list_type related_rows( void *keyval, vpf_table_type table,
			       char *keyfield, int tile_id );

fcrel_type select_feature_class_relate( char *covpath, char *fcname,
					char *start_table, char *end_table,
                                        int npath );

int fc_row_number( row_type row, fcrel_type fcrel, int tile );

linked_list_type fc_row_numbers( row_type row, fcrel_type fcrel,
				 int tile );

void deselect_feature_class_relate( fcrel_type *fcrel );

#endif

