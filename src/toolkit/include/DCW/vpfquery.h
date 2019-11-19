#ifndef __VPFQUERY_H__
#define __VPFQUERY_H__ 1

#include "set.h"
#include "vpftable.h"

set_type query_table( char *query, vpf_table_type table );

int query_table_row( char *query, row_type row,
		     vpf_table_type table );

#endif

