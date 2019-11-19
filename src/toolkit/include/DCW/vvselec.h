
#ifndef __VVSELEC_H__
#define __VVSELEC_H__ 1

#include "vpfview.h"
#include "vpfrelat.h"

set_type read_selected_features( view_type *view, 
                                 int themenum );

void save_selected_features( view_type *view,
                             int themenum,
                             set_type selset );

set_type get_selected_features( view_type *view,
                                int themenum );

set_type read_selected_primitives( view_type *view,
                                   int themenum,
                                   int tile,
                                   int pclass );

void save_selected_primitives( view_type *view,
                               int themenum,
                               int tile,
                               int ntiles,
                               set_type selset,
                               int pclass );

set_type primitives_within_extent( extent_type mapextent,
                                   char *covpath,
                                   char *tiledir,
                                   int primclass,
                                   int numprims,
                                   vpf_projection_type proj );

set_type get_selected_tile_primitives( char *covpath,
                                       char *fclass,
                                       char *expression,
                                       fcrel_type fcrel,
                                       int primclass,
                                       int tile,
                                       int *status );

set_type get_fit_tile_primitives( char *covpath,
                                  int primclass,
                                  char *expression,
                                  vpf_table_type feature_table,
                                  int tile,
                                  int fca_id,
                                  int numprims,
                                  int *status );

int draw_selected_features( view_type *view,
                            int themenum,
                            map_environment_type *mapenv );

#endif

