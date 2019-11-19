
#ifndef __VVSPREL_H__
#define __VVSPREL_H__ 1

#include "vpftable.h"
#include "vpfprim.h"
#include "coorgeom.h"

int intersect_polygon_edge( double x, double y, edge_rec_type edge_rec );

int intersect_polygon_loop( double x, double y, int face_id,
		    int start_edge, vpf_table_type edgetable,
                    int (*projfunc)(double *, double *) );

int point_in_face( double x, double y, int face_id, 
                   vpf_table_type facetable,
		   vpf_table_type ringtable, 
                   vpf_table_type edgetable,
                   int (*projfunc)(double *, double *) );

int point_in_face_table( double x, double y, int face_id, 
                         char *facepath, int (*projfunc)(double *, double *) );

double distance_to_edge_rec( double x, double y, edge_rec_type edge_rec,
			     int dec_degrees, int units );

double distance_to_edge( double x, double y, int edge_id,
			 vpf_table_type edgetable,
                         int (*projfunc)(double *, double *), int units );

double distance_to_edge_table( double x, double y, int edge_id,
			       char *edgepath, 
                               int (*projfunc)(double *, double *),
                               int units );

void vpf_edge_clip( char *covpath, extent_type extent, char *outpath );

#endif

