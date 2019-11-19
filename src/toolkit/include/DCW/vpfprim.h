
#ifndef __VPFPRIM_H__
#define __VPFPRIM_H__ 1

#include "coorgeom.h"
#include "set.h"
#include "vpftable.h"
#include "vpf.h"

/* VPF edge record internal structure */
typedef struct {
   int id;
   int start_node;
   int end_node;
   int right_face;
   int left_face;
   int right_edge;
   int left_edge;
   char dir;
   int npts;
   double_coordinate_type *coords;
   /* For coordinate arrays too large for memory (DOS support) */
   FILE *fp;
   int startpos, pos, current_coordinate;
   char coord_type;
   int (*projfunc)();
} edge_rec_type;


/* "static" part of the edge record (non-variable) */
typedef struct {
   int id;
   int start_node;
   int end_node;
   int right_face;
   int left_face;
   int right_edge;
   int left_edge;
   char dir;
   int npts;
} edge_static_type;
 
 
/* VPF face record structure */
typedef struct {
   int id;
   int ring;
} face_rec_type;
 
 
/* VPF ring record structure */
typedef struct {
   int id;
   int face;
   int edge;
} ring_rec_type;
 
 
/* VPF entity node record internal structure */
typedef struct {
   int id;
   int face;
   int first_edge;
   double x;
   double y;
} node_rec_type;
 
 
/* VPF text record internal structure */
typedef struct {
   long  int id;
   char  *text;
   double x;
   double y;
} text_rec_type;
 


edge_rec_type create_edge_rec( row_type row, vpf_table_type edge_table,
                               int (*projfunc)(double *, double *) );
 
edge_rec_type read_edge( int id,
                         vpf_table_type edge_table,
                         int (*projfunc)(double *, double *) );

edge_rec_type read_next_edge( vpf_table_type edge_table,
                              int (*projfunc)(double *, double *) );

double_coordinate_type first_edge_coordinate( edge_rec_type *edge_rec );

double_coordinate_type next_edge_coordinate( edge_rec_type *edge_rec );

double_coordinate_type get_edge_coordinate( int n,
                                            edge_rec_type *edge_rec );
 
face_rec_type read_face( int id,
                         vpf_table_type face_table );
 
face_rec_type read_next_face( vpf_table_type face_table );
 
ring_rec_type read_ring( int id,
                         vpf_table_type ring_table );

ring_rec_type read_next_ring( vpf_table_type ring_table );
 
node_rec_type read_node( int id,
                         vpf_table_type node_table,
                         int (*projfunc)(double *, double *) );
 
node_rec_type read_next_node( vpf_table_type node_table, 
			      int (*projfunc)(double *, double *) );
 
text_rec_type read_text( int id,
                         vpf_table_type text_table,
                         int (*projfunc)(double *, double *) );
 
text_rec_type read_next_text( vpf_table_type text_table,
                              int (*projfunc)(double *, double *) );
 
set_type bounding_select( char *brpath, extent_type extent, 
                          vpf_projection_type invproj );

vpf_table_type open_bounding_rect( char *covpath, char *tiledir,
                                   int pclass );

extent_type read_bounding_rect( int row, vpf_table_type brtable,
                                int (*projfunc)(double *, double *) );

extent_type read_next_bounding_rect( vpf_table_type brtable,
                                     int (*projfunc)(double *, double *) );

#endif

