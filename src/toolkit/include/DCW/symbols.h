
#ifndef __SYMBOLS_H__
#define __SYMBOLS_H__ 1


#include "color.h"

typedef struct {
   int id;
   int xhot, yhot;
   int npts;
   int *marker;
} marker_type;


typedef struct {
  int height;
  int distance;
  int gap;
} tick_struct;

typedef struct {
  int dashlistlen;
  int *dashlist;
  int gap;
} dash_struct;

typedef struct {
  char *filename;
  int just;
  int distance;
  int gap;
} marker_line_struct;

typedef struct {
  int height;
  int distance;
  int radius;
} dot_line_struct;

typedef union {
    tick_struct tick;
    dash_struct dash;
    marker_line_struct marker;
    dot_line_struct dot;
} line_attribute_type;

typedef struct {
  int width;
  int style;
  line_attribute_type attr;
} linestyle_type;

typedef struct {
  int fillstyle;
  char *stipple;	   	 /* If fillstyle not FillSolid */
  int outline_width;
  int outline_style;
  line_attribute_type outline_attr;
} area_type;


typedef char *text_type;


/*** Symbol set ***/

typedef struct {
  int nlinesym;
  linestyle_type *linesym;
  int nmarkersym;
  marker_type *markersym;
  int nareasym;
  area_type *areasym;
  int ntextsym;
  text_type *textsym;
  int ncolors;
  color_type *color;
} symbol_set_type;


 
void clear_symbol_set( symbol_set_type *sym );
 
symbol_set_type init_symbol_set( char *sympath, int *status );
 
#endif


