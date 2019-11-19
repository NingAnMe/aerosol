/* VPFVIEW.H */

#ifndef __VPFVIEW_H__

#define __VPFVIEW_H__ 1


/*#include <Xlib.h> */

#include "set.h"
#include "vpftable.h"
#include "linklist.h"
#include "symbols.h"
#include "vpf.h"
#include "coorgeom.h"



/* VPF library internal structure */
typedef struct {
   char name[9];            /* Name of the library */
   boolean viewable;        /* Is this library accessible to the view? */
   char *path;              /* DOS path name to the library */
   int ntiles;         /* Number of tiles in the library */
   set_type tile_set;       /* Set of 'active' tiles in the library */
   vpf_projection_code projection; /* Projection of stored coord data */
   vpf_units_type units;    /* Units of the stored coordinate data */
} library_type;

/* VPF database internal structure */
typedef struct {
   char name[9];      /* Name of the VPF database */
   char *path;        /* UNIX path name to the database */
   library_type *library; /* Array of library structures for the database */
   int  nlibraries;   /* Number of libraries in the database */
} database_type;

/* Each theme has a symbol structure associated with it.  Themes on */
/* simple feature classes just have relevant symbol information for */
/* one of the four primitive types, but complex feature themes may  */
/* have any or all of the primitive type symbols.                   */
typedef struct {
   int point_color;
   int point;
   int line_color;
   int line;
   int area_color;
   int area;
   int text_color;
   int text;
} theme_symbol_type;

/* A theme is a single entry for a view of the database.  It can be */
/* thought of as a stored query with a description and symbology.   */
/* Each theme is associated with a feature class.                   */
typedef struct {
   char *description;              /* Description of the theme */
   char *database;                 /* Source Database path */
   char *library;                  /* Source Library name */
   char *coverage;                 /* Source coverage name */
   char *fc;                       /* Feature class name for the theme */
   char *ftable;                   /* Feature table path for the fc */
   primitive_class_type primclass; /* Primitive class(es) of theme */
   char *expression;               /* Logical selection expression */
   theme_symbol_type symbol;       /* Drawing symbol */
} theme_type;


/* View structure.  Each view is associated with a particular database */
/* and a particular library within that datbase.                       */
typedef struct {
   char name[9];             /* View name */
   database_type *database;  /* Array of Databases in the view */
   int  ndb;                 /* Number of databases in the view */
   char *path;		     /* Directory path to the view */
   int  nthemes;             /* Number of themes in the view */
   theme_type *theme;        /* Array of themes */
   set_type selected;        /* Set of themes selected for display */
   set_type displayed;       /* Set of displayed themes */
   linked_list_type sellist; /* List of selected themes (ordered) */
   extent_type extent;       /* MBR of all library extents */
   double tileheight;        /* Min of all library tile heights */
   char sympath[255];        /* Symbol set path */
   symbol_set_type sym;      /* Symbol set for the view */
} view_type;


/* Map environment information */
typedef struct {
   extent_type mapextent;           /* Current map extent */
   boolean     mapchanged;          /* Flag - has anything changed? */
   boolean     mapdisplayed;        /* Flag - has the map been displayed? */
   boolean     user_escape;         /* Flag - has the user hit escape? */
   boolean     study_area_selected; /* Flag - study area selected? */
   boolean     latlongrid;          /* Flag - lat-lon grid to be displayed?*/
   boolean     scale_bar;           /* Flag - scale bar to be displayed? */
   vpf_projection_type projection;  /* Current map display projection */
   coord_units_type distance_unit;  /* Units for distance display */
   coord_units_type scale_bar_unit; /* Units for scale bar display */
   vpf_units_type   locator_unit;   /* Units for map locate display */
} map_environment_type;

typedef struct {
  extent_type  	mapextent;	/* Current map extent */
  boolean	mapdisplayed;	/* Flag - has the map been displayed? */
  boolean	latlongrid;	/* Flag - lat-lon grid to be displayed? */
  boolean	tilegrid;	/* Flag - tile boundaries to be displayed? */
  boolean	points;		/* Flag - libref points to be displayed? */
  boolean	text;		/* Flag - libref text to be displayed? */
  vpf_units_type locator_unit;  /* Units for libref map locate display */
} libref_map_environment_type;

/* Window specifier flags */
#define COVERAGE_WINDOW 0
#define LIBREF_WINDOW 1 

/* Functions: */
/* main() */

#endif

