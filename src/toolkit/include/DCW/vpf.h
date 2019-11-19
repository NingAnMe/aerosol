/* VPF.H */

#ifndef __VPF_H__

#define __VPF_H__ 1

/* Define some standard VPF-related data types and structures */


/* Currently supported VPF versions */
typedef enum { VPF_0_7, VPF_0_8, VPF_0_9, VPF_1_0 } vpf_version_type;


/* VPF feature types */
typedef enum { LINE=1, AREA, ANNO, POINT, COMPLEX=6 } vpf_feature_type;

/* VPF primitive types */
typedef enum { EDGE=1, FACE, TEXT, ENTITY_NODE, CONNECTED_NODE }
   vpf_primitive_type;

typedef struct {
   unsigned char edge;
   unsigned char face;
   unsigned char text;
   unsigned char entity_node;
   unsigned char connected_node;
} primitive_class_type;


typedef enum { UNKNOWN_SECURITY, UNCLASSIFIED, RESTRICTED, CONFIDENTIAL,
               SECRET, TOP_SECRET } security_type;

static char *security_name[] = { "UNKNOWN", "UNCLASSIFIED", "RESTRICTED",
                                 "CONFIDENTIAL", "SECRET", "TOP_SECRET" };


/* Units of measure */
typedef enum { UNKNOWN_UNITS, METERS, FEET, INCHES,
	       KILOMETERS, OTHER_UNITS, DEC_DEGREES,
               CENTIMETERS, DECIMETERS,FATHOMS_AND_FEET,
               FATHOMS, ARC_MINUTES, STATUTE_MILES,
               MILS, MILLIMETERS, NAUTICAL_MILES,
               ARC_SECONDS, MICROMETERS, YARDS } vpf_units_type;

static char *unit_names[] = {"Unknown Units", "Meters", "Feet",
                             "Inches", "Kilometers", "Other Units",
                             "Degrees of Arc", "Centimeters",
                             "Decimeters", "Fathoms and Feet",
                             "Fathoms", "Minutes of Arc",
                             "Statute Miles", "Mils", "Millimeters",
                             "Nautical Miles", "Seconds of Arc",
                             "Micrometers", "Yards"};


/* Map coordinate projection definitions */

typedef enum { 
   DD,   /* Decimal Degrees */
   AC,   /* Albers Equal Area */
   AK,   /* Azimuthal Equal Area */
   AL,   /* Azimuthal Equal Distance */
   GN,   /* Gnomonic */
   LE,   /* Lambert Conformal Conic */
   LJ,   /* Lambert (Cylindrical) Equal Area */
   MC,   /* Mercator */
   OC,   /* Oblique Mercator */
   OD,   /* Orthographic */
   PG,   /* Polar Stereographic */
   TC,   /* Transverse Mercator */
 
         /*     Std. 600006A,   kklc August, 93 */
         
   RB,   /* Hotine Oblique Mercator */
   PH,   /* Polyconic */
   OS,   /* Oblique Stereographic (code to-be-determined) */
   RC,   /* Relative coordinates */
 
         /* not defined in Std. */
 
   UT,   /* UTM */
   PC    /* Plate-Carree */
} vpf_projection_code;

/* static int GCTP_code[] = {0,3,11,12,13,4,21,5,20,14,6,9,1,17}; */
static int GCTP_code[] = {0,3,11,12,13,4,21,5,20,14,6,9, 20,7,10,0, 1,17};

static char *projection_names[] = {"Decimal Degrees", 
                                   "Albers Equal Area",
                                   "Azimuthal Equal Area",
                                   "Azimuthal Equal Distance",
                                   "Gnomonic",
                                   "Lambert Conformal Conic",
                                   "Lambert Equal Area",
                                   "Mercator",
                                   "Oblique Mercator",
                                   "Orthographic",
                                   "Polar Stereographic",
                                   "Transverse Mercator",
 
                                        /*      Std. 600006A */
 
                                   "Hotine Oblique Mercator",
                                   "Polyconic",
                                   "Oblique Stereographic",
                                   "Relative coordinates",
 
                                        /* not defined in Std. */
 
                                   "UTM",
                                   "Plate-Carree"};

static float projection_limits[] = {360.0,  /* Decimal degrees */
                                    360.0,  /* Albers equal area */
                                     89.0,  /* Azimuthal equal area */
                                    360.0,  /* Azimuthal equal distance */
                                     89.0,  /* Gnomonic */
                                    360.0,  /* Lambert conformal conic */
                                     89.0,  /* Lambert equal area */
                                    360.0,  /* Mercator */
                                     60.0,  /* Oblique mercator */
                                     89.0,  /* Orthographic */
                                    360.0,  /* Polar stereographic */
                                     89.0,  /* Transverse mercator */
                                    
                                     60.0,   /* Hotine Oblique Mercator */
                                    360.0,   /* Polyconic */
                                    360.0,   /* Oblique Stereographic */
                                    360.0,   /* Relative coordinates */
 
                                     89.0,  /* UTM */
                                    360.0}; /* Plate-Carree */

typedef struct {
   vpf_projection_code code;
   char name[21];
   double parm1, parm2, parm3, parm4;
   vpf_units_type units;
   double false_easting, false_northing;
   int (*forward_proj)();
   int (*inverse_proj)();
} vpf_projection_type;


static const vpf_projection_type NOPROJ =
                                 {DD,"Decimal Degrees     ",
                                 0.0, 0.0, 0.0, 0.0,
                                 DEC_DEGREES, 0.0, 0.0,
                                 NULL,NULL};

typedef unsigned char boolean;

#endif

