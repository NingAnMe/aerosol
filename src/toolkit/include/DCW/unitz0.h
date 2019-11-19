#ifndef __UNITZ0__
#define __UNITZ0__ 1

#ifndef __VPF_H__
#include "vpf.h"
#endif

	

/* 
**	generally it is not a very good idea to have these many inline's
**	  especially when I don't think they will ever get expanded because
**	  I am passing around their pointers
**	but, ...
**
**		I ran out of idea and don't want to do FORTRAN;
**		the following should replace much of unitz0() in gctp.f
**
*/
	
inline float cm2m( float cm ) { return cm * .01 ; }
inline float m2cm( float m ) { return m * 100. ; }

inline float dd2r( float dd ) { return dd * .017453292 ; }
inline float r2dd( float r ) { return r * 57.2958 ; }

inline float dm2m( float dm ) { return dm * .1 ; }
inline float m2dm( float m ) { return m * 10. ; }

inline float ft2m( float ft ) { return ft * .3048 ; }
inline float m2ft( float m ) { return m * 3.280839895 ; }

inline float in2m( float in ) { return in * .0254 ; }
inline float m2in( float m ) { return m * 39.37007874 ; }

inline float km2m( float km ) { return km * 1000. ; }
inline float m2km( float m ) { return m * .001 ; }

inline float min2r( float min ) { return min * .0002909 ; }
inline float r2min( float r ) { return r * 3437.748 ; }

inline float mi2m( float mi ) { return mi * 1609.344 ; }
inline float m2mi( float m ) { return m * .0006213711922 ; }

inline float mil2m( float mil ) { return mil * .0000254 ; }
inline float m2mil( float m ) { return m * 39370.07874 ; }

inline float mm2m( float mm ) { return mm * .001 ; }
inline float m2mm( float m ) { return m * 1000. ; }

inline float nm2m( float nm ) { return nm * 1852 ; }
inline float m2nm( float m ) { return m * .000539957 ; }

inline float sec2r( float sec ) { return sec * .000004848 ; }
inline float r2sec( float r ) { return r * 206264.88 ; }

inline float mc2m( float mc ) { return mc * .0001 ; }
inline float m2mc( float m ) { return m * 10000. ; }

inline float y2m( float y ) { return y * .9144 ; }
inline float m2y( float m ) { return m * 1.093613298 ; }


inline float NULLCON( float val ) { return val; }

typedef struct {
	vpf_units_type 	u_type ;
        float (* normalize)( float );	/* put it into units gctp can handle */
        float (* denormalize)( float );	/* get it back into something we want */
} UNITSCON ;
 
UNITSCON  unitsCon[] = {
	{ UNKNOWN_UNITS, NULLCON, NULLCON },
	{ METERS, NULLCON, NULLCON } ,
	{ FEET, ft2m, m2ft } ,
	{ INCHES, in2m, m2in } ,
	{ KILOMETERS, km2m, m2km } ,
	{ OTHER_UNITS, NULLCON, NULLCON },
	{ DEC_DEGREES, dd2r, r2dd } ,
	{ CENTIMETERS, cm2m, m2cm } ,
	{ DECIMETERS, dm2m, m2dm } ,
	{ FATHOMS_AND_FEET, NULLCON, NULLCON },
	{ FATHOMS, NULLCON, NULLCON },
	{ ARC_MINUTES, min2r, r2min },
	{ STATUTE_MILES, mi2m, m2mi } ,
	{ MILS, mil2m, m2mil },
	{ MILLIMETERS, mm2m, m2mm } ,
	{ NAUTICAL_MILES, nm2m, m2nm } ,
	{ ARC_SECONDS, sec2r, r2sec },
	{ MICROMETERS, mc2m, m2mc } ,
	{ YARDS, y2m, m2y }
} ;

#endif
