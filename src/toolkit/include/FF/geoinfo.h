/*
 * FILENAME: geoinfo.h
 *
 * CONTAINS: structs and control id's for dialog for setting parameters
 *
 */
#ifndef GEOINFO_H_INC
#define GEOINFO_H_INC

#define GEO_INFO_IMAGE  1
#define GEO_INFO_VECTOR 2
#define GEO_INFO_POINT  3

#define SearchParameterLimitsButton	3 
#define LinearRadio		6 
#define FrequencyRadio		7 
#define UserDefinedRadio	8 
#define HistoEqualizedRadio	9 
#define Field11			11 
#define ImageRadio		14 
#define TableRadio		15 
#define WriteFileRadio		17 
#define SaveFileRadio		16
#define PalBox			19 
#define XYPlotType		21 
#define VectorType		22	/* no longer radios, just symbolic const's */ 

/* define GEO_INFO events */
#define GEO_INFO_TYPE		100
#define GEO_INFO_TITLE  	101
#define GEO_INFO_SCALE  	102
#define GEO_INFO_GRAPHICS 	103
#define GEO_INFO_DISPLAY 	104
#define GEO_INFO_GEOREF  	105
#define INFO_DBAND			106
#define INFO_PALETTE		107
#define INFO_XAXIS			108
#define INFO_XLIMIT         	109
#define INFO_YAXIS          	110
#define INFO_YLIMIT         	111
#define INFO_XAREA          	112
#define INFO_XALIMIT        	113
#define INFO_YAREA			114
#define INFO_YALIMIT        	115
#define INFO_VOID_POINTER   	116
#define INFO_SET_PLOT       	117

#define GEO_INFO_TO_VIEW    	130

typedef struct  {
		BOOLEAN		result;
		BOOLEAN        display;
		short		LinearChoice;
		char			Field11Text [80];
   		short		ImageChoice;
		BOOLEAN		GeographicReferenceChecked;
			} ViewParamRec, *ViewParamPtr;

typedef struct {
		int palette;
		int dband;
	}VIEW_IMAGE;

typedef struct {
		VARIABLE *xaxis;
		float xmin;
		float xmax;
		VARIABLE *yaxis;
		float ymin;
		float ymax;
		VARIABLE *xarea;
		float axmin;
		float axmax;
		VARIABLE *yarea;
		float aymin;
		float aymax;
		void *list;
		DLL_NODE_PTR multi_y_list;
	} XYPLOT;

typedef struct {
		ViewParamRec *ViewParam;
		int type;
		void *info;
	} GEO_INFO, *GEO_INFO_PTR;

#ifdef PROTO
	int	geo_info_event(GEO_INFO_PTR, ...);
	GEO_INFO_PTR make_geo_info(int);
	int set_geo_info(GEO_INFO_PTR, ...);
	void free_geo_info(GEO_INFO_PTR);
	BOOLEAN plotset(XYPLOT *, DATA_BIN *, BOOLEAN);
	XYPLOT *makeplot(void);
#else
	int	geo_info_event();
	GEO_INFO_PTR make_geo_info( );
	int set_geo_info();
	void free_geo_info( );
	BOOLEAN plotset();
	XYPLOT *makeplot();
#endif

#endif
