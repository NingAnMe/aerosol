#ifndef DATA_PARAM_INCLUDED
#define DATA_PARAM_INCLUDED

/* define image format */
#define IMAGE_FORMAT_BSQ          1
#define IMAGE_FORMAT_BIL          2
#define IMAGE_FORMAT_BIP          3
#define IMAGE_FORMAT_BIP2         4
#define IMAGE_FORMAT_PCX          5	/* NOTE: there is a LPD test in the code */
#define IMAGE_FORMAT_BMP          6	/* using "<" comparison on these symbols. */
#define IMAGE_FORMAT_DIB	  7
#define IMAGE_FORMAT_GIF	  8

#define ORIGIN_UPPER_LEFT		1
#define ORIGIN_LOWER_LEFT		2
#define ORIGIN_LOWER_RIGHT		3
#define ORIGIN_UPPER_RIGHT		4
#define ORIGIN_UPPER_LEFT_Y		5
#define ORIGIN_LOWER_LEFT_Y		6
#define ORIGIN_LOWER_RIGHT_Y		7
#define ORIGIN_UPPER_RIGHT_Y		8


/* Define Parameters for various data types */

typedef struct {
	int data_type;		/* byte, int, float .. */
	int nrows;		/* number of rows */
	int ncols;		/* number of cloumns */
	int nbands;		/* number of bands: 1 */
	int nclasses;		/* number of classes in the image */
	int bytes_per_pixel;    /* byte per pixel */
	int head_bytes;		/* number of header bytes: 0 */
	short format;		/* image format: BSQ */
	short origin;		/* grid origin */
	float min_value;
	float max_value;
	float upy, lowy, leftx, rightx;
	float pixelx,pixely;		  
}IMAGE;

typedef struct{
	/* these coordinates may be the maximum and minmum values in 
	the x, y directions, they are not necessarily map coordinates */ 
	float	upy;	
	float	lowy;
	float	leftx;
	float	rightx;  
}VECTOR;

typedef struct{
	/* these coordinates may be the maximum and minmum values in 
	the x, y directions, they are not necessarily map coordinates */ 
	char y_name[MAX_NAME_LENGTH + 1];
	char x_name[MAX_NAME_LENGTH + 1];
	float	upy;	
	float	lowy;
	float	leftx;
	float	rightx;  
}POINT_DATA;

#endif
