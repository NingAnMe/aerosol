/*
 *
 * NAME:	index.h
 *		
 * PURPOSE: Define index constants and data structures
 *
 * AUTHOR:	T. Habermann, NGDC, (303) 497 - 6472, haber@ngdc1.colorado.edu
 *
 * USAGE:	#include <index.h>
 *
 * COMMENTS:
 *
 * 
 * 
 */


#ifndef FF_INDEX_INCLUDED
#define FF_INDEX_INCLUDED
typedef struct {	 	/* Structure for each Level vers 3 */
	char name[64];		/*	vers 2 did not include num_boxes */
	long min;		/*	vers 1 had only min,max & size and */
	long max;		/*	always had 2 levels x and y */
	long size;
	unsigned long num_boxes;
} LEVEL, *LEVEL_PTR;


typedef struct {			/* Structure for Index Header: */
	unsigned char	version;
	unsigned char	offset_count;	/* Toggle for offset array */
	unsigned char	num_levels;	/* Number of dimensions indexed */	
	unsigned int	record_length;	
	unsigned long	total_boxes; 	/* Number of boxes in each direction and total */
	
	LEVEL_PTR	levels;   		/* Dynamically allocated for variable dimensions */
	long		*offsets;		/* Ptrs to first and last index box */
	long		*last_offset;
} INDEX, *INDEX_PTR;


#define INDEX_IS_OFFSETS 0
#define INDEX_IS_COUNTS  1
#define INDEX_VERSION	 3

/* Function Prototypes */
#ifdef PROTO
	INDEX_PTR db_make_index(char *, char *, int, long );
	void db_free_index(INDEX_PTR);
#else
	INDEX_PTR db_make_index( );
	void db_free_index();
#endif

#endif
