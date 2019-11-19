

#ifndef __VVMISC_H__
#define __VVMISC_H__ 1

#include <sys/types.h>
#include "vpftable.h"

#ifdef __cplusplus
extern "C" {
#endif

void *checkmalloc( int size );

void *checkrealloc( void *pointer, unsigned int size );

char *vpfcatpath( char *dest, const char *src );

int fileaccess( const char *filename, int amode );

char *rand_name( int length, char *name );

char **findall( char *path, char *pattern, int *member );

int displayerror( char *text[], int nlines );

void displayinfo( char *text[], int nlines );

void display_message( char *str );

void var_display_message( char *str, ... );

int stricmp(char *cs, char *ct);

int is_vpf_null_float( float num );

int is_primitive( char *tablename );

void vpf_nullify_table( vpf_table_type *table );

/*remove defs for strncasecmp, strcasecmp, strdup to be fine for LINUX/C++ */

#ifndef LINUX    
int strncasecmp(const char *, const char *, size_t);

int strcasecmp(const char *, const char *);

char *strdup(const char *); 
#endif


int ftruncate(int, off_t);

#ifdef __cplusplus
}
#endif

#endif





