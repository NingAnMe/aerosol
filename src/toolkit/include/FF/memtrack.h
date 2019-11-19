/* MEM.H -- ** Copyright (c) 1990, Cornerstone Systems Group, Inc. 
 * Dr. Dobbs Journal, August 1990, p. 180
 *
 * MODIFIED BY:  Mark A. Ohrenschall, NGDC, (303) 497-6124, mao@ngdc.noaa.gov
*/
/*
 * HISTORY:
 *	Rich Fozzard	6/16/95		-rf01
 *		add code for #ifdef CCLSC for _fmemMemcpy, etc.
 *	Rich Fozzard	7/28/95		-rf02
 *		Include string.h and stdlib.h to fix memory stomps in CW
*/


#ifndef MEM_CHECKING
#define MEM_CHECKING
#define NO_TAG "generic tag"


#include <string.h>	/* needed by CW to avoid memory stomps -rf02 */
#include <stdlib.h>	/* needed by CW to avoid memory stomps -rf02 */


#ifdef FIND_UNWRAPPED_MEM
#define MEMTRACK

#define calloc	I_should_be_memCalloc
#define free	I_should_be_memFree
#define malloc	I_should_be_memMalloc
#define realloc	I_should_be_memRealloc
#define strdup	I_should_be_memStrdup
#define strcpy	I_should_be_memStrcpy
#define strncpy	I_should_be_memStrncpy
#define strcat	I_should_be_memStrcat
#define strncat	I_should_be_memStrncat
#define strcmp	I_should_be_memStrcmp
#define strncmp	I_should_be_memStrncmp
#define strchr	I_should_be_memStrchr
#define strrchr	I_should_be_memStrrchr
#define strstr	I_should_be_memStrstr
#define memcpy	I_should_be_memMemcpy
#define memmove	I_should_be_memMemmove
#define memset	I_should_be_memMemset

#define _fstrchr	I_should_be__fmemStrchr
#define _fmemcpy	I_should_be__fmemMemcpy
#define _fmemmove	I_should_be__fmemMemmove

#define read	I_should_be_memRead

#endif  /* FIND_UNWRAPPED_MEM */

#ifdef MEMTRACK

#ifndef ROUTINE_NAME
#define ROUTINE_NAME "unfilled () name"
#endif

#define memCalloc(NOBJ, SIZE, TAG)	MEMCalloc(NOBJ, SIZE, TAG, ROUTINE_NAME, __FILE__, __LINE__)
#define memFree(P, TAG)	MEMFree(P, TAG, ROUTINE_NAME, __FILE__, __LINE__)
#define memMalloc(SIZE, TAG)	MEMMalloc(SIZE, TAG, ROUTINE_NAME, __FILE__, __LINE__)
#define memRealloc(P, SIZE, TAG)	MEMRealloc(P, SIZE, TAG, ROUTINE_NAME, __FILE__, __LINE__)
#define memStrdup(STRING, TAG)	MEMStrdup(STRING, TAG, ROUTINE_NAME, __FILE__, __LINE__)
#define memStrcpy(S, CT, TAG)	MEMStrcpy(S, CT, TAG, ROUTINE_NAME, __FILE__, __LINE__)
#define memStrncpy(S, CT, N, TAG)	MEMStrncpy(S, CT, N, TAG, ROUTINE_NAME, __FILE__, __LINE__)
#define memStrcat(S, CT, TAG)	MEMStrcat(S, CT, TAG, ROUTINE_NAME, __FILE__, __LINE__)
#define memStrncat(S, CT, N, TAG)	MEMStrncat(S, CT, N, TAG, ROUTINE_NAME, __FILE__, __LINE__)
#define memStrcmp(CS, CT, TAG)	MEMStrcmp(CS, CT, TAG, ROUTINE_NAME, __FILE__, __LINE__)
#define memStrncmp(CS, CT, N, TAG)	MEMStrncmp(CS, CT, N, TAG, ROUTINE_NAME, __FILE__, __LINE__)
#define memStrchr(CS, C, TAG)	MEMStrchr(CS, C, TAG, ROUTINE_NAME, __FILE__, __LINE__)
#define memStrrchr(CS, C, TAG)	MEMStrrchr(CS, C, TAG, ROUTINE_NAME, __FILE__, __LINE__)
#define memStrstr(CS, CT, TAG)	MEMStrstr(CS, CT, TAG, ROUTINE_NAME, __FILE__, __LINE__)
#define memMemcpy(S, CT, N, TAG)	MEMMemcpy(S, CT, N, TAG, ROUTINE_NAME, __FILE__, __LINE__)
#define memMemmove(S, CT, N, TAG)	MEMMemmove(S, CT, N, TAG, ROUTINE_NAME, __FILE__, __LINE__)
#define memMemset(DEST, C, COUNT, TAG) MEMMemset(DEST, C, COUNT, TAG, ROUTINE_NAME, __FILE__, __LINE__)

#define _fmemStrchr(CS, C, TAG) _fMEMStrchr(CS, C, TAG, ROUTINE_NAME, __FILE__, __LINE__)
#define _fmemMemcpy(S, CT, N, TAG) _fMEMMemcpy(S, CT, N, TAG, ROUTINE_NAME, __FILE__, __LINE__)
#define _fmemMemmove(S, CT, N, TAG) _fMEMMemmove(S, CT, N, TAG, ROUTINE_NAME, __FILE__, __LINE__)

#define memRead(HANDLE, BUFFER, COUNT, TAG)	MEMRead(HANDLE, BUFFER, COUNT, TAG, ROUTINE_NAME, __FILE__, __LINE__)

void *MEMCalloc(size_t nobj, size_t size, char *tag, char *routine_name, char *cfile_name, int line_number);
void  MEMFree(void *p, char *tag, char *routine_name, char *cfile_name, int line_number);
void *MEMMalloc(size_t size, char *tag, char *routine_name, char *cfile_name, int line_number);
void *MEMRealloc(void *p, size_t size, char *tag, char *routine_name, char *cfile_name, int line_number);
char *MEMStrdup(const char *string, char *tag, char *routine_name, char *cfile_name, int line_number);
char *MEMStrcpy(char *s, const char *ct, char *tag, char *routine_name, char *cfile_name, int line_number);
char *MEMStrncpy(char *s, const char *ct, size_t n, char *tag, char *routine_name, char *cfile_name, int line_number);
char *MEMStrcat(char *s, const char *ct, char *tag, char *routine_name, char *cfile_name, int line_number);
char *MEMStrncat(char *s, const char *ct, size_t n, char *tag, char *routine_name, char *cfile_name, int line_number);
int   MEMStrcmp(const char *cs, const char *ct, char *tag, char *routine_name, char *cfile_name, int line_number);
int   MEMStrncmp(const char *cs, const char *ct, size_t n, char *tag, char *routine_name, char *cfile_name, int line_number);
char *MEMStrchr(const char *cs, int c, char *tag, char *routine_name, char *cfile_name, int line_number);
char *MEMStrrchr(const char *cs, int c, char *tag, char *routine_name, char *cfile_name, int line_number);
char *MEMStrstr(const char *cs, const char *ct, char *tag, char *routine_name, char *cfile_name, int line_number);
void *MEMMemset(void *dest, int c, unsigned int count, char *tag, char *routine_name, char *cfile_name, int line_number);

#ifdef CCMSC
void *MEMMemcpy(void *s, const void *ct, size_t n, char *tag, char *routine_name, char *cfile_name, int line_number);
void *MEMMemmove(void *s, const void *ct, size_t n, char *tag, char *routine_name, char *cfile_name, int line_number);

char __far * __far _fMEMStrchr(const char __far *s, int c, char *tag, char *routine_name, char *cfile_name, int line_number);
void __far * __far _fMEMMemcpy(void __far *s, const void __far *ct, size_t n, char *tag, char *routine_name, char *cfile_name, int line_number);
void __far * __far _fMEMMemmove(void __far *s, const void __far *ct, size_t n, char *tag, char *routine_name, char *cfile_name, int line_number);
#endif

#ifdef SUNCC
char *MEMMemcpy(char *s, const char *ct, size_t n, char *tag, char *routine_name, char *cfile_name, int line_number);
char *MEMMemmove(char *s, const char *ct, size_t n, char *tag, char *routine_name, char *cfile_name, int line_number);

char *_fMEMStrchr(char *s, int c, char *tag, char *routine_name, char *cfile_name, int line_number);
void *_fMEMMemcpy(char *s, const char *ct, size_t n, char *tag, char *routine_name, char *cfile_name, int line_number);
void *_fMEMMemmove(char *s, const char *ct, size_t n, char *tag, char *routine_name, char *cfile_name, int line_number);
#endif

#ifdef CCLSC	/* these are needed for mac -rf01 */
void *MEMMemcpy(char *s, const char *ct, size_t n, char *tag, char *routine_name, char *cfile_name, int line_number);
void *MEMMemmove(char *s, const char *ct, size_t n, char *tag, char *routine_name, char *cfile_name, int line_number);

char *_fMEMStrchr(char *s, int c, char *tag, char *routine_name, char *cfile_name, int line_number);
void *_fMEMMemcpy(char *s, const char *ct, size_t n, char *tag, char *routine_name, char *cfile_name, int line_number);
void *_fMEMMemmove(char *s, const char *ct, size_t n, char *tag, char *routine_name, char *cfile_name, int line_number);
#endif	/* -rf01 */

int MEMRead(int handle, void *buffer, unsigned int count, char *tag, char *routine_name, char *cfile_name, int line_number);

int read(int handle, void *buffer, unsigned int count);

#else /* _DEBUG or MEMTRACK */

#define memCalloc(NOBJ, SIZE, TAG)      calloc(NOBJ, SIZE)

#ifdef MEMSAFREE
#define memFree(P, TAG) {free(P);P = NULL;}
#else
#define memFree(P, TAG) free(P)
#endif /* MEMSAFREE */

#define memMalloc(SIZE, TAG)    malloc(SIZE)
#define memRealloc(P, SIZE, TAG)        realloc(P, SIZE)
#define memStrdup(STRING, TAG)  strdup(STRING)
#define memStrcpy(S, CT, TAG)   strcpy(S, CT)
#define memStrncpy(S, CT, N, TAG)       strncpy(S, CT, N)
#define memStrcat(S, CT, TAG)   strcat(S, CT)
#define memStrncat(S, CT, N, TAG)       strncat(S, CT, N)
#define memStrcmp(CS, CT, TAG)  strcmp(CS, CT)
#define memStrncmp(CS, CT, N, TAG) strncmp(CS, CT, N)
#define memStrchr(CS, C, TAG)   strchr(CS, C)
#define memStrrchr(CS, C, TAG)  strrchr(CS, C)
#define memStrstr(CS, CT, TAG)  strstr(CS, CT)
#define memMemcpy(S, CT, N, TAG)        memcpy(S, CT, N)
#define memMemmove(S, CT, N, TAG)       memmove(S, CT, N)
#define memMemset(DEST,C,COUNT,TAG)	memset(DEST,C,COUNT)

#ifdef CCMSC

#define _fmemStrchr(S, C, TAG) _fstrchr(S, C)
#define _fmemMemcpy(S, CT, N, TAG)	_fmemcpy(S, CT, N)
#define _fmemMemmove(S, CT, N, TAG)	_fmemmove(S, CT, N)

#endif

#ifdef SUNCC

#define _fmemStrchr(S, C, TAG) strchr(S, C)
#define _fmemMemcpy(S, CT, N, TAG)	memcpy(S, CT, N)
#define _fmemMemmove(S, CT, N, TAG)	memmove(S, CT, N)

#endif

#ifdef CCLSC	/* these are needed for mac -rf01 */

#define _fmemStrchr(S, C, TAG) strchr(S, C)
#define _fmemMemcpy(S, CT, N, TAG)	memcpy(S, CT, N)
#define _fmemMemmove(S, CT, N, TAG)	memmove(S, CT, N)

#endif /* -rf01 */

#define memRead(HANDLE, BUFFER, COUNT, TAG)	read(HANDLE, BUFFER, COUNT)

#endif /* MEMTRACK */
#ifndef LINUX
char *strdup(const char *);
#endif
#endif /* MEM_CHECKING */

