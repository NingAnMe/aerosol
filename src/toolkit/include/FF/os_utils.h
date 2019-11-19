/* Operating System Dependent Includes: */
/*
 * HISTORY:
 *	r fozzard	6/21/95		-rf01
 *		add strdup definition for Think C
 *	r fozzard	7/31/95		-rf02
 *		add protos for os_mac_load_env  
 *	r fozzard	8/21/95		-rf03
 *		fix protos for os_mac_load_env 
*/

#ifndef OS_UTILS
#define OS_UTILS

#include <math.h>

#ifdef CCLSC
#include <errno.h>
#endif

#ifdef CCMSC
#include <stdlib.h>
#endif

#ifdef SUNCC
#include <unistd.h>
#endif
  
/*  system dependent definitions */

#ifdef CCMSC
/* MAO This is never used; use EOL_1STCHAR from freeform.h instead
#define EOL_MARKER 	13
*/
#endif

#ifdef SUNCC 
#define O_BINARY 	0
/* MAO This is never used; use EOL_1STCHAR from freeform.h instead
#define EOL_MARKER 	10
*/#endif

#ifndef max /* maximum macro */
#define max(a,b) (a > b ? a : b)
#endif

#ifndef min /* minimum macro */
#define min(a,b) (a < b ? a : b)
#endif

#ifndef ROUND
/* usage: int_var = (int_var_type)ROUND(expr); -- expr should be floating point type */
#define ROUND(a) ((a) < 0 ? ceil((a) - 0.5) : floor((a) + 0.5))
#else
#include "ROUND macro is already defined -- contact support"
#endif

#ifndef TRUNC
#define TRUNC(a) ((a) < 0 ? ceil(a) : floor(a))
#else
#include "TRUNC macro is already defined -- contact support"
#endif

#define ok_strlen(a) ((a)?strlen(a):0)

#define OS_ESCAPER '\\'
#define OS_INVERSE_ESCAPE 0
#define OS_NORMAL_ESCAPE 1

/* casts below are to ensure logical, rather than arithmetic, bit shifts */
#define FLIP_4_BYTES(a)	(	(((a) & 0x000000FFu) << 24) | \
							(((a) & 0x0000FF00u) <<  8) | \
							(((unsigned long)(a) & 0x00FF0000u) >>  8) | \
							(((unsigned long)(a) & 0xFF000000u) >> 24) )

#define FLIP_2_BYTES(a)	( (((unsigned short)(a) & 0xFF00u) >> 8) | \
                          (((a) & 0x00FFu) << 8) )

/*  system dependent MACROS */

#ifdef CCMSC

#define os_getch() 	(getch())

#elif SUNCC

#define os_getch()	(getc(stdin))

#endif

/* Think C on the Mac needs the following, since there is no ANSI "strdup" -rf01 */
#ifdef CCLSC  /* -rf01 */
#define strdup(S1) strcpy((char *)malloc(strlen(S1)+1),S1)  /* -rf01 */
#include <string.h>
Handle PathNameFromFSSpec(FSSpecPtr myFSSPtr);
#endif /* CCLSC  -rf01 */


/* prototypes for functions in os_utils.c */

#ifndef _BOOLEAN_DEFINED
#define _BOOLEAN_DEFINED
typedef short BOOLEAN; /* Boolean type */
#ifdef TRUE
#undef TRUE
#endif
#define TRUE 1
#ifdef FALSE
#undef FALSE
#endif
#define FALSE 0
#endif

#ifdef PROTO
extern char *os_strlwr(char *string);
extern char *os_strupr(char *);
extern long os_filelength(char *filename);
extern BOOLEAN os_file_exist(char *filename);
extern int os_strcmpi(const char* s1, const char* s2);
extern int os_strncmpi(const char* s1, const char* s2, size_t n);
#ifdef CCLSC
extern void *os_mac_load_env(char * buffer); /* -rf02,-rf03 */
#endif
extern char *os_get_env(char *variable_name);
extern char *os_itoa(int value, char *string, int radix);

int os_path_cmp_paths(char *s, char *t);
short os_path_is_native(char *path);
char *os_path_make_native(char *native_path, char *path);
void os_path_find_parts(char *path, char **pathname, char **filename, char **fileext);
char *os_path_return_ext(char *pfname);
char *os_path_return_name(char *pfname);
void os_path_get_parts(char *path, char *pathname, char *filename, char *fileext);
char *os_path_put_parts(char *fullpath, char *dirpath, char *filename, char *fileext);
void os_str_replace_char(char *string, char oldc, char newc);
short os_path_prepend_special(char *in_name, char *home_path, char *out_name);
char *os_str_trim_whitespace(char *dest, char *source);
void os_str_replace_unescaped_char1_with_char2(char char1, char char2, char *str);
void os_str_replace_escaped_char1_with_char2(char char1, char char2, char *str);
void os_str_replace_xxxcaped_char1_with_char2(int mode, char char1, char char2, char *str);
#else

extern char *os_strlwr(/* char *string */);
extern char *os_strupr( );
extern int os_strcmpi(/* const char* s1, const char* s2 */);
extern int os_strncmpi(/*const char* s1, const char* s2, size_t n*/);
extern long os_filelength(/* char *filename */);
extern BOOLEAN os_file_exist(/* char *filename */);
extern void *os_mac_load_env(/* char * buffer */); /* -rf02,-rf03 */
extern char *os_get_env(/* char *variable_name */);
extern char *os_itoa(/* int value, char *string, int radix */);

int os_path_cmp_paths( /* char *s, char *t */ );
short os_path_is_native( /* char *path */ );
char *os_path_make_native( /* char *native_path, char *path */ );
void os_path_find_parts( /* char *path, char **pathname, char **filename, char **fileext */ );
char *os_path_return_ext( /* char *pfname */ );
char *os_path_return_name( /* char *prname */ );
void os_path_get_parts( /* char *path, char *pathname, char *filename, char *fileext */ );
char *os_path_put_parts( /* char *fullpath, char *dirpath, char *filename, char *fileext */ );
void os_str_replace_char( /* char *string, char oldc, char newc */ );
short os_path_prepend_special( /* char *in_name, char *home_path, char *out_name */ );
char *os_str_trim_whitespace( /* char *dest, char *source */ );
void os_str_replace_unescaped_char1_with_char2( /* char char1, char char2, char *str */ );
void os_str_replace_escaped_char1_with_char2( /* char char1, char char2, char *str */ );
void os_str_replace_xxxcaped_char1_with_char2( /* int mode, char char1, char char2, char *str */ );
#endif /* PROTO */

#endif /* OS_UTILS */

