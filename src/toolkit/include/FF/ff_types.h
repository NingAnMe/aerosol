/*
   Currently supported machine types:
   
   PC
   SUN
   IRIS4
   MAC
   IBM6000
   HP9000
   DEC_ALPHA
   IRIX
   LINUX
   
*/

#if defined(FF_TYPES_H__) && defined(FF_CHECK_SIZES)

if (SIZE_FLOAT32 != sizeof(float32))
	fprintf(stderr, "Type size mismatch for float32: expected %d, is %d\n",
	       (int)SIZE_FLOAT32, (int)sizeof(float32));

if (SIZE_FLOAT64 != sizeof(float64))
	fprintf(stderr, "Type size mismatch for float64: expected %d, is %d\n",
	       (int)SIZE_FLOAT64, (int)sizeof(float64));

if (SIZE_INT8    != sizeof(int8))
	fprintf(stderr, "Type size mismatch for int8: expected %d, is %d\n",
	       (int)SIZE_INT8, (int)sizeof(int8));

if (SIZE_UINT8   != sizeof(uint8))
	fprintf(stderr, "Type size mismatch for uint8: expected %d, is %d\n",
	       (int)SIZE_UINT8, (int)sizeof(uint8));

if (SIZE_INT16   != sizeof(int16))
	fprintf(stderr, "Type size mismatch for int16: expected %d, is %d\n",
	       (int)SIZE_INT16, (int)sizeof(int16));

if (SIZE_UINT16  != sizeof(uint16))
	fprintf(stderr, "Type size mismatch for uint16: expected %d, is %d\n",
	       (int)SIZE_UINT16, (int)sizeof(uint16));

if (SIZE_INT32   != sizeof(int32))
	fprintf(stderr, "Type size mismatch for int32: expected %d, is %d\n",
	       (int)SIZE_UINT32, (int)sizeof(int32));

if (SIZE_UINT32  != sizeof(uint32))
	fprintf(stderr, "Type size mismatch for uint32: expected %d, is %d\n",
	       (int)SIZE_UINT32, (int)sizeof(uint32));

#ifdef LONGS_ARE_64

if (SIZE_INT64   != sizeof(int64))
	fprintf(stderr, "Type size mismatch for int64: expected %d, is %d\n",
	       (int)SIZE_UINT64, (int)sizeof(int64));

if (SIZE_UINT64  != sizeof(uint64))
	fprintf(stderr, "Type size mismatch for uint64: expected %d, is %d\n",
	       (int)SIZE_UINT64, (int)sizeof(uint64));

#endif /* LONGS_ARE_64 */

#endif /* defined(FF_TYPES_H__) && defined(FF_CHECK_SIZES) */

#ifndef FF_TYPES_H__
#define FF_TYPES_H__

#ifdef GOT_MACHINE
#undef GOT_MACHINE
#endif

#ifdef PC

#ifdef GOT_MACHINE
#define TOO_MANY_MACHINES
#else
#define GOT_MACHINE
#endif

#ifndef LONGS_ARE_32
#define LONGS_ARE_32
#endif

#endif /* PC */

#ifdef SUN

#ifdef GOT_MACHINE
#define TOO_MANY_MACHINES
#else
#define GOT_MACHINE
#endif

#ifndef LONGS_ARE_32
#define LONGS_ARE_32
#endif

#endif /* SUN */

#ifdef IRIS4

#ifdef GOT_MACHINE
#define TOO_MANY_MACHINES
#else
#define GOT_MACHINE
#endif

#ifndef LONGS_ARE_32
#define LONGS_ARE_32
#endif

#endif /* IRIS4 */

#ifdef MAC

#ifdef GOT_MACHINE
#define TOO_MANY_MACHINES
#else
#define GOT_MACHINE
#endif

#ifndef LONGS_ARE_32
#define LONGS_ARE_32
#endif

#endif /* MAC */

#ifdef IBM6000

#ifdef GOT_MACHINE
#define TOO_MANY_MACHINES
#else
#define GOT_MACHINE
#endif

#ifndef LONGS_ARE_32
#define LONGS_ARE_32
#endif

#endif /* IBM6000 */

#ifdef HP9000

#ifdef GOT_MACHINE
#define TOO_MANY_MACHINES
#else
#define GOT_MACHINE
#endif

#ifndef LONGS_ARE_32
#define LONGS_ARE_32
#endif

#endif /* HP9000 */

#ifdef DEC_ALPHA

#ifdef GOT_MACHINE
#define TOO_MANY_MACHINES
#else
#define GOT_MACHINE
#endif

#ifndef LONGS_ARE_64
#define LONGS_ARE_64
#endif

#endif /* DEC_ALPHA */

#ifdef IRIX

#ifndef IRIX
#define IRIX
#endif

#ifdef GOT_MACHINE
#define TOO_MANY_MACHINES
#else
#define GOT_MACHINE
#endif

#ifndef LONGS_ARE_64
#define LONGS_ARE_64
#endif

#endif /* IRIX */

#ifdef LINUX

#ifdef GOT_MACHINE
#define TOO_MANY_MACHINES
#else
#define GOT_MACHINE
#endif

#ifndef LONGS_ARE_32
#define LONGS_ARE_32
#endif

#endif /* LINUX */

#ifndef GOT_MACHINE
#include "Machine type has not been defined."
#include "Please define one in your makefile or project."
#endif

#ifdef TOO_MANY_MACHINES
#include "More than one machine type has been defined"
#ifdef PC
#include "PC is currently defined"
#endif
#ifdef SUN
#include "SUN is currently defined"
#endif
#ifdef IRIS4
#include "IRIS4 is currently defined"
#endif
#ifdef MAX
#include "MAC is currently defined"
#endif
#ifdef IBM6000
#include "IBM6000 is currently defined"
#endif
#ifdef HP9000
#include "HP9000 is currently defined"
#endif
#ifdef DEC_ALPHA
#include "DEC_ALPHA is currently defined"
#endif
#ifdef IRIX
#include "IRIX is currently defined"
#endif
#ifdef LINUX
#include "LINUX is currently defined"
#endif
#include "Please check your makefile or project."
#endif /* TOO_MANY_MACHINES */

#if !defined(LONGS_ARE_32) && !defined(LONGS_ARE_64)
#include "longs have not been defined as either 32 or 64 bits"
#include "This should never happen, contact support"
#endif

#define FFNT_INT8    0
#define FFNT_UINT8   1
#define FFNT_INT16   2
#define FFNT_UINT16  3
#define FFNT_INT32   4
#define FFNT_UINT32  5
#define FFNT_INT64   6
#define FFNT_UINT64  7
#define FFNT_FLOAT32 8
#define FFNT_FLOAT64 9

#ifdef WANT_NCSA_TYPES
#ifndef IBM6000
typedef signed char         int8;
typedef signed short int    int16;
#endif
typedef unsigned char       uint8;
typedef unsigned short int  uint16;
typedef float               float32;
typedef double              float64;

#endif

#define SIZE_INT8       1
#define SIZE_UINT8      1
#define SIZE_INT16      2
#define SIZE_UINT16     2
#define SIZE_INT32      4
#define SIZE_UINT32     4
#define SIZE_INT64      8
#define SIZE_UINT64     8
#define SIZE_INT128    16   /* No current plans for support */
#define SIZE_UINT128   16   /* No current plans for support */

#define SIZE_FLOAT32    4
#define SIZE_FLOAT64    8
#define SIZE_FLOAT128  16    /* No current plans for support */

typedef double big_var_type;
typedef big_var_type align_var_type;

#define FFV_INT8_MIN    SCHAR_MIN
#define FFV_INT8_MAX    SCHAR_MAX
#define FFV_UINT8_MIN           0
#define FFV_UINT8_MAX   UCHAR_MAX
#define FFV_INT16_MIN    SHRT_MIN
#define FFV_INT16_MAX    SHRT_MAX
#define FFV_UINT16_MIN          0
#define FFV_UINT16_MAX  USHRT_MAX
#define FFV_FLOAT32_MIN   FLT_MIN
#define FFV_FLOAT32_MAX   FLT_MAX
#define FFV_FLOAT64_MIN   DBL_MIN
#define FFV_FLOAT64_MAX   DBL_MAX

#define FFV_FLOAT32_EPSILON FLT_EPSILON
#define FFV_FLOAT64_EPSILON DBL_EPSILON

#ifdef LONGS_ARE_32

#ifdef LONGS_ARE_64
#include "longs have been defined as both 32 and 64 bits"
#include "This should never happen, contact support"
#endif

#ifdef DEFINE_DATA

char *fft_cnv_flags[FFNT_FLOAT64 + 1] =
{
"%d",  /* int8 */
"%u",  /* uint8 */
"%hd", /* int16 */
"%hu", /* uint16 */
"%ld", /* int32 */
"%lu", /* uint32 */
"?",   /* int64 */
"?",   /* uint64 */
"%f",  /* float32 */
"%f"   /* float64 */
};


char *fft_cnv_flags_width[FFNT_FLOAT64 + 1] =
{
"%*d",  /* int8 */
"%*u",  /* uint8 */
"%*hd", /* int16 */
"%*hu", /* uint16 */
"%*ld", /* int32 */
"%*lu", /* uint32 */
"?",    /* int64 */
"?",    /* uint64 */
"%*f",  /* float32 */
"%*f"   /* float64 */
};

char *fft_cnv_flags_prec[FFNT_FLOAT64 + 1] =
{
"%.*d",  /* int8 */
"%.*u",  /* uint8 */
"%.*hd", /* int16 */
"%.*hu", /* uint16 */
"%.*ld", /* int32 */
"%.*lu", /* uint32 */
"?",     /* int64 */
"?",     /* uint64 */
"%.*f",  /* float32 */
"%.*f"   /* float64 */
};

char *fft_cnv_flags_width_prec[FFNT_FLOAT64 + 1] =
{
"%*.*d",  /* int8 */
"%*.*u",  /* uint8 */
"%*.*hd", /* int16 */
"%*.*hu", /* uint16 */
"%*.*ld", /* int32 */
"%*.*lu", /* uint32 */
"?",      /* int64 */
"?",      /* uint64 */
"%*.*g",  /* float32 */
"%*.*g"   /* float64 */
};

#else

extern char *fft_cnv_flags[FFNT_FLOAT64 + 1];
extern char *fft_cnv_flags_width[FFNT_FLOAT64 + 1];
extern char *fft_cnv_flags_prec[FFNT_FLOAT64 + 1];
extern char *fft_cnv_flags_width_prec[FFNT_FLOAT64 + 1];

#endif /* (else) DEFINE_DATA */

#ifdef WANT_NCSA_TYPES
#ifndef IBM6000
typedef signed long int     int32;
typedef char                int64;  /* not a real type for the PC */
#endif
typedef unsigned long int   uint32;
typedef char                uint64; /* not a real type for the PC */

#endif

#define FFV_INT32_MIN    LONG_MIN
#define FFV_INT32_MAX    LONG_MAX
#define FFV_UINT32_MIN          0
#define FFV_UINT32_MAX  ULONG_MAX
#define FFV_INT64_MIN           0
#define FFV_INT64_MAX           0
#define FFV_UINT64_MIN          0
#define FFV_UINT64_MAX          0

#endif /* LONGS_ARE_32 */

#ifdef LONGS_ARE_64

#ifdef LONGS_ARE_32
#include "longs have been defined as both 32 and 64 bits"
#include "This should never happen, contact support"
#endif

#ifdef DEFINE_DATA

char *fft_cnv_flags[FFNT_FLOAT64 + 1] =
{
"%d",  /* int8 */
"%u",  /* uint8 */
"%hd", /* int16 */
"%hu", /* uint16 */
"%d",  /* int32 */
"%u",  /* uint32 */
"%ld", /* int64 */
"%lu", /* uint64 */
"%f",  /* float32 */
"%f"   /* float64 */
};

char *fft_cnv_flags_width[FFNT_FLOAT64 + 1] =
{
"%*d",  /* int8 */
"%*u",  /* uint8 */
"%*hd", /* int16 */
"%*hu", /* uint16 */
"%*d",  /* int32 */
"%*u",  /* uint32 */
"%*ld", /* int64 */
"%*lu", /* uint64 */
"%*f",  /* float32 */
"%*f"   /* float64 */
};

char *fft_cnv_flags_prec[FFNT_FLOAT64 + 1] =
{
"%.*d",  /* int8 */
"%.*u",  /* uint8 */
"%.*hd", /* int16 */
"%.*hu", /* uint16 */
"%.*d",  /* int32 */
"%.*u",  /* uint32 */
"%.*ld", /* int64 */
"%.*lu", /* uint64 */
"%.*f",  /* float32 */
"%.*f"   /* float64 */
};

char *fft_cnv_flags_width_prec[FFNT_FLOAT64 + 1] =
{
"%*.*d",  /* int8 */
"%*.*u",  /* uint8 */
"%*.*hd", /* int16 */
"%*.*hu", /* uint16 */
"%*.*d",  /* int32 */
"%*.*u",  /* uint32 */
"%*.*ld", /* int64 */
"%*.*lu", /* uint64 */
"%*.*f",  /* float32 */
"%*.*f"   /* float64 */
};

#else

extern char *fft_cnv_flags[FFNT_FLOAT64 + 1];
extern char *fft_cnv_flags_width[FFNT_FLOAT64 + 1];
extern char *fft_cnv_flags_prec[FFNT_FLOAT64 + 1];
extern char *fft_cnv_flags_width_prec[FFNT_FLOAT64 + 1];

#endif /* (else) DEFINE_DATA */

#ifdef WANT_NCSA_TYPES

typedef signed int          int32;
typedef unsigned int        uint32;
typedef signed long int     int64;
typedef unsigned long int   uint64;

#endif

#define FFV_INT32_MIN     INT_MIN
#define FFV_INT32_MAX     INT_MAX
#define FFV_UINT32_MIN          0
#define FFV_UINT32_MAX   UINT_MAX
#define FFV_INT64_MIN    LONG_MIN
#define FFV_INT64_MAX    LONG_MAX
#define FFV_UINT64_MIN          0
#define FFV_UINT64_MAX  ULONG_MAX

#endif /* LONGS_ARE_64 */

#ifdef PC
#ifndef CCMSC
#define CCMSC
#endif
#endif /* PC */

#ifdef SUN
#ifndef SUNCC
#define SUNCC
#endif
#endif /* SUN */

#ifdef IRIS4
#ifndef SUNCC
#define SUNCC
#endif
#endif /* IRIS4 */

#ifdef MAC
#ifndef CCLSC
#define CCLSC
#endif
#endif /* MAC */

/* Variable Types */
#define FFV_SIZE_1          (FF_TYPES_t)0x0001
#define FFV_SIZE_2          (FF_TYPES_t)0x0002
#define FFV_UNSIGNED        (FF_TYPES_t)0x0004
#define FFV_INTEGER         (FF_TYPES_t)0x0008
#define FFV_REAL            (FF_TYPES_t)0x0010
#define FFV_TEXT            (FF_TYPES_t)0X0020
/* FFV_CHAR is used for strings, but I think FFV_TEXT is a better name.
*/
#define FFV_CHAR     FFV_TEXT

/* above are masked by FFV_DATA_TYPES */
#define FFV_DATA_TYPES      (FF_TYPES_t)0x003F

/* interim definition of FFV_TYPES until FFV_TYPES can be replaced in source
   code -- using FFV_DATA_TYPE() macro */
#define FFV_TYPES FFV_DATA_TYPES 

#define FFV_ARRAY       (FF_TYPES_t)0X0040
#define FFV_BIT_FIELD   (FF_TYPES_t)0x0080
#define FFV_CONVERT     (FF_TYPES_t)0x0100
#define FFV_CONSTANT    (FF_TYPES_t)0x0200
#define FFV_INITIAL     (FF_TYPES_t)0x0400  
#define FFV_HEADER      (FF_TYPES_t)0x0800
#define FFV_EQUATION    (FF_TYPES_t)0x1000
#define FFV_EQUIV       (FF_TYPES_t)0x2000
#define FFV_TRANSLATOR  (FF_TYPES_t)0x4000

/* FFV_NULL MUST BE zero -- NOTHING else can be zero */
#define FFV_NULL        (FF_TYPES_t)0X0000
#define IS_NULL(v) ((v) ? (v->type == FFV_NULL) : FFV_NULL)
			
#define FFV_INT8      (               FFV_INTEGER)
#define FFV_UINT8     (FFV_UNSIGNED | FFV_INTEGER)
#define FFV_INT16     (               FFV_INTEGER | FFV_SIZE_1)
#define FFV_UINT16    (FFV_UNSIGNED | FFV_INTEGER | FFV_SIZE_1)
#define FFV_INT32     (               FFV_INTEGER | FFV_SIZE_2)
#define FFV_UINT32    (FFV_UNSIGNED | FFV_INTEGER | FFV_SIZE_2)
#define FFV_INT64     (               FFV_INTEGER | FFV_SIZE_1 | FFV_SIZE_2)
#define FFV_UINT64    (FFV_UNSIGNED | FFV_INTEGER | FFV_SIZE_1 | FFV_SIZE_2)
#define FFV_FLOAT32   (               FFV_REAL    | FFV_SIZE_2)
#define FFV_FLOAT64   (               FFV_REAL    | FFV_SIZE_1 | FFV_SIZE_2)

#define FFV_UCHAR    FFV_UINT8
#define FFV_SHORT    FFV_INT16
#define FFV_USHORT   FFV_UINT16

#ifdef LONGS_ARE_32
#define FFV_LONG     FFV_INT32
#define FFV_ULONG    FFV_UINT32
#endif

#ifdef LONGS_ARE_64
#define FFV_LONG     FFV_INT64
#define FFV_ULONG    FFV_UINT64
#endif

#define FFV_FLOAT    FFV_FLOAT32
#define FFV_DOUBLE   FFV_FLOAT64

#define IS_TEXT_TYPE(t) (FFV_DATA_TYPE_TYPE(t) == FFV_TEXT)
                     
#define IS_TEXT(v)    (FFV_DATA_TYPE(v) == FFV_TEXT)
#define IS_INT8(v)    (FFV_DATA_TYPE(v) == FFV_INT8)
#define IS_UINT8(v)   (FFV_DATA_TYPE(v) == FFV_UINT8)
#define IS_INT16(v)   (FFV_DATA_TYPE(v) == FFV_INT16)
#define IS_UINT16(v)  (FFV_DATA_TYPE(v) == FFV_UINT16)
#define IS_INT32(v)   (FFV_DATA_TYPE(v) == FFV_INT32)
#define IS_UINT32(v)  (FFV_DATA_TYPE(v) == FFV_UINT32)
#define IS_INT64(v)   (FFV_DATA_TYPE(v) == FFV_INT64)
#define IS_UINT64(v)  (FFV_DATA_TYPE(v) == FFV_UINT64)
#define IS_FLOAT32(v) (FFV_DATA_TYPE(v) == FFV_FLOAT32)
#define IS_FLOAT64(v) (FFV_DATA_TYPE(v) == FFV_FLOAT64)

#define IS_TEXT_TYPE(t)    (FFV_DATA_TYPE_TYPE(t) == FFV_TEXT)
#define IS_INT8_TYPE(t)    (FFV_DATA_TYPE_TYPE(t) == FFV_INT8)
#define IS_UINT8_TYPE(t)   (FFV_DATA_TYPE_TYPE(t) == FFV_UINT8)
#define IS_INT16_TYPE(t)   (FFV_DATA_TYPE_TYPE(t) == FFV_INT16)
#define IS_UINT16_TYPE(t)  (FFV_DATA_TYPE_TYPE(t) == FFV_UINT16)
#define IS_INT32_TYPE(t)   (FFV_DATA_TYPE_TYPE(t) == FFV_INT32)
#define IS_UINT32_TYPE(t)  (FFV_DATA_TYPE_TYPE(t) == FFV_UINT32)
#define IS_INT64_TYPE(t)   (FFV_DATA_TYPE_TYPE(t) == FFV_INT64)
#define IS_UINT64_TYPE(t)  (FFV_DATA_TYPE_TYPE(t) == FFV_UINT64)
#define IS_FLOAT32_TYPE(t) (FFV_DATA_TYPE_TYPE(t) == FFV_FLOAT32)
#define IS_FLOAT64_TYPE(t) (FFV_DATA_TYPE_TYPE(t) == FFV_FLOAT64)

#define IS_INTEGER(v) (FFV_DATA_TYPE(v) & FFV_INTEGER)
#define IS_REAL(v)    (FFV_DATA_TYPE(v) & FFV_REAL)

#define IS_INTEGER_TYPE(t) (FFV_DATA_TYPE_TYPE(t) & FFV_INTEGER)
#define IS_REAL_TYPE(t)    (FFV_DATA_TYPE_TYPE(t) & FFV_REAL)

#define IS_ARRAY(v) ((v) ? ((v->type & FFV_ARRAY) == FFV_ARRAY) : FFV_NULL)
#define IS_BIT_FIELD(v) ((v) ? ((v->type & FFV_BIT_FIELD) == FFV_BIT_FIELD) : FFV_NULL)
#define IS_CONVERT(v) ((v) ? ((v->type & FFV_CONVERT) == FFV_CONVERT) : FFV_NULL)
#define IS_CONSTANT(v) ((v) ? ((v->type & FFV_CONSTANT) == FFV_CONSTANT) : FFV_NULL)
#define IS_INITIAL(v) ((v) ? ((v->type & FFV_INITIAL) == FFV_INITIAL) : FFV_NULL)
#define IS_HEADER(v) ((v) ? ((v->type & FFV_HEADER) == FFV_HEADER) : FFV_NULL)
#define IS_EQUATION(v) ((v) ? (v->type & FFV_EQUATION) == FFV_EQUATION : FFV_NULL)
#define IS_EQUIV(v) ((v) ? (v->type & FFV_EQUIV) == FFV_EQUIV : FFV_NULL)
#define IS_TRANSLATOR(v) ((v) ? (v->type & FFV_TRANSLATOR) == FFV_TRANSLATOR : FFV_NULL)

typedef unsigned int FF_TYPES_t;
typedef FF_TYPES_t FFV_TYPE_type;
typedef FF_TYPES_t FFF_TYPE_type;

/* Define the lookup structures */
typedef struct
{
	char       *string;
	FF_TYPES_t  number;
} FFF_LOOKUP, *FFF_LOOKUP_PTR;

#define NUM_VARIABLE_TYPES 25
#ifdef DEFINE_DATA

FFF_LOOKUP variable_types[NUM_VARIABLE_TYPES] = {
	{"text",        FFV_TEXT},
	{"int8",        FFV_INT8},
	{"uint8",       FFV_UINT8},
	{"int16",       FFV_INT16},
	{"uint16",      FFV_UINT16},
	{"int32",       FFV_INT32},
	{"uint32",      FFV_UINT32},
	{"int64",       FFV_INT64},
	{"uint64",      FFV_UINT64},
	{"float32",     FFV_FLOAT32},
	{"float64",     FFV_FLOAT64},

	{"constant",    FFV_CONSTANT},
	{"header",      FFV_HEADER},
	{"convert",     FFV_CONVERT},
	{"initial",     FFV_INITIAL},
	{"null",        FFV_NULL},

	{"char",        FFV_TEXT},     /* provided for backwards compatibility */
	{"uchar",       FFV_UINT8},    /* provided for backwards compatibility */
	{"short",       FFV_INT16},    /* provided for backwards compatibility */
	{"ushort",      FFV_UINT16},   /* provided for backwards compatibility */
	{"long",        FFV_INT32},    /* provided for backwards compatibility */
	{"ulong",       FFV_UINT32},   /* provided for backwards compatibility */
	{"float",       FFV_FLOAT32},  /* provided for backwards compatibility */
	{"double",      FFV_FLOAT64},  /* provided for backwards compatibility */

	{(char *)NULL,  FFV_NULL},
};
#else
EXTERN FFF_LOOKUP variable_types[NUM_VARIABLE_TYPES];
#endif

/* There are three general types of formats:
 *      1) ASCII formats have data represented as ASCII and followed
 *              by a carrige return.
 *      2) Binary formats have binary data and no returns.
 *      3) DBASE formats have data represented as ASCII and no carrige returns.
 *
 * In addition, there are several classes of formats:
 *      Headers in the same file
 *      Headers in separate files
 *      Data
 *
 * The format type is an integer which allows 16 bits to be set. These bits
 *      are used to keep track of the format characteristics.
*/

/* FreeForm Format bit field masks */
#define FFF_ALL_TYPES           (FF_TYPES_t)0XFFFF
#define FFF_TYPES               (FF_TYPES_t)0x01FF
#define FFF_FORMAT_TYPES        (FF_TYPES_t)0x0007
#define FFF_FILE_TYPES          (FF_TYPES_t)0x0007
#define FD_TYPES                (FF_TYPES_t)0x0C38
/* FD_TYPES == FFF_TABLE | FFF_DATA | FFF_HD | FFF_INPUT | FFF_OUTPUT */

/* FreeForm Format bit field constants */
/* There are implicit assumptions that FFF_NULL is all bits zero */
#define FFF_NULL                (FF_TYPES_t)0x0000
#define FFF_BINARY              (FF_TYPES_t)0x0001
#define FFF_ASCII               (FF_TYPES_t)0x0002
#define FFF_DBASE               (FF_TYPES_t)0x0004
#define FFF_GROUP               (FF_TYPES_t)0x0007
/* above masked by FFF_FORMAT_TYPES but not by FFF_TYPE */

#define FFF_TABLE               (FF_TYPES_t)0x0008		
#define FFF_DATA                (FF_TYPES_t)0x0010
#define FFF_HD                  (FF_TYPES_t)0x0020
#define FFF_FILE                (FF_TYPES_t)0X0040
#define FFF_REC                 (FF_TYPES_t)0x0080
#define FFF_SEPARATE            (FF_TYPES_t)0x0100
/* above masked by FFF_TYPE */

#define FFF_VARIED              (FF_TYPES_t)0x0200
#define FFF_INPUT               (FF_TYPES_t)0x0400
#define FFF_OUTPUT              (FF_TYPES_t)0x0800
#define FFF_IO                  (FF_TYPES_t)0x0C00

#define FFF_SCALE               (FF_TYPES_t)0x1000
#define FFF_RETURN              (FF_TYPES_t)0x2000
#define FFF_DELETE_ME           (FF_TYPES_t)0X8000
/* above masked by FFF_ALL_TYPES */

#define IS_BINARY(f) (FFF_TYPE(f) & FFF_BINARY)
#define IS_ASCII(f) (FFF_TYPE(f) & FFF_ASCII)
#define IS_DBASE(f) (FFF_TYPE(f) & FFF_DBASE)

#define IS_TABLE(f) (FFF_TYPE(f) & FFF_TABLE)
#define IS_DATA(f) (FFF_TYPE(f) & FFF_DATA)
#define IS_HD(f) (FFF_TYPE(f) & FFF_HD)
#define IS_FILE(f) (FFF_TYPE(f) & FFF_FILE)
#define IS_REC(f) (FFF_TYPE(f) & FFF_REC)
#define IS_SEPARATE(f) (FFF_TYPE(f) & FFF_SEPARATE)
#define IS_VARIED(f) (FFF_TYPE(f) & FFF_VARIED)

#define IS_INPUT(f) (FFF_TYPE(f) & FFF_INPUT)
#define IS_OUTPUT(f) (FFF_TYPE(f) & FFF_OUTPUT)


/* Define the composite Format types */
#define FFF_BINARY_DATA         (FFF_BINARY | FFF_DATA)
#define FFF_ASCII_DATA          (FFF_ASCII  | FFF_DATA)
#define FFF_DBASE_DATA          (FFF_DBASE  | FFF_DATA)

#define FFF_BINARY_INPUT_DATA   (FFF_BINARY | FFF_DATA | FFF_INPUT)
#define FFF_ASCII_INPUT_DATA    (FFF_ASCII  | FFF_DATA | FFF_INPUT)
#define FFF_DBASE_INPUT_DATA    (FFF_DBASE  | FFF_DATA | FFF_INPUT)

#define FFF_BINARY_OUTPUT_DATA  (FFF_BINARY | FFF_DATA | FFF_OUTPUT)    
#define FFF_ASCII_OUTPUT_DATA   (FFF_ASCII  | FFF_DATA | FFF_OUTPUT)    
#define FFF_DBASE_OUTPUT_DATA   (FFF_DBASE  | FFF_DATA | FFF_OUTPUT)    

/* Header Format Types */
#define FFF_BINARY_FILE_HD              (FFF_BINARY | FFF_FILE | FFF_HD)
#define FFF_ASCII_FILE_HD               (FFF_ASCII  | FFF_FILE | FFF_HD)
#define FFF_DBASE_FILE_HD               (FFF_DBASE  | FFF_FILE | FFF_HD)

#define FFF_INPUT_BINARY_FILE_HD        (FFF_INPUT | FFF_BINARY | FFF_FILE | FFF_HD)
#define FFF_INPUT_ASCII_FILE_HD (FFF_INPUT | FFF_ASCII  | FFF_FILE | FFF_HD)
#define FFF_INPUT_DBASE_FILE_HD (FFF_INPUT | FFF_DBASE  | FFF_FILE | FFF_HD)

#define FFF_OUTPUT_BINARY_FILE_HD       (FFF_OUTPUT | FFF_BINARY | FFF_FILE | FFF_HD)
#define FFF_OUTPUT_ASCII_FILE_HD        (FFF_OUTPUT | FFF_ASCII  | FFF_FILE | FFF_HD)
#define FFF_OUTPUT_DBASE_FILE_HD        (FFF_OUTPUT | FFF_DBASE  | FFF_FILE | FFF_HD)

#define FFF_BINARY_FILE_HD_SEP  (FFF_BINARY | FFF_SEPARATE | FFF_FILE | FFF_HD)
#define FFF_ASCII_FILE_HD_SEP   (FFF_ASCII  | FFF_SEPARATE | FFF_FILE | FFF_HD)
#define FFF_DBASE_FILE_HD_SEP   (FFF_DBASE  | FFF_SEPARATE | FFF_FILE | FFF_HD)

#define FFF_BINARY_FILE_HD_SEP_VAR (FFF_BINARY | FFF_SEPARATE | FFF_FILE | FFF_HD | FFF_VARIED)
#define FFF_ASCII_FILE_HD_SEP_VAR  (FFF_ASCII  | FFF_SEPARATE | FFF_FILE | FFF_HD | FFF_VARIED)
#define FFF_DBASE_FILE_HD_SEP_VAR  (FFF_DBASE  | FFF_SEPARATE | FFF_FILE | FFF_HD | FFF_VARIED)

#define FFF_INPUT_BINARY_FILE_HD_SEP    (FFF_INPUT | FFF_BINARY | FFF_SEPARATE | FFF_FILE | FFF_HD)
#define FFF_INPUT_ASCII_FILE_HD_SEP             (FFF_INPUT | FFF_ASCII  | FFF_SEPARATE | FFF_FILE | FFF_HD)
#define FFF_INPUT_DBASE_FILE_HD_SEP             (FFF_INPUT | FFF_DBASE  | FFF_SEPARATE | FFF_FILE | FFF_HD)

#define FFF_INPUT_BINARY_FILE_HD_SEP_VAR (FFF_INPUT | FFF_BINARY | FFF_SEPARATE | FFF_FILE | FFF_HD | FFF_VARIED)
#define FFF_INPUT_ASCII_FILE_HD_SEP_VAR  (FFF_INPUT | FFF_ASCII  | FFF_SEPARATE | FFF_FILE | FFF_HD | FFF_VARIED)
#define FFF_INPUT_DBASE_FILE_HD_SEP_VAR  (FFF_INPUT | FFF_DBASE  | FFF_SEPARATE | FFF_FILE | FFF_HD | FFF_VARIED)

#define FFF_OUTPUT_BINARY_FILE_HD_SEP   (FFF_OUTPUT | FFF_BINARY | FFF_SEPARATE | FFF_FILE | FFF_HD)
#define FFF_OUTPUT_ASCII_FILE_HD_SEP    (FFF_OUTPUT | FFF_ASCII  | FFF_SEPARATE | FFF_FILE | FFF_HD)
#define FFF_OUTPUT_DBASE_FILE_HD_SEP    (FFF_OUTPUT | FFF_DBASE  | FFF_SEPARATE | FFF_FILE | FFF_HD)

#define FFF_OUTPUT_BINARY_FILE_HD_SEP_VAR (FFF_OUTPUT | FFF_BINARY | FFF_SEPARATE | FFF_FILE | FFF_HD | FFF_VARIED)
#define FFF_OUTPUT_ASCII_FILE_HD_SEP_VAR  (FFF_OUTPUT | FFF_ASCII  | FFF_SEPARATE | FFF_FILE | FFF_HD | FFF_VARIED)
#define FFF_OUTPUT_DBASE_FILE_HD_SEP_VAR  (FFF_OUTPUT | FFF_DBASE  | FFF_SEPARATE | FFF_FILE | FFF_HD | FFF_VARIED)

#define FFF_BINARY_REC_HD                       (FFF_BINARY | FFF_REC | FFF_HD)
#define FFF_ASCII_REC_HD                        (FFF_ASCII  | FFF_REC | FFF_HD)
#define FFF_DBASE_REC_HD                (FFF_DBASE  | FFF_REC | FFF_HD)

#define FFF_INPUT_BINARY_REC_HD  (FFF_INPUT | FFF_BINARY | FFF_REC | FFF_HD)
#define FFF_INPUT_ASCII_REC_HD  (FFF_INPUT | FFF_ASCII  | FFF_REC | FFF_HD)
#define FFF_INPUT_DBASE_REC_HD  (FFF_INPUT | FFF_DBASE  | FFF_REC | FFF_HD)

#define FFF_OUTPUT_BINARY_REC_HD (FFF_OUTPUT | FFF_BINARY | FFF_REC | FFF_HD)
#define FFF_OUTPUT_ASCII_REC_HD (FFF_OUTPUT | FFF_ASCII  | FFF_REC | FFF_HD)
#define FFF_OUTPUT_DBASE_REC_HD (FFF_OUTPUT | FFF_DBASE  | FFF_REC | FFF_HD)

#define FFF_BINARY_REC_HD_SEP   (FFF_BINARY | FFF_SEPARATE | FFF_REC | FFF_HD)
#define FFF_ASCII_REC_HD_SEP    (FFF_ASCII  | FFF_SEPARATE | FFF_REC | FFF_HD)
#define FFF_DBASE_REC_HD_SEP    (FFF_DBASE  | FFF_SEPARATE | FFF_REC | FFF_HD)

#define FFF_INPUT_BINARY_REC_HD_SEP     (FFF_INPUT | FFF_BINARY | FFF_SEPARATE | FFF_REC | FFF_HD)
#define FFF_INPUT_ASCII_REC_HD_SEP      (FFF_INPUT | FFF_ASCII  | FFF_SEPARATE | FFF_REC | FFF_HD)
#define FFF_INPUT_DBASE_REC_HD_SEP      (FFF_INPUT | FFF_DBASE  | FFF_SEPARATE | FFF_REC | FFF_HD)

#define FFF_OUTPUT_BINARY_REC_HD_SEP    (FFF_OUTPUT | FFF_BINARY | FFF_SEPARATE | FFF_REC | FFF_HD)
#define FFF_OUTPUT_ASCII_REC_HD_SEP     (FFF_OUTPUT | FFF_ASCII  | FFF_SEPARATE | FFF_REC | FFF_HD)
#define FFF_OUTPUT_DBASE_REC_HD_SEP     (FFF_OUTPUT | FFF_DBASE  | FFF_SEPARATE | FFF_REC | FFF_HD)


#ifdef DEFINE_DATA
FFF_LOOKUP format_types[58] = {
	{"binary_data",        FFF_BINARY_DATA},
	{"ASCII_data",         FFF_ASCII_DATA},
	{"dbase_data",         FFF_DBASE_DATA},
	{"binary_input_data",  FFF_BINARY_INPUT_DATA},
	{"ASCII_input_data",   FFF_ASCII_INPUT_DATA},
	{"dbase_input_data",   FFF_DBASE_INPUT_DATA},
	{"binary_output_data", FFF_BINARY_OUTPUT_DATA},
	{"ASCII_output_data",  FFF_ASCII_OUTPUT_DATA},
	{"dbase_output_data",  FFF_DBASE_OUTPUT_DATA},
	{"return",             FFF_RETURN},

	{"RETURN",                    FFF_RETURN},
	{"EOL",                       FFF_RETURN},
	{"binary_file_header",        FFF_BINARY_FILE_HD},
	{"ASCII_file_header",         FFF_ASCII_FILE_HD},
	{"dbase_file_header",         FFF_DBASE_FILE_HD},
	{"binary_input_file_header",  FFF_INPUT_BINARY_FILE_HD},
	{"ASCII_input_file_header",   FFF_INPUT_ASCII_FILE_HD},
	{"dbase_input_file_header",   FFF_INPUT_DBASE_FILE_HD},
	{"binary_output_file_header", FFF_OUTPUT_BINARY_FILE_HD},
	{"ASCII_output_file_header",  FFF_OUTPUT_ASCII_FILE_HD},

	{"dbase_output_file_header",           FFF_OUTPUT_DBASE_FILE_HD},
	{"binary_file_header_separate",        FFF_BINARY_FILE_HD_SEP},
	{"ASCII_file_header_separate",         FFF_ASCII_FILE_HD_SEP},
	{"dbase_file_header_separate",         FFF_DBASE_FILE_HD_SEP},
	{"binary_file_header_separate_varied", FFF_BINARY_FILE_HD_SEP_VAR},
	{"ASCII_file_header_separate_varied",  FFF_ASCII_FILE_HD_SEP_VAR},
	{"dbase_file_header_separate_varied",  FFF_DBASE_FILE_HD_SEP_VAR},
	{"binary_input_file_header_separate",  FFF_INPUT_BINARY_FILE_HD_SEP},
	{"ASCII_input_file_header_separate",   FFF_INPUT_ASCII_FILE_HD_SEP},
	{"dbase_input_file_header_separate",   FFF_INPUT_DBASE_FILE_HD_SEP},

	{"binary_input_file_header_separate_varied",  FFF_INPUT_BINARY_FILE_HD_SEP_VAR},
	{"ASCII_input_file_header_separate_varied",   FFF_INPUT_ASCII_FILE_HD_SEP_VAR},
	{"dbase_input_file_header_separate_varied",   FFF_INPUT_DBASE_FILE_HD_SEP_VAR},
	{"binary_output_file_header_separate",        FFF_OUTPUT_BINARY_FILE_HD_SEP},
	{"ASCII_output_file_header_separate",         FFF_OUTPUT_ASCII_FILE_HD_SEP},
	{"dbase_output_file_header_separate",         FFF_OUTPUT_DBASE_FILE_HD_SEP},
	{"binary_output_file_header_separate_varied", FFF_OUTPUT_BINARY_FILE_HD_SEP_VAR},
	{"ASCII_output_file_header_separate_varied",  FFF_OUTPUT_ASCII_FILE_HD_SEP_VAR},
	{"dbase_output_file_header_separate_varied",  FFF_OUTPUT_DBASE_FILE_HD_SEP_VAR},
	{"binary_record_header",                      FFF_BINARY_REC_HD},

	{"ASCII_record_header",           FFF_ASCII_REC_HD},
	{"dbase_record_header",           FFF_DBASE_REC_HD},
	{"binary_input_record_header",    FFF_INPUT_BINARY_REC_HD},
	{"ASCII_input_record_header",     FFF_INPUT_ASCII_REC_HD},
	{"dbase_input_record_header",     FFF_INPUT_DBASE_REC_HD},
	{"binary_output_record_header",   FFF_OUTPUT_BINARY_REC_HD},
	{"ASCII_output_record_header",    FFF_OUTPUT_ASCII_REC_HD},
	{"dbase_output_record_header",    FFF_OUTPUT_DBASE_REC_HD},
	{"binary_record_header_separate", FFF_BINARY_REC_HD_SEP},
	{"ASCII_record_header_separate",  FFF_ASCII_REC_HD_SEP},

	{"dbase_record_header_separate",         FFF_DBASE_REC_HD_SEP},
	{"binary_input_record_header_separate",  FFF_INPUT_BINARY_REC_HD_SEP},
	{"ASCII_input_record_header_separate",   FFF_INPUT_ASCII_REC_HD_SEP},
	{"dbase_input_record_header_separate",   FFF_INPUT_DBASE_REC_HD_SEP},
	{"binary_output_record_header_separate", FFF_OUTPUT_BINARY_REC_HD_SEP},
	{"ASCII_output_record_header_separate",  FFF_OUTPUT_ASCII_REC_HD_SEP},
	{"dbase_output_record_header_separate",  FFF_OUTPUT_DBASE_REC_HD_SEP},
	{(char *)NULL,                           FFF_NULL},
};

#else
EXTERN FFF_LOOKUP format_types[49];
#endif

/* Function Prototypes */

size_t ffv_type_size(FFV_TYPE_type var_type);

#endif /* FF_TYPES_H__ */

