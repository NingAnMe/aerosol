/* Define MAX_MIN structure */

typedef struct {
	void* check_address;		/* MAX_MIN address in memory */
	VARIABLE_PTR var;		/* Variable */
	long min_record;		/* Record where min occurs */
	long max_record;		/* Record where max occurs */
	void *minimum;			/* Minimum variable value */
	void *maximum;			/* Maximum variable value */
	void *max_flag;			/* Upper missing data limit */
	void *min_flag;			/* Lower missing data limit */
} MAX_MIN, *MAX_MIN_PTR;

/* Define MAX_MIN Attributes/messages */
#define MM_MAX_MIN		101
#define MM_MISSING_DATA_FLAGS	102

/* MAX_MIN prototypes */
int		mm_free(MAX_MIN_PTR);
double 		mm_getmx(MAX_MIN_PTR);
double 		mm_getmn(MAX_MIN_PTR);
MAX_MIN_PTR 	mm_make(VARIABLE_PTR, unsigned short);
int 		mm_print(MAX_MIN_PTR);
int 		mm_set(MAX_MIN_PTR, ...);


