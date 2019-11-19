/* adtype.h: include for assorted data types */

#ifndef ADTYPE

#define ADTYPE

/* Struct for the queue_stack_op function */
typedef struct queue_stack_struct{
	struct queue_stack_struct *prev;
	struct queue_stack_struct *next;
	void *data;
} *QSTACK_PTR, QSTACK;

/* #DEFINE values: */
#define QS_NEW  0
#define QS_PUSH 1
#define QS_PULL 2
#define QS_POP  3
#define QS_KILL 4

/* Function prototypes */
void *queue_stack_op(QSTACK_PTR qs, int op, void *data);
int ff_get_buffer_eol_str(char *buffer, char *buffer_eol_str);
#endif
