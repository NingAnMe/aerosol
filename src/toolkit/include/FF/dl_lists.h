/*
 * NAME: dl_lists.h
 *		
 * PURPOSE: Include file for double-linked list management functions
 *
 * AUTHOR: T. Habermann, NGDC, (303) 497 - 6472, haber@mail.ngdc.noaa.gov
 *
 * USAGE: #include <lists.h>
 *
 * COMMENTS: Many of the ideas in this generic list handling system
 *		originated in the Vermont Views list management code.
 *
 * The DLL module incorporates self tests when compiled with the symbolic
 * macro NO_DLL_CHECKS not defined; define NO_DLL_CHECKS on the compile
 * command line if you wish these self tests to be disabled.  Note that a
 * build MUST BE homogeneous with respect to the setting of NO_DLL_CHECKS.
 * In other words, compile ALL files with NO_DLL_CHECKS defined, or NO files.
 * A heterogeneous build in this respect will produce code that is internally
 * inconsistent and cannot possibly run correctly.
 *
 */

/* Protect against multiple inclusions */
#ifndef DLL_DEFS
#define DLL_DEFS

typedef struct list_node		/* NODE STRUCTURE */
{
	struct	list_node *previous;		/* previous node pointer */
	struct	list_node *next;		/* next node pointer */
	void	*data_ptr;	   		/* data pointer	*/
#ifdef DLL_CHECKS_ON
	unsigned int status;
	size_t length; /* head node: count of non-head nodes, node: byte-size of trojan data */
#endif
} DLL_NODE, *DLL_NODE_PTR;

typedef struct {
	int fileid;
	FILE *stream;
	fpos_t pos;
	int nline;
} FILE_DLL_DATA;				

/* linked list macros */

#define dll_data(n)	((n)->data_ptr)

#define dll_next(n)	((n)->next)
#define dll_previous(n)	((n)->previous)

#define dll_first(h)	((h)->next)
#define dll_last(h)	((h)->previous)

/* Function Prototypes */

DLL_NODE_PTR dll_init(void);
DLL_NODE_PTR dll_insert(DLL_NODE_PTR, unsigned int);
DLL_NODE_PTR dll_add(DLL_NODE_PTR, unsigned int);
DLL_NODE_PTR dll_init_contiguous(void *, void *, unsigned int);
void 	     dll_delete(DLL_NODE_PTR, void (*)(void *));
int dll_free(DLL_NODE_PTR head, void (*)(void *));

#endif		/* End of DLL_DEFS */

