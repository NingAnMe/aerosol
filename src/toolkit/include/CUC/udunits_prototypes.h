/*-------------------------------------------------------------------------*/
/*                                                                         */
/*  COPYRIGHT[copyright mark] 1999, Raytheon Systems Company, its vendors, */
/*  and suppliers.  ALL RIGHTS RESERVED.                                   */
/*                                                                         */
/*-------------------------------------------------------------------------*/
/*****************************************************************************
BEGIN_FILE_PROLOG:

FILENAME:

	udunits_prototypes.h

DESCRIPTION:

	This file contains function prototypes that are specific to the
	CUC Tools

AUTHOR:
	Ray Milburn / Steven Myers & Associates

HISTORY:
	02-Feb-99 RM Initial version

END_FILE_PROLOG:
*****************************************************************************/

#ifndef udunits_prototypes
#define udunits_prototypes

#ifdef __cplusplus
extern "C" {
#endif

/*****************************************************************
    Function prototypes.
*****************************************************************/

void utrestart(FILE *fptr);
int utparse();
int utAdd(char *name, int HasPlural, utUnit *unit);
void utLexReset();
int utlex();
int utFind(char *spec, utUnit *up);
int utwrap(void);

#ifdef __cplusplus
}
#endif

#endif
