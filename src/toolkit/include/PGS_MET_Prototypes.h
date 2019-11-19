/*-------------------------------------------------------------------------*/
/*                                                                         */
/*  COPYRIGHT[copyright mark] 1999, Raytheon Systems Company, its vendors, */
/*  and suppliers.  ALL RIGHTS RESERVED.                                   */
/*                                                                         */
/*-------------------------------------------------------------------------*/
/*****************************************************************************
BEGIN_FILE_PROLOG:

FILENAME:

	PGS_MET_Prototypes.H

DESCRIPTION:

	This file contains function prototypes that are specific to the
	MET Tools

AUTHOR:
	Ray Milburn / Steven Myers & Associates

HISTORY:
	28-Jan-99 RM Initial version

END_FILE_PROLOG:
*****************************************************************************/

#ifndef PGS_MET_Prototypes_h
#define PGS_MET_Prototypes_h

#include <PGS_TYPES.h>  
#include <CUC/odldef.h>
#include <CUC/odlinter.h>

/*****************************************************************
    Function prototypes.
*****************************************************************/

PGSt_SMF_status PGS_MET_SearchAttrF(PGSt_MET_handle mdHandle, 
                       AGGREGATE mdGroupNode, char attrName[], 
                       void *attrValue, unsigned int strLength);
PGSt_SMF_status PGS_MET_HDFToODL(char *fileName, char *aggName, 
                       char *hdfAttrName, AGGREGATE   *aggNode);

#endif
