/*char VPFLINKLIST_H_ID[] = "@(#) linklist.h 2.3 11/9/91 ESRI Applications Programming" ;*/
/* =======================================================================

   Environmental Systems Research Institute (ESRI) Applications Programming

       Project: 		Conversion from ARC/INFO to VPF 
       Original Coding:		Tom Wood	Fall 1990
       Modifications:		David Flinn	February 1991

   ======================================================================== */

#ifndef __LINKLIST_H__
#define __LINKLIST_H__

#include "machine.h"

typedef struct linked_list_cell {
   void      	           *element;
   unsigned                 element_size;
   struct linked_list_cell *next;
} cell_type, *linked_list_type, *position_type;


#define LL_FIRST(list) (position_type)list

#define LL_NEXT(position) position->next

#ifndef TRUE
#define TRUE 1
#endif
#ifndef FALSE
#define FALSE 0
#endif


linked_list_type ll_init();

int ll_empty( linked_list_type list );

position_type ll_first( linked_list_type list );

position_type ll_last( linked_list_type list );

position_type ll_next( position_type position );

position_type ll_previous( position_type    position,
			   linked_list_type list );

int ll_end( position_type position );

void ll_element( position_type position,
		 void         *element );

void ll_insert( void          *element,
		unsigned      size,
		position_type position );

void ll_delete( position_type position );

void ll_reset( linked_list_type list );

position_type ll_locate( void             *element,
			 linked_list_type list );

void ll_replace( void          *element,
		 position_type position );

#endif /* ifdef __LINKLIST_H__ */
