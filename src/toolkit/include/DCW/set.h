/* char VPFSET_H_ID[] = "@(#) set.h 2.3 11/9/91 ESRI Applications Programming" ; */
/* ========================================================================

   Environmental Systems Research Institute (ESRI) Applications Programming

       Project: 		VPF display software 
       Original Coding:		Barry Micheals	November 1990  PC version
       Modifications:		David Flinn	October 1991   UNIX

   ======================================================================== */

#ifndef __SET_H__
#define __SET_H__

/* A set is represented as an array of characters with each character */
/* holding 8 bits of the set. */
typedef struct {
   int size;
   unsigned char *buf;
} set_type;


/* Functions: */

set_type set_init( int n );

int  set_empty( set_type set );

void set_insert( int element,
		 set_type set );

void set_delete( int element,
		 set_type set );

int set_member( int element,
		set_type set );

int set_min( set_type set );

int set_max( set_type set );

int  num_in_set( set_type set );

void set_on( set_type set );

void set_off( set_type set );

int  set_equal( set_type a,
		set_type b );

void set_assign( set_type *a,
		 set_type b );

set_type set_union( set_type a,
		    set_type b );

set_type set_intersection( set_type a,
			   set_type b );

set_type set_difference( set_type a,
			 set_type b );

void set_nuke( set_type *set );

#endif /* ifdef __SET_H__ */
