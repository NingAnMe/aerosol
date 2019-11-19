/* char VPFIO_H_ID[] = "@(#) vpfio.h 2.3 11/9/91 ESRI Applications Programming" ; */
/* ========================================================================

   Environmental Systems Research Institute (ESRI) Applications Programming

       Project: 		Conversion from ARC/INFO to VPF 
       Original Coding:		David Flinn     July 1991
       Modifications:	      

   ======================================================================== */

#ifndef _VPF_IO_
#define _VPF_IO_

#include <stdio.h>

/* These are all the metacharacters used to parse out input text files */

#define	 	COMPONENT_SEPERATOR	';'
#define		LINE_CONTINUE		'\\'
#define		SPACE			' '
#define		TEXT_NULL		"-"
#define		COMMENT_CHAR		'"'
#define		FIELD_COUNT		'='
#define		FIELD_SEPERATOR		','
#define		END_OF_FIELD		':'
#define		FIELD_PARTITION		'^'
#define		NEXT_ELEMENT		'|'
#define		COMMENT			'#'
#define		NEW_LINE		'\n'
#define		VARIABLE_COUNT		'*'
#define		TAB			'\t'

/* Set up the macros to read in data from disk and to write */

#ifndef __MSDOS__
#define		SEEK_SET	0
#define		SEEK_CUR	1
#define		SEEK_END	2
#endif


/* types */
typedef enum {
  VpfNull,
  VpfChar, 
  VpfShort,
  VpfInteger,
  VpfFloat,  
  VpfDouble,
  VpfDate, 
  VpfKey,
  VpfCoordinate,
  VpfTriCoordinate,
  VpfDoubleCoordinate,
  VpfDoubleTriCoordinate,
  VpfUndefined
} VpfDataType ;

#define   Read_Vpf_Char(tobuffer,fromfile,count) \
             VpfRead(tobuffer,VpfChar,count,fromfile)

#define   Read_Vpf_Short(tobuffer,fromfile,count) \
             VpfRead(tobuffer,VpfShort,count,fromfile)

#define   Read_Vpf_Int(tobuffer,fromfile,count) \
             VpfRead(tobuffer,VpfInteger,count,fromfile)

#define   Read_Vpf_Float(tobuffer,fromfile,count) \
             VpfRead(tobuffer,VpfFloat,count,fromfile)

#define   Read_Vpf_Double(tobuffer,fromfile,count) \
             VpfRead(tobuffer,VpfDouble,count,fromfile)

#define   Read_Vpf_Date(tobuffer,fromfile,count) \
             VpfRead(tobuffer,VpfDate,count,fromfile)

#define   Read_Vpf_Coordinate(tobuffer,fromfile,count) \
             VpfRead(tobuffer,VpfCoordinate,count,fromfile)

#define   Read_Vpf_CoordinateZ(tobuffer,fromfile,count) \
             VpfRead(tobuffer,VpfTriCoordinate,count,fromfile)

#define   Read_Vpf_DoubleCoordinate(tobuffer,fromfile,count) \
             VpfRead(tobuffer,VpfDoubleCoordinate,count,fromfile)

#define   Read_Vpf_DoubleCoordinateZ(tobuffer,fromfile,count) \
             VpfRead(tobuffer,VpfDoubleTriCoordinate,count,fromfile)

#define   Write_Vpf_Char(tobuffer,fromfile,count) \
             VpfWrite(tobuffer,VpfChar,count,fromfile)

#define   Write_Vpf_Short(tobuffer,fromfile,count) \
             VpfWrite(tobuffer,VpfShort,count,fromfile)

#define   Write_Vpf_Int(tobuffer,fromfile,count) \
             VpfWrite(tobuffer,VpfInteger,count,fromfile)

#define   Write_Vpf_Float(tobuffer,fromfile,count) \
             VpfWrite(tobuffer,VpfFloat,count,fromfile)

#define   Write_Vpf_Double(tobuffer,fromfile,count) \
             VpfWrite(tobuffer,VpfDouble,count,fromfile)

#define   Write_Vpf_Date(tobuffer,fromfile,count) \
             VpfWrite(tobuffer,VpfDate,count,fromfile)

#define   Write_Vpf_Coordinate(tobuffer,fromfile,count) \
             VpfWrite(tobuffer,VpfCoordinate,count,fromfile)

#define   Write_Vpf_CoordinateZ(tobuffer,fromfile,count) \
             VpfWrite(tobuffer,VpfTriCoordinate,count,fromfile)

#define   Write_Vpf_DoubleCoordinate(tobuffer,fromfile,count) \
             VpfWrite(tobuffer,VpfDoubleCoordinate,count,fromfile)

#define   Write_Vpf_DoubleCoordinateZ(tobuffer,fromfile,count) \
             VpfWrite(tobuffer,VpfDoubleTriCoordinate,count,fromfile)

/*  subroutines */

int VpfRead ( void *to, VpfDataType type, int count, FILE *from ) ;

int VpfWrite ( void *from, VpfDataType type, int count, FILE *to ) ;

#endif		/* ifndef _VPF_IO_ */








