#-----------------------------------------------------------------------
#Copyright (C) 2004,  Space Science and Engineering Center, University
#of Wisconsin-Madison, Madison WI.
#
#This program is free software; you can redistribute it and/or modify
#it under the terms of the GNU General Public License as published by
#the Free Software Foundation; either version 2 of the License, or
#(at your option) any later version.
#
#This program is distributed in the hope that it will be useful,
#but WITHOUT ANY WARRANTY; without even the implied warranty of
#MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#GNU General Public License for more details.
#
#You should have received a copy of the GNU General Public License
#along with this program; if not, write to the Free Software
#Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
#--------------------------------------------------------------------------

###########################################################################
#
#  Filename:
#     makefile
#     This make file was developed as a creation tool for the MODIS direct
#     Broadcast dateplus.exe program.  This tool was developed by
#     Bob Orlando.  For more information on this tool, please
#     see his web site: http://www.orlandokuntao.com/dateplus_c.html
#
#
#   PROCEDURE:
#
#    type:
#    make 
#
#    The platform dependent compilation is started by selecting the required
#    definitions based on the output of the unix
#
#     uname -s
#
#    command.
#
#############################################################################

# Objects
OBJ = dateplus.o

# Compiler options are set based on MODIS production software flags,
#  and the machine Operating System.
#
#  FC are FORTRAN compiler options, while FL are FORTRAN linking
#  options.  The COMMON_FLAGS variables are flags which are used in
#  both the compiling and linking stages.

SUNOS_COMMON_FLAGS=
IRIX64_COMMON_FLAGS= -n32 
AIX_COMMON_FLAGS=

target:
	@echo "OS type detected: "`uname -s`
	@case `uname -s` in \
	  "SunOS")   case `uname -p` in \
		     "i386") make $(TARGET) \
		             FC="g77" \
		             FL="g77" \
		             CC="gcc" \
		             CC_FLAGS=" -c -O2 -D_POSIX_SOURCE " \
		             FC_FLAGS=" -c -O2 -ffixed-line-length-132 -fno-automatic " \
		             FL_FLAGS=" -o" \
	                     ;; \
		     *) make $(TARGET) \
			     FC="f77" \
			     FL="f77" \
			     CC="cc" \
			     CC_FLAGS=" -c -O2 -D_POSIX_SOURCE ${SUNOS_COMMON_FLAGS}" \
			     FC_FLAGS=" -c -O2 -e ${SUNOS_COMMON_FLAGS}" \
			     FL_FLAGS=" ${SUNOS_COMMON_FLAGS} -o" \
			     ;; \
		     esac \
		     ;; \
	  "IRIX64" ) make $(TARGET)  \
		     FC="f77" \
		     FL="f77" \
		     CC="cc" \
		     CC_FLAGS=" -c -O2 -D_POSIX_SOURCE ${IRIX64_COMMON_FLAGS}" \
		     FC_FLAGS=" -c -O2  -u -bytereclen -col120 -woff 2271,2272,2274 ${IRIX64_COMMON_FLAGS} " \
		     FL_FLAGS="${IRIX64_COMMON_FLAGS} -o" \
	             ;; \
	  "AIX" ) make $(TARGET)  \
		     FC="xlf" \
		     FL="xlf" \
		     CC="cc" \
		     CC_FLAGS=" -c -O2 -qlanglvl=ansi -D_POSIX_SOURCE -DAIX43 ${AIX_COMMON_FLAGS}" \
		     FC_FLAGS=" -c -O2 -u -qfixed=120 -qmaxmem=-1 ${AIX_COMMON_FLAGS} " \
		     FL_FLAGS="${AIX_COMMON_FLAGS} -o" \
		     ;; \
	  "Linux" ) make $(TARGET)  \
		     FC="g77" \
		     FL="g77" \
		     CC="gcc" \
		     CC_FLAGS=" -c -O -D_POSIX_SOURCE " \
		     FC_FLAGS=" -c -O -ffixed-line-length-132 -fno-automatic " \
		     FL_FLAGS=" -o" \
		     ;; \
	  "HP-UX" ) make $(TARGET)  \
		     FC="f90" \
		     FL="f90" \
		     CC="cc" \
		     CC_FLAGS=" -c +O2 -Ae " \
		     FC_FLAGS=" -c +O2 +extend_source +implicit_none " \
		     FL_FLAGS=" -o" \
		     ;; \
	  *) echo "This system is not supported" ;; \
	esac

# Define target

TARGET = dateplus.exe

# Make executable
$(TARGET): $(OBJ) 
	$(FL) $(OBJ) $(LIB) $(FL_FLAGS) $@

clean:
	rm -f *.o $(TARGET)

#----------------------------------------------------------------------
# Suffix rules used for process dependency files
#----------------------------------------------------------------------

.SUFFIXES: .f .c .o

.f.o:   
	$(FC) $(FC_FLAGS) $(INC) $?

.c.o:  
	$(CC) $(CC_FLAGS) $<


#********************** END OF MAKEFILE ***************************
