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
#     makefile.debug
#     This make file was developed as a creation tool for the MODIS direct
#     Broadcast Aerosol Product (MOD04) software.  This version uses
#     optimized compiler options.
#
#
#   PROCEDURE:
#
#    make -f makefile.debug
#
#    The platform dependent compilation is started by selecting the required
#    definitions based on the output of the unix
#
#     uname -s
#
#    command.
#
#############################################################################

# Set up Libraries

LIB = -lm

# Set up include files

INC = -I../include

# Dependecy science include files by name

INC_FILES_SCIENCE = ../include/aerosol.inc \
                    ../include/db_mod04uw_debug.inc \
                    ../include/platform_name.inc \
                    ../include/COMMONS.inc \
                    ../include/mod04_land.inc \
                    ../include/mod04.inc

# Objects
OBJ = \
        aerosol.o                   db_mod04_get_rp.o \
        hdrgetkeyint.o              hdrgetkeystr.o \
        message.o                   strcompress.o \
        db_mod04_file_open.o        db_mod04_file_close.o \
        db_mod04_Get_Metadata.o     hdrgetkeydbl.o \
        db_mod04_chk_input.o        get_mod04_data.o \
        mod04_get_angles.o          db_read_flat_file.o \
        db_get_swath_metadata.o     mod04_get_L1b.o \
        mod04_get_mask.o            db_read_mask_file.o \
        db_CldMsk_Info_MOD04.o      get_ancillary_aer.o \
        db_read_anc_data.o          ppv.o \
        Process_ocean_V2.o          Fillvalue_MOD04_V2.o \
        Process_land_V6.o           db_mod04_write_products.o \
        db_mod04_write_flat_file.o  big_endian.o \
        string_length.o             bl_int.o \
        blint_met.o                 ezlh_convert.o

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
		     FC="gfortran" \
		     FL="gfortran" \
		     CC="gcc" \
		     CC_FLAGS=" -c -O -D_POSIX_SOURCE " \
		     FC_FLAGS=" -c -O -ffixed-line-length-132 -fno-automatic -std=legacy" \
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

TARGET = aerosol.exe

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
