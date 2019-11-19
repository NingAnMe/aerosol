#-------------------------------------------------------------------------#
#                                                                         #
#  COPYRIGHT(C) 2000, Raytheon System Company, its vendors,		  #
#  and suppliers.  ALL RIGHTS RESERVED.                                   #
#                                                                         #
#-------------------------------------------------------------------------#
#----------------------------------------------------------------------------
# file:		makefile for SDP Toolkit DCW header files
#
# author:  Guru Tej S. Khalsa / Applied Research Corporation
#
# notes:
#	This file is intended for internal use by ECS CM processes only.
#
#----------------------------------------------------------------------------

# set sub-group
GRP = DCW

# force make to use the 'sh' shell
SHELL = /bin/sh

# list of DCW header files to be moved to stage area
HEADER_FILES= \
	$(PGSINC)/$(GRP)/PGS_AA_DCW.h	\
	$(PGSINC)/$(GRP)/Xlib.h		\
	$(PGSINC)/$(GRP)/color.h	\
	$(PGSINC)/$(GRP)/coorgeom.h	\
	$(PGSINC)/$(GRP)/gctp.for.h	\
	$(PGSINC)/$(GRP)/linklist.h	\
	$(PGSINC)/$(GRP)/machine.h	\
	$(PGSINC)/$(GRP)/set.h		\
	$(PGSINC)/$(GRP)/strfunc.h	\
	$(PGSINC)/$(GRP)/symbols.h	\
	$(PGSINC)/$(GRP)/tiff.h		\
	$(PGSINC)/$(GRP)/tiffcompat.h	\
	$(PGSINC)/$(GRP)/tiffio.h	\
	$(PGSINC)/$(GRP)/unitz0.h	\
	$(PGSINC)/$(GRP)/vpf.h		\
	$(PGSINC)/$(GRP)/vpfio.h	\
	$(PGSINC)/$(GRP)/vpfprim.h	\
	$(PGSINC)/$(GRP)/vpfquery.h	\
	$(PGSINC)/$(GRP)/vpfrelat.h	\
	$(PGSINC)/$(GRP)/vpfsprel.h	\
	$(PGSINC)/$(GRP)/vpfspx.h	\
	$(PGSINC)/$(GRP)/vpftable.h	\
	$(PGSINC)/$(GRP)/vpftable.h.dec	\
	$(PGSINC)/$(GRP)/vpftidx.h	\
	$(PGSINC)/$(GRP)/vpfview.h	\
	$(PGSINC)/$(GRP)/vvmisc.h	\
	$(PGSINC)/$(GRP)/vvselec.h	\
	$(PGSINC)/$(GRP)/vvspqry.h	\
	$(PGSINC)/$(GRP)/vvtheme.h	\
	$(PGSINC)/$(GRP)/vvutil.h	\
	$(PGSINC)/$(GRP)/xtiff.h

#
# targets
#
default:
	@echo "This file is for use by ECS Configuration Management processes only."

stage: message $(HEADER_FILES)

message:
	@echo ""
	@echo "staging SDP Toolkit $(GRP) header files to $(PGSINC)/$(GRP) ..."

# individual header files
# each header file must be listed here with its own target

$(PGSINC)/$(GRP)/PGS_AA_DCW.h: PGS_AA_DCW.h
	cp $? $@

$(PGSINC)/$(GRP)/Xlib.h: Xlib.h
	cp $? $@

$(PGSINC)/$(GRP)/color.h: color.h
	cp $? $@

$(PGSINC)/$(GRP)/coorgeom.h: coorgeom.h
	cp $? $@

$(PGSINC)/$(GRP)/gctp.for.h: gctp.for.h
	cp $? $@

$(PGSINC)/$(GRP)/linklist.h: linklist.h
	cp $? $@

$(PGSINC)/$(GRP)/machine.h: machine.h
	cp $? $@

$(PGSINC)/$(GRP)/set.h: set.h
	cp $? $@

$(PGSINC)/$(GRP)/strfunc.h: strfunc.h
	cp $? $@

$(PGSINC)/$(GRP)/symbols.h: symbols.h
	cp $? $@

$(PGSINC)/$(GRP)/tiff.h: tiff.h
	cp $? $@

$(PGSINC)/$(GRP)/tiffcompat.h: tiffcompat.h
	cp $? $@

$(PGSINC)/$(GRP)/tiffio.h: tiffio.h
	cp $? $@

$(PGSINC)/$(GRP)/unitz0.h: unitz0.h
	cp $? $@

$(PGSINC)/$(GRP)/vpf.h: vpf.h
	cp $? $@

$(PGSINC)/$(GRP)/vpfio.h: vpfio.h
	cp $? $@

$(PGSINC)/$(GRP)/vpfprim.h: vpfprim.h
	cp $? $@

$(PGSINC)/$(GRP)/vpfquery.h: vpfquery.h
	cp $? $@

$(PGSINC)/$(GRP)/vpfrelat.h: vpfrelat.h
	cp $? $@

$(PGSINC)/$(GRP)/vpfsprel.h: vpfsprel.h
	cp $? $@

$(PGSINC)/$(GRP)/vpfspx.h: vpfspx.h
	cp $? $@

$(PGSINC)/$(GRP)/vpftable.h: vpftable.h
	cp $? $@

$(PGSINC)/$(GRP)/vpftable.h.dec: vpftable.h.dec
	cp $? $@

$(PGSINC)/$(GRP)/vpftidx.h: vpftidx.h
	cp $? $@

$(PGSINC)/$(GRP)/vpfview.h: vpfview.h
	cp $? $@

$(PGSINC)/$(GRP)/vvmisc.h: vvmisc.h
	cp $? $@

$(PGSINC)/$(GRP)/vvselec.h: vvselec.h
	cp $? $@

$(PGSINC)/$(GRP)/vvspqry.h: vvspqry.h
	cp $? $@

$(PGSINC)/$(GRP)/vvtheme.h: vvtheme.h
	cp $? $@

$(PGSINC)/$(GRP)/vvutil.h: vvutil.h
	cp $? $@

$(PGSINC)/$(GRP)/xtiff.h: xtiff.h
	cp $? $@

