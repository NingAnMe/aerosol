#-------------------------------------------------------------------------#
#                                                                         #
#  COPYRIGHT(C) 2000, Raytheon System Company, its vendors,		  #
#  and suppliers.  ALL RIGHTS RESERVED.                                   #
#                                                                         #
#-------------------------------------------------------------------------#
#----------------------------------------------------------------------------
# file:		makefile for SDP Toolkit CUC header files
#
# author:  Guru Tej S. Khalsa / Applied Research Corporation
#
# notes:
#	This file is intended for internal use by ECS CM processes only.
#
#----------------------------------------------------------------------------

# set sub-group
GRP = CUC

# force make to use the 'sh' shell
SHELL = /bin/sh

# list of CUC header files to be moved to stage area
HEADER_FILES= \
	$(PGSINC)/$(GRP)/odldef.h		\
	$(PGSINC)/$(GRP)/odlinter.h		\
	$(PGSINC)/$(GRP)/odlparse.h		\
	$(PGSINC)/$(GRP)/udalloc.h		\
	$(PGSINC)/$(GRP)/udposix.h		\
	$(PGSINC)/$(GRP)/udunits.h		\
	$(PGSINC)/$(GRP)/utparse.h		\
	$(PGSINC)/$(GRP)/utprivate.h		\
	$(PGSINC)/$(GRP)/utscan.h

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

$(PGSINC)/$(GRP)/odldef.h: odldef.h
	cp $? $@

$(PGSINC)/$(GRP)/odlinter.h: odlinter.h
	cp $? $@

$(PGSINC)/$(GRP)/odlparse.h: odlparse.h
	cp $? $@

$(PGSINC)/$(GRP)/udalloc.h: udalloc.h
	cp $? $@

$(PGSINC)/$(GRP)/udposix.h: udposix.h
	cp $? $@

$(PGSINC)/$(GRP)/udunits.h: udunits.h
	cp $? $@

$(PGSINC)/$(GRP)/utparse.h: utparse.h
	cp $? $@

$(PGSINC)/$(GRP)/utprivate.h: utprivate.h
	cp $? $@

$(PGSINC)/$(GRP)/utscan.h: utscan.h
	cp $? $@

