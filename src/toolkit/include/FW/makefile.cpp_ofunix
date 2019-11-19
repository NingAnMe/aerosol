#-------------------------------------------------------------------------#
#                                                                         #
#  COPYRIGHT(C) 2000, Raytheon System Company, its vendors,		  #
#  and suppliers.  ALL RIGHTS RESERVED.                                   #
#                                                                         #
#-------------------------------------------------------------------------#
#----------------------------------------------------------------------------
# file:		makefile for SDP Toolkit FW header files
#
# author:  Guru Tej S. Khalsa / Applied Research Corporation
#
# notes:
#	This file is intended for internal use by ECS CM processes only.
#
#----------------------------------------------------------------------------

# set sub-group
GRP = FW

# force make to use the 'sh' shell
SHELL = /bin/sh

# list of CUC header files to be moved to stage area
HEADER_FILES= \
	$(PGSINC)/$(GRP)/cproj.h	\
	$(PGSINC)/$(GRP)/isin.h		\
	$(PGSINC)/$(GRP)/proj.h

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

$(PGSINC)/$(GRP)/cproj.h: cproj.h
	cp $? $@

$(PGSINC)/$(GRP)/isin.h: isin.h
	cp $? $@

$(PGSINC)/$(GRP)/proj.h: proj.h
	cp $? $@

