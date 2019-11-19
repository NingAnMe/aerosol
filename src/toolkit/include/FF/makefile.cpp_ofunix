#-------------------------------------------------------------------------#
#                                                                         #
#  COPYRIGHT(C) 2000, Raytheon System Company, its vendors,		  #
#  and suppliers.  ALL RIGHTS RESERVED.                                   #
#                                                                         #
#-------------------------------------------------------------------------#
#----------------------------------------------------------------------------
# file:		makefile for SDP Toolkit FF header files
#
# author:  Guru Tej S. Khalsa / Applied Research Corporation
#
# notes:
#	This file is intended for internal use by ECS CM processes only.
#
#----------------------------------------------------------------------------

# set sub-group
GRP = FF

# force make to use the 'sh' shell
SHELL = /bin/sh

# list of CUC header files to be moved to stage area
HEADER_FILES= \
	$(PGSINC)/$(GRP)/adtype.h	\
	$(PGSINC)/$(GRP)/avl.h		\
	$(PGSINC)/$(GRP)/avltree.h	\
	$(PGSINC)/$(GRP)/data_par.h	\
	$(PGSINC)/$(GRP)/databin.h	\
	$(PGSINC)/$(GRP)/dataview.h	\
	$(PGSINC)/$(GRP)/dl_lists.h	\
	$(PGSINC)/$(GRP)/err.h		\
	$(PGSINC)/$(GRP)/eval_eqn.h	\
	$(PGSINC)/$(GRP)/ff_types.h	\
	$(PGSINC)/$(GRP)/freeform.h	\
	$(PGSINC)/$(GRP)/geodata.h	\
	$(PGSINC)/$(GRP)/geoinfo.h	\
	$(PGSINC)/$(GRP)/index.h	\
	$(PGSINC)/$(GRP)/maxmin.h	\
	$(PGSINC)/$(GRP)/memtrack.h	\
	$(PGSINC)/$(GRP)/menuindx.h	\
	$(PGSINC)/$(GRP)/name_tab.h	\
	$(PGSINC)/$(GRP)/os_utils.h

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

$(PGSINC)/$(GRP)/adtype.h: adtype.h
	cp $? $@

$(PGSINC)/$(GRP)/avl.h: avl.h
	cp $? $@

$(PGSINC)/$(GRP)/avltree.h: avltree.h
	cp $? $@

$(PGSINC)/$(GRP)/data_par.h: data_par.h
	cp $? $@

$(PGSINC)/$(GRP)/databin.h: databin.h
	cp $? $@

$(PGSINC)/$(GRP)/dataview.h: dataview.h
	cp $? $@

$(PGSINC)/$(GRP)/dl_lists.h: dl_lists.h
	cp $? $@

$(PGSINC)/$(GRP)/err.h: err.h
	cp $? $@

$(PGSINC)/$(GRP)/eval_eqn.h: eval_eqn.h
	cp $? $@

$(PGSINC)/$(GRP)/ff_types.h: ff_types.h
	cp $? $@

$(PGSINC)/$(GRP)/freeform.h: freeform.h
	cp $? $@

$(PGSINC)/$(GRP)/geodata.h: geodata.h
	cp $? $@

$(PGSINC)/$(GRP)/geoinfo.h: geoinfo.h
	cp $? $@

$(PGSINC)/$(GRP)/index.h: index.h
	cp $? $@

$(PGSINC)/$(GRP)/maxmin.h: maxmin.h
	cp $? $@

$(PGSINC)/$(GRP)/memtrack.h: memtrack.h
	cp $? $@

$(PGSINC)/$(GRP)/menuindx.h: menuindx.h
	cp $? $@

$(PGSINC)/$(GRP)/name_tab.h: name_tab.h
	cp $? $@

$(PGSINC)/$(GRP)/os_utils.h: os_utils.h
	cp $? $@

