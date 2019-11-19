#-------------------------------------------------------------------------#
#                                                                         #
#  COPYRIGHT(C) 2000, Raytheon System Company, its vendors,		  #
#  and suppliers.  ALL RIGHTS RESERVED.                                   #
#                                                                         #
#-------------------------------------------------------------------------#
#----------------------------------------------------------------------------
# file:		makefile for SDP Toolkit header files
#
# author:  Guru Tej S. Khalsa / Applied Research Corporation
#
# notes:
#	This file is intended for internal use by ECS CM processes only.
#
#----------------------------------------------------------------------------

# force make to use the 'sh' shell
SHELL = /bin/sh
MFLAGS2= -f makefile.CM.cpp
# list of header files to be moved to stage area
HEADER_FILES= \
	$(PGSINC)/PGS_AA.f		\
	$(PGSINC)/PGS_AA.h		\
	$(PGSINC)/PGS_AA_Global.h	\
	$(PGSINC)/PGS_AA_Tools.h	\
	$(PGSINC)/PGS_CBP.f		\
	$(PGSINC)/PGS_CBP.h		\
	$(PGSINC)/PGS_CSC.f		\
	$(PGSINC)/PGS_CSC.h		\
	$(PGSINC)/PGS_CUC.h		\
	$(PGSINC)/PGS_DEM.f		\
	$(PGSINC)/PGS_DEM.h		\
	$(PGSINC)/PGS_EPH.h		\
	$(PGSINC)/PGS_GCT.f		\
	$(PGSINC)/PGS_GCT.h		\
	$(PGSINC)/PGS_IO.f		\
	$(PGSINC)/PGS_IO.h		\
	$(PGSINC)/PGS_IO_Gen.h		\
	$(PGSINC)/PGS_IO_Gen_Wrap.h	\
	$(PGSINC)/PGS_IO_L0.h		\
	$(PGSINC)/PGS_IO_L0_Wrap.h	\
	$(PGSINC)/PGS_MEM.h		\
	$(PGSINC)/PGS_MEM1.h		\
	$(PGSINC)/PGS_MET.f		\
	$(PGSINC)/PGS_MET.h		\
	$(PGSINC)/PGS_PC.f		\
	$(PGSINC)/PGS_PC.h		\
	$(PGSINC)/PGS_PC_Prototypes.h	\
	$(PGSINC)/PGS_SIM.h		\
	$(PGSINC)/PGS_SMF.f		\
	$(PGSINC)/PGS_SMF.h		\
	$(PGSINC)/PGS_TD.f		\
	$(PGSINC)/PGS_TD.h		\
	$(PGSINC)/PGS_TYPES.h		\
	$(PGSINC)/PGS_math.h		\
	$(PGSINC)/cfortran.h		


#
# targets
#
default:
	@echo "This file is for use by ECS Configuration Management processes only."

stage: message $(HEADER_FILES) subgroups

message:
	@echo ""
	@echo "staging SDP Toolkit header files to $(PGSINC) ..."

subgroups: cuc dcw ff fw

cuc:
	@cd CUC; $(MAKE) $(MFLAGS) -f makefile.CM stage

dcw:
	@cd DCW; $(MAKE) $(MFLAGS) -f makefile.CM stage

ff:
	@cd FF; $(MAKE) $(MFLAGS) -f makefile.CM stage

fw:
	@cd FW; $(MAKE) $(MFLAGS) -f makefile.CM stage


# individual header files
# each header file must be listed here with its own target

$(PGSINC)/PGS_AA.f: PGS_AA.f
	cp $? $@

$(PGSINC)/PGS_AA.h: PGS_AA.h
	cp $? $@

$(PGSINC)/PGS_AA_Global.h: PGS_AA_Global.h
	cp $? $@

$(PGSINC)/PGS_AA_Tools.h: PGS_AA_Tools.h
	cp $? $@

$(PGSINC)/PGS_CBP.f: PGS_CBP.f
	cp $? $@

$(PGSINC)/PGS_CBP.h: PGS_CBP.h
	cp $? $@

$(PGSINC)/PGS_CSC.f: PGS_CSC.f
	cp $? $@

$(PGSINC)/PGS_CSC.h: PGS_CSC.h
	cp $? $@

$(PGSINC)/PGS_CUC.h: PGS_CUC.h
	cp $? $@

$(PGSINC)/PGS_DEM.f: PGS_DEM.f
	cp $? $@

$(PGSINC)/PGS_DEM.h: PGS_DEM.h
	cp $? $@

$(PGSINC)/PGS_EPH.h: PGS_EPH.h
	cp $? $@

$(PGSINC)/PGS_GCT.f: PGS_GCT.f
	cp $? $@

$(PGSINC)/PGS_GCT.h: PGS_GCT.h
	cp $? $@

$(PGSINC)/PGS_IO.f: PGS_IO.f
	cp $? $@

$(PGSINC)/PGS_IO.h: PGS_IO.h
	cp $? $@

$(PGSINC)/PGS_IO_Gen.h: PGS_IO_Gen.h
	cp $? $@

$(PGSINC)/PGS_IO_Gen_Wrap.h: PGS_IO_Gen_Wrap.h
	cp $? $@

$(PGSINC)/PGS_IO_L0.h: PGS_IO_L0.h
	cp $? $@

$(PGSINC)/PGS_IO_L0_Wrap.h: PGS_IO_L0_Wrap.h
	cp $? $@

$(PGSINC)/PGS_MEM.h: PGS_MEM.h
	cp $? $@

$(PGSINC)/PGS_MEM1.h: PGS_MEM1.h
	cp $? $@

$(PGSINC)/PGS_MET.f: PGS_MET.f
	cp $? $@

$(PGSINC)/PGS_MET.h: PGS_MET.h
	cp $? $@

$(PGSINC)/PGS_PC.f: PGS_PC.f
	cp $? $@

$(PGSINC)/PGS_PC.h: PGS_PC.h
	cp $? $@

$(PGSINC)/PGS_PC_Prototypes.h: PGS_PC_Prototypes.h
	cp $? $@

$(PGSINC)/PGS_SIM.h: PGS_SIM.h
	cp $? $@

$(PGSINC)/PGS_SMF.f: PGS_SMF.f
	cp $? $@

$(PGSINC)/PGS_SMF.h: PGS_SMF.h
	cp $? $@

$(PGSINC)/PGS_TD.f: PGS_TD.f
	cp $? $@

$(PGSINC)/PGS_TD.h: PGS_TD.h
	cp $? $@

$(PGSINC)/PGS_TYPES.h: PGS_TYPES.h
	cp $? $@

$(PGSINC)/PGS_math.h: PGS_math.h
	cp $? $@

$(PGSINC)/cfortran.h: cfortran.h
	cp $? $@

