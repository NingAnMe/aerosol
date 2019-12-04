#!/bin/csh -f
#
# Adapted for NIR water vapor by Peter Albert September 2004
# peter.albert@gmx.dw
#
# Run the IMAPP MODIS algorithm for retrieving total water vapor
# from visilble channels.
#
# You must first set up the environmental variables to match your system and
# directory structure prior to execution.  These variables can be 
# automatically set by editing the one of the scripts found in the 
# imapp_modisl2/env directory.  Set the top level
# directory for the modis level 2 processing path and then type:
#  source imapp_modisl2.bash_env for bash shells or
#  source imapp_modisl2.csh_env for csh shells.
#
# The variables that need to be set to run this code are:
#
# MODIS_L2_HOME - home directory for MODIS Level 2 processing
# LOCAL_ANC_DIR - Local directory for ancillary data.  The default is:
#                 ${MODIS_L2_HOME}/ancillary
# REMOTE_ANC_DIR - Remote directory from which to fetch ancillary data if
#                 it is not found locally.  The default is: 
#                 ftp://aqua.ssec.wisc.edu/pub/terra/ancillary
# MODIS_L2_CFG   - Directory where the configuration files can be found. The 
#                 default is:  ${MODIS_L2_HOME}/config
# MODIS_L2_COEFF - Directory where the software coefficients can be found. The
#                 default is: ${MODIS_L2_HOME}/coeff
# MODIS_L2_BIN   - Directory where the statically linked pre-compiled binaries
#                 can be found.  The default is: ${MODIS_L2_HOME}/bin
#  In addtion, sourcing the bash or csh environmental scripts will set the path
#  so that the binaries can be executed from any directory.
# PATH=.:${MODIS_L2_BIN}:${MODIS_L2_HOME}/scripts:${PATH} 
#
# Also new with this release is the option to create binary, hdf or both types
# of output files.  With this inclusion, the IDL software to convert the binary
# to hdf is no longer needed.  
#
#-------------------------------------------------------------------------------
# SETUP
#-------------------------------------------------------------------------------

# Check arguments
if ($#argv != 4) then
  echo "Usage: modis_wvnir.csh SAT OUTPUT_TYPE FIL1KM OUTDIR"
  echo "where"
  echo "SAT is the MODIS platform name (Terra or Aqua)"
  echo "OUTPUT_TYPE is the format of the output product file"
  echo "  1 = binary only, 2 = hdf only, 3 = binary and hdf"
  echo "FIL1KM is the MODIS L1B 1000 meter resolution radiance flat file"
  echo "OUTDIR is the directory where the products will be stored"
  exit(-1)
endif

# Extract file names
set SAT = $argv[1]
set OUTPUT_TYPE = $argv[2]
set FIL1KM = $argv[3]
set OUTDIR = $argv[4]

# Set root of output file name (e.g. 't1.02001.1815')
set ROOT = $FIL1KM:r
set ROOT = $ROOT:r
set ROOT = $ROOT:t

#------------------------
# RUN THE WVNIR ALGORITHM
#------------------------

# Print start message for processing log
echo
echo ">>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>"
echo "Started IMAPP MODIS NIR water vapor processing at "`date`

# Set WVNIR output file names
set OUTNIRWVFIL   = $ROOT.wvnir.img
set OUTNIRWVHDR   = $ROOT.wvnir.hdr
set OUTWVNIRHDF   = $ROOT.wvnir.hdf

# Create a configuration file from the template
set TEMPLATE = ${MODIS_L2_CFG}/wvnir.cfg
/bin/sed \
  -e "s/FIL1KM_IMG/${ROOT}.1000m.img/g" \
  -e "s/FIL1KM_HDR/${ROOT}.1000m.hdr/g" \
  -e "s/FILGEO_IMG/${ROOT}.geo.img/g" \
  -e "s/FILGEO_HDR/${ROOT}.geo.hdr/g" \
  -e "s/FILWVNIR_IMG/${OUTNIRWVFIL}/g" \
  -e "s/FILWVNIR_HDR/${OUTNIRWVHDR}/g" \
  -e "s/FILWVNIR_HDF/${OUTWVNIRHDF}/g" \
  $TEMPLATE > wvnir.cfg

# Run the wvnir executable
wvnir.exe wvnir.cfg $OUTPUT_TYPE

# Move the output files to the product directory
/bin/mv wvnir.cfg $OUTDIR/${ROOT}.wvnir.cfg

if ( $OUTPUT_TYPE == 1 || $OUTPUT_TYPE == 3) then
    /bin/mv $OUTNIRWVFIL $OUTNIRWVHDR $OUTDIR
endif

if ( $OUTPUT_TYPE == 2 || $OUTPUT_TYPE == 3) then
    /bin/mv $OUTWVNIRHDF $OUTDIR
endif

#-------------------------------------------------------------------------------
# CLEANUP AND EXIT
#-------------------------------------------------------------------------------

# Print end message for processing log
echo
echo ">>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>"
echo "Finished IMAPP MODIS NIR water vapor processing at "`date`

# Exit gracefully
exit(0)
