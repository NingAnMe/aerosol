#!/bin/csh -f

# Run the IMAPP flatfile extractors which strip out radiances and reflectances
# from the input L1B hdf files.  It writes the output into a binary file
# accompanied by a header file in ENVI compatible format.  It is used as
# an input to the IMAPP L2 processing stream.

# You must first set up the environmental variables to match your system and
# directory structure prior to execution.  These variables can be 
# automatically set by editing the one of the scripts found in the 
# imapp_modisl2/env directory.  Set the top level
# directory for the modis level 2 processing path and then type:
#  source imapp_modisl2.bash_env for bash shells or
#  source imapp_modisl2.csh_env for csh shells.

# The variables that need to be set to run this code are:

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
#
#-------------------------------------------------------------------------------
# SETUP
#-------------------------------------------------------------------------------

# Check arguments
if ($#argv != 5) then
  echo "Usage: run_modis_flatfile.csh SAT FIL1KM FILHKM FILQKM FILGEO"
  echo "where"
  echo "SAT is the satellite platform (Aqua or Terra)"
  echo "FIL1KM is MODIS L1B 1000 meter resolution HDF image file"
  echo "FILHKM is MODIS L1B 500  meter resolution HDF image file"
  echo "FILQKM is MODIS L1B 250  meter resolution HDF image file"
  echo "FILGEO is MODIS L1B 1000 meter resolution HDF geolocation file"
  echo "   from which all other binary files will be named"
  exit(-1)
endif

# Extract file names
set SAT = $argv[1]
set FIL1KM = $argv[2]
set FILHKM = $argv[3]
set FILQKM = $argv[4]
set FILGEO = $argv[5]

# Set root of output file name (e.g. 't1.02001.1815')
set ROOT = $FIL1KM:r
set ROOT = $ROOT:r
set ROOT = $ROOT:t

#-------------------------------------------------------------------------------
# EXTRACT FLAT FILES
#-------------------------------------------------------------------------------

# Print start message for processing log
echo
echo ">>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>"
echo "Started MODIS flat file extraction at "`date`

# Extract flat files
modis_extract_1km.exe $FIL1KM $SAT ${ROOT}.1000m.img ${ROOT}.1000m.hdr
modis_extract_hkm.exe $FILHKM $SAT ${ROOT}.500m.img  ${ROOT}.500m.hdr
modis_extract_qkm.exe $FILQKM $SAT ${ROOT}.250m.img  ${ROOT}.250m.hdr
modis_extract_geo.exe $FILGEO $SAT ${ROOT}.geo.img   ${ROOT}.geo.hdr
modis_extract_met.exe $FIL1KM $SAT ${ROOT}.met.img   ${ROOT}.met.hdr

echo "Finished flatfile extraction at "`date`
echo ">>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>"
echo
#-------------------------------------------------------------------------------


# Exit gracefully
exit(0)
