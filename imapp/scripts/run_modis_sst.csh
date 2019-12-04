#!/bin/csh -f
#
# Run the IMAPP MODIS sea surface temperature software.
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
#------------------------------------------------------------------------------
# Check arguments
if ($#argv != 4) then
  echo "Usage: run_modis_sst.csh SAT OUTPUT_TYPE FIL1KM OUTDIR"
  echo "where"
  echo "SAT    is the satellite name (Aqua or Terra)"
  echo "OUTPUT_TYPE is the format of the output product file"
  echo "   1 = binary only, 2 = hdf only, 3 = binary and hdf"
  echo "FIL1KM is the MODIS L1B 1000 meter resolution radiance flat file"
  echo "OUTDIR is the directory where the products will be stored"
  exit(-1)
endif
#------------------------------------------------------------------------------

# Extract file names
set SAT    = $argv[1]
set OUTPUT_TYPE = $argv[2]
set FIL1KM = $argv[3]
set OUTDIR = $argv[4]

# Set root of output file name (e.g. 't1.02001.1815')
set ROOT = $FIL1KM:r
set ROOT = $ROOT:r
set ROOT = $ROOT:t

# Get year and date for ancillary data extraction
set YR   = `echo $FIL1KM:t | cut -c4-5`
set YEAR   = `expr 2000 + $YR`
set DAY    = `echo $FIL1KM:t | cut -c 6-8`
set DATE   =  ${YEAR}${DAY}

echo ">>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>"
echo "Getting ancillary data for MODIS sst processing"

# get required ancillary data - Reynolds Blended SST
# ----------------------------------------------------------------
# Run script which checks local directory first, and then ftp site
set RSST=`get_anc_rsst_wday.csh $DATE`
# If you can't find a file, set it equal to missing
if ($status != 0) set RSST = "MISSING"

# Convert backslashes so that they will be properly inserted using sed
set ROISST = `echo ${RSST} | sed s/"\/"/"\\\/"/g`

# Print start message for processing log
echo ""
echo ">>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>"
echo "Started MODIS sea surface temperature processing at "`date`

# Set sea surface temperature output file names
set OUTSSTFIL = $ROOT.mod28.img
set OUTSSTHDR = $ROOT.mod28.hdr
set OUTSSTHDF   = $ROOT.mod28.hdf
set SSTDEBUG  = $ROOT.mod28debug.txt

# Create a configuration file from the template
set TEMPLATE = ${MODIS_L2_CFG}/sst.cfg
/bin/sed \
  -e "s/FIL1KM_IMG/${ROOT}.1000m.img/g" \
  -e "s/FIL1KM_HDR/${ROOT}.1000m.hdr/g" \
  -e "s/FILGEO_IMG/${ROOT}.geo.img/g" \
  -e "s/FILGEO_HDR/${ROOT}.geo.hdr/g" \
  -e "s/MOD28.img/${OUTSSTFIL}/g" \
  -e "s/MOD28.hdr/${OUTSSTHDR}/g" \
  -e "s/MOD28.hdf/${OUTSSTHDF}/g" \
  -e "s/sstdebug.dat/${SSTDEBUG}/g" \
  -e "s/oisst_weekly/${ROISST}/g" \
  $TEMPLATE > sst.cfg

# Run the sea surface temperature algorithm
sst.exe sst.cfg $SAT $OUTPUT_TYPE

# Rename the configuration file
/bin/mv sst.cfg $ROOT.mod28.cfg

# Set name of SST HDF file
set OUTSSTHDF = $ROOT.mod28.hdf

# Move the output files to the product directory
/bin/mv $SSTDEBUG $ROOT.mod28.cfg $OUTDIR

if ( $OUTPUT_TYPE == 1 || $OUTPUT_TYPE == 3) then
    /bin/mv $OUTSSTFIL $OUTSSTHDR $OUTDIR
endif

if ( $OUTPUT_TYPE == 2 || $OUTPUT_TYPE == 3) then
    /bin/mv $OUTSSTHDF $OUTDIR
endif

# Print end message for processing log
echo ""
echo ">>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>"
echo "Finished MODIS sea surface temperature processing at "`date`

# Exit gracefully
exit(0)
