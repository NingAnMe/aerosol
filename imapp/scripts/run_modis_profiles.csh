#!/bin/csh -f

# Run the IMAPP MODIS atmopheric profiles (vertical profiles of
# temperature and moisture plus total precipitable water vapor
# and stability indices.
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
# The option to save both the binary and/or hdf files in now included
# with this release. 
#
#-------------------------------------------------------------------------------
# SETUP
#-------------------------------------------------------------------------------

# Check arguments
if ($#argv != 4) then
  echo "Usage: run_modis_profiles.csh SAT OUTPUT_TYPE FIL1KM OUTDIR"
  echo "where"
  echo "SAT is the satellite platform (aqua or terra)"
  echo "OUTPUT_TYPE is the format of the output product file"
  echo "  1 = binary only, 2 = hdf only, 3 = binary and hdf"
  echo "FIL1KM is MODIS L1B 1000 meter resolution HDF image file"
  echo "OUTDIR is the directory where products will be stored"
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

# Get year and date for ancillary data extraction
set YR     = `echo $FIL1KM:t | cut -c4-5`
set YEAR   = `expr 2000 + $YR`
set DAY    = `echo $FIL1KM:t | cut -c 6-8`
set HOUR   = `echo $FIL1KM:t | cut -c 10-11`
set MINUTE = `echo $FIL1KM:t | cut -c 12-13`

set DATE   =  ${YEAR}${DAY}
set TIME   = ${HOUR}${MINUTE}

echo ">>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>"
echo "Getting ancillary data for MODIS profiles processing"

# GDAS1 numerical weather prediction model grid field analysis
# ----------------------------------------------------------------
set GDAS = `get_anc_gdas_gfs.csh $DATE $TIME`
# If you can't find a file, try and older gdas file.
if ($status != 0) then
  set GDAS = `get_nearest_gdas.csh $DATE $TIME`
  if ($status != 0) then
    set GDAS = "MISSING"
  endif
endif

set GDAS1_BIN = "MISSING"
set GDAS_BIN = "MISSING"
# GDAS or GFS file
if ($GDAS != "MISSING") then
   set GDAS1_BIN = ${GDAS:t}.bin
   extract_ncep_gdas1.csh $GDAS $GDAS1_BIN
endif
if ($GDAS1_BIN == "") set $GDAS1_BIN = "MISSING"
# Convert backslashes so that they will be properly inserted using sed
set GDAS_BIN = `echo ${GDAS1_BIN} | sed s/"\/"/"\\\/"/g`

# Prepare the coeffcient file names for insertion into config file
# ----------------------------------------------------------------
set RF = `find ${MODIS_L2_COEFF} -name "MODIS_REGCOEF_FACTORS.${SAT}.v6.le" \
    -print`
if ( $status == 0 ) set REG_FILE = `echo ${RF} | sed s/"\/"/"\\\/"/g`
set VAF = `find ${MODIS_L2_COEFF} -name "MODIS_senzen.bin" -print`
if ( $status == 0 ) set VAF_FILE = `echo ${VAF} | sed s/"\/"/"\\\/"/g`
set BIAS = `find ${MODIS_L2_COEFF} -name "${SAT}_bias.dat.v6" -print`
if ( $status == 0 ) set BIAS_FILE = `echo ${BIAS} | sed s/"\/"/"\\\/"/g`
set DECT = `find ${MODIS_L2_COEFF} -name "${SAT}_det.dat.v2" -print`
if ( $status == 0 ) set DECT_FILE = `echo ${DECT} | sed s/"\/"/"\\\/"/g`


#-------------------------------------------------------------------------------
# Print start message for processing log
echo
echo ">>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>"
echo "Started MODIS atmospheric profiles processing at "`date`

# Set cloud top properties output file names
set OUTATMFIL   = $ROOT.mod07.img
set OUTATMHDR   = $ROOT.mod07.hdr
set ATMDEBUG    = $ROOT.mod07debug.txt

# Create a configuration file from the template
set TEMPLATE = ${MODIS_L2_CFG}/profiles.cfg
/bin/sed \
  -e "s/FIL1KM_IMG/${ROOT}.1000m.img/g" \
  -e "s/FIL1KM_HDR/${ROOT}.1000m.hdr/g" \
  -e "s/FILGEO_IMG/${ROOT}.geo.img/g" \
  -e "s/FILGEO_HDR/${ROOT}.geo.hdr/g" \
  -e "s/FILMET_IMG/${ROOT}.met.img/g" \
  -e "s/FILMET_HDR/${ROOT}.met.hdr/g" \
  -e "s/REG_FILE/${REG_FILE}/g" \
  -e "s/VAF_FILE/${VAF_FILE}/g" \
  -e "s/BIASFILE/${BIAS_FILE}/g" \
  -e "s/DETECTORFILE/${DECT_FILE}/g" \
  -e "s/MOD35_IMG/${ROOT}.mod35.img/g" \
  -e "s/MOD35_HDR/${ROOT}.mod35.hdr/g" \
  -e "s/NCEP_GDAS/${GDAS1_BIN}/g" \
  -e "s/MOD07.img/${OUTATMFIL}/g" \
  -e "s/MOD07.hdr/${OUTATMHDR}/g" \
  -e "s/profiles_debug.dat/${ATMDEBUG}/g" \
  $TEMPLATE > profiles.cfg

# Get current month for use as input - needed to determine correct coefficients
# Compute the Gregorian date (yyyymmdd) of the input MODIS file
set modis_date = `dateplus.exe -J $DATE`
set MONTH = `echo $modis_date | cut -c5-6`
echo "Month of dataset" ${MONTH}

# Run the cloud top properties algorithm
profiles.exe profiles.cfg $SAT $MONTH

# Set name of MOD07 HDF file
set OUTATMHDF = $ROOT.mod07.hdf

if ( $OUTPUT_TYPE == 2 || $OUTPUT_TYPE == 3) then
  if (-e  ${MODIS_L2_BIN}/create_fake_mod07.exe) then
    create_fake_mod07.exe $FIL1KM $OUTATMFIL $OUTATMHDF
    /bin/mv $OUTATMHDF $OUTDIR
  endif
endif

# Rename the configuration file
/bin/mv profiles.cfg $ROOT.mod07.cfg

# Move the product and debug files to the product directory
/bin/mv $ATMDEBUG $ROOT.mod07.cfg $OUTDIR

# Copy the binary output and header files into the output directory
if ( $OUTPUT_TYPE == 1 || $OUTPUT_TYPE == 3 ) then
  /bin/mv $OUTATMFIL $OUTATMHDR $OUTDIR
endif

# Print finish message for processing log
echo
echo ">>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>"
echo "Finished atmospheric profiles processing at "`date`
#-------------------------------------------------------------------------------

# Exit gracefully
exit(0)
