#!/bin/csh -f

# Run the IMAPP direct broadcast cloud top property (pressure, temperature,
# emissivity) and cloud phase software 
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
  echo "Usage: run_modis_cloudtop.csh SAT OUTPUT_TYPE FIL1KM OUTDIR"
  echo "where"
  echo "SAT is the satellite platform (Aqua or Terra)"
  echo "OUTPUT_TYPE is the format of the output product file"
  echo "   1 = binary only, 2 = hdf only, 3 = binary and hdf"
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
echo "Getting ancillary data for MODIS cloud top properties processing"

# get required ancillary data - Reynolds Blended SST
# ----------------------------------------------------------------
# Run script which checks local directory first, and then ftp site
set RSST=`get_anc_rsst_wday.csh $DATE`
# If you can't find a file, set it equal to missing
if ($status != 0) set RSST = 'MISSING'
# Convert backslashes so that they will be properly inserted using sed
set ROISST = `echo ${RSST} | sed s/"\/"/"\\\/"/g`

# GDAS1 numerical weather prediction model grid field analysis
# ----------------------------------------------------------------
set GDAS = `get_nearest_gdas.csh $DATE $TIME`
# If you can't find a file, set it equal to missing
if ($status != 0) set GDAS = "MISSING"

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

# Get the Clear Sky Radiance Bias File
# ----------------------------------------------------------------

set MBIAS1 = "MISSING"
set MBIAS1=`get_anc_clearsky.csh $SAT $DATE`
if ($MBIAS1 == "") set $MBIAS1 = "MISSING"
set MBIAS = `echo ${MBIAS1} | sed s/"\/"/"\\\/"/g`

# Find the 1km ecosystem file
# ----------------------------------------------------------------
set ECO1 = `find ${MODIS_L2_COEFF} -name "goge1_2_img" -print`
if ( $status == 0 ) set ECO_FILE = `echo ${ECO1} | sed s/"\/"/"\\\/"/g`

# Prepare the coefficient files for insertion into config file
# ----------------------------------------------------------------
set DRY = `find ${MODIS_L2_COEFF} -name "modisdet.dry.101.v2" -print`
if ( $status == 0 ) set DRY_FILE = `echo ${DRY} | sed s/"\/"/"\\\/"/g`
set OZ = `find ${MODIS_L2_COEFF} -name "modisdet.ozo.101.v2" -print`
if ( $status == 0 ) set OZ_FILE = `echo ${OZ} | sed s/"\/"/"\\\/"/g`
set W1 = `find ${MODIS_L2_COEFF} -name "modisdet.wts.101.v2" -print`
if ( $status == 0 ) set WT1_FILE = `echo ${W1} | sed s/"\/"/"\\\/"/g`
set W2 = `find ${MODIS_L2_COEFF} -name "modisdet.wtl.101.v2" -print`
if ( $status == 0 ) set WT2_FILE = `echo ${W2} | sed s/"\/"/"\\\/"/g`
set WV = `find ${MODIS_L2_COEFF} -name "modisdet.wco.101.v2" -print`
if ( $status == 0 ) set WV_FILE = `echo ${WV} | sed s/"\/"/"\\\/"/g`

echo "CTP COEFF FILES" $DRY_FILE $OZ_FILE $WT1_FILE $WT2_FILE $WV_FILE

#-------------------------------------------------------------------------------
# Print start message for processing log
echo
echo ">>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>"
echo "Started MODIS cloud top properties processing at "`date`

# Set cloud top properties output file names
set OUTCTPFIL   = $ROOT.mod06.img
set OUTCTPQA    = $ROOT.mod06qa.img
set OUTCTPHDR   = $ROOT.mod06.hdr
set OUTCTPQAHDR = $ROOT.mod06qa.hdr
set CTPDEBUG    = $ROOT.mod06debug.txt

# Create a configuration file from the template
set TEMPLATE = ${MODIS_L2_CFG}/cloudtop.cfg
/bin/sed \
  -e "s/FIL1KM_IMG/${ROOT}.1000m.img/g" \
  -e "s/FIL1KM_HDR/${ROOT}.1000m.hdr/g" \
  -e "s/FILGEO_IMG/${ROOT}.geo.img/g" \
  -e "s/FILGEO_HDR/${ROOT}.geo.hdr/g" \
  -e "s/FILMET_IMG/${ROOT}.met.img/g" \
  -e "s/FILMET_HDR/${ROOT}.met.hdr/g" \
  -e "s/MOD35_IMG/${ROOT}.mod35.img/g" \
  -e "s/MOD35_HDR/${ROOT}.mod35.hdr/g" \
  -e "s/NCEP_GDAS/${GDAS1_BIN}/g" \
  -e "s/MODBIAS_FILE/${MBIAS}/g" \
  -e "s/oisst_weekly/${ROISST}/g" \
  -e "s/ECO_FILE/${ECO_FILE}/g" \
  -e "s/DRY_FILE/${DRY_FILE}/g" \
  -e "s/OZ_FILE/${OZ_FILE}/g" \
  -e "s/WT1_FILE/${WT1_FILE}/g" \
  -e "s/WT2_FILE/${WT2_FILE}/g" \
  -e "s/WV_FILE/${WV_FILE}/g" \
  -e "s/MOD06.img/${OUTCTPFIL}/g" \
  -e "s/MOD06QA.img/${OUTCTPQA}/g" \
  -e "s/MOD06.hdr/${OUTCTPHDR}/g" \
  -e "s/MOD06QA.hdr/${OUTCTPQAHDR}/g" \
  -e "s/ctpdebug.dat/${CTPDEBUG}/g" \
  $TEMPLATE > cloudtop.cfg

# Compute the Gregorian date (yyyymmdd) of the input MODIS file
set modis_date = `dateplus.exe -J $DATE`
set MONTH = `echo $modis_date | cut -c5-6`
echo "Month of dataset" ${MONTH}

# Run the cloud top properties algorithm
cloudtop.exe cloudtop.cfg $SAT $MONTH

# Set name of MOD06 HDF file
set OUTCTPHDF = $ROOT.mod06ct.hdf

if ( $OUTPUT_TYPE == 2 || $OUTPUT_TYPE == 3) then
  if (-e  ${MODIS_L2_BIN}/create_fake_mod06.exe) then
    create_fake_mod06.exe $FIL1KM $OUTCTPFIL $OUTCTPHDF
    /bin/mv $OUTCTPHDF $OUTDIR
  endif
endif

# Rename the configuration file
/bin/mv cloudtop.cfg $ROOT.mod06.cfg

# Move the product and debug files to the product directory
/bin/mv $CTPDEBUG $ROOT.mod06.cfg $OUTDIR

# Copy the binary output and header files into the output directory
if ( $OUTPUT_TYPE == 1 || $OUTPUT_TYPE == 3 ) then
  /bin/mv $OUTCTPFIL $OUTCTPHDR $OUTCTPQA $OUTCTPQAHDR $OUTDIR
endif


# Print end message for processing log
echo
echo ">>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>"
echo "Finished cloud top processing at "`date`

# Exit gracefully
exit(0)
