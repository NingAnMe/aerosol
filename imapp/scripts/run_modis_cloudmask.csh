#!/bin/csh

# Run the IMAPP direct broadcast cloud mask software

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
# Also new with this release is the option to create binary, hdf or both types
# of output files.  With this inclusion, the IDL software to convert the binary
# to hdf is no longer needed.  
#

#-------------------------------------------------------------------------------
# SETUP
#-------------------------------------------------------------------------------

# Check arguments
if ($#argv != 4) then
  echo "Usage: run_modis_cloudmask.csh SAT OUTPUT_TYPE FIL1KM OUTDIR"
  echo "where"
  echo "SAT is the satellite platform (Aqua or Terra)"
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

set OUT = $OUTPUT_TYPE

# Still need to create the binary file if you only wish to save the
#  HDF output file.  This is because the binary is used as input to
#  many of the other level 2 products.
if ( $OUTPUT_TYPE == 2 ) then
  set OUT = 3
endif


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
echo "Getting ancillary data for MODIS cloud mask processing"

# get required ancillary data - Reynolds Blended SST
# ----------------------------------------------------------------
# Run script which checks local directory first, and then ftp site
set RSST=`get_anc_rsst_wday.csh $DATE`
# If you can't find a file, set it equal to missing
if ($status != 0) set RSST = 'MISSING'
# Convert backslashes so that they will be properly inserted using sed
set ROISST = `echo ${RSST} | sed s/"\/"/"\\\/"/g`

# NCEP ice concentration file
# ----------------------------------------------------------------
set ICEC = `get_anc_icec.csh $DATE`
# If you can't find a file, set it equal to missing
if ($status != 0) set ICEC = "MISSING"

set ICE_BIN = "MISSING"
set NCEP_ICE_BIN = "MISSING"
# Extract binary Ice file from NCEP Grib file
if ($ICEC != "MISSING") then
   set NCEP_ICE_BIN = ${ICEC:t}.bin
   extract_ncep_ice.csh $ICEC $NCEP_ICE_BIN
endif
if ($NCEP_ICE_BIN == " ") set $NCEP_ICE_BIN = "MISSING"
# Convert backslashes so that they will be properly inserted using sed
set ICE_BIN = `echo ${NCEP_ICE_BIN} | sed s/"\/"/"\\\/"/g`

# NISE snow and ice files
# ----------------------------------------------------------------
set NISE = `get_anc_nise.csh $DATE`
# If you can't find a file, set it equal to missing
if ($status != 0) set NISE = "MISSING"

set NISEN_BIN = "MISSING"
set NISES_BIN = "MISSING"
set NISE_NORTH_BIN = "MISSING"
set NISE_SOUTH_BIN = "MISSING"
# NSIDC snow and ice file
if ($NISE != "MISSING") then
   set NISE_NORTH_BIN = ${NISE:t}_NORTH.bin
   set NISE_SOUTH_BIN = ${NISE:t}_SOUTH.bin
   extract_nsidc_nise.csh $NISE $NISE_NORTH_BIN \
      $NISE_SOUTH_BIN
endif
if ($NISE_NORTH_BIN == "") set $NISE_NORTH_BIN = "MISSING"
if ($NISE_SOUTH_BIN == "") set $NISE_SOUTH_BIN = "MISSING"
# Convert backslashes so that they will be properly inserted using sed
set NISEN_BIN = `echo ${NISE_NORTH_BIN} | sed s/"\/"/"\\\/"/g`
set NISES_BIN = `echo ${NISE_SOUTH_BIN} | sed s/"\/"/"\\\/"/g`

# GDAS1 numerical weather prediction model grid field analysis
# ----------------------------------------------------------------
set GDAS = `get_anc_gdas_gfs.csh $DATE $TIME`
# If you can't find a file, try older gdas files
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


# Find the cloud mask thresholds file
# ----------------------------------------------------------------
set CMTHRES1 = `find ${MODIS_L2_COEFF} -name "thresholds.dat.${SAT}" -print`
if ( $status == 0 ) set CMTHRES = `echo ${CMTHRES1} | sed s/"\/"/"\\\/"/g`

# Find the 1km ecosystem file
# ----------------------------------------------------------------
set ECO1 = `find ${MODIS_L2_COEFF} -name "goge1_2_img" -print`
if ( $status == 0 ) set ECO_FILE = `echo ${ECO1} | sed s/"\/"/"\\\/"/g`

echo $NISEN_BIN $NISES_BIN $ICE_BIN $GDAS_BIN $RSST $CMTHRES $ECO_FILE

# Print start message for processing log
echo ">>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>"
echo "Started MODIS cloud mask processing at "`date`

#
# Set output file names
set OUTFIL      = $ROOT.mod35.img
set OUTFILQA    = $ROOT.mod35qa.img
set OUTFILHDR   = $ROOT.mod35.hdr
set OUTFILQAHDR = $ROOT.mod35qa.hdr
set OUTFILHDF   = $ROOT.mod35.hdf
set DEBUG       = $ROOT.mod35debug.txt

# Create a configuration file from the template
set TEMPLATE = ${MODIS_L2_CFG}/cloudmask.cfg
/bin/sed \
  -e "s/FIL1KM_IMG/${ROOT}.1000m.img/g" \
  -e "s/FIL1KM_HDR/${ROOT}.1000m.hdr/g" \
  -e "s/FILQKM_IMG/${ROOT}.250m.img/g" \
  -e "s/FILQKM_HDR/${ROOT}.250m.hdr/g" \
  -e "s/FILGEO_IMG/${ROOT}.geo.img/g" \
  -e "s/FILGEO_HDR/${ROOT}.geo.hdr/g" \
  -e "s/FILMET_IMG/${ROOT}.met.img/g" \
  -e "s/FILMET_HDR/${ROOT}.met.hdr/g" \
  -e "s/NISE_NORTH/${NISEN_BIN}/g" \
  -e "s/NISE_SOUTH/${NISES_BIN}/g" \
  -e "s/NCEP_ICE/${ICE_BIN}/g" \
  -e "s/NCEP_GDAS/${GDAS_BIN}/g" \
  -e "s/oisst_weekly/${ROISST}/g" \
  -e "s/THRESHOLD_FILE/${CMTHRES}/g" \
  -e "s/ECO_FILE/${ECO_FILE}/g" \
  -e "s/MOD35.img/${OUTFIL}/g" \
  -e "s/MOD35QA.img/${OUTFILQA}/g" \
  -e "s/MOD35.hdr/${OUTFILHDR}/g" \
  -e "s/MOD35QA.hdr/${OUTFILQAHDR}/g" \
  -e "s/MOD35.hdf/${OUTFILHDF}/g" \
  -e "s/debug.dat/${DEBUG}/g" \
  $TEMPLATE > cloudmask.cfg

# Run the cloud mask
cloudmask.exe cloudmask.cfg $SAT $OUT

# Rename the configuration file
/bin/mv cloudmask.cfg $ROOT.mod35.cfg

# Move the HDF output file into the output directory
if ( $OUTPUT_TYPE == 2 || $OUTPUT_TYPE == 3) then
  /bin/mv $OUTFILHDF $OUTDIR
endif

# Copy the binary output and header files into the output directory
if ( $OUTPUT_TYPE == 1 || $OUTPUT_TYPE == 3) then
  /bin/cp $OUTFIL $OUTFILQA $OUTFILHDR $OUTFILQAHDR $OUTDIR
endif

# Move the product and debug files to the product directory
/bin/mv $DEBUG $ROOT.mod35.cfg $OUTDIR

# Print finish message for processing log
echo ">>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>"
echo "Finished cloud mask processing at "`date`
echo
# ---------------------------------------------------------------------------

# Exit gracefully
exit(0)
