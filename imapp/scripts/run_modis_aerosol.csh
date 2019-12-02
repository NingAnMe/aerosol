#!/bin/csh -f

# Run the IMAPP MODIS aerosol retrieval (optical depth and size fraction)
# software (MOD04).

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
# The option to save both the binary and/or hdf files in now included
# with this release.  
#
#-------------------------------------------------------------------------------
# SETUP
#-------------------------------------------------------------------------------

# Check arguments
if ($#argv != 4) then
  echo "Usage: run_modis_aerosol.csh SAT OUTPUT_TYPE FIL1KM OUTDIR"
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
echo "Getting ancillary data for MODIS aerosol processing"

# GDAS1 numerical weather prediction model grid field analysis
# ----------------------------------------------------------------
set GDAS = `get_anc_gdas_gfs.csh $DATE $TIME`
# If you can't find a file, then try an older gdas file.
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

# TOVS or TOAST ozone file
# ----------------------------------------------------------------
#    Decide on whether to look for the tovs or toast ozone grib file.
#    This is solely dependent upon the date:
#          TOVS  1999 through March 31 2005
#          TOAST April 2005 to the present
# Get baseline date of TOVS / TOAST changeover 
set oz_flip = `dateplus.exe -u 20050401`
set greg_day = `dateplus.exe -J ${YEAR}${DAY}`
set data_day = `dateplus.exe -u ${greg_day}`

echo "oz_flip" ${oz_flip}
echo "greg_day" ${greg_day}
echo "data_day" ${data_day}

if ( $data_day >= $oz_flip ) then
   set OZON = `get_anc_toast_ozone.csh $DATE`
   # If you can't find a file, set it equal to missing
   if ($status != 0) set OZON = "MISSING"
else
   set OZON = `get_anc_tovs_ozone.csh $DATE`
   # If you can't find a file, set it equal to missing
   if ($status != 0) set OZON = "MISSING"
endif
if ($OZON != "MISSING" || $OZON != "") then
   set OZONE_BIN = ${OZON:t}.bin
   extract_tovs_ozone.csh $OZON $OZONE_BIN
else
   set OZONE_BIN = "MISSING"
endif
# Convert backslashes so that they will be properly inserted using sed
set OZN = `echo ${OZONE_BIN} | sed s/"\/"/"\\\/"/g`


# Find coefficient files - start search in OPSDIR
# ----------------------------------------------------------------
set L1 = `find ${MODIS_L2_COEFF} -name "lookup_land_w0466.v1" -print`
if ( $status == 0 ) set L466 = `echo ${L1} | sed s/"\/"/"\\\/"/g`
set L2 = `find ${MODIS_L2_COEFF} -name "lookup_land_w0553.v1" -print`
if ( $status == 0 ) set L553 = `echo ${L2} | sed s/"\/"/"\\\/"/g`
set L3 = `find ${MODIS_L2_COEFF} -name "lookup_land_w0644.v1" -print`
if ( $status == 0 ) set L644 = `echo ${L3} | sed s/"\/"/"\\\/"/g`
set L4 = `find ${MODIS_L2_COEFF} -name "lookup_land_w2119.v1" -print`
if ( $status == 0 ) set L213 = `echo ${L4} | sed s/"\/"/"\\\/"/g`

set LM = `find ${MODIS_L2_COEFF} -name "aerosol_land_map.v2" -print`
if ( $status == 0 ) set LMAP = `echo ${LM} | sed s/"\/"/"\\\/"/g`
set MO = `find ${MODIS_L2_COEFF} -name "mod04ocean.in.v1" -print`
if ( $status == 0 ) set MOCN = `echo ${MO} | sed s/"\/"/"\\\/"/g`
set S1 = `find ${MODIS_L2_COEFF} -name "small_v2c1.dat.v3" -print`
if ( $status == 0 ) set SM1 = `echo ${S1} | sed s/"\/"/"\\\/"/g`
set S2 = `find ${MODIS_L2_COEFF} -name "small_v1c1.dat.v2" -print`
if ( $status == 0 ) set SM2 = `echo ${S2} | sed s/"\/"/"\\\/"/g`
set S3 = `find ${MODIS_L2_COEFF} -name "small_v3c1.dat.v2" -print`
if ( $status == 0 ) set SM3 = `echo ${S3} | sed s/"\/"/"\\\/"/g`


set B1 = `find ${MODIS_L2_COEFF} -name "big_v2c1.dat.v5" -print`
if ( $status == 0 ) set BG1 = `echo ${B1} | sed s/"\/"/"\\\/"/g`
set B2 = `find ${MODIS_L2_COEFF} -name "big_v1c1.dat.v2" -print`
if ( $status == 0 ) set BG2 = `echo ${B2} | sed s/"\/"/"\\\/"/g`
set B3 = `find ${MODIS_L2_COEFF} -name "big_v3c1.dat.v2" -print`
if ( $status == 0 ) set BG3 = `echo ${B3} | sed s/"\/"/"\\\/"/g`

#-------------------------------------------------------------------------------
# Print start message for processing log
echo
echo ">>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>"
echo "Started MODIS aerosol processing at "`date`

# Set cloud top properties output file names
set OUTAERFIL   = $ROOT.mod04.img
set OUTAERHDR   = $ROOT.mod04.hdr
set AERDEBUG    = $ROOT.mod04debug.txt

set TEMPLATE = ${MODIS_L2_CFG}/aerosol.cfg
/bin/sed \
  -e "s/FIL1KM_IMG/${ROOT}.1000m.img/g" \
  -e "s/FIL1KM_HDR/${ROOT}.1000m.hdr/g" \
  -e "s/FILHKM_IMG/${ROOT}.500m.img/g" \
  -e "s/FILHKM_HDR/${ROOT}.500m.hdr/g" \
  -e "s/FILQKM_IMG/${ROOT}.250m.img/g" \
  -e "s/FILQKM_HDR/${ROOT}.250m.hdr/g" \
  -e "s/FILGEO_IMG/${ROOT}.geo.img/g" \
  -e "s/FILGEO_HDR/${ROOT}.geo.hdr/g" \
  -e "s/FILMET_IMG/${ROOT}.met.img/g" \
  -e "s/FILMET_HDR/${ROOT}.met.hdr/g" \
  -e "s/MOD35_IMG/${ROOT}.mod35.img/g" \
  -e "s/MOD35_HDR/${ROOT}.mod35.hdr/g" \
  -e "s/MOD35QA_IMG/${ROOT}.mod35qa.img/g" \
  -e "s/MOD35QA_HDR/${ROOT}.mod35qa.hdr/g" \
  -e "s/L466/${L466}/g" \
  -e "s/L553/${L553}/g" \
  -e "s/L644/${L644}/g" \
  -e "s/L213/${L213}/g" \
  -e "s/LMAP/${LMAP}/g" \
  -e "s/MOCN/${MOCN}/g" \
  -e "s/SM1/${SM1}/g" \
  -e "s/SM2/${SM2}/g" \
  -e "s/SM3/${SM3}/g" \
  -e "s/BG1/${BG1}/g" \
  -e "s/BG2/${BG2}/g" \
  -e "s/BG3/${BG3}/g" \
  -e "s/NCEP_GDAS/${GDAS1_BIN}/g" \
  -e "s/TOVS_OZONE/${OZN}/g" \
  -e "s/MOD04.img/${OUTAERFIL}/g" \
  -e "s/MOD04.hdr/${OUTAERHDR}/g" \
  -e "s/aerosol_debug.dat/${AERDEBUG}/g" \
  $TEMPLATE > aerosol.cfg

# Get current month for use as input - needed to determine correct coefficients
# Compute the Gregorian date (yyyymmdd) of the input MODIS file
set modis_date = `dateplus.exe -J $DATE`
set MONTH = `echo $modis_date | cut -c5-6`
echo "Month of dataset" ${MONTH}

# Run the aerosol algorithm
aerosol.exe aerosol.cfg $SAT $MONTH

# Set name of MOD04 HDF file
set OUTAERHDF = $ROOT.mod04.hdf

if ( $OUTPUT_TYPE == 2 || $OUTPUT_TYPE == 3) then
  if (-e  ${MODIS_L2_BIN}/create_fake_mod04.exe) then
    create_fake_mod04.exe $FIL1KM $OUTAERFIL $OUTAERHDF
    /bin/mv $OUTAERHDF $OUTDIR
  endif
endif

# Rename the configuration file
/bin/mv aerosol.cfg $ROOT.mod04.cfg

# Move the product and debug files to the product directory
/bin/mv $AERDEBUG $ROOT.mod04.cfg $OUTDIR

# Copy the binary output and header files into the output directory
if ( $OUTPUT_TYPE == 1 || $OUTPUT_TYPE == 3 ) then
  /bin/mv $OUTAERFIL $OUTAERHDR $OUTDIR
endif

# Print finish message for processing log
echo
echo ">>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>"
echo "Finished MODIS aerosol processing at "`date`
#-------------------------------------------------------------------------------

# Exit gracefully
exit(0)
