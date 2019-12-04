#!/bin/csh

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
if ($#argv != 6) then
  echo "Usage: run_cloud_optical.csh SAT FIL1KM FILMASK FILGEO FILEMOD06 OUTDIR"
  echo "where"
  echo "SAT is the satellite platform (aqua or terra)"
  echo "FIL1KM is MODIS L1B 1000 meter resolution HDF image file"
  echo "FILMASK is the MODIS L1B 1000 meter Cloud Mask HDF file"
  echo "FILGEO is MODIS L1B 1000 meter resolution geolocation HDF file"
  echo "OUTDIR is the directory where products will be stored"
  exit(-1)
endif

# Extract file names
set SAT = $argv[1]
set FIL1KM = $argv[2]
set FILMASK = $argv[3]
set FILGEO = $argv[4]
set FILMOD06 = $argv[5]
set OUTDIR = $argv[6]

# Get satellite prefix
set sat_id=`basename $FIL1KM | cut -d. -f1`
if ( $sat_id == "t1" ) then
  set PREFIX=MOD
else
  set PREFIX=MYD
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
set INPUTS = "${YEAR} ${DAY} ${TIME}"
echo "Inputs" ${INPUTS}

# Get new filnames
set DAAC_FIL1KM = ${PREFIX}021KM.A${YEAR}${DAY}.${HOUR}${MINUTE}.hdf
set DAAC_FILMASK = ${PREFIX}35_L2.A${YEAR}${DAY}.${HOUR}${MINUTE}.hdf
set DAAC_FILGEO = ${PREFIX}03.A${YEAR}${DAY}.${HOUR}${MINUTE}.hdf
set DAAC_FILMOD06 = ${PREFIX}06_L2.A${YEAR}${DAY}.${HOUR}${MINUTE}.hdf
#Link DB file naming convention to MYD and MOD standard DAAC naming convention
ln -s $FIL1KM $DAAC_FIL1KM
ln -s $FILMASK $DAAC_FILMASK
ln -s $FILGEO $DAAC_FILGEO
ln -s $FILMOD06 $DAAC_FILMOD06

echo ">>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>"
echo "Getting ancillary data for MODIS cloud properties processing"

# get required ancillary data - Reynolds Blended SST
# ----------------------------------------------------------------
# Run script which checks local directory first, and then ftp site
set RSST=`get_anc_rsst_wday.csh $DATE`
# If you can't find a file, set it equal to missing
if ($status != 0) set RSST = 'MISSING'
# Link file to this directory
set ROISST = `basename $RSST`
ln -s ${RSST} .
# Convert backslashes so that they will be properly inserted using sed
set RSST = `echo ${PWD}/${ROISST} | sed s/"\/"/"\\\/"/g`

# NCEP ice concentration file
# ----------------------------------------------------------------
set ICEC = `get_anc_icec.csh $DATE`
# If you can't find a file, set it equal to missing
if ($status != 0) set ICEC = "MISSING"
# Link file to this directory
set GRIBICE = `basename $ICEC`
ln -s ${ICEC} .
# Convert backslashes so that they will be properly inserted using sed
set ICEC = `echo ${PWD}/${GRIBICE} | sed s/"\/"/"\\\/"/g`

# NISE snow and ice files
# ----------------------------------------------------------------
set NISE = `get_anc_nise.csh $DATE`
# If you can't find a file, set it equal to missing
if ($status != 0) set NISE = "MISSING"
# Convert backslashes so that they will be properly inserted using sed
set HDFNISE = `echo ${NISE} | sed s/"\/"/"\\\/"/g`

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

if ($data_day >= $oz_flip) then
   set OZON = `get_anc_toast_ozone.csh $DATE`
   # If you can't find a file, set it equal to missing
   if ($status != 0) set OZON = "MISSING"
else
   set OZON = `get_anc_tovs_ozone.csh $DATE`
   # If you can't find a file, set it equal to missing
   if ($status != 0) set OZON = "MISSING"
endif

# Link file to this directory
set GRIBOZONE = `basename $OZON`
# Convert backslashes so that they will be properly inserted using sed
ln -s ${OZON} .
# Convert backslashes so that they will be properly inserted using sed
set OZON = `echo ${PWD}/${GRIBOZONE} | sed s/"\/"/"\\\/"/g`

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

# Link file to this directory
set FILGDAS1 = `basename $GDAS`
ln -s ${GDAS} .
# Get separate path and file names
# Convert backslashes so that they will be properly inserted using sed
set GDAS = `echo ${PWD}/${FILGDAS1} | sed s/"\/"/"\\\/"/g`

set ALBDATE = `get_surface_albedo_date.csh $DATE`
if ($ALBDATE == "") set $ALBDATE = "MISSING"

echo "ALBDATE" $ALBDATE

# Prepare the coeffcient file names for insertion into config file
# ----------------------------------------------------------------
#IGBP base map
set E1 = `find ${MODIS_L2_COEFF} -follow -name "IGBP.EcoMap.NtoS.2004.149.v004.hdf" \
  -print` 
if ( $status == 0 ) set ECOM = `echo ${E1} | sed s/"\/"/"\\\/"/g`

# Snow Albedo Map
set S1 = `find ${MODIS_L2_COEFF} -follow -name "AlbSnwSts.ByNISE.W90.D90.WS.Hemi.2000-2004.YrAvg.hdf" -print` 
if ( $status == 0 ) set ALBSL = `echo ${S1} | sed s/"\/"/"\\\/"/g`

# Transmittance Map
set T1 = `find ${MODIS_L2_COEFF} -follow -name "Transmittance.hdf" -print` 
if ( $status == 0 ) set TRANW = `echo ${T1} | sed s/"\/"/"\\\/"/g`

# Forward Model Library for Ice
set I1 = `find ${MODIS_L2_COEFF} -follow -name "Library_ice.hdf" -print` 
if ( $status == 0 ) set ICEL = `echo ${I1} | sed s/"\/"/"\\\/"/g`

# Forward Model Library for Water
set W2 = `find ${MODIS_L2_COEFF} -follow -name "Library_water.hdf" -print`
if ( $status == 0 ) set WATERL = `echo ${W2} | sed s/"\/"/"\\\/"/g`

#Locate Surface Albedo files for 5 wavelengths
set A1 =  `find ${MODIS_L2_COEFF} -follow -name "AlbMap.WS.c004.v2.0.00-04.${ALBDATE}.0.659.hdf" -print` 
# unpack the file
set ATEMP1=`basename $A1`
hrepack -i $A1 -o $ATEMP1 -t '*:NONE'	
if ( $status == 0 ) set ALB659 = `echo $ATEMP1 | sed s/"\/"/"\\\/"/g`

set A2 =  `find ${MODIS_L2_COEFF} -follow -name "AlbMap.WS.c004.v2.0.00-04.${ALBDATE}.0.858.hdf" -print` 
# unpack the file
set ATEMP2=`basename $A2`
hrepack -i $A2 -o $ATEMP2 -t '*:NONE'	
if ( $status == 0 ) set ALB858 = `echo $ATEMP2 | sed s/"\/"/"\\\/"/g`

set A3 =  `find ${MODIS_L2_COEFF} -follow -name "AlbMap.WS.c004.v2.0.00-04.${ALBDATE}.1.24.hdf" -print` 
# unpack the file
set ATEMP3=`basename $A3`
hrepack -i $A3 -o $ATEMP3 -t '*:NONE'	
if ( $status == 0 ) set ALB124 = `echo $ATEMP3 | sed s/"\/"/"\\\/"/g`

set A4 =  `find ${MODIS_L2_COEFF} -follow -name "AlbMap.WS.c004.v2.0.00-04.${ALBDATE}.1.64.hdf" -print`
# unpack the file
set ATEMP4=`basename $A4`
hrepack -i $A4 -o $ATEMP4 -t '*:NONE'	
if ( $status == 0 ) set ALB164 = `echo $ATEMP4 | sed s/"\/"/"\\\/"/g`

set A5 =  `find ${MODIS_L2_COEFF} -follow -name "AlbMap.WS.c004.v2.0.00-04.${ALBDATE}.2.13.hdf" -print`
# unpack the file
set ATEMP5=`basename $A5`
hrepack -i $A5 -o $ATEMP5 -t '*:NONE'	
if ( $status == 0 ) set ALB213 = `echo $ATEMP5 | sed s/"\/"/"\\\/"/g`

#-------------------------------------------------------------------------------
# Print start message for processing log
echo
echo ">>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>"
echo "Started MODIS optical properties processing at "`date`

# Create a configuration file from the template
set TEMPLATE = ${MODIS_L2_CFG}/cloud_optical.cfg
/bin/sed \
  -e "s/TRACK_DATE/${INPUTS}/g" \
  -e "s/FIL1KM_HDF/${DAAC_FIL1KM}/g" \
  -e "s/FILMASK_HDF/${DAAC_FILMASK}/g" \
  -e "s/FILGEO_HDF/${DAAC_FILGEO}/g" \
  -e "s/FILMOD06_HDF/${DAAC_FILMOD06}/g" \
  -e "s/ANC_OZONE_NAME/${OZON}/g" \
  -e "s/OISST_NAME/${RSST}/g" \
  -e "s/ANC_ICEC_NAME/${ICEC}/g" \
  -e "s/ANC_NISE_NAME/${HDFNISE}/g" \
  -e "s/GDASBEFORE_NAME/${GDAS}/g" \
  -e "s/GDASAFTER_NAME/${GDAS}/g" \
  -e "s/IGBPMAP_NAME/${ECOM}/g" \
  -e "s/ASNOW_NAME/${ALBSL}/g" \
  -e "s/TRANSLIB_NAME/${TRANW}/g" \
  -e "s/FORICE_NAME/${ICEL}/g" \
  -e "s/FORWAT_NAME/${WATERL}/g" \
  -e "s/SALB659_NAME/${ALB659}/g" \
  -e "s/SALB858_NAME/${ALB858}/g" \
  -e "s/SALB124_NAME/${ALB124}/g" \
  -e "s/SALB164_NAME/${ALB164}/g" \
  -e "s/SALB213_NAME/${ALB213}/g" \
  $TEMPLATE > cloud_optical.cfg

# Run the cloud optical properties software
MOD_PR06OD.exe cloud_optical.cfg

# Rename the configuration file
/bin/mv cloud_optical.cfg $ROOT.mod06od.cfg

# Move the output files into the output directory
/bin/mv $FILMOD06 $ROOT.mod06od.cfg $OUTDIR

# Print finish message for processing log
echo
echo ">>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>"
echo "Finished cloud optical properties processing at "`date`
#-------------------------------------------------------------------------------

# Exit gracefully
exit(0)
