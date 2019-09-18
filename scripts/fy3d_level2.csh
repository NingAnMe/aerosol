#!/bin/csh

#-----------------------------------------------------------------------
# Script for MODIS Level-2 processing using direct broadcast data
#  Default directories are set within the env files located within the
#  $MODIS_L2_HOME/env sub-directory
#
#   Environmental variables that need to be set prior to execution of this
#    script:
#
#   MODIS_L2_HOME
#   LOCAL_ANC_DIR
#   REMOTE_ANC_DIR
#   MODIS_L2_CFG
#   MODIS_L2_COEFF
#   MODIS_L2_BIN
#   MODIS_L2_IMAGES
#   MODIS_L2_XSIZE
#   MODIS_L2_YSIZE
#   http_proxy
#   OUTPUT_TYPE
#
# In addition, the PATH environmental variable must be set to include the 
# ${MODIS_L2_BIN}  and  ${MODIS_L2_HOME}/scripts 
# directories.  Again, these can be set by sourcing the env file located
# within the $MODIS_L2_HOME/env sub-directory.
# 
# Finally, if you are using the McIDAS software to create images, you can
# set the output file size (XSIZE and YSIZE).  The software will 
# automatically scale the data to your size choice.  However, it is not 
# recommended to choose sizes less than 500 lines and 500 elements. 
#
# Note:
# - added polar products to option list.  Will create polar products
#    if center of pass is North of 60N Latitude or South of 60S 
#    Latitude.  Quicklooks will be created if HDFLook is found
#    and if quicklooks not disabled.
# Kathy Strabala December 2011
# - added options -nocloudmask -nosst -nocloudtop -noprofiles -noaerosol
#   -nowvnir -noquicklook to disable algorithm executions
# - added option -deltmp to force deletion of the tmpdir
# - added 4th positional argv is now optional tmp dir to use.
#   if it exists, flat file extraction is assumed to have run previously
#  These additions were suggested and added by Huw Lynch, Western
#     Australia Satellite Technology and Applications Consortium.
#     August, 2009.

set XSIZE=$MODIS_L2_XSIZE
set YSIZE=$MODIS_L2_YSIZE

#-----------------------------------------------------------------------
# GET ARGUMENTS
#-----------------------------------------------------------------------

set run_cloudmask = 0
set run_sst       = 0
set run_cloudtop  = 0
set run_profiles  = 0
set run_aerosol   = 0
set run_wvnir     = 0
set run_cloudopt  = 0
set run_ndsi      = 0
set run_ist       = 0
set run_icec      = 0
set run_inv       = 0
set run_quicklook = 0
set do_deltmp     = 0

set allargs = "$*"

while (1)
  echo checking arg $1
  switch ($1)
    case -nocloudmask:
      set run_cloudmask = 0
      breaksw
    case -nosst:
      set run_sst = 0
      breaksw
    case -nocloudtop:
      set run_cloudtop = 0
      breaksw
    case -noprofiles:
      set run_profiles = 0
      breaksw
    case -noaerosol:
      set run_aerosol = 0
      breaksw
    case -nowvnir:
      set run_wvnir = 0
      breaksw
    case -nocloudopt:
      set run_cloudopt = 0
      breaksw
    case -nondsi:
      set run_ndsi = 0
      breaksw
    case -noist:
      set run_ist = 0
      breaksw
    case -noicec:
      set run_icec = 0
      breaksw
    case -noinv:
      set run_inv = 0
      breaksw
    case -noquicklook:
      set run_quicklook = 0
      breaksw
    case -deltmp:
      set do_deltmp = 1
      breaksw
    default:
      break
      breaksw
  endsw
  shift
end

# Check arguments 
if ($#argv < 3 || $#argv > 4) then
  echo "--------------------------------------------------------------------"
  echo "Usage: modis_level2.csh [Options] SAT L1BFILE OUTDIR [TMPDIR]"
  echo ""
  echo "Where:"
  echo ""
  echo " [Options] - These flags are optional. Default is to run all IMAPP"
  echo "             product executables, and for the temporary run directory" 
  echo "             to remain upon completion."
  echo "   -nocloudmask - do not run cloudmask"
  echo "   -nosst       - do not run sst"
  echo "   -nocloudtop  - do not run cloudtop properties"
  echo "   -nocloudopt  - do not run cloud optical properties"
  echo "   -noprofiles  - do not run profiles"
  echo "   -noaerosol   - do not run aerosols"
  echo "   -nowvnir     - do not run wvnir"
  echo "   -noist       - do not run ice surface temperature"
  echo "   -nondsi      - do not run snow mask"
  echo "   -noicec      - do not run ice concentration"
  echo "   -noinv       - do not run temperature inversion software"
  echo "   -noquicklook - do not run mclite quicklook"
  echo "   -deltmp      - delete temporary directory when complete"
  echo ""
  echo " SAT     - satellite name (terra or aqua)"
  echo " L1BFILE - Level-1B 1KM file"
  echo " OUTDIR  - output file directory"
  echo " TMPDIR  - [Optional]  Name of temporary directory to create"
  echo "             or re-use for flat files. Default is date based name."
  echo ""
  exit(-1)
  echo "--------------------------------------------------------------------"
endif
  echo ""

# Set the Latitude Thresholds for Polar Products to be created
# If the center latitude of the pass is North or South of 
# these thresholds, the polar products will be created if the
# user choose to produce those products.
set NH_THRESHOLD=60
set SH_THRESHOLD=-60

# Get arguments
set SAT = $argv[1]
set FIL1KM = $argv[2]
set OUTDIR = $argv[3]
if ($#argv == 4) then
  set TMPDIR=$argv[4]
endif

if (! -e $FIL1KM) then
  echo "Input Level-1B 1KM file does not exist: "$FIL1KM
  exit(-1)
endif

# Get satellite name given input filename form imapp naming convention (a1 and t1)
set F1 = `echo $FIL1KM:t | cut -c1`
set F3 = `echo $FIL1KM:t | cut -c1-3`
if ($F1 == "t" || $F3 == "MOD") then
  set SAT="Terra"
  set SATMC="TERRA"
  set LSAT="terra"
  set PREFIX="t1"
else if ("$F1" == "a" || "$F3" == "MYD") then
  set SAT="Aqua"
  set SATMC="AQUA"
  set LSAT="aqua"
  set PREFIX="a1"
else
  echo "Satellite name incorrect" $SAT
  exit(1)
endif

# Get directory paths
set DIR_NAME=`dirname $FIL1KM`
set STARTDIR = $PWD

if ("$OUTDIR" == "." || "$OUTDIR" == "./") then
  set OUTDIR=$PWD
endif
if ("$DIR_NAME" == ".") then
  set DIR_NAME=$PWD
  set FIL1KM = $DIR_NAME/$FIL1KM
endif

# Print start message
echo "#################################################################"
echo "MODIS Level-2 processing started at "`date -u`
echo "#################################################################"
echo " "
echo " Input Arguments: "
echo "    Satellite Name: " $SAT
echo "    Input L1B Filename: "$FIL1KM
echo "    Output File Directory Name: "$OUTDIR
echo " "

# Determine if Day or Night data set defined as Day scans > 0
set DAY_SCANS = `ncdump -h ${FIL1KM} | grep "Number of Day mode scans" | awk  '{ print $7 }'`
if ( ${DAY_SCANS} > 100 ) then
   set LIGHT = "DAY"
else
   set LIGHT = "NIGHT"
endif
echo " "
echo " MODIS granule is a $LIGHT Granule"
echo " "

# Get number of 1 km lines of data
set NSCANS = `ncdump -h ${FIL1KM} | grep "Number of Scans" | awk  '{ print $5 }'`
echo " Number of Scans in File " $NSCANS
set NLINS = `expr 10 "*" $NSCANS`
echo " Number of Lines in File " $NLINS
echo " "

# Get the center Latitude of the data segment for determining the
#  projection of the HDF Quicklooks

set NLAT=`ncdump -h ${FIL1KM} | awk '/NORTHBOUNDINGCOORDINATE/,/END_OBJECT/ {print $4}' | grep "\." |cut -d. -f1`
echo "Extracted Northernmost Latitude: " $NLAT
echo " "

set SLAT=`ncdump -h ${FIL1KM} | awk '/SOUTHBOUNDINGCOORDINATE/,/END_OBJECT/ {print $4}' | grep "\." |cut -d. -f1`
echo "Extracted Southernmost Latitude: " $SLAT
echo " "

if ( "$NLAT" > "0" ) then
   set HEMI=1
else if (("$NLAT" < "0")  &&  ("$NLAT" > "-90")) then
   set HEMI=0
else
   echo "ERROR:  Could not determine data hemisphere " $NLAT
   exit 1
endif

# Get the center Latitude of the data segment for determining the
#  projection of the HDF Quicklooks
set CENTER_LAT = `expr $NLAT + $SLAT`
set CENTER_LAT = `expr $CENTER_LAT / 2`

echo "Center Latitude of Data Segment :" $CENTER_LAT
echo " "

set POLAR="FALSE"
if (("$CENTER_LAT" > "$NH_THRESHOLD") \
                  || \
      ("$CENTER_LAT" < "$SH_THRESHOLD" ))  then
   set POLAR="TRUE"
   set suffix="_polar"
   #Check if running in DBVM.  Then can add HDFLook variable"
   if ( ! $?DBVM_HOME ) then
       echo "Not running in DBVM mode"
   else
       # set HDFLook variables
       echo "DBVM_HOME set.  Adding HDFLook variables"
       setenv HDFLOOKMAPS ${DBVM_HOME}/apps/hdflook/MAPS
       setenv PATH .:${DBVM_HOME}/apps/hdflook/LINUX_INTEL32:${PATH}
   endif
else
   set suffix=""
endif


#-----------------------------------------------------------------------
# GET DATE/TIME INFORMATION FROM LEVEL-1B 1KM FILE
#-----------------------------------------------------------------------

# Get date and time
if ("$F1" == "a" || "$F1" == "t") then
  set YR   = `echo $FIL1KM:t | cut -c4-5`
  set YEAR   = `expr 2000 + $YR`
  set DAY    = `echo $FIL1KM:t | cut -c 6-8`
  set HOUR   = `echo $FIL1KM:t | cut -c 10-11`
  set MINUTE = `echo $FIL1KM:t | cut -c 12-13`
else 
  echo "Input file names is not in the DB standard a1 or t1 format"
  exit 1
endif

#-----------------------------------------------------------------------
# CREATE TEMPORARY DIRECTORY
#-----------------------------------------------------------------------

# default is to use a date stamped directory name (YYYY_MM_DD_sat_HHMM_hhmmss)
if ( ! ${?TMPDIR} ) then
  set TMPDIR = ${YEAR}_${DAY}_${SAT}_${HOUR}${MINUTE}_`date -u +%H%M%S`
endif

# Check for a temporary directory, otherwise create one
if ( -d "$TMPDIR" ) then
  set run_extract = 0
else
  mkdir $TMPDIR
  if ($status != 0) then
    echo "Could not create temporary directory: "$TMPDIR
    exit(-1)
  endif
  set run_extract = 1
endif

# Enter temporary directory
cd $TMPDIR

#-----------------------------------------------------------------------
# GET MODIS LEVEL-1 GEO, HKM, QKM FILENAMES
#-----------------------------------------------------------------------

# Set file identifier string (e.g., A2004089.1633)
set FILEID = ${YR}${DAY}.${HOUR}${MINUTE}

# Set standard DB names
# Set geolocation filename
set FILGEO = `find $DIR_NAME -name "${PREFIX}.$FILEID.geo.hdf" | tail -1`
if ($FILGEO == '') then
  echo "Input geolocation file does not exist"
  exit(-1)
endif
set FILHKM = `find $DIR_NAME  -name "${PREFIX}.${FILEID}.500m.hdf" | tail -1`
if ($FILHKM == '') set FILHKM = 'MISSING'
set FILQKM = `find $DIR_NAME  -name "${PREFIX}.${FILEID}.250m.hdf" | tail -1`
if ($FILQKM == '') set FILQKM = 'MISSING'

# Print MODIS Level-1 filenames for log
echo "   INPUT FILES  "
echo "MOD021KM file: "$FIL1KM
echo "MOD02HKM file: "$FILHKM
echo "MOD02QKM file: "$FILQKM
echo "MOD03 file: "$FILGEO
echo "Temporary Directory: "$TMPDIR

#-----------------------------------------------------------------------
# FIRST RUN THE FLATFILE EXTRACTORS 
#-----------------------------------------------------------------------

if ( $run_extract == 1 ) then
  if (-e ${MODIS_L2_BIN}/modis_extract_1km.exe) then
     run_modis_flatfile.csh $SAT $FIL1KM $FILHKM $FILQKM $FILGEO
  else
    # You need the flat binary extracted files in order to the run the 
    # rest of the software.
    echo "Flatfile extractors did not execute correctly."
    exit(-1)
  endif
else
  echo "Using previously created flat files from $TMPDIR"
endif

#-----------------------------------------------------------------------
# RUN THE CLOUD MASK (MOD35)
#-----------------------------------------------------------------------
  if ($run_cloudmask == 1) then
    # Run the cloudmask software if the executable exists
    if (-e ${MODIS_L2_BIN}/cloudmask.exe) then
      run_modis_cloudmask.csh $SAT $OUTPUT_TYPE $FIL1KM $OUTDIR
    else
      echo "cloudmask disabled - no executable"
    endif
  else
    echo "cloudmask disabled - by option"
  endif

  set FILMASK = `find $OUTDIR  -name "${PREFIX}.${FILEID}.mod35.hdf" | tail -1`
  if ($FILMASK == '') set FILMASK = 'MISSING'

#-----------------------------------------------------------------------
# RUN THE IMAPP SST ALGLORITHM
#-----------------------------------------------------------------------
 
   if ($run_sst == 1) then
     # Run the sst software if the executable exists
     if (-e ${MODIS_L2_BIN}/sst.exe) then
        run_modis_sst.csh $SAT $OUTPUT_TYPE $FIL1KM $OUTDIR
     else
        echo "sst disabled - no executable"
     endif
   else
     echo "sst disabled - by option"
   endif

#-----------------------------------------------------------------------
# RUN THE IMAPP CLOUD TOP PROPERTIES AND CLOUD PHASE ALGLORITHM (MOD06CT)
#-----------------------------------------------------------------------

   if ($run_cloudtop == 1) then
     # Run the ctp software if the executable exists
     if (-e ${MODIS_L2_BIN}/cloudtop.exe) then
        run_modis_cloudtop.csh $SAT $OUTPUT_TYPE $FIL1KM $OUTDIR
     else
        echo "cloud top properties disabled - no executable"
     endif
   else
     echo "cloud top properties disabled - by option"
   endif

   set FILMOD06CT = `find $OUTDIR  -name "${PREFIX}.${FILEID}.mod06ct.hdf" | tail -1`
   if ($FILMOD06CT == '') then 
       set FILMOD06CT = 'MISSING'
   endif

#-----------------------------------------------------------------------
# RUN THE IMAPP ATMOSPHERIC PROFILES ALGORITHM (MOD07)
#-----------------------------------------------------------------------

   if ($run_profiles == 1) then
     # Run the sst software if the executable exists
     if (-e ${MODIS_L2_BIN}/profiles.exe) then
        run_modis_profiles.csh $LSAT $OUTPUT_TYPE $FIL1KM $OUTDIR
     else
       echo "profiles disabled - no executable"
     endif
   else
     echo "profiles disabled - by option"
   endif

#-----------------------------------------------------------------------
# RUN THE IMAPP ICE SURFACE TEMPERATURE ALGLORITHM - Jeff Key, NOAA
#-----------------------------------------------------------------------

   if ( $POLAR == "TRUE" ) then
     if ( $run_ist == 1 ) then
       # Run the ice surface temperature software if the executable exists
       if ( -e ${MODIS_L2_BIN}/ice_surface_temperature.exe ) then
          echo "run_modis_ist.bash $HEMI $FIL1KM $FILMASK $OUTDIR" 
          run_modis_ist.bash $HEMI $FIL1KM $FILMASK $OUTDIR
       else
          echo "ice surface temperature disabled - no executable"
       endif
     else
       echo "ice surface temperature disabled - by option"
     endif
   else
     echo "Ice surface temperature not run - granule did not meet polar requirements or ice surface temperature software disabled."
   endif

   # Run the HDFLook Commands to create quicklooks
   if (( $run_ist == 1 && $POLAR == "TRUE" )) then
     if ( $run_quicklook == 1 ) then
        # Can the HDFLook executable be found?
        which HDFLook
        if ( $status == 0 ) then
        set OUTDIR_IMAGES=${MODIS_L2_IMAGES}
        echo "template file" ${MODIS_L2_HOME}/scripts/template_ist${suffix}.txt
        run_modis_pp_hdflook.bash $OUTDIR/${PREFIX}.${FILEID}.ist.hdf  \
            ${MODIS_L2_HOME}/scripts/template_ist${suffix}.txt \
            ${OUTDIR_IMAGES}
        else
          echo "No IST quicklook to be made - Could not find HDFLook executable"
        endif
     endif
   endif

#-----------------------------------------------------------------------
# RUN THE IMAPP ICE MASK AND ICE CONCENTRATION ALGLORITHM - Yinghui Liu,
#  UW-Madison, CIMSS/SSEC and Jeffry Key, NOAA/NESDIS/ASPB
#-----------------------------------------------------------------------

   if ( $POLAR == "TRUE" ) then
     if ( $run_icec == 1 ) then
       # Run the ice mask and ice concentration algorithm if it exists
       if ( -e ${MODIS_L2_BIN}/ice_concentration.exe ) then
          echo "run_modis_ice_concentration.bash $FIL1KM $FILMASK $FILGEO $OUTDIR" 
          run_modis_ice_concentration.bash $FIL1KM $FILMASK $FILGEO $OUTDIR
       else
          echo "ice concentration disabled - no executable"
       endif
     else
       echo "ice concentration disabled - by option"
     endif
   else
     echo "Ice concentration not run - granule did not meet polar requirements or ice concentration software disabled."
   endif

   # Run the HDFLook Commands to create quicklooks
   if (( $run_icec == 1 && $POLAR == "TRUE" )) then
     if ( $run_quicklook == 1 ) then
        # Can the HDFLook executable be found?
        which HDFLook
        if ( $status == 0 ) then
          set OUTDIR_IMAGES=${MODIS_L2_IMAGES}
          set LEADER=$OUTDIR_IMAGES/${PREFIX}.${FILEID}
          echo "template file" ${MODIS_L2_HOME}/scripts/template_ice_mask${suffix}.txt
          run_modis_pp_hdflook.bash $OUTDIR/${PREFIX}.${FILEID}.icecon.hdf  \
              ${MODIS_L2_HOME}/scripts/template_ice_mask${suffix}.txt \
              ${OUTDIR_IMAGES}
#         Move the .jpg file so that the names will not collide
          if (( -e ${LEADER}.icecon.jpg )) then
            mv ${LEADER}.icecon.jpg ${LEADER}.${FILEID}.icemask.jpg
            mv ${LEADER}.icecon.tif ${LEADER}.${FILEID}.icemask.tif
          endif
          echo "template file" ${MODIS_L2_HOME}/scripts/template_ice_concentration${suffix}.txt
          run_modis_pp_hdflook.bash $OUTDIR/${PREFIX}.${FILEID}.icecon.hdf  \
              ${MODIS_L2_HOME}/scripts/template_ice_concentration${suffix}.txt \
              ${OUTDIR_IMAGES}
        else
          echo "No Ice Concentration quicklook to be made - Could not find HDFLook executable"
        endif
     endif
   endif

#-----------------------------------------------------------------------
# RUN THE MODIS IMAPP TEMPERATURE INVERSION RETRIEVAL ALGLORITHM - 
#  Yinghui Liu, William Straka III UW-Madison, CIMSS/SSEC and 
#  Jeffry Key, NOAA/NESDIS/ASPB
#-----------------------------------------------------------------------

   if ( $POLAR == "TRUE" ) then
     if ( $run_inv == 1 ) then
       # Run the temperature inversion algorithm if it exists
       if ( -e ${MODIS_L2_BIN}/inversions.exe ) then
          echo "run_modis_inversions.bash $HEMI $FIL1KM $FILMASK $OUTDIR" 
          run_modis_inversions.bash $HEMI $FIL1KM $FILMASK $OUTDIR
       else
          echo "temperatures inversions disabled - no executable"
       endif
     else
       echo "temperatures inversions disabled - by option"
     endif
   else
     echo "Polar inversion software not run - granule did not meet polar requirements or polar inversion software disabled."
   endif

   # Run the HDFLook Commands to create quicklooks
   if (( $run_inv == 1 && $POLAR == "TRUE" )) then
     if ( $run_quicklook == 1 ) then
        # Can the HDFLook executable be found?
        which HDFLook
        if ( $status == 0 ) then
          set OUTDIR_IMAGES=${MODIS_L2_IMAGES}
          set LEADER=$OUTDIR_IMAGES/${PREFIX}.${FILEID}
          run_modis_pp_hdflook.bash $OUTDIR/${PREFIX}.${FILEID}.inversion.hdf  \
              ${MODIS_L2_HOME}/scripts/template_inversion_depth${suffix}.txt \
              ${OUTDIR_IMAGES}
#         Move the image files
          if (( -e ${LEADER}.inversion.jpg )) then
            mv ${LEADER}.inversion.jpg ${LEADER}.inversion_depth.jpg
            mv ${LEADER}.inversion.tif ${LEADER}.inversion_depth.tif
          endif
          run_modis_pp_hdflook.bash $OUTDIR/${PREFIX}.${FILEID}.inversion.hdf  \
             ${MODIS_L2_HOME}/scripts/template_inversion_strength${suffix}.txt \
             ${OUTDIR_IMAGES}
#         Move the image files
          if (( -e ${LEADER}.inversion.jpg )) then
            mv ${LEADER}.inversion.jpg ${LEADER}.inversion_strength.jpg
            mv ${LEADER}.inversion.tif ${LEADER}.inversion_strength.tif
          endif
        else
          echo "No Temperature Inversion quicklook to be made - Could not find HDFLook executable"
        endif
     endif
   endif

#-----------------------------------------------------------------------
# RUN THE IMAPP DAYTIME ONLY PROCESSES
#-----------------------------------------------------------------------

   if ( ${LIGHT} == "DAY" ) then

#-----------------------------------------------------------------------
#   RUN THE AEROSOL ALGORITHM (MOD04)
#-----------------------------------------------------------------------

    if ($run_aerosol == 1) then
       if (-e ${MODIS_L2_BIN}/aerosol.exe) then
         run_modis_aerosol.csh $SAT $OUTPUT_TYPE $FIL1KM $OUTDIR
       else
         echo "aerosol disabled - no executable"
       endif
    else
      echo "aerosol disabled - by option"
    endif

#-----------------------------------------------------------------------
#   RUN THE PETER ALBERT WATER VAPOR RETRIEVAL ALGORITHM
#-----------------------------------------------------------------------

    if ($run_wvnir == 1) then
       if (-e ${MODIS_L2_BIN}/wvnir.exe) then
         run_modis_wvnir.csh $SAT $OUTPUT_TYPE $FIL1KM $OUTDIR
       else
         echo "wvnir disabled - no executable"
       endif
    else
      echo "wvnir disabled - by option"
    endif

#-----------------------------------------------------------------------
# RUN THE MODIS CLOUD OPTICAL PROPERTIES ALGLORITHM (MOD06OD)
#-----------------------------------------------------------------------
   if ($run_cloudopt == 1) then
   # Run the cloud optical properties software if the executable exists
      if (-e ${MODIS_L2_BIN}/MOD_PR06OD.exe) then
        # You must have unpacked the MOD06OC coefficients file
        if (-e ${MODIS_L2_COEFF}/Forward_Libraries/Library_ice.hdf) then
          if ($FILMOD06CT != ''MISSING'')  then
            # Copy the MOD06CT File to MOD06 for use by the cloud optical properties
            #  software
            set FILMOD06 = ${PREFIX}.${FILEID}.mod06.hdf
            cp $FILMOD06CT $FILMOD06
            run_modis_cloudoptical.csh $SAT $FIL1KM $FILMASK $FILGEO $FILMOD06 $OUTDIR
          endif
        else
          echo "cloud optical properties coefficient files not found"
        endif
      else
        echo "cloud optical properties disabled - no executable"
      endif
    else
      echo "cloud optical properties disabled - by option"
    endif

#-----------------------------------------------------------------------
# RUN THE IMAPP NDSI SNOW MASK ALGORITHM - William Straka III, CIMSS
#-----------------------------------------------------------------------

   if ( $POLAR == "TRUE" ) then
     if ( $run_ndsi == 1 ) then
       # Run the snow mask software if the executable exists
       if ( -e ${MODIS_L2_BIN}/snow_mask.exe ) then
          run_modis_snowmask.bash $SAT $FIL1KM $FILMASK $OUTDIR
       else
          echo "snow mask disabled - no executable"
       endif
     else
       echo "snow mask disabled - by option"
     endif
   else
     echo "Snow mask not run - granule did not meet polar requirements or snow mask software disabled."
   endif

   # Run the HDFLook Commands to create quicklooks
   if (( $run_ndsi == 1 && $POLAR == "TRUE" )) then
     if ( $run_quicklook == 1 ) then
        # Can the HDFLook executable be found?
        which HDFLook
        if ( $status == 0 ) then
          set OUTDIR_IMAGES=${MODIS_L2_IMAGES}
          echo "template file" ${MODIS_L2_HOME}/scripts/template_snowmask${suffix}.txt
          run_modis_pp_hdflook.bash $OUTDIR/${PREFIX}.${FILEID}.snowmask.hdf  \
             ${MODIS_L2_HOME}/scripts/template_snowmask${suffix}.txt \
             ${OUTDIR_IMAGES}
        else
              echo "No Snow Mask quicklook to be made - Could not find HDFLook executable"
        endif
     endif
   endif


#-----------------------------------------------------------------------
# END THE DAYTIME ONLY PROCESSING
#-----------------------------------------------------------------------

  endif

#-----------------------------------------------------------------------
# Run the McIDAS commands to make quick look images.
#------------------------------------------------------------------------

  if ($run_quicklook == 1) then
     if (-e ${MODIS_L2_HOME}/mclite/bin/mcenv) then
       set OUTDIR_IMAGES = ${MODIS_L2_IMAGES}
       make_mclite_images.sh $SATMC $FIL1KM \
          $LIGHT $NLINS $XSIZE $YSIZE $OUTDIR $OUTDIR_IMAGES
     else
        echo "McIDAS quicklook disabled - no executable"
     endif
  else
     echo "McIDAS quicklook disabled - by option"
  endif

#-----------------------------------------------------------------------
# CLEANUP AND EXIT
#-----------------------------------------------------------------------

endif

#------------------------------------------------------------------------

# Remove the temporary directory if option is set
if ($do_deltmp == 1) then
  cd $STARTDIR
  rm -rf $TMPDIR
endif

# Print finish message
echo "MODIS Level-2 processing finished at "`date -u`

exit(0)
