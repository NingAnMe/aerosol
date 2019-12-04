#!/bin/bash
# 
# This script uses McIDAS commands to create single images of 
# MODIS products. 
# 
# Originally written by Kathleen Strabala August 2006 
#
# This script is released as part of the International MODIS/AIRS
# Processing Package (IMAPP).  For more information on McIDAS and 
# McLITE, the free version of McIDAS, please see:  
#   http://www.ssec.wisc.edu/mcidas/
#   http://www.ssec.wisc.edu/mcidas/software/mclite/
# 
#------------------------------------------------------------------------------- 
# SETUP
#-------------------------------------------------------------------------------

# Write out arguments
if [ "$#" -lt 8 ] || [ "$#" -gt 8 ]; then
  echo "Usage: make_mclite_images.sh SATMC FIL1KM LIGHT NLINS XSIZE YSIZE OUTDIR OUTIMAGE"
  echo "where"
  echo "SATMC is MODIS satellite platform (specific to McIDAS naming "
  echo    "either AQUA or TERRA"
  echo "FIL1KM is MODIS L1B 1000 meter resolution HDF image file"
  echo "LIGHT either DAY or NIGHT instrument mode"
  echo "NLINS  Number of lines in data set"
  echo "XSIZE Size of images to create X direction"
  echo "YSIZE Size of images to create Y direction"
  echo "OUTDIR Output directory containing IMAPP L2 output HDF files"
  echo "OUTIMAGE Output directory to transfer Image files"
  exit 
fi

# Extract file names
export SATMC=$1
export FIL1KM=$2
export LIGHT=$3
export NLINS=$4
export XSIZE=$5
export YSIZE=$6
export OUTDIR=$7
export OUTIMAGE=$8
echo "arguments $SATMC $FIL1KM $LIGHT $NLINS $XSIZE $YSIZE $OUTDIR $OUTIMAGE"

# Get name of path to l1b data
export DIR_NAME=`dirname $FIL1KM`

# Set mcidas root directory
export MC=${MODIS_L2_HOME}/mclite
export MCINDATA=${DIR_NAME}
export MCDATA=${OUTDIR}
export PATH=${MC}/bin:${MC}/data:${PATH}
export MCPATH=${MC}/data
export MCIN=${MC}/data

# Make sure you include the right libraries for mclite to work
export LD_LIBRARY_PATH=${MODIS_L2_HOME}/mclite/lib:${LD_LIBRARY_PATH}

# Set root of output file name (e.g. 't1.02001.1815')
ROOT=${FIL1KM##*/}

#Get date/time parameters from parsing out L1B
export DATE=`echo $ROOT | cut -c4-8`
export HR=`echo $ROOT | cut -c10-11`
export MIN=`echo $ROOT | cut -c12-13`
export PASSTIME=${HR}:${MIN}
echo "PASSTIME" ${PASSTIME} 

export RES=$(( NLINS / YSIZE + 1 ))
echo "Data Resolution"  $RES

export MCY=$(( YSIZE - 50 ))
export MCX1=$(( XSIZE - 50 ))
export MCX2=$(( XSIZE - 100 ))

#Run a single mcidas command to get the date in desired format dd_mmm_yyyy
export DDMMYY=`datelist.k ${DATE} FORM='(DD)_(SMON)_(YYYY)'`
export QUICKLD=`datelist.k ${DATE} FORM='(YYYY)_(MM)_(DD)_(DDD)'`

echo "DDMMYY QUICKLD MCY MCX1 MCX2" $DDMMYY $QUICKLD $MCY $MCX1 $MCX2 $MCDATA

export SHELL=/bin/bash

# Start McIDAS remapping and web image production 
mcenv -f 3@${YSIZE}x${XSIZE} -graphicsColors 12 -i 192 << 'EOF'

#BAND 31 (11 micron) IMAGE
#######################################################################
dsserve.k ADD L${SATMC}/1KM MODS TYPE=IMAGE DIR="${MCINDATA}/*.1000m.hdf"
dataloc.k ADD L${SATMC} LOCAL-DATA
dsserve.k ADD A/A AREA 1 9999 "Local Areas"
dataloc.k ADD A LOCAL-DATA
imglist.k L${SATMC}/1KM.ALL


# If you were able to find the 1 km date set for this date/time then proceed
dataloc.k LIST 
dsinfo.k I A
dsinfo.k I L${SATMC}

# Get the center lat/lon of the file and use this information for 
#  the projection.  If center latitude is greater than 60 N or less than
#  -60 S then use Polar Stereographic.  Otherwise, use Lambert Conformal.
export CLAT=`imglist.k L${SATMC}/1KM DAY=${DATE} TIME=${PASSTIME} | grep ${SATMC}-L1B \
       | awk '{print $7}'`
echo "Center Latitude of Image =" $CLAT
export CLON=`imglist.k L${SATMC}/1KM DAY=${DATE} TIME=${PASSTIME} | grep ${SATMC}-L1B \
       | awk '{print $8}'`
echo "Center Longitude of Image =" $CLON

# McIDAS labels the lat as -999 and lon as 999 if it cannot figure out
# center lat/lon.
if [[ $CLAT == -999 || $CLON == 999 ]] ; then
    echo  "##########################################"
    echo  "Problem with navigation in 1 KM HDF file."
    echo  " Exiting McIDAS image creation script."
    echo  "##########################################"
    exit 1
fi

if [[ $CLAT -gt 60 || $CLAT -lt -60 ]] ; then
   PROJECTION=PS
else   
   PROJECTION=LAMB
fi

echo "Projection to be used is :" $PROJECTION
 
# If you were able to find the 1 km date set for this date/time then proceed
imgcopy.k L${SATMC}/1KM A/A.1 DAY=${DATE} TIME=${PASSTIME} BAND=31 SIZE=SAME

if [ $? == 0 ] ; then
  echo "Found MODIS 1 km L1B file to process"
  sf.k 1
  eg.k
  gd.k 1
  if [[ ${PROJECTION} == "PS" ]] ; then
    imgremap.k A/A.1 A/A.1001 BAND=31 SIZE=${YSIZE} ${XSIZE} \
      PRO=${PROJECTION} RES=${RES}
  else
    imgremap.k A/A.1 A/A.1001 BAND=31 SIZE=${YSIZE} ${XSIZE} \
      PRO=${PROJECTION} RES=${RES} SPLINE=10 SMO=YES
  fi
  gu.k MAKE 1 WHITE
  imgdisp.k A/A.1001 
  map.k 
  map.k LALO LABEL=EASTPOS
  eu.k REST B31
  frmlabel.k \"MODIS $SATMC Band 31 \(11 micron\) $QUICKLD ${HR}${MIN}UTC   CIMSS/NASA

  frmsave.k 1 ${SATMC}_${DDMMYY}_${HR}${MIN}_Band_31
  if [ $? == 0 ] ; then
     /bin/mv ${MCIN}/${SATMC}_${DDMMYY}_${HR}${MIN}_Band_31.GIF $OUTIMAGE
  fi
fi
imgdel.k A/A.1
imgdel.k A/A.1001

# Band 27 Image - Water Vapor Channel
#######################################################################
imgcopy.k L${SATMC}/1KM A/A.1 DAY=${DATE} TIME=${PASSTIME} BAND=27 SIZE=SAME

if [ $? == 0 ] ; then
  echo "Found MODIS 1 km L1B file to process"
  sf.k 1
  eg.k
  gd.k 1
  if [[ ${PROJECTION} == "PS" ]] ; then
    imgremap.k A/A.1 A/A.1001 BAND=27 SIZE=${YSIZE} ${XSIZE} \
      PRO=${PROJECTION} RES=${RES}
  else
    imgremap.k A/A.1 A/A.1001 BAND=27 SIZE=${YSIZE} ${XSIZE} \
      PRO=${PROJECTION} RES=${RES} SPLINE=10 SMO=YES 
  fi
  gu.k MAKE 1 WHITE
  imgdisp.k A/A.1001 SU=B27
  map.k 
  map.k LALO LABEL=EASTPOS
  eu.k REST B27
  frmlabel.k \"MODIS $SATMC Band 27 \(6.5 micron\) $QUICKLD ${HR}${MIN}UTC   CIMSS/NASA

  frmsave.k 1 ${SATMC}_${DDMMYY}_${HR}${MIN}_Band_27
  if [ $? == 0 ] ; then
     /bin/mv ${MCIN}/${SATMC}_${DDMMYY}_${HR}${MIN}_Band_27.GIF $OUTIMAGE
  fi
fi
imgdel.k A/A.1
imgdel.k A/A.1001

# Make Cloud Mask Image
#######################################################################
dsserve.k ADD L${SATMC}/MASK MODX TYPE=IMAGE DIR="${MCDATA}/*.mod35.hdf"
imglist.k L${SATMC}/MASK.ALL
# Copy down several bands which will be used later
# Cloud Mask Band 2 is the 4 category cloud mask 
imgcopy.k L${SATMC}/MASK A/A.1 DAY=${DATE} TIME=${PASSTIME} BAND=2 SIZE=SAME

if [ $? == 0 ] ; then
  echo "Found MODIS Cloud Mask file to process"
  # Cloud Mask Band 5 is the Snow/Ice Mask
  imgcopy.k L${SATMC}/MASK A/A.2 DAY=${DATE} TIME=${PASSTIME} BAND=5 SIZE=SAME
  # Cloud Mask Band 6 is the Land/Sea Mask
  imgcopy.k L${SATMC}/MASK A/A.3 DAY=${DATE} TIME=${PASSTIME} BAND=6 SIZE=SAME

  if [[ ${PROJECTION} == "PS" ]] ; then
    imgremap.k A/A.1 A/A.1001 BAND=2 SIZE=${YSIZE} ${XSIZE} \
       PRO=${PROJECTION} RES=${RES}
  else
    imgremap.k A/A.1 A/A.1001 BAND=2 SIZE=${YSIZE} ${XSIZE} \
       PRO=${PROJECTION} RES=${RES} SPLINE=10 SMO=YES
  fi

  # Display Image
  sf.k 1
  eg.k
  eu.k
  imgdisp.k A/A.1001
  map.k LIN=X ${MCY} 
  map.k LALO LABEL=EASTPOS LIN=X ${MCY}
  eu.k REST CLOUDMASK
  pc.k T $(( YSIZE - 50 )) 15
  zlm.k FILL 8 17 17 MODE=N
  zlm.k FILL 4 15 15 MODE=N
  pc.k T $(( YSIZE - 25 )) 15
  zlm.k FILL 8 17 17 MODE=N
  zlm.k FILL 2 15 15 MODE=N
  gd.k 3 
  za.k 8 15 POS=$(( YSIZE - 57 )) 34 FONT=HELBO \"CLEAR
  gd.k 1
  za.k 7 15 POS=$(( YSIZE - 57 )) 34 FONT=HELBO \"CLEAR
  gd.k 3 
  za.k 8 15 POS=$(( YSIZE - 32 )) 34 FONT=HELBO \"PROBABLY CLEAR
  gd.k 1
  za.k 7 15 POS=$(( YSIZE - 32 )) 34 FONT=HELBO \"PROBABLY CLEAR
  pc.k T $(( YSIZE - 50 )) 225
  zlm.k FILL 8 17 17 MODE=N
  zlm.k FILL 5 15 15 MODE=N
  pc.k T $(( YSIZE - 25 )) 225
  zlm.k FILL 8 17 17 MODE=N
  zlm.k FILL 7 15 15 MODE=N
  gd.k 3 
  za.k 8 15 POS=$(( YSIZE - 57 )) 247 FONT=HELBO \"UNCERTAIN
  gd.k 1
  za.k 7 15 POS=$(( YSIZE - 57 )) 247 FONT=HELBO \"UNCERTAIN
  gd.k 3 
  za.k 8 15 POS=$(( YSIZE - 32 )) 247 FONT=HELBO \"CLOUDY
  gd.k 1
  za.k 7 15 POS=$(( YSIZE - 32 )) 247 FONT=HELBO \"CLOUDY
  frmlabel.k \"MODIS $SATMC Cloud Mask $QUICKLD ${HR}${MIN}UTC    CIMSS/NASA
  frmsave.k 1 ${SATMC}_${DDMMYY}_${HR}${MIN}_CloudMask
  if [ $? == 0 ] ; then 
      /bin/mv ${MCIN}/${SATMC}_${DDMMYY}_${HR}${MIN}_CloudMask.GIF $OUTIMAGE
  fi
fi
# Don't delete the original cloud mask areas because you use them later
imgdel.k A/A.1001

# Make Cloud Top Pressure Image
#######################################################################
dsserve.k ADD L${SATMC}/TOP MODX TYPE=IMAGE DIR="${MCDATA}/*.mod06ct.hdf"
imglist.k L${SATMC}/TOP.ALL
# Erase Image
sf.k 1
eg.k
eu.k
imgcopy.k L${SATMC}/TOP A/A.4 DAY=${DATE} TIME=${PASSTIME} BAND=12 SIZE=SAME

if [ $? == 0 ] ; then
  echo "Found MODIS Cloud Top Properties file to process"
  if [[ ${PROJECTION} == "PS" ]] ; then
    imgremap.k A/A.4 A/A.1001 BAND=12 SIZE=${YSIZE} ${XSIZE} LATLON=${LAT} ${LON} \
       PRO=${PROJECTION} RES=${RES} 
  else
    imgremap.k A/A.4 A/A.1001 BAND=12 SIZE=${YSIZE} ${XSIZE} LATLON=${LAT} ${LON} \
       PRO=${PROJECTION} RES=${RES} SPLINE=10 SMO=YES
  fi
  imgoper.k A/A.1001 A/A.1002 SIZE=ALL
  imgdel.k A/A.1001 
  imgcopy.k A/A.1002 A/A.1001 SIZE=SAME
  imgdel.k A/A.1002
  prdutil.k ADD A/A.1001 2 CTP 10 1100 250 10 MB 1 0
  imgdisp.k A/A.1001 LATLON=${LAT} ${LON} SU=DBCTP
  map.k ELE=X ${MCX2}
  map.k LALO LABEL=EASTPOS ELE=X ${MCX2}
  bar.k COL=1 LINT=100 RANGE=100 1000 SU=DBCTP 
  eu.k REST DBCTP
  frmlabel.k \"MODIS $SATMC Cloud Top Pressures \(hPa\)  $QUICKLD ${HR}${MIN}UTC   CIMSS/NASA
  frmsave.k 1 ${SATMC}_${DDMMYY}_${HR}${MIN}_CloudTopPressure
  if [ $? == 0 ] ; then 
     /bin/mv ${MCIN}/${SATMC}_${DDMMYY}_${HR}${MIN}_CloudTopPressure.GIF $OUTIMAGE
  fi
fi
imgdel.k A/A.4
imgdel.k A/A.1001

# Make Cloud Phase Image
#######################################################################
dsserve.k ADD L${SATMC}/TOP MODX TYPE=IMAGE DIR="${MCDATA}/*.mod06ct.hdf"
imglist.k L${SATMC}/TOP.ALL
# Erase Image Frame
sf.k 1
eg.k
eu.k
imgcopy.k L${SATMC}/TOP A/A.4 DAY=${DATE} TIME=${PASSTIME} BAND=46 SIZE=SAME

if [ $? == 0 ] ; then
  echo "Found MODIS Cloud Top Properties file to process"
  if [[ ${PROJECTION} == "PS" ]] ; then
    imgremap.k A/A.4 A/A.1001 BAND=46 SIZE=${YSIZE} ${XSIZE} LATLON=${LAT} ${LON} \
       PRO=${PROJECTION} RES=${RES}
  else
    imgremap.k A/A.4 A/A.1001 BAND=46 SIZE=${YSIZE} ${XSIZE} LATLON=${LAT} ${LON} \
       PRO=${PROJECTION} RES=${RES} SPLINE=10 SMO=YES
  fi
  imgdisp.k A/A.1001
  map.k LIN=X ${MCY} 
  map.k LALO LABEL=EASTPOS LIN=X ${MCY}
  pc.k T $(( YSIZE - 50 )) 15
  zlm.k FILL 7 17 17 MODE=N
  zlm.k FILL 6 15 15 MODE=N
  pc.k T $(( YSIZE - 25 )) 15
  zlm.k FILL 7 17 17 MODE=N
  zlm.k FILL 10 15 15 MODE=N
  gd.k 3 
  za.k 8 15 POS=$(( YSIZE - 57 )) 34 FONT=HELBO \"WATER
  gd.k 1
  za.k 7 15 POS=$(( YSIZE - 57 )) 34 FONT=HELBO \"WATER
  gd.k 3 
  za.k 8 15 POS=$(( YSIZE - 32 )) 34 FONT=HELBO \"ICE 
  gd.k 1
  za.k 7 15 POS=$(( YSIZE - 32 )) 34 FONT=HELBO \"ICE
  pc.k T $(( YSIZE - 50 )) 225
  zlm.k FILL 7 17 17 MODE=N
  gu.k MAKE 9 140 140 140
  zlm.k FILL 9 15 15 MODE=N
  pc.k T $(( YSIZE - 25 )) 225
  zlm.k FILL 7 17 17 MODE=N
  gu.k MAKE 11 200 200 200
  zlm.k FILL 11 15 15 MODE=N
  gd.k 3 
  za.k 8 15 POS=$(( YSIZE - 57 )) 247 FONT=HELBO \"MIXED PHASE
  gd.k 1
  za.k 7 15 POS=$(( YSIZE - 57 )) 247 FONT=HELBO \"MIXED PHASE
  gd.k 3 
  za.k 8 15 POS=$(( YSIZE - 32 )) 247 FONT=HELBO \"UNCERTAIN
  gd.k 1
  za.k 7 15 POS=$(( YSIZE - 32 )) 247 FONT=HELBO \"UNCERTAIN
  eu.k REST IRPHASE3
  frmlabel.k \"MODIS $SATMC Cloud Phase $QUICKLD ${HR}${MIN}UTC   CIMSS/NASA
  frmsave.k 1 ${SATMC}_${DDMMYY}_${HR}${MIN}_CloudPhase
  if [ $? == 0 ] ; then 
     /bin/mv ${MCIN}/${SATMC}_${DDMMYY}_${HR}${MIN}_CloudPhase.GIF $OUTIMAGE
  fi
fi
imgdel.k A/A.4
imgdel.k A/A.1001


# Make Sea Surface Temperature Image
#######################################################################
dsserve.k ADD L${SATMC}/SST MOD8 TYPE=IMAGE DIR="${MCDATA}/*.mod28.hdf"
imglist.k L${SATMC}/SST.ALL
# Clear Frame
sf.k 1
eg.k
eu.k
gu.k
imgoper.k L${SATMC}/SST A/A.4 DAY=${DATE} TIME=${PASSTIME} BAND=1 SIZE=ALL \
  UNIT=RAW LLMT=26817 ULMT=30817 SCALE=26817 30817 5 245

if [ $? == 0 ] ; then
  echo "Found MODIS Sea Surface Temperature file to process"
  imgfilt.k A/A.4 A/A.1 A/A.1001 FILT=DIS 0 255 32 96 SIZE=ALL \
    SCALE=0 255 0 255 BAND=1 2
  imgfilt.k A/A.1001 A/A.3 A/A.1002 FILT=DIS 0 255 160 224 SIZE=ALL \
    SCALE=0 255 0 255 BAND=1 6
  imgfilt.k A/A.1002 A/A.2 A/A.1003 FILT=DIS 0 255 255 255 SIZE=ALL \
    SCALE=0 255 0 255 BAND=1 5
  if [[ ${PROJECTION} == "PS" ]] ; then
    imgremap.k A/A.1003 A/A.1004 BAND=1 SIZE=${YSIZE} ${XSIZE} LATLON=${LAT} ${LON} \
      PRO=${PROJECTION} RES=${RES}
  else
    imgremap.k A/A.1003 A/A.1004 BAND=1 SIZE=${YSIZE} ${XSIZE} LATLON=${LAT} ${LON} \
      PRO=${PROJECTION} RES=${RES} SPLINE=10 SMO=YES
  fi
  imgdel.k A/A.1001 1003
  prdutil.k ADD A/A.1004 2 SST -500 3500 5 245 C 100 -99999

  imgdisp.k A/A.1004 SU=DBSST
  map.k ELE=X ${MCX1}
  gu.k MAKE 1 WHITE
  eu.k REST DBSST
  bar.k COL=1 LINT=5.0 SU=DBSST
  frmlabel.k \"MODIS $SATMC Sea Surface Temperatures \(C\)  $QUICKLD ${HR}${MIN}UTC   CIMSS/NASA
  frmsave.k 1 ${SATMC}_${DDMMYY}_${HR}${MIN}_SeaSurfaceTemperatures
  if [ $? == 0 ] ; then
     /bin/mv ${MCIN}/${SATMC}_${DDMMYY}_${HR}${MIN}_SeaSurfaceTemperatures.GIF $OUTIMAGE
  fi
fi
imgdel.k A/A.1 4 
imgdel.k A/A.1004


#Make Total Precipitable Water Vapor Image
#######################################################################
dsserve.k ADD L${SATMC}/ATM MODX TYPE=IMAGE DIR="${MCDATA}/*.mod07.hdf"
imglist.k L${SATMC}/ATM.ALL
# Display Image
sf.k 1
eg.k
eu.k
imgcopy.k L${SATMC}/ATM A/A.4 DAY=${DATE} TIME=${PASSTIME} BAND=26 SIZE=SAME

if [ $? == 0 ] ; then
  echo "Found MODIS Atmospheric Profiles file to process"
  if [[ ${PROJECTION} == "PS" ]] ; then
    imgremap.k A/A.4 A/A.1001 BAND=26 SIZE=${YSIZE} ${XSIZE} LATLON=${LAT} ${LON} \
       PRO=${PROJECTION} RES=${RES}
  else
    imgremap.k A/A.4 A/A.1001 BAND=26 SIZE=${YSIZE} ${XSIZE} LATLON=${LAT} ${LON} \
       PRO=${PROJECTION} RES=${RES} SPLINE=10 SMO=YES
  fi
  imgoper.k A/A.1001 A/A.1002 SIZE=ALL
  prdutil.k ADD A/A.1002 2 TPW 0 2000 80 250 MM 10 -9999

  imgdisp.k A/A.1002 LATLON=${LAT} ${LON} SU=DBTPW
  map.k ELE=X ${MCX1}
  map.k LALO LABEL=EASTPOS ELE=X ${MCX1}
  bar.k COL=1 LINT=5.0 SU=DBSST
  eu.k REST DBTPW
  frmlabel.k \"MODIS $SATMC Total Precipitable Water Vapor \(mm\)  $QUICKLD ${HR}${MIN}UTC   CIMSS/NASA
  frmsave.k 1 ${SATMC}_${DDMMYY}_${HR}${MIN}_WaterVapor
  if [ $? == 0 ] ; then
     /bin/mv ${MCIN}/${SATMC}_${DDMMYY}_${HR}${MIN}_WaterVapor.GIF $OUTIMAGE
  fi

fi
imgdel.k A/A.4
imgdel.k A/A.1001
imgdel.k A/A.1002


# Make images from the visible channel data
#######################################################################
if [ "${LIGHT}" == "DAY" ] ; then
   imgcopy.k L${SATMC}/1KM A/A.4 DAY=${DATE} TIME=${PASSTIME} BAND=1 3 4 7 26 SIZE=SAME

   if [ $? == 0 ] ; then
   echo "Found MODIS 1 km L1B visible file to process"

   if [[ ${PROJECTION} == "PS" ]] ; then
     imgremap.k A/A.4 A/A.1001 BAND=1 SIZE=${YSIZE} ${XSIZE} LATLON=${LAT} ${LON} \
        PRO=${PROJECTION} RES=${RES}
     imgremap.k A/A.4 A/A.1002 BAND=4 SIZE=${YSIZE} ${XSIZE} LATLON=${LAT} ${LON} \
        PRO=${PROJECTION} RES=${RES}
     imgremap.k A/A.4 A/A.1003 BAND=3 SIZE=${YSIZE} ${XSIZE} LATLON=${LAT} ${LON} \
        PRO=${PROJECTION} RES=${RES}
     imgremap.k A/A.4 A/A.1004 BAND=7 SIZE=${YSIZE} ${XSIZE} LATLON=${LAT} ${LON} \
        PRO=${PROJECTION} RES=${RES}
     imgremap.k A/A.4 A/A.1005 BAND=26 SIZE=${YSIZE} ${XSIZE} LATLON=${LAT} ${LON} \
        PRO=${PROJECTION} RES=${RES}
   else
     imgremap.k A/A.4 A/A.1001 BAND=1 SIZE=${YSIZE} ${XSIZE} LATLON=${LAT} ${LON} \
        PRO=${PROJECTION} RES=${RES} SPLINE=10 SMO=YES
     imgremap.k A/A.4 A/A.1002 BAND=4 SIZE=${YSIZE} ${XSIZE} LATLON=${LAT} ${LON} \
        PRO=${PROJECTION} RES=${RES} SPLINE=10 SMO=YES
     imgremap.k A/A.4 A/A.1003 BAND=3 SIZE=${YSIZE} ${XSIZE} LATLON=${LAT} ${LON} \
        PRO=${PROJECTION} RES=${RES} SPLINE=10 SMO=YES
     imgremap.k A/A.4 A/A.1004 BAND=7 SIZE=${YSIZE} ${XSIZE} LATLON=${LAT} ${LON} \
        PRO=${PROJECTION} RES=${RES} SPLINE=10 SMO=YES
     imgremap.k A/A.4 A/A.1005 BAND=26 SIZE=${YSIZE} ${XSIZE} LATLON=${LAT} ${LON} \
        PRO=${PROJECTION} RES=${RES} SPLINE=10 SMO=YES
   fi

   sf.k 1
   eg.k
   eu.k
   imgdisp.k A/A.1001 LATLON=${LAT} ${LON} SU=TC
   gu.k MAKE 1 WHITE
   map.k LIN=X ${MCY}
   map.k LALO LABEL=EASTPOS LIN=X ${MCY}
   eu.k REST DBRGB.ET
frmlabel.k \"MODIS $SATMC Band 31 \(11 micron\) $QUICKLD ${HR}${MIN}UTC   CIMSS/NASA
   za.k 5 12 POS=$(( MCY + 10 )) 25 FONT=HELBO \"MODIS True Color \(.67 r, .55 g, .46 b micron\) 
   frmlabel.k \"MODIS $SATMC  $QUICKLD ${HR}${MIN}UTC   CIMSS/NASA
   
   sf.k 2
   eg.k
   eu.k
   imgdisp.k A/A.1002 LATLON=${LAT} ${LON} SU=TC
   gu.k MAKE 1 WHITE
   map.k LIN=X ${MCY}
   map.k LALO LABEL=EASTPOS LIN=X ${MCY}
   eu.k REST DBRGB.ET
   za.k 5 12 POS=$(( MCY + 10 )) 25 FONT=HELBO \"MODIS True Color \(.67 r, .55 g, .46 b micron\) 
   frmlabel.k \"MODIS $SATMC  $QUICKLD ${HR}${MIN}UTC   CIMSS/NASA
   
   sf.k 3
   eg.k
   eu.k
   imgdisp.k A/A.1003 LATLON=${LAT} ${LON} SU=TC
   gu.k MAKE 1 WHITE
   map.k LIN=X ${MCY}
   map.k LALO LABEL=EASTPOS LIN=X ${MCY}
   eu.k REST DBRGB.ET
   za.k 5 12 POS=$(( MCY + 10 )) 25 FONT=HELBO \"MODIS True Color \(.67 r, .55 g, .46 b micron\) 
   frmlabel.k \"MODIS $SATMC  $QUICKLD ${HR}${MIN}UTC   CIMSS/NASA

   sf.k 1
   sf.k 2
   sf.k 3
   combine.k 0 1 2 3 VIEW=NO
   frmsave.k 0 ${SATMC}_${DDMMYY}_${HR}${MIN}_RGB TYPE=COMBINE
   if [ $? == 0 ] ; then
      /bin/mv ${MCIN}/${SATMC}_${DDMMYY}_${HR}${MIN}_RGB.JPG $OUTIMAGE
   fi
  
   sf.k 1
   eg.k
   eu.k
   imgdisp.k A/A.1001 LATLON=${LAT} ${LON} SU=TC
   gu.k MAKE 1 WHITE
   map.k 
   map.k LALO LABEL=EASTPOS
   eu.k REST DBRGB.ET
   frmlabel.k \"MODIS $SATMC Band 1 \(.65 micron\) $QUICKLD ${HR}${MIN}UTC   CIMSS/NASA
   frmsave.k 1 ${SATMC}_${DDMMYY}_${HR}${MIN}_Band_01
   if [ $? == 0 ] ; then
      /bin/mv ${MCIN}/${SATMC}_${DDMMYY}_${HR}${MIN}_Band_01.GIF $OUTIMAGE
   fi

   sf.k 1
   eg.k
   eu.k
   imgdisp.k A/A.1004 LATLON=${LAT} ${LON}
   gu.k MAKE 1 WHITE
   map.k 
   map.k LALO LABEL=EASTPOS
   eu.k REST B7.ET
   frmlabel.k \"MODIS $SATMC Band 7 \(2.1 micron\) $QUICKLD ${HR}${MIN}UTC   CIMSS/NASA
   frmsave.k 1 ${SATMC}_${DDMMYY}_${HR}${MIN}_Band_07
   if [ $? == 0 ] ; then
      /bin/mv ${MCIN}/${SATMC}_${DDMMYY}_${HR}${MIN}_Band_07.GIF $OUTIMAGE
   fi

   sf.k 1
   eg.k
   eu.k
   imgdisp.k A/A.1005 LATLON=${LAT} ${LON}
   gu.k MAKE 1 WHITE
   map.k 
   map.k LALO LABEL=EASTPOS
   eu.k REST B26.ET
   frmlabel.k \"MODIS $SATMC Band 26 \(1.38 micron\) $QUICKLD ${HR}${MIN}UTC   CIMSS/NASA
   frmsave.k 1 ${SATMC}_${DDMMYY}_${HR}${MIN}_Band_26
   if [ $? == 0 ] ; then
     /bin/mv ${MCIN}/${SATMC}_${DDMMYY}_${HR}${MIN}_Band_26.GIF $OUTIMAGE
   fi
   imgdel.k A/A.1001 1005

#  End Visible 1km band if statement
   fi
   imgdel.k A/A.4


   # Make Aerosol Product (Day only) Image
   #######################################################################
   dsserve.k ADD L${SATMC}/AOD MOD4 TYPE=IMAGE DIR="${MCDATA}/*.mod04.hdf"
   imglist.k L${SATMC}/AOD.ALL
   # Display Image
   sf.k 1
   eg.k
   eu.k
   imgoper.k L${SATMC}/AOD A/A.4 DAY=${DATE} TIME=${PASSTIME} BAND=1 SIZE=ALL ZER=DATA \
     UNIT=RAW PROD=MOD4 SCALE=0001 2000 5 205 

   if [ $? == 0 ] ; then
     echo "Found MODIS Aerosol product file to process"
     if [[ ${PROJECTION} == "PS" ]] ; then
       imgremap.k A/A.4 A/A.1001 BAND=1 SIZE=${YSIZE} ${XSIZE} LATLON=${LAT} ${LON} \
         PRO=${PROJECTION} RES=${RES}
     else
       imgremap.k A/A.4 A/A.1001 BAND=1 SIZE=${YSIZE} ${XSIZE} LATLON=${LAT} ${LON} \
         PRO=${PROJECTION} RES=${RES} SPLINE=10 SMO=YES
     fi
     prdutil.k ADD A/A.1001 2 AOD 0001 2000 5 205 t 1000 -999  
     imgdisp.k A/A.1001 LATLON=${LAT} ${LON} SU=DBAOD
     map.k ELE=X ${MCX2}
     map.k LALO LABEL=EASTPOS ELE=X ${MCX2}
     bar.k LINT=0.1 SU=DBAOD COL=1 
     eu.k REST DBAOD
     frmlabel.k \"MODIS $SATMC Aerosol Optical Depth $QUICKLD ${HR}${MIN}UTC   CIMSS/NASA
     frmsave.k 1 ${SATMC}_${DDMMYY}_${HR}${MIN}_AerosolOpticalDepth
     if [ $? == 0 ] ; then
        /bin/mv ${MCIN}/${SATMC}_${DDMMYY}_${HR}${MIN}_AerosolOpticalDepth.GIF $OUTIMAGE
     fi
     imgdel.k A/A.1001
   fi

fi

#  Remove the AREA files
imgdel.k A/A.1 4
imgdel.k A/A.1001 1005

exit
EOF

# Remove the RESOLV.SRV and the MCTABLE.TXT files
/bin/rm ${MC}/data/RESOLV.SRV
/bin/rm ${MC}/data/MCTABLE.TXT

# Print end message for processing log
echo
echo ">>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>"
echo "Finished MODIS product image McIDAS processing at "`/bin/date`

# Exit gracefully
exit 0
