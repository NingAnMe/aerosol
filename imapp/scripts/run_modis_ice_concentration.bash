#!/bin/bash
#
#-----------------------------------------------------------------------
# Run the IMAPP MODIS ice mask and ice concentration software. This
# software was created by Yinghui Liu, University of Wisconsin-Madison,
# CIMSS/SSEC, under the directionn of Dr. Jeffrey Key, NOAA/NESDIS/ASPB.
# 
#-----------------------------------------------------------------------
# This Script runs the MODIS Ice Mask and Ice Concentration software to
# create a binary output file, and then runs a second executable to 
# convert the binary file into an HDF4 output file.
#
# The converter F77 code was written by Kathy Strabala.
#
#    KIS   20 April 2011
#-----------------------------------------------------------------------

#-------------------------------------------------------------------------------
# Check arguments
if [ "$#" -lt 4 ] || [ "$#" -gt 4 ]; then
  echo "Usage: run_modis_ice_concentration.bash FIL1KM FILMASK FILGEO OUTDIR"
  echo "where"
  echo "FIL1KM is MODIS L1B 1000 meter resolution HDF image file"
  echo "FILMASK is MODIS L1B 1000 cloud mask HDF file"
  echo "FILGEO is MODIS L1B 1000 geolocation HDF file"
  echo "OUTDIR is the directory where products will be stored"
  exit 1
fi
#-------------------------------------------------------------------------------

# Extract file names
FIL1KM=$1
FILMASK=$2
FILGEO=$3
OUTDIR=$4

#  Write out arguments
echo "  "
echo "Inputs for the MODIS Snow Mask retrieval software"
echo " Input MODIS L1B 1KM file: " $FIL1KM
echo " Input MODIS Cloud Mask file: " $FILMASK
echo " Input MODIS Geolocation file: " $FILGEO
echo " Output File Directory: "$OUTDIR
echo "  "

# Set root of output file name (e.g. 't1.02001.1815')
ROOT=`basename $FIL1KM | cut -d. -f1-3`

# Print start message for processing log
echo ">>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>"
echo "Started MODIS ice concentration processing at "`date`

#
# Set output file names
OUTFIL=$ROOT.icecon.bin
OUTFILHDF=$ROOT.icecon.hdf

# Run the snow mask
ice_concentration.exe $FIL1KM $FILMASK $FILGEO $OUTFIL
if [ $? != 0 ] ; then
   echo "Problems running the MODIS ice mask and ice concentration software."
   echo "Terminating."
   exit 1
else
   echo "  "
   echo " Finished IMAPP MODIS Ice Mask and Ice Concentration retrievals."
   echo " "
fi


# Create an HDF4 version of the file from the binarhy
convert_icecon_tohdf.exe $FIL1KM $OUTFIL $OUTFILHDF
if [ $? != 0 ] ; then
   echo "Problems converting the MODIS ice concentration binary file to HDF."
   echo "Terminating."
   exit 1
fi

# Move the output binary and HDF4 files into the output directory.
/bin/mv $OUTFIL $OUTFILHDF $OUTDIR

echo ">>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>"
echo "Finished MODIS ice concentration processing at "`date`
echo " "
# ---------------------------------------------------------------------------

# Exit gracefully
exit 0 
