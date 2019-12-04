#!/bin/bash
#
#-----------------------------------------------------------------------
# Run the IMAPP MODIS snow mask software, that was written by
# William Straka, University of Wisconsin-Madison, CIMSS/SSEC.
# It is a stand alone technique taken from the MODIS Cloud Mask
# processing, which utilities the Normalized Different Snow
# Index (NDSI), Salomonson, V. V. and Appel, I., 2004: Estimating 
# fractional snow cover from MODIS using the normalized difference 
# snow index.  Romote Sensing of the Environment, Vol. 89, Issue 3,
# pp. 351-360.
# 
#
# This script requires use of the HDF " ncdump " utility .
#  Please visit the HDF Group website for more information about the
#  HDF utilities: http://www.hdfgroup.org/products/hdf4_tools/  
#
#-----------------------------------------------------------------------
# This Script runs the MODIS NDSI code and the code that converts the binary
# file into an HDF file.  The NDSI C code was written by William Straka 
# III.
#
# The converter F77 code was written by Kathy Strabala.
#
#    KIS   6 February 2008
#-----------------------------------------------------------------------

#-------------------------------------------------------------------------------
# Check arguments
if [ "$#" -lt 4 ] || [ "$#" -gt 4 ]; then
  echo "Usage: run_modis_snowmask.bash SAT FIL1KM MASK OUTDIR"
  echo "where"
  echo "SAT is the satellite platform (Aqua or Terra)"
  echo "FIL1KM is MODIS L1B 1000 meter resolution HDF image file"
  echo "MASK is MODIS L1B 1000 cloud mask HDF file"
  echo "OUTDIR is the directory where products will be stored"
  exit 1
fi
#-------------------------------------------------------------------------------

# Extract file names
SAT=$1
FIL1KM=$2
MASK=$3
OUTDIR=$4

#  Write out arguments
echo "  "
echo "Inputs for the MODIS Snow Mask retrieval software"
echo " NASA Satellite name: "$SAT
echo " Input MODIS L1B 1KM file: " $FIL1KM
echo " Input MODIS Cloud Mask file: " $MASKFIL
echo " Output File Directory: "$OUTDIR
echo "  "

# Set root of output file name (e.g. 't1.02001.1815')
ROOT=`basename $FIL1KM | cut -d. -f1-3`

# Print start message for processing log
echo ">>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>"
echo "Started MODIS snow mask processing at "`date`

#
# Set output file names
OUTFIL=$ROOT.snowmask.bin
OUTFILHDF=$ROOT.snowmask.hdf

# Run the snow mask
snow_mask.exe $SAT $FIL1KM $MASK $OUTFIL
if [ $? != 0 ] ; then
   echo "Problems running the MODIS snow mask retrieval software."
   echo "Terminating."
   exit 1
else
   echo "  "
   echo " Finished Snow Mask retrievals."
   echo " "
fi


# Copy the HDF file to the output directory.
convert_ndsi_tohdf.exe $FIL1KM $OUTFIL $OUTFILHDF
if [ $? != 0 ] ; then
   echo "Problems converting the MODIS snow mask binary file to HDF."
   echo "Terminating."
   exit 1
fi

# Move the output binary and HDF files into the output directory.
/bin/mv $OUTFIL $OUTFILHDF $OUTDIR

echo ">>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>"
echo "Finished MODIS snow mask processing at "`date`
echo " "
# ---------------------------------------------------------------------------

# Exit gracefully
exit 0 
