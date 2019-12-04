#!/bin/bash
#
# Run the IMAPP MODIS Inversions software package, originally created
# by Yinghui Liu and William Straka of the University of Wisconsin
# Space Science and Engineering Center (SSEC) under the direction
# and funding of Dr. Jeffrey Key, NOAA/NESDIS/ASPB.
#
# This script requires use of the HDF " ncdump " utility .
#  Please visit the HDF Group website for more information about the
#  HDF utilities: http://www.hdfgroup.org/products/hdf4_tools/  
#
#------------------------------------------------------------------------------
# Check arguments
if [ "$#" -lt 4 ] || [ "$#" -gt 4 ]; then
  echo "Usage: run_modis_inversions.bash HEMI FIL1KM MASKFIL OUTDIR" 
  echo "where"
  echo "HEMI is the data Hemishpere 1 = Northern  0 = Southern "
  echo "FIL1KM is the MODIS L1B 1000 meter resolution radiance HDF file"
  echo "MASKFIL is the MODIS MOD35 cloud mask HDF file"
  echo "OUTDIR is the directory where the products will be stored"
  exit 1
fi
#------------------------------------------------------------------------------

# Extract file names
HEMI=$1
FIL1KM=$2
MASKFIL=$3
OUTDIR=$4

#  Write out arguments
echo "  "
echo "Inputs for Ice Surface Temperature Products"
echo " Hemishpere (1=Northern, 0=Southern):" $HEMI
echo " Input MODIS L1B 1KM file: " $FIL1KM
echo " Input MODIS Cloud Mask file: " $MASKFIL
echo " Output File Directory: "$OUTDIR
echo "  "

#Link the input files to this directory
if [ ! -e $FIL1KM ] ; then
   echo "Invalid MODIS 1KM file: " $FIL1KM
   exit 1
fi

if [ ! -e $MASKFIL ] ; then
   echo "Invalid MODIS Cloud Mask file: " $MASKFIL
   exit 1
fi

SHORTFIL1KM=`basename $FIL1KM`
SHORTMASKFIL=`basename $MASKFIL`

ln -s $FIL1KM $SHORTFIL1KM
ln -s $MASKFIL $SHORTMASKFIL


# Set root of output file name (e.g. 't1.02001.1815')
ROOT=`basename $FIL1KM | cut -d. -f1-3`

# Print start message for processing log
echo " "
echo ">>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>"
echo "Started MODIS Inversions software processing at "`date`
echo ""

# Run the temperature inversions algorithm
inversions.exe $HEMI $SHORTFIL1KM $SHORTMASKFIL 
if [ $? != 0 ] ; then
   echo "Problems running the temperature inversions retrieval"
   echo "Terminating"
   exit 1
else
   echo "  "
   echo " Finished temperature inversions retrievals"
   echo " "
fi

# Rename the output file and move it 
# to the output directory 

mv invdepth.dat $ROOT.inversion_depth.bin
mv invstrength.dat $ROOT.inversion_strength.bin

# Convert the binary files into one HDF4 file
convert_inversions_tohdf.exe $SHORTFIL1KM $ROOT.inversion_depth.bin $ROOT.inversion_strength.bin $ROOT.inversion.hdf
if [ $? != 0 ] ; then
   echo "Problems converting the temperature inversions file to HDF4"
   echo "Terminating"
   exit 1
fi

# Move the file to the output file directory
mv $ROOT.inversion_depth.bin $ROOT.inversion_strength.bin $ROOT.inversion.hdf $OUTDIR/.

# Print end message for processing log
echo ""
echo ">>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>"
echo "Finished MODIS temperature inversions processing at "`date`

# Exit gracefully
exit 0
