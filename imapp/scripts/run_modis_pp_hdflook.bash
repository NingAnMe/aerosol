#!/bin/bash

# Script for MODIS image processing (HDFLook)
# Liam.Gumley@ssec.wisc.edu
# 29-APR-2009

# Modified for use by the polar products as part of the MODIS
# L2 IMAPP processing package
#  Kathy Strabala   January 2011

echo
echo "Creating MODIS polar image products; started at "$(date -u)

#-------------------------------------------------------------------------------
# Check arguments
#-------------------------------------------------------------------------------
# Check input arguments
if [ $# -ne 3 ] || [ "$#" -gt 3 ]; then
  echo "Usage: run_modis_pp_hdflook.bash input_file template_file OUTPUT_DIRECTORY"
  echo "where"
  echo "input_file is the full path and name of the input MODIS product HDF file"
  echo "template_file is the full path and name of the HDFLook template file"
  echo "OUTPUT_DIRECTORY is the MODIS L2 images directory"
  exit 1
fi

# Get arguments
input_file=$1
template_file=$2
OUT_DIR=$3

# Check for input files
if [ ! -r $input_file ]; then
  echo "Input file not found: "$input_file
  exit 1
fi
if [ ! -r $template_file ]; then
  echo "Template file not found: "$template_file
  exit 1
fi

out_short=`basename $input_file | cut -d. -f1-4`


#-------------------------------------------------------------------------------
# Run HDFLook image processing
#-------------------------------------------------------------------------------
# Convert backslashes so that they will be properly inserted using sed
new_input=`echo ${input_file} | awk '{gsub("/","\\\/")}1'`
# Create HDFLook script
cat $template_file | \
  sed -e "s/INPUT_FILE/$new_input/g"  |
  sed -e "s/OUTPUT_FILE/$out_short/g" > $input_file.scr

# Check to see if the HDFLook MAPS directory is available
if [ ! -d $HDFLOOKMAPS ] ; then
  echo "Cannot find the HDFLook MAPS directory.  Exiting"
  exit 1
fi

# If HDFLOOKTMP directory is not set, then point to current directory.
if [ ! -d $HDFLOOKTMP ] ; then
  export HDFLOOKTMP=.
fi

# Copy palettes to this directory
cp $MODIS_L2_HOME/scripts/palette_*.txt .

# Run HDFLook
HDFLook $input_file.scr
if [ $? -ne 0 ]; then
  echo "Error in HDFLook processing for file: "$input_file
  exit 1
fi

# move the output image to the output directory
mv ${out_short}.jpg $OUT_DIR
mv ${out_short}.tif $OUT_DIR

# Clean up
rm $input_file.scr

#-------------------------------------------------------------------------------
# EXIT
#-------------------------------------------------------------------------------
echo
echo "Finished MODIS image products at "$(date -u)
exit 0
