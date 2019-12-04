#!/bin/csh -f

#-----------------------------------------------------------------------
#Copyright (C) 2000,  Space Science and Engineering Center, University
#of Wisconsin-Madison, Madison WI.
#      
#This program is free software; you can redistribute it and/or modify
#it under the terms of the GNU General Public License as published by
#the Free Software Foundation; either version 2 of the License, or
#(at your option) any later version.
#
#This program is distributed in the hope that it will be useful,
#but WITHOUT ANY WARRANTY; without even the implied warranty of
#MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#GNU General Public License for more details.
#
#You should have received a copy of the GNU General Public License
#along with this program; if not, write to the Free Software
#Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
#--------------------------------------------------------------------------
#
#--------------------------------------------------------------------------
# Extract a binary array from a NCEP GDAS1 final analysis GRIB file on a
# global equal angle grid at 1 degree resolution. GDAS1 files are available from
# ftp://terra.ssec.wisc.edu/pub/terra/ancillary/    or 
#   (e.g., gdas1.PGrbF00.010602.18z)
# ftp://ftpprd.ncep.noaa.gov/pub/data/nccf/com/fnl/prod 
#   (e.g., gdas1.t12z.pgrbf00)
#
# Updated in July 2004 to extracting all levels
# Each output file contains 360x181x54 32-bit float values.
#
# Requires the 'wgrib' utility available from
# http://wesley.wwb.noaa.gov/
# -------------------------------------------------------------------------

# Environmental Variable MODIS_L2_HOME must be set prior to execution

# Check arguments
if ($#argv != 2) then
  echo "Usage: extract_ncep_gdas1.csh GDAS1FILE OUTFILE"
  exit(1)
endif
if (! -e $argv[1]) then
  echo "Input file does not exist: " $argv[1]
  exit(1)
endif

# Get inventory of GRIB file
${MODIS_L2_HOME}/bin/wgrib -s $argv[1] > gdas1_inventory

# Set list of parameters to extract from GRIB file
# SWS changed 10 nov 03 to include all levels from GDAS
#set LIST = ( "TMP:1000 mb" "TMP:925 mb" "TMP:850 mb" "TMP:700 mb" \
#              "TMP:500 mb" "TMP:400 mb" "TMP:300 mb" "TMP:250 mb" \
#              "TMP:200 mb" "TMP:150 mb" "TMP:100 mb"  "TMP:70 mb" \
#               "TMP:50 mb"  "TMP:30 mb"  "TMP:20 mb"  "TMP:10 mb" \
#              "RH:1000 mb"  "RH:925 mb"  "RH:850 mb"  "RH:700 mb" \
#               "RH:500 mb"  "RH:400 mb"  "RH:300 mb" \
#                "LAND:sfc" "TMP:sfc" "PRES:sfc" "PWAT:atmos col" \
#                "UGRD:10 m above gnd" "VGRD:10 m above gnd" "ICEC:sfc" )

set LIST = ( "TMP:1000 mb" "TMP:975 mb" "TMP:950 mb" "TMP:925 mb" \
             "TMP:900 mb" "TMP:850 mb" "TMP:800 mb" "TMP:750 mb" \
             "TMP:700 mb" "TMP:650 mb" "TMP:600 mb" "TMP:550 mb" \
             "TMP:500 mb" "TMP:450 mb" "TMP:400 mb" "TMP:350 mb" \
             "TMP:300 mb" "TMP:250 mb" "TMP:200 mb" "TMP:150 mb" \
             "TMP:100 mb"  "TMP:70 mb" "TMP:50 mb"  "TMP:30 mb" \
             "TMP:20 mb"  "TMP:10 mb"  \
             "RH:1000 mb" "RH:975 mb" "RH:950 mb" "RH:925 mb" \
             "RH:900 mb" "RH:850 mb" "RH:800 mb" "RH:750 mb" \
             "RH:700 mb" "RH:650 mb"  "RH:600 mb" "RH:550 mb" \
             "RH:500 mb" "RH:450 mb" "RH:400 mb" "RH:350 mb" \
             "RH:300 mb" "RH:250 mb" "RH:200 mb" "RH:150 mb" \
             "RH:100 mb" \
                "LAND:sfc" "TMP:sfc" "PRES:sfc" "PWAT:atmos col" \
                "UGRD:10 m above gnd" "VGRD:10 m above gnd" "ICEC:sfc" )

# Extract all parameters in list
/bin/rm -f $argv[2]
while ($#LIST)
  fgrep "$LIST[1]" gdas1_inventory | ${MODIS_L2_HOME}/bin/wgrib $argv[1] -i -bin -nh -append -o $argv[2] > /dev/null
  if ($status != 0) then
    echo "Error extracting parameter "$LIST[1]
    exit(1)
  endif
  shift LIST
end

# Exit cleanly
/bin/rm -f gdas1_inventory
exit(0)
