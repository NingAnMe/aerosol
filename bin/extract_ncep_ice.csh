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
# -------------------------------------------------------------------------------
# Extract a binary array from a NCEP ice concentration GRIB file on a
# global equal angle grid at 0.5 degree resolution. NCEP ice concentration files
# are available from
# ftp://ftp.ncep.noaa.gov/pub/data1/fnl/ (e.g., gdas1.T06Z.engicegrb)
#
# Each output file contains 720x360 32-bit float values.
#
# Requires the 'wgrib' utility available from
# http://wesley.wwb.noaa.gov/
# -------------------------------------------------------------------------------

# You must set the environmental variable MODIS_L2_HOME prior to execution

# Check arguments
if ($#argv != 2) then
  echo "Usage: extract_ncep_ice.csh ICEFILE OUTFILE"
  exit(1)
endif
if (! -e $argv[1]) then
  echo "Input file does not exist: " $argv[1]
  exit(1)
endif

# Extract the named parameter from the GRIB file
set PARAM = "ICEC:MSL"
${MODIS_L2_HOME}/bin/wgrib -s $argv[1] | fgrep "$PARAM" | ${MODIS_L2_HOME}/bin/wgrib $argv[1] -i -bin -nh -o $argv[2] > /dev/null
if ($status != 0) then
  echo "Error extracting parameter "$PARAM
  exit(1)
endif

# Exit cleanly
exit(0)
